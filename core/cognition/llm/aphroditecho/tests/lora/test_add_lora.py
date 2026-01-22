import asyncio
import time
import pytest
import aphrodite.common.envs as env
from aphrodite.engine.args_tools import AsyncEngineArgs
from aphrodite.inputs import TextPrompt
from aphrodite.lora.request import LoRARequest
from aphrodite.common.sampling_params import SamplingParams
from aphrodite.common.utils import merge_async_iterators
MODEL_PATH = 'THUDM/chatglm3-6b'
LORA_RANK = 64
DEFAULT_MAX_LORAS = 4 * 3
@pytest.fixture(autouse=True)
def v1(run_with_both_engines_lora):
    pass
def get_lora_requests(lora_path) -> list[LoRARequest]:
    lora_requests: list[LoRARequest] = [LoRARequest(lora_name=f'{i}', lora_int_id=i, lora_path=lora_path) for i in range(1, DEFAULT_MAX_LORAS + 1)]
    return lora_requests
async def requests_processing_time(llm, lora_requests: list[LoRARequest]) -> float:
    sampling_params = SamplingParams(n=1, temperature=0.0, top_p=1.0, ignore_eos=True, max_tokens=1)
    generators = []
    start = time.perf_counter()
    for lora_request in lora_requests:
        lora_int_id = lora_request.lora_int_id
        generator = llm.generate(prompt=TextPrompt(prompt=f'hello {lora_int_id}', multi_modal_data=None), sampling_params=sampling_params, lora_request=lora_request, request_id=f'test{lora_int_id}')
        generators.append(generator)
    all_gens = merge_async_iterators(*generators)
    async for i, res in all_gens:
        pass
    end = time.perf_counter()
    return end - start
@pytest.mark.asyncio
async def test_add_lora(chatglm3_lora_files):
    lora_requests: list[LoRARequest] = get_lora_requests(chatglm3_lora_files)
    max_loras = len(set([lr.lora_int_id for lr in lora_requests]))
    engine_args = AsyncEngineArgs(model=MODEL_PATH, enable_lora=True, max_loras=max_loras, max_lora_rank=LORA_RANK, max_model_len=128, gpu_memory_utilization=0.8, trust_remote_code=True, enforce_eager=True)
    import importlib
    import aphrodite.engine.async_aphrodite
    importlib.reload(aphrodite.engine.async_aphrodite)
    from aphrodite.endpoints.openai.api_server import build_async_engine_client_from_engine_args
    part_size = len(lora_requests) // 3
    dummy_run_requests = lora_requests[:part_size]
    warmup_run_requests = lora_requests[part_size:part_size * 2]
    cold_run_requests = lora_requests[part_size * 2:]
    async with build_async_engine_client_from_engine_args(engine_args) as llm:
        await requests_processing_time(llm, dummy_run_requests)
        add_lora_tasks = [llm.add_lora(lr) for lr in warmup_run_requests]
        add_lora_results = await asyncio.gather(*add_lora_tasks)
        if env.APHRODITE_USE_V1:
            assert all(add_lora_results)
        else:
            pass
        time_with_add_lora = await requests_processing_time(llm, warmup_run_requests)
        time_cold_start = await requests_processing_time(llm, cold_run_requests)
    print(f'time hot-start {time_with_add_lora} vs time cold-start {time_cold_start} ')
    assert time_with_add_lora < time_cold_start, f'time_with_add_lora={time_with_add_lora}, time_cold_start={time_cold_start}The engine request processing time with LoRA pre-loading must be less than the version that does on-demand LoRA loading.'