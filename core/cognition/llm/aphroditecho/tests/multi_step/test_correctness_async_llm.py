from typing import Optional
import pytest
from aphrodite.common.utils import STR_BACKEND_ENV_VAR
from ..models.utils import check_logprobs_close
from ..utils import completions_with_server_args, get_client_text_generations, get_client_text_logprob_generations
MODELS = ['JackFram/llama-160m']
NUM_SCHEDULER_STEPS = [8]
NUM_PROMPTS = [10]
DEFAULT_SERVER_ARGS: list[str] = ['--distributed-executor-backend', 'ray', '--gpu-memory-utilization', '0.85', '--swap-space', '16']
@pytest.mark.parametrize('model', MODELS)
@pytest.mark.parametrize('tp_size, pp_size', [(1, 1), (2, 2)])
@pytest.mark.parametrize('eager_mode', [False, True])
@pytest.mark.parametrize('num_scheduler_steps', NUM_SCHEDULER_STEPS)
@pytest.mark.parametrize('num_prompts', NUM_PROMPTS)
@pytest.mark.parametrize('num_logprobs', [5])
@pytest.mark.parametrize('is_async', [True])
@pytest.mark.parametrize('attention_backend', ['FLASHINFER', 'FLASH_ATTN'])
@pytest.mark.parametrize('enable_chunked_prefill', [True, False])
@pytest.mark.asyncio
async def test_multi_step(example_prompts, model: str, tp_size: int, pp_size: int, eager_mode: int, num_scheduler_steps: int, num_prompts: int, is_async: bool, num_logprobs: Optional[int], attention_backend: str, enable_chunked_prefill: bool, monkeypatch: pytest.MonkeyPatch) -> None:
    if enable_chunked_prefill and (pp_size > 1 or attention_backend != 'FLASH_ATTN'):
        pytest.skip('Multi-step with Chunked-Prefill only supportsPP=1 and FLASH_ATTN backend')
    with monkeypatch.context() as m:
        m.setenv(STR_BACKEND_ENV_VAR, attention_backend)
        prompts = example_prompts
        if len(prompts) < num_prompts:
            prompts = prompts * (num_prompts // len(prompts) + 1)
        prompts = prompts[:num_prompts]
        assert len(prompts) == num_prompts
        server_args = DEFAULT_SERVER_ARGS + ['--enforce-eager']
        ms_server_args = DEFAULT_SERVER_ARGS + ['--num-scheduler-steps', f'{num_scheduler_steps}']
        if not is_async:
            ms_server_args += ['--disable-async-output-proc']
        if eager_mode:
            ms_server_args.append('--enforce-eager')
        if enable_chunked_prefill:
            ms_server_args.append('--enable-chunked-prefill')
        distributed_args = ['--tensor-parallel-size', str(tp_size), '--pipeline-parallel-size', str(pp_size)]
        ref_completions = await completions_with_server_args(prompts, model, server_args + distributed_args, num_logprobs, max_wait_seconds=5 * 240)
        test_completions = await completions_with_server_args(prompts, model, ms_server_args + distributed_args, num_logprobs, max_wait_seconds=5 * 240)
        ref_generations = get_client_text_generations(ref_completions)
        test_generations = get_client_text_generations(test_completions)
        assert ref_generations == test_generations
        ref_text_logprobs = get_client_text_logprob_generations(ref_completions)
        test_text_logprobs = get_client_text_logprob_generations(test_completions)
        check_logprobs_close(outputs_0_lst=ref_text_logprobs, outputs_1_lst=test_text_logprobs, name_0='hf', name_1='aphrodite')
@pytest.mark.parametrize('tp_size, pp_size', [(1, 2)])
@pytest.mark.asyncio
async def test_multi_step_pp_smoke(tp_size: int, pp_size: int, monkeypatch: pytest.MonkeyPatch) -> None:
    model = 'JackFram/llama-160m'
    num_scheduler_steps = 8
    attention_backend = 'FLASH_ATTN'
    max_num_seqs = 3
    with monkeypatch.context() as m:
        m.setenv(STR_BACKEND_ENV_VAR, attention_backend)
        prompts = ['in the jtbd context whats a push?', 'in the jtbd context whats a push?', 'in the jtbd context whats a push?', 'in the jtbd context whats a push?']
        max_tokens = [10 * i for i in range(1, len(prompts) + 1)]
        assert len(prompts) == len(max_tokens)
        test_args = ['--tensor-parallel-size', str(tp_size), '--pipeline-parallel-size', str(pp_size), '--max-num-seqs', str(max_num_seqs)]
        server_args = DEFAULT_SERVER_ARGS + test_args
        ms_server_args = DEFAULT_SERVER_ARGS + ['--num-scheduler-steps', f'{num_scheduler_steps}'] + test_args
        ref_completions = await completions_with_server_args(prompts=prompts, model_name=model, server_cli_args=server_args, num_logprobs=None, max_wait_seconds=5 * 240, max_tokens=max_tokens)
        test_completions = await completions_with_server_args(prompts=prompts, model_name=model, server_cli_args=ms_server_args, num_logprobs=None, max_wait_seconds=5 * 240, max_tokens=max_tokens)
        ref_generations = get_client_text_generations(ref_completions)
        test_generations = get_client_text_generations(test_completions)
        assert ref_generations == test_generations