import copy
from typing import Optional
import pytest
from aphrodite.common.utils import STR_BACKEND_ENV_VAR
from ..models.utils import check_logprobs_close, check_outputs_equal
MODELS = ['JackFram/llama-160m']
NUM_SCHEDULER_STEPS = [8]
NUM_PROMPTS = [10]
@pytest.mark.parametrize('model', MODELS)
@pytest.mark.parametrize('dtype', ['half'])
@pytest.mark.parametrize('tp_size', [1])
@pytest.mark.parametrize('enable_chunked_prefill', [False, True])
@pytest.mark.parametrize('max_tokens', [5])
@pytest.mark.parametrize('enforce_eager', [True, False])
@pytest.mark.parametrize('num_scheduler_steps', NUM_SCHEDULER_STEPS)
@pytest.mark.parametrize('num_prompts', NUM_PROMPTS)
@pytest.mark.parametrize('num_logprobs', [None, 5])
@pytest.mark.parametrize('attention_backend', ['FLASH_ATTN', 'FLASHINFER'])
def test_multi_step_llm(hf_runner, aphrodite_runner, example_prompts, model: str, dtype: str, tp_size: int, enable_chunked_prefill: bool, max_tokens: int, enforce_eager: int, num_scheduler_steps: int, num_prompts: int, num_logprobs: Optional[int], attention_backend: str, monkeypatch: pytest.MonkeyPatch) -> None:
    with monkeypatch.context() as m:
        m.setenv(STR_BACKEND_ENV_VAR, attention_backend)
        prompts = example_prompts
        if len(prompts) < num_prompts:
            prompts = prompts * (num_prompts // len(prompts) + 1)
        prompts = prompts[:num_prompts]
        assert len(prompts) == num_prompts
        with aphrodite_runner(model, dtype=dtype, enforce_eager=enforce_eager, gpu_memory_utilization=0.7, tensor_parallel_size=tp_size, enable_chunked_prefill=enable_chunked_prefill, num_scheduler_steps=num_scheduler_steps) as aphrodite_model:
            aphrodite_outputs = aphrodite_model.generate_greedy(prompts, max_tokens) if num_logprobs is None else aphrodite_model.generate_greedy_logprobs(prompts, max_tokens, num_logprobs)
        with hf_runner(model, dtype=dtype) as hf_model:
            hf_outputs = hf_model.generate_greedy(prompts, max_tokens) if num_logprobs is None else hf_model.generate_greedy_logprobs_limit(prompts, max_tokens, num_logprobs)
        if num_logprobs is None:
            check_outputs_equal(outputs_0_lst=hf_outputs, outputs_1_lst=aphrodite_outputs, name_0='hf', name_1='aphrodite')
        else:
            check_logprobs_close(outputs_0_lst=hf_outputs, outputs_1_lst=aphrodite_outputs, name_0='hf', name_1='aphrodite')
@pytest.mark.parametrize('model', MODELS)
@pytest.mark.parametrize('dtype', ['half'])
@pytest.mark.parametrize('tp_size', [1])
@pytest.mark.parametrize('max_tokens', [5])
@pytest.mark.parametrize('enforce_eager', [True])
@pytest.mark.parametrize('num_scheduler_steps', NUM_SCHEDULER_STEPS)
@pytest.mark.parametrize('num_prompts', NUM_PROMPTS)
@pytest.mark.parametrize('num_logprobs,num_prompt_logprobs', [(5, 5)])
@pytest.mark.parametrize('attention_backend', ['FLASH_ATTN'])
def test_multi_step_llm_w_prompt_logprobs(aphrodite_runner, example_prompts, model: str, dtype: str, tp_size: int, max_tokens: int, enforce_eager: int, num_scheduler_steps: int, num_prompts: int, num_logprobs: Optional[int], num_prompt_logprobs: Optional[int], attention_backend: str, monkeypatch: pytest.MonkeyPatch) -> None:
    with monkeypatch.context() as m:
        m.setenv(STR_BACKEND_ENV_VAR, attention_backend)
        prompts = example_prompts
        if len(prompts) < num_prompts:
            prompts = prompts * (num_prompts // len(prompts) + 1)
        prompts = prompts[:num_prompts]
        assert len(prompts) == num_prompts
        with aphrodite_runner(model, dtype=dtype, enforce_eager=enforce_eager, gpu_memory_utilization=0.7, tensor_parallel_size=tp_size, num_scheduler_steps=num_scheduler_steps) as aphrodite_model:
            aphrodite_outputs = aphrodite_model.generate_greedy_logprobs(prompts, max_tokens, num_logprobs, num_prompt_logprobs=num_prompt_logprobs)
        with aphrodite_runner(model, dtype=dtype, enforce_eager=enforce_eager, gpu_memory_utilization=0.7, tensor_parallel_size=tp_size) as aphrodite_model:
            single_step_aphrodite_outputs = aphrodite_model.generate_greedy_logprobs(prompts, max_tokens, num_logprobs, num_prompt_logprobs=num_prompt_logprobs)
        check_logprobs_close(outputs_0_lst=single_step_aphrodite_outputs, outputs_1_lst=aphrodite_outputs, name_0='hf', name_1='aphrodite')
@pytest.mark.parametrize('model', MODELS)
@pytest.mark.parametrize('dtype', ['half'])
@pytest.mark.parametrize('tp_size', [1])
@pytest.mark.parametrize('max_tokens', [5])
@pytest.mark.parametrize('enforce_eager', [True])
@pytest.mark.parametrize('num_scheduler_steps', NUM_SCHEDULER_STEPS)
@pytest.mark.parametrize('num_prompts', NUM_PROMPTS)
@pytest.mark.parametrize('num_logprobs', [None, 5])
@pytest.mark.parametrize('attention_backend', ['FLASH_ATTN'])
def test_multi_step_llm_chunked_prefill_prefix_cache(aphrodite_runner, example_prompts, model: str, dtype: str, tp_size: int, max_tokens: int, enforce_eager: int, num_scheduler_steps: int, num_prompts: int, num_logprobs: Optional[int], attention_backend: str, monkeypatch: pytest.MonkeyPatch) -> None:
    with monkeypatch.context() as m:
        m.setenv(STR_BACKEND_ENV_VAR, attention_backend)
        assert len(example_prompts) >= 2
        challenge_prompts = copy.deepcopy(example_prompts)
        challenge_prompts[0] = 'Aphrodite is a high-throughput and memory-efficient inference and serving engine for LLMs.\n'
        challenge_prompts[1] = 'Briefly describe the major milestones in the development of artificial intelligence from 1950 to 2020.\n'
        if len(challenge_prompts) < num_prompts:
            challenge_prompts = challenge_prompts * (num_prompts // len(challenge_prompts) + 1)
        challenge_prompts = challenge_prompts[:num_prompts]
        assert len(challenge_prompts) == num_prompts
        with aphrodite_runner(model, dtype=dtype, enforce_eager=enforce_eager, gpu_memory_utilization=0.7, tensor_parallel_size=tp_size, num_scheduler_steps=num_scheduler_steps, max_model_len=48, max_num_batched_tokens=48, max_num_seqs=4, block_size=16) as aphrodite_model:
            outputs_baseline = aphrodite_model.generate_greedy(challenge_prompts, max_tokens) if num_logprobs is None else aphrodite_model.generate_greedy_logprobs(challenge_prompts, max_tokens, num_logprobs)
        with aphrodite_runner(model, dtype=dtype, enforce_eager=enforce_eager, gpu_memory_utilization=0.7, tensor_parallel_size=tp_size, enable_chunked_prefill=True, enable_prefix_caching=True, num_scheduler_steps=num_scheduler_steps, max_model_len=48, max_num_batched_tokens=48, max_num_seqs=4, block_size=16) as aphrodite_model:
            outputs_w_features = aphrodite_model.generate_greedy(challenge_prompts, max_tokens) if num_logprobs is None else aphrodite_model.generate_greedy_logprobs(challenge_prompts, max_tokens, num_logprobs)
        if num_logprobs is None:
            check_outputs_equal(outputs_0_lst=outputs_baseline, outputs_1_lst=outputs_w_features, name_0='multi-step', name_1='multi-step+features')
        else:
            check_logprobs_close(outputs_0_lst=outputs_baseline, outputs_1_lst=outputs_w_features, name_0='multi-step', name_1='multi-step+features')