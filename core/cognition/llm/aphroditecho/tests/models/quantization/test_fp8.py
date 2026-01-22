import pytest
from tests.quantization.utils import is_quant_method_supported
from aphrodite.platforms import current_platform
from aphrodite.utils import STR_BACKEND_ENV_VAR
from ..utils import check_logprobs_close
@pytest.mark.skipif(not is_quant_method_supported('fp8'), reason='fp8 is not supported on this GPU type.')
@pytest.mark.parametrize('kv_cache_dtype,base_model,test_model', [('fp8_e4m3', 'meta-llama/Llama-3.2-1B-Instruct', 'nm-testing/Llama-3.2-1B-Instruct-FP8-KV'), ('fp8_e5m2', 'meta-llama/Llama-3.2-1B-Instruct', 'meta-llama/Llama-3.2-1B-Instruct'), ('fp8_e4m3', 'meta-llama/Llama-3.2-1B-Instruct', 'meta-llama/Llama-3.2-1B-Instruct')])
@pytest.mark.parametrize('max_tokens', [4])
@pytest.mark.parametrize('enforce_eager', [True])
@pytest.mark.parametrize('backend', ['FLASH_ATTN', 'XFORMERS', 'FLASHINFER'])
@pytest.mark.parametrize('tensor_parallel_size', [1])
@pytest.mark.parametrize('disable_async_output_proc', [True])
def test_models(aphrodite_runner, example_prompts, kv_cache_dtype: str, base_model: str, test_model: str, max_tokens: int, enforce_eager: bool, backend: str, tensor_parallel_size: int, disable_async_output_proc: bool, monkeypatch: pytest.MonkeyPatch) -> None:
    if backend == 'FLASHINFER' and current_platform.is_rocm():
        pytest.skip('Flashinfer does not support ROCm/HIP.')
    if kv_cache_dtype == 'fp8_e5m2' and current_platform.is_rocm():
        pytest.skip(f'{kv_cache_dtype} is currently not supported on ROCm/HIP.')
    with monkeypatch.context() as m:
        m.setenv('TOKENIZERS_PARALLELISM', 'true')
        m.setenv(STR_BACKEND_ENV_VAR, backend)
        MAX_MODEL_LEN = 1024
        NUM_LOG_PROBS = 8
        with aphrodite_runner(base_model, max_model_len=MAX_MODEL_LEN, tensor_parallel_size=tensor_parallel_size, enforce_eager=enforce_eager, kv_cache_dtype='auto', disable_async_output_proc=disable_async_output_proc) as aphrodite_model:
            baseline_outputs = aphrodite_model.generate_greedy_logprobs(example_prompts, max_tokens, NUM_LOG_PROBS)
        with aphrodite_runner(test_model, max_model_len=MAX_MODEL_LEN, tensor_parallel_size=tensor_parallel_size, enforce_eager=enforce_eager, kv_cache_dtype=kv_cache_dtype, disable_async_output_proc=disable_async_output_proc) as aphrodite_model:
            test_outputs = aphrodite_model.generate_greedy_logprobs(example_prompts, max_tokens, NUM_LOG_PROBS)
        check_logprobs_close(outputs_0_lst=baseline_outputs, outputs_1_lst=test_outputs, name_0='fp16_kv_cache', name_1='fp8_kv_cache')
@pytest.mark.cpu_model
@pytest.mark.skipif(not current_platform.is_cpu(), reason='test for the CPU backend.')
@pytest.mark.parametrize('kv_cache_dtype,base_model,test_model', [('fp8_e5m2', 'meta-llama/Llama-3.2-1B-Instruct', 'meta-llama/Llama-3.2-1B-Instruct')])
@pytest.mark.parametrize('max_tokens', [4])
@pytest.mark.parametrize('disable_async_output_proc', [True])
def test_cpu_models(aphrodite_runner, example_prompts, kv_cache_dtype: str, base_model: str, test_model: str, max_tokens: int, disable_async_output_proc: bool, monkeypatch: pytest.MonkeyPatch) -> None:
    with monkeypatch.context() as m:
        m.setenv('TOKENIZERS_PARALLELISM', 'true')
        MAX_MODEL_LEN = 1024
        NUM_LOG_PROBS = 8
        with aphrodite_runner(base_model, max_model_len=MAX_MODEL_LEN, dtype='bfloat16', kv_cache_dtype='auto', disable_async_output_proc=disable_async_output_proc) as aphrodite_model:
            baseline_outputs = aphrodite_model.generate_greedy_logprobs(example_prompts, max_tokens, NUM_LOG_PROBS)
        with aphrodite_runner(test_model, max_model_len=MAX_MODEL_LEN, dtype='bfloat16', kv_cache_dtype=kv_cache_dtype, disable_async_output_proc=disable_async_output_proc) as aphrodite_model:
            test_outputs = aphrodite_model.generate_greedy_logprobs(example_prompts, max_tokens, NUM_LOG_PROBS)
        check_logprobs_close(outputs_0_lst=baseline_outputs, outputs_1_lst=test_outputs, name_0='bf16_kv_cache', name_1='fp8_kv_cache')