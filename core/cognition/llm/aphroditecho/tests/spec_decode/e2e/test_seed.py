import pytest
from .conftest import run_equality_correctness_test
MAIN_MODEL = 'JackFram/llama-68m'
SPEC_MODEL = 'JackFram/llama-160m'
@pytest.mark.parametrize('common_llm_kwargs', [{'model_name': 'JackFram/llama-68m', 'enforce_eager': True, 'speculative_config': {'model': 'JackFram/llama-160m', 'num_speculative_tokens': 3}}])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [{}])
@pytest.mark.parametrize('baseline_llm_kwargs', [{'seed': 1}])
@pytest.mark.parametrize('test_llm_kwargs', [{'seed': 5}])
@pytest.mark.parametrize('batch_size', [1, 8, 32])
@pytest.mark.parametrize('temperature', [0.1, 1.0])
@pytest.mark.parametrize('output_len', [20])
def test_seeded_consistency(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size: int, temperature: float, output_len: int):
    run_equality_correctness_test(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size, max_output_len=output_len, temperature=temperature, disable_seed=False)
    with pytest.raises(AssertionError):
        run_equality_correctness_test(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size, max_output_len=output_len, temperature=temperature, disable_seed=True)