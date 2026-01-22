import pytest
from .conftest import run_equality_correctness_test
MAIN_MODEL = 'luccafong/deepseek_mtp_main_random'
MAX_SPEC_TOKENS = 1
PRECISION = 'bfloat16'
@pytest.mark.parametrize('common_llm_kwargs', [{'enforce_eager': True, 'disable_log_stats': False, 'dtype': PRECISION, 'model_name': MAIN_MODEL, 'gpu_memory_utilization': 0.85}])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [{}])
@pytest.mark.parametrize('baseline_llm_kwargs', [{}])
@pytest.mark.parametrize('test_llm_kwargs', [{'speculative_config': {'num_speculative_tokens': MAX_SPEC_TOKENS}}])
@pytest.mark.parametrize('output_len', [128])
@pytest.mark.parametrize('batch_size', [1, 32])
@pytest.mark.parametrize('seed', [1])
def test_mtp_e2e_greedy_correctness(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size: int, output_len: int, seed: int):
    run_equality_correctness_test(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size, output_len, seed)
@pytest.mark.parametrize('common_llm_kwargs', [{'enforce_eager': True, 'disable_log_stats': False, 'dtype': PRECISION, 'model_name': MAIN_MODEL, 'gpu_memory_utilization': 0.85}])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [{}])
@pytest.mark.parametrize('baseline_llm_kwargs', [{}])
@pytest.mark.parametrize('test_llm_kwargs', [{'speculative_config': {'num_speculative_tokens': MAX_SPEC_TOKENS, 'disable_logprobs': False}}, {'speculative_config': {'num_speculative_tokens': MAX_SPEC_TOKENS, 'disable_logprobs': True}}])
@pytest.mark.parametrize('output_len', [128])
@pytest.mark.parametrize('batch_size', [8])
@pytest.mark.parametrize('seed', [1])
@pytest.mark.parametrize('logprobs', [1, 6])
def test_mtp_e2e_greedy_logprobs(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size: int, output_len: int, seed: int, logprobs: int):
    run_equality_correctness_test(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size, output_len, seed, logprobs=logprobs, prompt_logprobs=logprobs, disable_logprobs=test_llm_kwargs['speculative_config']['disable_logprobs'])
@pytest.mark.parametrize('common_llm_kwargs', [{'enforce_eager': False, 'disable_log_stats': False, 'dtype': PRECISION, 'model_name': MAIN_MODEL, 'gpu_memory_utilization': 0.85}])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [{}])
@pytest.mark.parametrize('baseline_llm_kwargs', [{}])
@pytest.mark.parametrize('test_llm_kwargs', [{'speculative_config': {'num_speculative_tokens': MAX_SPEC_TOKENS}}])
@pytest.mark.parametrize('output_len', [128])
@pytest.mark.parametrize('batch_size', [1, 32])
@pytest.mark.parametrize('seed', [1])
def test_mtp_e2e_greedy_correctness_cuda_graph(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size: int, output_len: int, seed: int):
    run_equality_correctness_test(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size, output_len, seed)
@pytest.mark.parametrize('common_llm_kwargs', [{'block_size': 8, 'num_gpu_blocks_override': 2 + 256 // 8, 'max_model_len': (2 + 256 // 8) * 8, 'enforce_eager': True, 'dtype': PRECISION, 'model_name': MAIN_MODEL, 'gpu_memory_utilization': 0.9}])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [{}])
@pytest.mark.parametrize('baseline_llm_kwargs', [{}])
@pytest.mark.parametrize('test_llm_kwargs', [{'speculative_config': {'num_speculative_tokens': MAX_SPEC_TOKENS}}])
@pytest.mark.parametrize('output_len', [128])
@pytest.mark.parametrize('batch_size', [4])
@pytest.mark.parametrize('seed', [1])
def test_mtp_e2e_greedy_correctness_with_preemption(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size: int, output_len: int, seed: int):
    run_equality_correctness_test(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size, output_len, seed)
@pytest.mark.parametrize('common_llm_kwargs', [{'enforce_eager': True, 'dtype': PRECISION, 'model_name': MAIN_MODEL, 'gpu_memory_utilization': 0.9}])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [{}])
@pytest.mark.parametrize('baseline_llm_kwargs', [{}])
@pytest.mark.parametrize('test_llm_kwargs', [{'speculative_config': {'num_speculative_tokens': k}} for k in range(1, 1 + MAX_SPEC_TOKENS)])
@pytest.mark.parametrize('batch_size', [2])
@pytest.mark.parametrize('output_len', [32])
@pytest.mark.parametrize('seed', [1])
def test_mtp_different_k(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size: int, output_len: int, seed: int):
    run_equality_correctness_test(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size, output_len, seed)
@pytest.mark.parametrize('common_llm_kwargs', [{'enforce_eager': True, 'dtype': PRECISION, 'model_name': MAIN_MODEL, 'gpu_memory_utilization': 0.9}])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [{}])
@pytest.mark.parametrize('baseline_llm_kwargs', [{}])
@pytest.mark.parametrize('test_llm_kwargs', [{'speculative_config': {'num_speculative_tokens': MAX_SPEC_TOKENS, 'disable_by_batch_size': 4}}])
@pytest.mark.parametrize('batch_size', [1, 5])
@pytest.mark.parametrize('output_len', [32])
@pytest.mark.parametrize('seed', [1])
def test_mtp_disable_queue(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size: int, output_len: int, seed: int):
    run_equality_correctness_test(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size, output_len, seed)
if __name__ == '__main__':
    import pytest
    pytest.main([__file__])