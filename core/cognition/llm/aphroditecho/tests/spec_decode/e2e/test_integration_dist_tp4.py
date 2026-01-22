import json
import openai
import pytest
import torch
from .conftest import run_equality_correctness_test_tp
MAIN_MODEL = 'JackFram/llama-68m'
SPEC_MODEL = 'JackFram/llama-68m'
@pytest.mark.skipif(torch.cuda.device_count() < 4, reason='Need at least 4 GPUs to run the test.')
@pytest.mark.parametrize('common_llm_kwargs', [['--enforce_eager', '--tensor-parallel-size', '4']])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [[]])
@pytest.mark.parametrize('baseline_llm_kwargs', [[]])
@pytest.mark.parametrize('test_llm_kwargs', [['--speculative_config', json.dumps({'model': f'{SPEC_MODEL}', 'num_speculative_tokens': 5, 'draft_tensor_parallel_size': 1})]])
@pytest.mark.parametrize('batch_size', [2])
@pytest.mark.parametrize('seed', [1])
def test_draft_model_tp_lt_target_model_tp4(common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size: int, seed: int):
    run_equality_correctness_test_tp(MAIN_MODEL, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size, max_output_len=32, seed=seed, temperature=0.0)
@pytest.mark.skipif(torch.cuda.device_count() < 4, reason='Need at least 4 GPUs to run the test.')
@pytest.mark.parametrize('common_llm_kwargs', [['--enforce-eager', '--tensor-parallel-size', '4']])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [[]])
@pytest.mark.parametrize('baseline_llm_kwargs', [[]])
@pytest.mark.parametrize('test_llm_kwargs', [['--speculative_config', json.dumps({'model': f'{SPEC_MODEL}', 'num_speculative_tokens': 5, 'max_model_len': 32})]])
@pytest.mark.parametrize('batch_size', [8])
@pytest.mark.parametrize('output_len', [64])
@pytest.mark.parametrize('seed', [1])
def test_skip_speculation(common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size: int, output_len: int, seed: int):
    with pytest.raises((openai.APIConnectionError, openai.InternalServerError)):
        run_equality_correctness_test_tp(MAIN_MODEL, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size, output_len, seed, temperature=0.0)