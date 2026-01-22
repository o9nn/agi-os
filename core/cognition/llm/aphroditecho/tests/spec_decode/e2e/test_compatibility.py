import pytest
from aphrodite import SamplingParams
from .conftest import get_output_from_llm_generator
@pytest.mark.parametrize('common_llm_kwargs', [{'model': 'meta-llama/Llama-3.2-1B-Instruct'}])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [{'speculative_config': {'model': 'JackFram/llama-68m', 'num_speculative_tokens': 5, 'max_model_len': 129}, 'max_model_len': 128}, {'speculative_config': {'model': 'JackFram/llama-68m', 'num_speculative_tokens': 5, 'max_model_len': 2048 + 1}}, {'speculative_config': {'model': 'JackFram/llama-68m', 'num_speculative_tokens': 5, 'max_model_len': 131072 + 1}}])
@pytest.mark.parametrize('test_llm_kwargs', [{}])
@pytest.mark.parametrize('seed', [1])
def test_spec_decode_xfail_spec_max_model_len(test_llm_generator):
    output_len = 128
    temperature = 0.0
    prompts = ['Hello, my name is']
    sampling_params = SamplingParams(max_tokens=output_len, ignore_eos=True, temperature=temperature)
    with pytest.raises(ValueError, match='cannot be larger than'):
        get_output_from_llm_generator(test_llm_generator, prompts, sampling_params)