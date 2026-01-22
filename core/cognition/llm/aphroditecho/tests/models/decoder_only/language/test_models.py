import pytest
from ...utils import check_outputs_equal
MODELS = ['facebook/opt-125m', 'gpt2', 'bigcode/tiny_starcoder_py', 'EleutherAI/pythia-70m', 'bigscience/bloom-560m', 'microsoft/phi-2', 'stabilityai/stablelm-3b-4e1t', 'bigcode/starcoder2-3b', 'google/gemma-1.1-2b-it']
@pytest.mark.parametrize('model', MODELS)
@pytest.mark.parametrize('dtype', ['float'])
@pytest.mark.parametrize('max_tokens', [96])
def test_models(hf_runner, aphrodite_runner, example_prompts, model: str, dtype: str, max_tokens: int) -> None:
    assert dtype == 'float'
    with hf_runner(model, dtype=dtype) as hf_model:
        hf_outputs = hf_model.generate_greedy(example_prompts, max_tokens)
    with aphrodite_runner(model, dtype=dtype) as aphrodite_model:
        aphrodite_outputs = aphrodite_model.generate_greedy(example_prompts, max_tokens)
    check_outputs_equal(outputs_0_lst=hf_outputs, outputs_1_lst=aphrodite_outputs, name_0='hf', name_1='aphrodite')
@pytest.mark.parametrize('model', MODELS)
@pytest.mark.parametrize('dtype', ['float'])
def test_model_print(aphrodite_runner, model: str, dtype: str) -> None:
    with aphrodite_runner(model, dtype=dtype) as aphrodite_model:
        print(aphrodite_model.model.llm_engine.model_executor.driver_worker.model_runner.model)