from dataclasses import dataclass
import pytest
from aphrodite import LLM, SamplingParams
from ...core.block.e2e.test_correctness_sliding_window import check_answers, prep_prompts
@dataclass
class TestConfig:
    sliding_window: int
    ln_range: tuple[int, int]
model_config = {'bigcode/starcoder2-3b': TestConfig(4096, (800, 1100)), 'google/gemma-2-2b-it': TestConfig(4096, (400, 800))}
@pytest.mark.parametrize('model', ['bigcode/starcoder2-3b', 'google/gemma-2-2b-it'])
@pytest.mark.parametrize('batch_size', [5])
@pytest.mark.parametrize('seed', [1])
def test_sliding_window_retrival(monkeypatch, model, batch_size, seed):
    with monkeypatch.context() as m:
        m.setenv('APHRODITE_USE_V1', '1')
        test_config = model_config[model]
        llm = LLM(model=model)
        sampling_params = SamplingParams(temperature=0.0, max_tokens=100)
        prompts, answer, indices = prep_prompts(batch_size, ln_range=test_config.ln_range)
        check_length(prompts, llm, test_config.sliding_window)
        responses = llm.generate(prompts, sampling_params)
        check_answers(indices, answer, [response.outputs[0].text for response in responses], accept_rate=1.0)
        responses = llm.generate(prompts, sampling_params)
        check_answers(indices, answer, [response.outputs[0].text for response in responses], accept_rate=1.0)
def check_length(prompts: list[str], llm: LLM, sliding_window: int):
    tokenizer = llm.get_tokenizer()
    max_model_len = llm.llm_engine.model_config.max_model_len
    assert any((len(tokenizer.encode(prompt)) > sliding_window for prompt in prompts)), 'Prompt is too short for test'
    assert all((len(tokenizer.encode(prompt)) <= max_model_len for prompt in prompts)), 'Prompt is too long for test'