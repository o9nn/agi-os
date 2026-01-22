from __future__ import annotations
import random
from typing import Any
import pytest
from aphrodite import LLM, SamplingParams
@pytest.fixture
def test_prompts():
    prompt_types = ['repeat', 'sentence']
    num_prompts = 100
    prompts = []
    random.seed(0)
    random_prompt_type_choices = random.choices(prompt_types, k=num_prompts)
    for kind in random_prompt_type_choices:
        word_choices = ['test', 'temp', 'hello', 'where']
        word = random.choice(word_choices)
        if kind == 'repeat':
            prompt = f"\n            please repeat the word '{word}' 10 times.\n            give no other output than the word at least ten times in a row,\n            in lowercase with spaces between each word and without quotes.\n            "
        elif kind == 'sentence':
            prompt = f'\n            please give a ten-word sentence that\n            uses the word {word} at least once.\n            give no other output than that simple sentence without quotes.\n            '
        else:
            raise ValueError(f'Unknown prompt type: {kind}')
        prompts.append([{'role': 'user', 'content': prompt}])
    return prompts
@pytest.fixture
def sampling_config():
    return SamplingParams(temperature=0, max_tokens=10, ignore_eos=False)
@pytest.fixture
def model_name():
    return 'meta-llama/Llama-3.1-8B-Instruct'
def eagle_model_name():
    return 'yuhuili/EAGLE-LLaMA3.1-Instruct-8B'
def eagle3_model_name():
    return 'yuhuili/EAGLE3-LLaMA3.1-Instruct-8B'
def test_ngram_correctness(monkeypatch: pytest.MonkeyPatch, test_prompts: list[list[dict[str, Any]]], sampling_config: SamplingParams, model_name: str):
    with monkeypatch.context() as m:
        m.setenv('APHRODITE_USE_V1', '1')
        ref_llm = LLM(model=model_name, max_model_len=1024)
        ref_outputs = ref_llm.chat(test_prompts, sampling_config)
        del ref_llm
        spec_llm = LLM(model=model_name, speculative_config={'method': 'ngram', 'prompt_lookup_max': 5, 'prompt_lookup_min': 3, 'num_speculative_tokens': 3}, max_model_len=1024)
        spec_outputs = spec_llm.chat(test_prompts, sampling_config)
        matches = 0
        misses = 0
        for ref_output, spec_output in zip(ref_outputs, spec_outputs):
            if ref_output.outputs[0].text == spec_output.outputs[0].text:
                matches += 1
            else:
                misses += 1
                print(f'ref_output: {ref_output.outputs[0].text}')
                print(f'spec_output: {spec_output.outputs[0].text}')
        assert matches > int(0.7 * len(ref_outputs))
        del spec_llm
@pytest.mark.parametrize('use_eagle3', [False, True], ids=['eagle', 'eagle3'])
def test_eagle_correctness(monkeypatch: pytest.MonkeyPatch, test_prompts: list[list[dict[str, Any]]], sampling_config: SamplingParams, model_name: str, use_eagle3: bool):
    with monkeypatch.context() as m:
        m.setenv('APHRODITE_USE_V1', '1')
        ref_llm = LLM(model=model_name, max_model_len=2048)
        ref_outputs = ref_llm.chat(test_prompts, sampling_config)
        del ref_llm
        spec_model_name = eagle3_model_name() if use_eagle3 else eagle_model_name()
        spec_llm = LLM(model=model_name, trust_remote_code=True, speculative_config={'method': 'eagle3' if use_eagle3 else 'eagle', 'model': spec_model_name, 'num_speculative_tokens': 3, 'max_model_len': 2048}, max_model_len=2048)
        spec_outputs = spec_llm.chat(test_prompts, sampling_config)
        matches = 0
        misses = 0
        for ref_output, spec_output in zip(ref_outputs, spec_outputs):
            if ref_output.outputs[0].text == spec_output.outputs[0].text:
                matches += 1
            else:
                misses += 1
                print(f'ref_output: {ref_output.outputs[0].text}')
                print(f'spec_output: {spec_output.outputs[0].text}')
        assert matches > int(0.66 * len(ref_outputs))
        del spec_llm