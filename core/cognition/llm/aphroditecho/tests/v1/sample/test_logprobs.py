import itertools
from collections.abc import Generator
import pytest
import torch
from tests.v1.sample.utils import BatchLogprobsComposition, BatchLogprobsSpecType, assert_incr_detok_str_matches_non_incr_detok_str, compute_correct_cumulative_logprob, get_test_batch
from aphrodite import SamplingParams
from ...conftest import HfRunner, AphroditeRunner
MODEL = 'meta-llama/Llama-3.2-1B-Instruct'
DTYPE = 'half'
NONE = BatchLogprobsComposition.NONE
SAMPLE = BatchLogprobsComposition.SAMPLE
PROMPT = BatchLogprobsComposition.PROMPT
SAMPLE_PROMPT = BatchLogprobsComposition.SAMPLE_PROMPT
@pytest.fixture(scope='module', params=[False, True])
def aphrodite_model(aphrodite_runner, request) -> Generator[AphroditeRunner, None, None]:
    with aphrodite_runner(MODEL, dtype=DTYPE, max_logprobs=7, max_num_batched_tokens=16, max_num_seqs=16, max_model_len=128, enforce_eager=True, enable_prefix_caching=request.param, gpu_memory_utilization=0.5) as aphrodite_model:
        yield aphrodite_model
@pytest.fixture(scope='module')
def hf_model(hf_runner) -> Generator[HfRunner, None, None]:
    with hf_runner(MODEL, dtype=DTYPE) as hf_model:
        yield hf_model
def _repeat_logprob_config(test_prompts, logprob_prompt_logprob_list: BatchLogprobsSpecType) -> BatchLogprobsSpecType:
    num_test_prompts = len(test_prompts)
    logprob_prompt_logprob_list = list(itertools.islice(itertools.cycle(logprob_prompt_logprob_list), num_test_prompts))
    assert num_test_prompts == len(logprob_prompt_logprob_list)
    return logprob_prompt_logprob_list
def _run_and_validate(aphrodite_model: AphroditeRunner, test_prompts: list[str], aphrodite_sampling_params: SamplingParams, hf_logprobs: list[list[torch.Tensor]], hf_outputs: list[tuple[list[int], str]], logprob_prompt_logprob_list: BatchLogprobsSpecType, temperature: float, max_tokens: int, do_apc: bool) -> None:
    aphrodite_results = aphrodite_model.model.generate(test_prompts, sampling_params=aphrodite_sampling_params)
    for aphrodite_result, hf_logprob, hf_output, logprob_prompt_logprob in zip(aphrodite_results, hf_logprobs, hf_outputs, logprob_prompt_logprob_list):
        num_top_logprobs, num_top_prompt_logprobs = logprob_prompt_logprob
        if temperature == 0.0:
            assert aphrodite_result.prompt_token_ids + aphrodite_result.outputs[0].token_ids == hf_output[0]
        else:
            assert aphrodite_result.prompt_token_ids == hf_output[0][:len(aphrodite_result.prompt_token_ids)]
        if num_top_logprobs is not None:
            assert num_top_logprobs is not None
            assert aphrodite_result.outputs[0].logprobs is not None
            assert len(aphrodite_result.outputs[0].logprobs) == max_tokens
            for logprobs, token_id in zip(aphrodite_result.outputs[0].logprobs, aphrodite_result.outputs[0].token_ids):
                assert logprobs is not None
                assert token_id in logprobs
                token_in_topk = logprobs[token_id].rank <= num_top_logprobs
                if token_in_topk and num_top_logprobs != 0:
                    assert len(logprobs) == num_top_logprobs
                else:
                    assert len(logprobs) == num_top_logprobs + 1
                if num_top_logprobs > 0:
                    all_ranks = {lp.rank for lp in logprobs.values()}
                    assert all((r in all_ranks for r in range(1, num_top_logprobs + 1)))
            output_text = aphrodite_result.outputs[0].text
            output_string_from_most_likely_tokens_lst: list[str] = []
            for top_logprobs in aphrodite_result.outputs[0].logprobs:
                top_logprob = next(iter(top_logprobs.values()))
                output_string_from_most_likely_tokens_lst.append(top_logprob.decoded_token)
            output_string_from_most_likely_tokens = ''.join(output_string_from_most_likely_tokens_lst)
            assert_incr_detok_str_matches_non_incr_detok_str(output_text, output_string_from_most_likely_tokens, 'The output text from the top logprob for each token position should be the same as the output text in the result.')
            aphrodite_sample_logprobs = aphrodite_result.outputs[0].logprobs
            for i, top_logprobs in enumerate(aphrodite_sample_logprobs):
                for token_id, sample_logprob in top_logprobs.items():
                    if temperature == 0.0 or i == 0:
                        logprob = sample_logprob.logprob
                        torch.testing.assert_close(logprob, hf_logprob[i][-1][token_id].item(), atol=0.01, rtol=0.01)
                    assert isinstance(sample_logprob.decoded_token, str), 'The token should be decoded by the time it is returned to the user.'
            torch.testing.assert_close(aphrodite_result.outputs[0].cumulative_logprob, compute_correct_cumulative_logprob(aphrodite_result.outputs[0]), atol=1e-06, rtol=1e-06)
        else:
            assert aphrodite_result.outputs[0].logprobs is None
        if num_top_prompt_logprobs is not None:
            assert aphrodite_result.prompt_logprobs is not None
            assert aphrodite_result.prompt_logprobs[0] is None
            assert len(aphrodite_result.prompt_logprobs) == len(aphrodite_result.prompt_token_ids)
            for prompt_logprobs, prompt_token_id in zip(aphrodite_result.prompt_logprobs[1:], aphrodite_result.prompt_token_ids[1:]):
                assert prompt_logprobs is not None
                assert prompt_token_id in prompt_logprobs
                token_in_topk = prompt_logprobs[prompt_token_id].rank <= num_top_prompt_logprobs
                if token_in_topk and num_top_prompt_logprobs != 0:
                    assert len(prompt_logprobs) == num_top_prompt_logprobs
                else:
                    assert len(prompt_logprobs) == num_top_prompt_logprobs + 1
                if num_top_prompt_logprobs > 0:
                    all_ranks = {lp.rank for lp in prompt_logprobs.values()}
                    assert all((r in all_ranks for r in range(1, num_top_prompt_logprobs + 1)))
            aphrodite_prompt_logprobs = aphrodite_result.prompt_logprobs[1:]
            for i, aphrodite_prompt_logprob_dict in enumerate(aphrodite_prompt_logprobs):
                for token_id, logprob in aphrodite_prompt_logprob_dict.items():
                    torch.testing.assert_close(logprob.logprob, hf_logprob[0][i][token_id].item(), atol=0.02, rtol=0.02)
        else:
            assert aphrodite_result.prompt_logprobs is None
@pytest.mark.parametrize('batch_logprobs_composition', [NONE, SAMPLE, PROMPT, SAMPLE_PROMPT])
@pytest.mark.parametrize('temperature', [0.0, 2.0])
def test_get_logprobs_and_prompt_logprobs(hf_model, aphrodite_model, batch_logprobs_composition: BatchLogprobsComposition, temperature: float, example_prompts: list[str], monkeypatch: pytest.MonkeyPatch) -> None:
    with monkeypatch.context() as m:
        m.setenv('APHRODITE_USE_V1', '1')
        do_apc = aphrodite_model.model.llm_engine.cache_config.enable_prefix_caching
        if do_apc and (temperature < 2.0 or batch_logprobs_composition != SAMPLE_PROMPT):
            pytest.skip()
        test_prompts = example_prompts
        max_tokens = 5
        hf_outputs = hf_model.generate_greedy(test_prompts, max_tokens=max_tokens)
        hf_logprobs = hf_model.generate_greedy_logprobs(test_prompts, max_tokens=max_tokens)
        logprob_prompt_logprob_list = get_test_batch(batch_logprobs_composition)
        logprob_prompt_logprob_list = _repeat_logprob_config(test_prompts, logprob_prompt_logprob_list)
        aphrodite_sampling_params = [SamplingParams(max_tokens=max_tokens, logprobs=num_lp, prompt_logprobs=num_plp, temperature=temperature, seed=1984) for num_lp, num_plp in logprob_prompt_logprob_list]
        for _ in range(2 if do_apc else 1):
            _run_and_validate(aphrodite_model=aphrodite_model, test_prompts=test_prompts, aphrodite_sampling_params=aphrodite_sampling_params, hf_logprobs=hf_logprobs, hf_outputs=hf_outputs, logprob_prompt_logprob_list=logprob_prompt_logprob_list, temperature=temperature, max_tokens=max_tokens, do_apc=do_apc)
def test_max_logprobs(monkeypatch: pytest.MonkeyPatch):
    with monkeypatch.context() as m:
        m.setenv('APHRODITE_USE_V1', '1')
        runner = AphroditeRunner('facebook/opt-125m', max_logprobs=1, enable_prefix_caching=False, max_model_len=256)
        aphrodite_sampling_params = SamplingParams(logprobs=1)
        runner.generate(['Hello world'], sampling_params=aphrodite_sampling_params)
        bad_sampling_params = SamplingParams(logprobs=2)
        with pytest.raises(ValueError):
            runner.generate(['Hello world'], sampling_params=bad_sampling_params)
def test_none_logprobs(aphrodite_model, example_prompts, monkeypatch: pytest.MonkeyPatch):
    with monkeypatch.context() as m:
        m.setenv('APHRODITE_USE_V1', '1')
        max_tokens = 5
        sampling_params_logprobs_none = SamplingParams(max_tokens=max_tokens, logprobs=None, prompt_logprobs=None, temperature=0.0)
        results_logprobs_none = aphrodite_model.model.generate(example_prompts, sampling_params=sampling_params_logprobs_none)
        for i in range(len(results_logprobs_none)):
            assert results_logprobs_none[i].outputs[0].logprobs is None
            assert results_logprobs_none[i].outputs[0].cumulative_logprob is None
            assert results_logprobs_none[i].prompt_logprobs is None
def test_zero_logprobs(aphrodite_model, example_prompts, monkeypatch: pytest.MonkeyPatch):
    with monkeypatch.context() as m:
        m.setenv('APHRODITE_USE_V1', '1')
        max_tokens = 5
        sampling_params_logprobs_zero = SamplingParams(max_tokens=max_tokens, logprobs=0, prompt_logprobs=0, temperature=0.0)
        results_logprobs_zero = aphrodite_model.model.generate(example_prompts, sampling_params=sampling_params_logprobs_zero)
        for i in range(len(results_logprobs_zero)):
            logprobs = results_logprobs_zero[i].outputs[0].logprobs
            prompt_logprobs = results_logprobs_zero[i].prompt_logprobs
            sampled_token_ids = results_logprobs_zero[i].outputs[0].token_ids
            prompt_token_ids = results_logprobs_zero[i].prompt_token_ids
            assert logprobs is not None
            assert len(sampled_token_ids) == len(logprobs)
            assert results_logprobs_zero[i].outputs[0].cumulative_logprob is not None
            assert prompt_logprobs is not None
            assert len(prompt_token_ids) == len(prompt_logprobs)