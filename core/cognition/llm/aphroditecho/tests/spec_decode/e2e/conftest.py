from collections.abc import Sequence
from itertools import cycle
from typing import Optional, Union
import pytest
import torch
from aphrodite import LLM, SamplingParams
from aphrodite.distributed import cleanup_dist_env_and_memory
from aphrodite.modeling.utils import set_random_seed
from aphrodite.common.sequence import PromptLogprobs, SampleLogprobs
from ...models.utils import TokensTextLogprobs, TokensTextLogprobsPromptLogprobs, check_logprobs_close, check_outputs_equal
from ...utils import RemoteOpenAIServer
PROMPTS = ['Hello, my name is', 'The president of the United States is', 'The capital of France is', 'The future of AI is', 'San Francisco is know for its', 'Facebook was created in 2004 by', 'Curious George is a', 'Python 3.11 brings improvements to its']
@pytest.fixture
def test_llm_generator(common_llm_kwargs, per_test_common_llm_kwargs, test_llm_kwargs, seed):
    def generate():
        kwargs = {**common_llm_kwargs, **per_test_common_llm_kwargs, **test_llm_kwargs}
        llm = LLM(**kwargs)
        if seed is not None:
            set_random_seed(seed)
        yield llm
        del llm
        cleanup_dist_env_and_memory()
    return generate
def maybe_assert_ngram_worker(llm):
    if llm.llm_engine.speculative_config is not None and llm.llm_engine.speculative_config.method == 'ngram':
        from aphrodite.spec_decode.ngram_worker import NGramWorker
        assert isinstance(llm.llm_engine.model_executor.driver_worker.proposer_worker, NGramWorker)
def get_output_from_llm_generator(llm_generator, prompts, sampling_params) -> tuple[list[str], list[list[int]], float]:
    tokens: list[str] = []
    token_ids: list[list[int]] = []
    acceptance_rate: float = -1.0
    for llm in llm_generator():
        maybe_assert_ngram_worker(llm)
        outputs = llm.generate(prompts, sampling_params, use_tqdm=True)
        token_ids = [output.outputs[0].token_ids for output in outputs]
        tokens = [output.outputs[0].text for output in outputs]
        if (stat_loggers := getattr(llm.llm_engine, 'stat_loggers', None)):
            stat_logger = stat_loggers['prometheus']
            acceptance_rate = stat_logger.metrics.gauge_spec_decode_draft_acceptance_rate.labels(**stat_logger.labels)._value.get()
        del llm
    return (tokens, token_ids, acceptance_rate)
def check_logprobs_correctness(spec_outputs: Sequence[Union[TokensTextLogprobs, TokensTextLogprobsPromptLogprobs]], baseline_outputs: Sequence[Union[TokensTextLogprobs, TokensTextLogprobsPromptLogprobs]], disable_logprobs: bool=False):
    if not disable_logprobs:
        return check_logprobs_close(outputs_0_lst=baseline_outputs, outputs_1_lst=spec_outputs, name_0='org', name_1='sd')
    for spec_output, baseline_output in zip(spec_outputs, baseline_outputs):
        spec_logprobs = spec_output[2]
        baseline_logprobs = baseline_output[2]
        _check_logprobs_when_output_disabled(spec_logprobs, baseline_logprobs, is_prompt_logprobs=False)
        if len(baseline_output) == 4:
            assert len(spec_output) == 4
            spec_prompt_logprobs = spec_output[3]
            baseline_prompt_logprobs = baseline_output[3]
            _check_logprobs_when_output_disabled(spec_prompt_logprobs, baseline_prompt_logprobs, is_prompt_logprobs=True)
def _check_logprobs_when_output_disabled(spec_logprobs: Union[Optional[PromptLogprobs], SampleLogprobs], baseline_logprobs: Union[Optional[PromptLogprobs], SampleLogprobs], is_prompt_logprobs: bool=False):
    if is_prompt_logprobs and baseline_logprobs is None:
        assert spec_logprobs is None
        return
    assert spec_logprobs is not None
    assert baseline_logprobs is not None
    assert len(spec_logprobs) == len(baseline_logprobs)
    for pos, (spec_pos_logprobs, baseline_pos_logprobs) in enumerate(zip(spec_logprobs, baseline_logprobs)):
        if is_prompt_logprobs and baseline_pos_logprobs is None:
            assert spec_pos_logprobs is None
            assert pos == 0
            continue
        assert spec_pos_logprobs is not None
        assert baseline_pos_logprobs is not None
        assert len(spec_pos_logprobs) == 1
        spec_pos_logprob_token_id, spec_pos_logprob = next(iter(spec_pos_logprobs.items()))
        assert spec_pos_logprob.rank == -1
        assert spec_pos_logprob.logprob == 0.0
        if isinstance(spec_pos_logprob_token_id, torch.Tensor):
            spec_pos_logprob_token_id = spec_pos_logprob_token_id.item()
        assert spec_pos_logprob_token_id in baseline_pos_logprobs
def run_equality_correctness_test(aphrodite_runner, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size: int, max_output_len: int, seed: Optional[int]=0, temperature: float=0.0, disable_seed: bool=False, ignore_eos: bool=True, ensure_all_accepted: bool=False, expected_acceptance_rate: Optional[float]=None, logprobs: Optional[int]=None, prompt_logprobs: Optional[int]=None, disable_logprobs: bool=False):
    org_args = {**common_llm_kwargs, **per_test_common_llm_kwargs, **baseline_llm_kwargs}
    sd_args = {**common_llm_kwargs, **per_test_common_llm_kwargs, **test_llm_kwargs}
    prompts = [prompt for prompt, _ in zip(cycle(PROMPTS), range(batch_size))]
    if disable_seed:
        seed = None
    sampling_params = SamplingParams(temperature=temperature, max_tokens=max_output_len, seed=seed, ignore_eos=ignore_eos, logprobs=logprobs, prompt_logprobs=prompt_logprobs)
    with aphrodite_runner(**org_args) as aphrodite_model:
        org_outputs = aphrodite_model.generate_w_logprobs(prompts, sampling_params)
    with aphrodite_runner(**sd_args) as aphrodite_model:
        if ensure_all_accepted or expected_acceptance_rate is not None:
            stat_logger = aphrodite_model.model.llm_engine.stat_loggers['prometheus']
            stat_logger.local_interval = -100
        sd_outputs = aphrodite_model.generate_w_logprobs(prompts, sampling_params)
        if ensure_all_accepted or expected_acceptance_rate is not None:
            acceptance_rate = stat_logger.metrics.gauge_spec_decode_draft_acceptance_rate.labels(**stat_logger.labels)._value.get()
            if ensure_all_accepted:
                assert True
            if expected_acceptance_rate is not None:
                assert acceptance_rate >= expected_acceptance_rate - 0.01
    check_outputs_equal(outputs_0_lst=[out[0:2] for out in org_outputs], outputs_1_lst=[out[0:2] for out in sd_outputs], name_0='org', name_1='sd')
    if logprobs is not None or prompt_logprobs is not None:
        check_logprobs_correctness(sd_outputs, org_outputs, disable_logprobs)
def run_equality_correctness_test_tp(model, common_llm_kwargs, per_test_common_llm_kwargs, baseline_llm_kwargs, test_llm_kwargs, batch_size: int, max_output_len: int, seed: int=0, temperature: float=0.0, logprobs: Optional[int]=None):
    arg1 = common_llm_kwargs + per_test_common_llm_kwargs + baseline_llm_kwargs
    arg2 = common_llm_kwargs + per_test_common_llm_kwargs + test_llm_kwargs
    env1 = env2 = None
    max_wait_seconds = 240
    results = []
    prompts = [prompt for prompt, _ in zip(cycle(PROMPTS), range(batch_size))]
    for args, env in ((arg1, env1), (arg2, env2)):
        with RemoteOpenAIServer(model, args, env_dict=env, max_wait_seconds=max_wait_seconds) as server:
            client = server.get_client()
            completion = client.completions.create(model=model, prompt=prompts, max_tokens=max_output_len, seed=seed, temperature=temperature, logprobs=logprobs)
            results.append({'test': 'seeded_sampling', 'text': [choice.text for choice in completion.choices], 'logprobs': [choice.logprobs for choice in completion.choices], 'finish_reason': [choice.finish_reason for choice in completion.choices], 'usage': completion.usage})
    n = len(results) // 2
    arg1_results = results[:n]
    arg2_results = results[n:]
    arg1_logprobs = [r.pop('logprobs') for r in arg1_results]
    arg2_logprobs = [r.pop('logprobs') for r in arg2_results]
    for arg1_result, arg2_result in zip(arg1_results, arg2_results):
        assert arg1_result == arg2_result, f'Results for model={model!r} are not the same with arg1={arg1!r} and arg2={arg2!r}. arg1_result={arg1_result!r} != arg2_result={arg2_result!r}'
    if logprobs:
        for logs1, logs2 in zip(arg1_logprobs, arg2_logprobs):
            for l1, l2 in zip(logs1, logs2):
                assert l1.tokens == l2.tokens