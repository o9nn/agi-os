import pytest
from prometheus_client import REGISTRY
import aphrodite.common.envs as envs
from aphrodite import SamplingParams
from aphrodite.processing.scheduler import ARTIFICIAL_PREEMPTION_MAX_CNT, ENABLE_ARTIFICIAL_PREEMPT
from ..models.utils import check_outputs_equal
MODELS = ['distilbert/distilgpt2']
@pytest.fixture(scope='function', autouse=True)
def use_v0_only(monkeypatch):
    monkeypatch.setenv('APHRODITE_USE_V1', '0')
@pytest.fixture(scope='module', autouse=True)
def check_settings():
    assert ENABLE_ARTIFICIAL_PREEMPT is True, 'Use an env var APHRODITE_TEST_ENABLE_ARTIFICIAL_PREEMPT=1.`APHRODITE_TEST_ENABLE_ARTIFICIAL_PREEMPT=1 pytest tests/basic_correctness/test_preemption.py`'
@pytest.fixture
def distributed_executor_backend() -> str:
    return 'ray' if envs.APHRODITE_USE_RAY_SPMD_WORKER else 'mp'
@pytest.mark.parametrize('model', MODELS)
@pytest.mark.parametrize('dtype', ['half'])
@pytest.mark.parametrize('max_tokens', [96])
@pytest.mark.parametrize('chunked_prefill_token_size', [16])
def test_chunked_prefill_recompute(hf_runner, aphrodite_runner, example_prompts, model: str, dtype: str, max_tokens: int, chunked_prefill_token_size: int, distributed_executor_backend: str) -> None:
    max_num_seqs = min(chunked_prefill_token_size, 256)
    enable_chunked_prefill = False
    max_num_batched_tokens = None
    if chunked_prefill_token_size != -1:
        enable_chunked_prefill = True
        max_num_batched_tokens = chunked_prefill_token_size
    with hf_runner(model, dtype=dtype) as hf_model:
        hf_outputs = hf_model.generate_greedy(example_prompts, max_tokens)
    with aphrodite_runner(model, dtype=dtype, max_num_batched_tokens=max_num_batched_tokens, enable_chunked_prefill=enable_chunked_prefill, max_num_seqs=max_num_seqs, distributed_executor_backend=distributed_executor_backend, disable_log_stats=False) as aphrodite_model:
        aphrodite_outputs = aphrodite_model.generate_greedy(example_prompts, max_tokens)
        assert aphrodite_model.model.llm_engine.scheduler[0].artificial_preempt_cnt < ARTIFICIAL_PREEMPTION_MAX_CNT
    for i in range(len(example_prompts)):
        hf_output_ids, hf_output_str = hf_outputs[i]
        aphrodite_output_ids, aphrodite_output_str = aphrodite_outputs[i]
        assert hf_output_str == aphrodite_output_str, f'Test{i}:\nHF: {hf_output_str!r}\nAphrodite: {aphrodite_output_str!r}'
        assert hf_output_ids == aphrodite_output_ids, f'Test{i}:\nHF: {hf_output_ids}\nAphrodite: {aphrodite_output_ids}'
@pytest.mark.parametrize('model', MODELS)
@pytest.mark.parametrize('dtype', ['float'])
@pytest.mark.parametrize('max_tokens', [96])
def test_preemption(caplog_aphrodite, hf_runner, aphrodite_runner, example_prompts, model: str, dtype: str, max_tokens: int, distributed_executor_backend: str) -> None:
    with hf_runner(model, dtype=dtype) as hf_model:
        hf_outputs = hf_model.generate_greedy(example_prompts, max_tokens)
    with aphrodite_runner(model, dtype=dtype, disable_log_stats=False, distributed_executor_backend=distributed_executor_backend) as aphrodite_model:
        aphrodite_outputs = aphrodite_model.generate_greedy(example_prompts, max_tokens)
        assert aphrodite_model.model.llm_engine.scheduler[0].artificial_preempt_cnt < ARTIFICIAL_PREEMPTION_MAX_CNT
        total_preemption = aphrodite_model.model.llm_engine.scheduler[0].num_cumulative_preemption
    check_outputs_equal(outputs_0_lst=hf_outputs, outputs_1_lst=aphrodite_outputs, name_0='hf', name_1='aphrodite')
    assert 'is preempted by PreemptionMode.RECOMPUTE mode because there is not enough KV cache space.' in caplog_aphrodite.text
    preemption_metrics = None
    for m in REGISTRY.collect():
        if m.name == 'aphrodite:num_preemptions':
            preemption_metrics = m
    assert preemption_metrics is not None
    total_recorded_preemption = 0
    for sample in preemption_metrics.samples:
        total_recorded_preemption += sample.value
    assert total_preemption == total_recorded_preemption
@pytest.mark.parametrize('model', MODELS)
@pytest.mark.parametrize('dtype', ['float'])
@pytest.mark.parametrize('max_tokens', [96])
def test_preemption_infeasible(aphrodite_runner, example_prompts, model: str, dtype: str, max_tokens: int, distributed_executor_backend: str) -> None:
    BLOCK_SIZE = 16
    prefill_blocks = 2
    decode_blocks = max_tokens // BLOCK_SIZE
    with aphrodite_runner(model, dtype=dtype, block_size=BLOCK_SIZE, num_gpu_blocks_override=prefill_blocks + decode_blocks // 2, max_model_len=(prefill_blocks + decode_blocks // 2) * BLOCK_SIZE, distributed_executor_backend=distributed_executor_backend) as aphrodite_model:
        sampling_params = SamplingParams(max_tokens=max_tokens, ignore_eos=True)
        req_outputs = aphrodite_model.model.generate(example_prompts, sampling_params=sampling_params)
        assert aphrodite_model.model.llm_engine.scheduler[0].artificial_preempt_cnt < ARTIFICIAL_PREEMPTION_MAX_CNT
    for req_output in req_outputs:
        outputs = req_output.outputs
        assert len(outputs) == 1
        assert outputs[0].finish_reason == 'length'