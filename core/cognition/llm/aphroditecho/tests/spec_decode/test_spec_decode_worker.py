import random
from collections import defaultdict
from types import SimpleNamespace
from unittest.mock import MagicMock
import pytest
import torch
from aphrodite.modeling.layers.sampler import SamplerOutput
from aphrodite.modeling.utils import set_random_seed
from aphrodite.common.sequence import ExecuteModelRequest, SequenceOutput
from aphrodite.spec_decode.batch_expansion import BatchExpansionTop1Scorer
from aphrodite.spec_decode.draft_model_runner import TP1DraftModelRunner
from aphrodite.spec_decode.interfaces import SpeculativeProposals
from aphrodite.spec_decode.metrics import AsyncMetricsCollector, SpecDecodeWorkerMetrics
from aphrodite.spec_decode.multi_step_worker import MultiStepWorker
from aphrodite.spec_decode.spec_decode_worker import SpecDecodeWorker, split_num_cache_blocks_evenly
from aphrodite.worker.worker import Worker
from .test_utils import mock_spec_decode_sampler
from .utils import create_batch, create_sampler_output_list, create_worker, mock_worker
@pytest.mark.parametrize('k', [1, 2, 6])
@pytest.mark.parametrize('batch_size', [1, 2, 32])
@pytest.mark.parametrize('acceptance_sampler_method', ['rejection_sampler', 'typical_acceptance_sampler'])
@torch.inference_mode()
def test_correctly_calls_draft_model(k: int, batch_size: int, acceptance_sampler_method: str):
    draft_worker = mock_worker(cls=MultiStepWorker)
    target_worker = mock_worker()
    metrics_collector = MagicMock(spec=AsyncMetricsCollector)
    worker = SpecDecodeWorker(draft_worker, target_worker, mock_spec_decode_sampler(acceptance_sampler_method), disable_logprobs=False, metrics_collector=metrics_collector)
    exception_secret = 'artificial stop'
    draft_worker.get_spec_proposals.side_effect = ValueError(exception_secret)
    seq_group_metadata_list, _, _ = create_batch(batch_size, k)
    execute_model_req = ExecuteModelRequest(seq_group_metadata_list=seq_group_metadata_list, num_lookahead_slots=k)
    with pytest.raises(ValueError, match=exception_secret):
        worker.execute_model(execute_model_req=execute_model_req)
    call_args_list = draft_worker.get_spec_proposals.call_args_list
    assert len(call_args_list) == 1
    for args, _ in call_args_list:
        actual_execute_model_data = args[0]
        assert actual_execute_model_data == execute_model_req
@pytest.mark.parametrize('k', [1, 2, 6])
@pytest.mark.parametrize('batch_size', [1, 2, 32])
@pytest.mark.parametrize('acceptance_sampler_method', ['rejection_sampler', 'typical_acceptance_sampler'])
@torch.inference_mode()
def test_batch_expansion_correctly_calls_target_model(k: int, batch_size: int, acceptance_sampler_method: str):
    draft_worker = mock_worker(cls=MultiStepWorker, use_spec=False)
    target_worker = mock_worker(use_spec=False)
    metrics_collector = MagicMock(spec=AsyncMetricsCollector)
    draft_worker.device = 'cuda'
    target_worker.device = 'cuda'
    set_random_seed(1)
    worker = SpecDecodeWorker(draft_worker, target_worker, mock_spec_decode_sampler(acceptance_sampler_method), disable_logprobs=False, metrics_collector=metrics_collector, disable_mqa_scorer=True)
    worker.init_device()
    vocab_size = 32000
    proposal_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, k), dtype=torch.int64, device='cuda')
    proposal_probs = torch.rand(batch_size, k, vocab_size, dtype=torch.float32, device='cuda')
    proposal_lens = torch.ones(batch_size, dtype=torch.int64, device='cuda') * k
    seq_group_metadata_list, prompts, prev_output_tokens = create_batch(batch_size, k)
    draft_worker.get_spec_proposals.return_value = SpeculativeProposals(proposal_token_ids=proposal_token_ids, proposal_probs=proposal_probs, proposal_lens=proposal_lens)
    exception_secret = 'artificial stop'
    target_worker.execute_model.side_effect = ValueError(exception_secret)
    with pytest.raises(ValueError, match=exception_secret):
        worker.execute_model(execute_model_req=ExecuteModelRequest(seq_group_metadata_list=seq_group_metadata_list, num_lookahead_slots=k))
    seen_contexts: list[list[int]] = []
    call_args_list = target_worker.execute_model.call_args_list
    assert len(call_args_list) == 1
    for _, kwargs in call_args_list:
        seq_group_metadata_list = kwargs['execute_model_req'].seq_group_metadata_list
        assert len(seq_group_metadata_list) == (k + 1) * batch_size
        for seq_group_metadata in seq_group_metadata_list:
            for seq_data in seq_group_metadata.seq_data.values():
                seen_contexts.append(seq_data.get_token_ids())
    expected_seen_contexts: list[list[int]] = []
    for prompt, prev_generated, draft_tokens in zip(prompts, prev_output_tokens, proposal_token_ids.tolist()):
        for i in range(len(draft_tokens) + 1):
            expected_seen_contexts.append(prompt + prev_generated + draft_tokens[:i])
    seen_contexts.sort()
    expected_seen_contexts.sort()
    assert expected_seen_contexts == seen_contexts
@pytest.mark.parametrize('k', [1, 2, 6])
@pytest.mark.parametrize('batch_size', [1, 2, 32])
@pytest.mark.parametrize('acceptance_sampler_method', ['rejection_sampler', 'typical_acceptance_sampler'])
@torch.inference_mode()
def test_correctly_calls_spec_decode_sampler(k: int, batch_size: int, acceptance_sampler_method: str):
    vocab_size = 32000
    draft_worker = mock_worker(cls=MultiStepWorker, vocab_size=vocab_size, use_spec=False)
    target_worker = mock_worker(vocab_size=vocab_size, use_spec=False)
    spec_decode_sampler = mock_spec_decode_sampler(acceptance_sampler_method)
    metrics_collector = MagicMock(spec=AsyncMetricsCollector)
    draft_worker.device = 'cuda'
    target_worker.device = 'cuda'
    set_random_seed(1)
    worker = SpecDecodeWorker(draft_worker, target_worker, spec_decode_sampler, disable_logprobs=False, metrics_collector=metrics_collector)
    worker.init_device()
    proposal_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, k), dtype=torch.int64, device='cuda')
    proposal_probs = torch.rand(batch_size, k, vocab_size, dtype=torch.float32, device='cuda')
    proposal_lens = torch.ones(batch_size, dtype=torch.int64, device='cuda') * k
    seq_group_metadata_list, _, _ = create_batch(batch_size, k)
    draft_worker.get_spec_proposals.return_value = SpeculativeProposals(proposal_token_ids=proposal_token_ids, proposal_probs=proposal_probs, proposal_lens=proposal_lens)
    target_token_ids = torch.randint(low=0, high=vocab_size, size=(1, batch_size * (k + 1)), dtype=torch.int64, device='cuda')
    target_token_probs = torch.rand(1, batch_size * (k + 1), vocab_size, dtype=torch.float32, device='cuda')
    target_token_logprobs = torch.rand(1, batch_size * (k + 1), vocab_size, dtype=torch.float32, device='cuda')
    target_output = create_sampler_output_list(target_token_ids, target_token_probs, target_token_logprobs)
    target_worker.execute_model.return_value = [target_output[0]]
    exception_secret = 'artificial stop'
    spec_decode_sampler.side_effect = ValueError(exception_secret)
    with pytest.raises(ValueError, match=exception_secret):
        worker.execute_model(execute_model_req=ExecuteModelRequest(seq_group_metadata_list=seq_group_metadata_list, num_lookahead_slots=k))
    assert len(spec_decode_sampler.call_args_list) == 1
    _, kwargs = spec_decode_sampler.call_args_list[0]
    actual = SimpleNamespace(**kwargs)
    assert torch.equal(actual.bonus_token_ids, target_token_ids.reshape(batch_size, k + 1)[:, -1:])
    assert torch.equal(actual.target_with_bonus_probs, target_token_probs.reshape(batch_size, k + 1, -1))
    assert torch.equal(actual.draft_token_ids, proposal_token_ids)
    assert torch.equal(actual.draft_probs, proposal_probs)
@pytest.mark.parametrize('k', [1, 2, 6])
@pytest.mark.parametrize('batch_size', [1, 2, 32])
@pytest.mark.parametrize('acceptance_sampler_method', ['rejection_sampler', 'typical_acceptance_sampler'])
@torch.inference_mode()
def test_correctly_formats_output(k: int, batch_size: int, acceptance_sampler_method: str):
    vocab_size = 32000
    draft_worker = mock_worker(cls=MultiStepWorker, vocab_size=vocab_size, use_spec=False)
    target_worker = mock_worker(vocab_size=vocab_size, use_spec=False)
    metrics_collector = MagicMock(spec=AsyncMetricsCollector)
    draft_worker.device = 'cuda'
    target_worker.device = 'cuda'
    set_random_seed(1)
    spec_decode_sampler = mock_spec_decode_sampler(acceptance_sampler_method)
    worker = SpecDecodeWorker(draft_worker, target_worker, spec_decode_sampler, disable_logprobs=False, metrics_collector=metrics_collector)
    worker.init_device()
    proposal_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, k), dtype=torch.int64, device='cuda')
    proposal_probs = torch.rand(batch_size, k, vocab_size, dtype=torch.float32, device='cuda')
    proposal_lens = torch.ones(batch_size, dtype=torch.int64, device='cuda') * k
    seq_group_metadata_list, _, _ = create_batch(batch_size, k)
    draft_worker.get_spec_proposals.return_value = SpeculativeProposals(proposal_token_ids=proposal_token_ids, proposal_probs=proposal_probs, proposal_lens=proposal_lens)
    target_token_ids = torch.randint(low=0, high=vocab_size, size=(1, batch_size * (k + 1)), dtype=torch.int64, device='cuda')
    target_token_probs = torch.rand(1, batch_size * (k + 1), vocab_size, dtype=torch.float32, device='cuda')
    target_token_logprobs = torch.rand(1, batch_size * (k + 1), vocab_size, dtype=torch.float32, device='cuda')
    target_output = create_sampler_output_list(target_token_ids, target_token_probs, target_token_logprobs)
    target_worker.execute_model.return_value = [target_output[0]]
    spec_decode_sampler_output = torch.randint(low=0, high=vocab_size, size=(batch_size, k + 1), dtype=torch.int64, device='cuda')
    for i in range(batch_size):
        minimum_accepted_tokens = 1
        spec_decode_sampler_output[i][-random.randint(minimum_accepted_tokens, k + 1):] = -1
    spec_decode_sampler.return_value = spec_decode_sampler_output
    output = worker.execute_model(execute_model_req=ExecuteModelRequest(seq_group_metadata_list=seq_group_metadata_list, num_lookahead_slots=k))
    expected_output = create_sampler_output_list(token_ids=spec_decode_sampler_output.transpose(0, 1), probs=[None for _ in range(k + 1)], logprobs=[None for _ in range(k + 1)])
    seq_ids = [next(iter(seq_group_metadata.seq_data.keys())) for seq_group_metadata in seq_group_metadata_list]
    actual_output_by_seq: dict[int, list[SequenceOutput]] = {seq_id: [] for seq_id in seq_ids}
    expected_output_by_seq: dict[int, list[SequenceOutput]] = {seq_id: [] for seq_id in seq_ids}
    for step in output:
        for seq_group in step:
            for sample in seq_group.samples:
                seq_id = sample.parent_seq_id
                actual_output_by_seq[seq_id].append(sample)
    for step in expected_output:
        for seq_group in step:
            for sample in seq_group.samples:
                seq_id = sample.parent_seq_id
                expected_output_by_seq[seq_id].append(sample)
    all_seen_seq_ids = set(list(actual_output_by_seq.keys()) + list(expected_output_by_seq.keys()))
    for seq_id in all_seen_seq_ids:
        actual_by_step = actual_output_by_seq[seq_id]
        expected_by_step = expected_output_by_seq[seq_id]
        for i in range(k + 1):
            if i >= len(actual_by_step):
                assert expected_by_step[i].output_token == -1
                continue
            assert actual_by_step[i].output_token == expected_by_step[i].output_token
@pytest.mark.parametrize('k', [1, 2])
@pytest.mark.parametrize('batch_size', [1])
@pytest.mark.parametrize('returns_metrics', [True, False])
@pytest.mark.parametrize('acceptance_sampler_method', ['rejection_sampler', 'typical_acceptance_sampler'])
@torch.inference_mode()
def test_collects_metrics(k: int, batch_size: int, returns_metrics: bool, acceptance_sampler_method: str):
    vocab_size = 32000
    draft_worker = mock_worker(cls=MultiStepWorker, vocab_size=vocab_size, use_spec=False)
    target_worker = mock_worker(vocab_size=vocab_size, use_spec=False)
    spec_decode_sampler = mock_spec_decode_sampler(acceptance_sampler_method)
    metrics_collector = MagicMock(spec=AsyncMetricsCollector)
    draft_worker.device = 'cuda'
    target_worker.device = 'cuda'
    set_random_seed(1)
    worker = SpecDecodeWorker(draft_worker, target_worker, spec_decode_sampler, disable_logprobs=False, metrics_collector=metrics_collector)
    worker.init_device()
    proposal_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, k), dtype=torch.int64, device='cuda')
    proposal_probs = torch.rand(batch_size, k, vocab_size, dtype=torch.float32, device='cuda')
    proposal_lens = torch.ones(batch_size, dtype=torch.int64, device='cuda') * k
    seq_group_metadata_list, _, _ = create_batch(batch_size, k)
    draft_worker.get_spec_proposals.return_value = SpeculativeProposals(proposal_token_ids=proposal_token_ids, proposal_probs=proposal_probs, proposal_lens=proposal_lens)
    target_token_ids = torch.randint(low=0, high=vocab_size, size=(1, batch_size * (k + 1)), dtype=torch.int64, device='cuda')
    target_token_probs = torch.rand(1, batch_size * (k + 1), vocab_size, dtype=torch.float32, device='cuda')
    target_token_logprobs = torch.rand(1, batch_size * (k + 1), vocab_size, dtype=torch.float32, device='cuda')
    target_output = create_sampler_output_list(target_token_ids, target_token_probs, target_token_logprobs)
    target_worker.execute_model.return_value = [target_output[0]]
    spec_decode_sampler_output = torch.randint(low=0, high=vocab_size, size=(batch_size, k + 1), dtype=torch.int64, device='cuda')
    for i in range(batch_size):
        minimum_accepted_tokens = 1
        spec_decode_sampler_output[i][-random.randint(minimum_accepted_tokens, k + 1):] = -1
    spec_decode_sampler.return_value = spec_decode_sampler_output
    mock_rejsample_metrics = MagicMock(spec=SpecDecodeWorkerMetrics) if returns_metrics else None
    metrics_collector.maybe_collect_rejsample_metrics.return_value = mock_rejsample_metrics
    output = worker.execute_model(execute_model_req=ExecuteModelRequest(seq_group_metadata_list=seq_group_metadata_list, num_lookahead_slots=k))
    assert output[0].spec_decode_worker_metrics == mock_rejsample_metrics
    call_args_list = metrics_collector.maybe_collect_rejsample_metrics.call_args_list
    assert len(call_args_list) == 1
    args, kwargs = call_args_list[0]
    assert args[0] == k or kwargs.get('k', -1) == k
@pytest.mark.parametrize('k', [0])
@pytest.mark.parametrize('batch_size', [1, 2, 32])
@pytest.mark.parametrize('acceptance_sampler_method', ['rejection_sampler', 'typical_acceptance_sampler'])
@torch.inference_mode()
def test_k_equals_zero(k: int, batch_size: int, acceptance_sampler_method: str):
    draft_worker = mock_worker(cls=MultiStepWorker)
    target_worker = mock_worker()
    metrics_collector = MagicMock(spec=AsyncMetricsCollector)
    sampler_output = MagicMock(spec=SamplerOutput)
    sampler_output.hidden_states = None
    target_worker.execute_model.return_value = [sampler_output]
    draft_worker.device = 'cuda'
    target_worker.device = 'cuda'
    set_random_seed(1)
    worker = SpecDecodeWorker(proposer_worker=draft_worker, scorer_worker=target_worker, spec_decode_sampler=mock_spec_decode_sampler(acceptance_sampler_method), disable_logprobs=False, metrics_collector=metrics_collector)
    seq_group_metadata_list, _, _ = create_batch(batch_size, k, prev_output_token_len=0)
    execute_model_req = ExecuteModelRequest(seq_group_metadata_list=seq_group_metadata_list, num_lookahead_slots=k)
    out = worker.execute_model(execute_model_req=execute_model_req)
    assert len(out) == 1, f'expected only one token output when k={k!r}'
    assert out[0].sampled_token_probs is None, 'expect gpu tensor references to be None'
    assert out[0].sampled_token_ids is None, 'expect gpu tensor references to be None'
    draft_worker.execute_model.assert_called_once_with(execute_model_req)
    target_worker.execute_model.assert_called_once_with(execute_model_req)
@pytest.mark.parametrize('k', [0, 5])
@pytest.mark.parametrize('batch_size', [0])
@pytest.mark.parametrize('acceptance_sampler_method', ['rejection_sampler', 'typical_acceptance_sampler'])
@torch.inference_mode()
def test_empty_input_batch(k: int, batch_size: int, acceptance_sampler_method: str):
    draft_worker = mock_worker(cls=MultiStepWorker)
    target_worker = mock_worker()
    metrics_collector = MagicMock(spec=AsyncMetricsCollector)
    sampler_output = MagicMock(spec=SamplerOutput)
    sampler_output.hidden_states = None
    target_worker.execute_model.return_value = [sampler_output]
    draft_worker.device = 'cuda'
    target_worker.device = 'cuda'
    set_random_seed(1)
    worker = SpecDecodeWorker(proposer_worker=draft_worker, scorer_worker=target_worker, spec_decode_sampler=mock_spec_decode_sampler(acceptance_sampler_method), disable_logprobs=False, metrics_collector=metrics_collector)
    seq_group_metadata_list, _, _ = create_batch(batch_size, k, prev_output_token_len=0)
    execute_model_req = ExecuteModelRequest(seq_group_metadata_list=seq_group_metadata_list, num_lookahead_slots=k)
    out = worker.execute_model(execute_model_req=execute_model_req)
    assert len(out) == 1, f'expected only one token output when k={k!r}'
    assert out[0].sampled_token_probs is None, 'expect gpu tensor references to be None'
    assert out[0].sampled_token_ids is None, 'expect gpu tensor references to be None'
    draft_worker.execute_model.assert_called_once_with(execute_model_req)
    target_worker.execute_model.assert_called_once_with(execute_model_req)
@pytest.mark.parametrize('acceptance_sampler_method', ['rejection_sampler', 'typical_acceptance_sampler'])
@pytest.mark.skip_global_cleanup
def test_init_device(acceptance_sampler_method: str):
    draft_worker = mock_worker(cls=MultiStepWorker, use_spec=False)
    target_worker = mock_worker(use_spec=False)
    spec_decode_sampler = mock_spec_decode_sampler(acceptance_sampler_method)
    metrics_collector = MagicMock(spec=AsyncMetricsCollector)
    worker = SpecDecodeWorker(proposer_worker=draft_worker, scorer_worker=target_worker, spec_decode_sampler=spec_decode_sampler, disable_logprobs=False, metrics_collector=metrics_collector)
    worker.init_device()
    draft_worker.init_device.assert_called_once()
    target_worker.init_device.assert_called_once()
    metrics_collector.init_tensors.assert_called_once()
    spec_decode_sampler.init_tensors.assert_called_once()
@pytest.mark.parametrize('acceptance_sampler_method', ['rejection_sampler', 'typical_acceptance_sampler'])
@torch.inference_mode()
def test_initialize_cache(acceptance_sampler_method):
    draft_worker = mock_worker(cls=MultiStepWorker)
    target_worker = mock_worker()
    metrics_collector = MagicMock(spec=AsyncMetricsCollector)
    worker = SpecDecodeWorker(proposer_worker=draft_worker, scorer_worker=target_worker, spec_decode_sampler=mock_spec_decode_sampler(acceptance_sampler_method), metrics_collector=metrics_collector)
    kwargs = {'num_gpu_blocks': 1024, 'num_cpu_blocks': 1023}
    worker.initialize_cache(**kwargs)
    draft_worker.initialize_cache.assert_called_once_with(**kwargs)
    target_worker.initialize_cache.assert_called_once_with(**kwargs)
@pytest.mark.parametrize('available_gpu_blocks', [1, 1024])
@pytest.mark.parametrize('available_cpu_blocks', [500])
@pytest.mark.parametrize('target_cache_block_size_bytes', [2 * 2 * 4096])
@pytest.mark.parametrize('draft_kv_size_bytes', [0, 2 * 2 * 768, 2 * 2 * 4096])
@pytest.mark.parametrize('acceptance_sampler_method', ['rejection_sampler', 'typical_acceptance_sampler'])
@pytest.mark.skip_global_cleanup
def test_determine_num_available_blocks(available_gpu_blocks: int, available_cpu_blocks: int, target_cache_block_size_bytes: int, draft_kv_size_bytes: int, acceptance_sampler_method: str):
    draft_worker = mock_worker(cls=MultiStepWorker)
    target_worker = mock_worker()
    metrics_collector = MagicMock(spec=AsyncMetricsCollector)
    target_worker.determine_num_available_blocks.return_value = (available_gpu_blocks, available_cpu_blocks)
    target_worker.get_cache_block_size_bytes.return_value = target_cache_block_size_bytes
    draft_worker.get_cache_block_size_bytes.return_value = draft_kv_size_bytes
    worker = SpecDecodeWorker(draft_worker, target_worker, mock_spec_decode_sampler(acceptance_sampler_method), metrics_collector)
    num_gpu_blocks, num_cpu_blocks = worker.determine_num_available_blocks()
    target_worker.determine_num_available_blocks.assert_called_once()
    assert num_cpu_blocks == available_cpu_blocks
    assert num_gpu_blocks == split_num_cache_blocks_evenly(target_cache_block_size_bytes, draft_kv_size_bytes, available_gpu_blocks)
@pytest.mark.parametrize('available_gpu_blocks', list(range(20)) + [1024, 1024 ** 2])
@pytest.mark.parametrize('target_cache_block_size_bytes', [2 * 2 * 4096, 2 * 2 * 8192])
@pytest.mark.parametrize('draft_kv_size_bytes', [0, 2 * 2 * 768, 2 * 2 * 4096])
@pytest.mark.skip_global_cleanup
def test_split_num_cache_blocks_evenly(available_gpu_blocks: int, target_cache_block_size_bytes: int, draft_kv_size_bytes: int):
    num_blocks = split_num_cache_blocks_evenly(target_cache_block_size_bytes, draft_kv_size_bytes, available_gpu_blocks)
    assert num_blocks * target_cache_block_size_bytes + num_blocks * draft_kv_size_bytes <= available_gpu_blocks * target_cache_block_size_bytes
@torch.inference_mode()
def test_populate_seq_ids_with_bonus_tokens():
    batch_size = 10
    k = 5
    vocab_size = 10000
    num_sequences_with_bonus_tokens = 5
    target_worker = mock_worker(vocab_size=vocab_size, use_spec=False)
    metrics_collector = MagicMock(spec=AsyncMetricsCollector)
    target_worker.execute_model.return_value = [MagicMock(spec=SamplerOutput)]
    target_worker.device = 'cuda'
    set_random_seed(1)
    draft_worker = mock_worker(cls=MultiStepWorker)
    draft_worker.device = 'cuda'
    assigned_seq_ids = list(range(batch_size))
    seq_group_metadata_list, _, _ = create_batch(batch_size, k, seq_ids=assigned_seq_ids, prev_output_token_len=10)
    target_token_logprobs = torch.rand(batch_size, k + 1, vocab_size, dtype=torch.float32, device='cuda')
    accepted_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, k + 1), dtype=torch.int64, device='cuda')
    expected_request_id_seq_ids_mapping: dict[str, set[int]] = defaultdict(set)
    for seq_group_metadata in seq_group_metadata_list:
        for seq_id in seq_group_metadata.seq_data:
            expected_request_id_seq_ids_mapping[seq_group_metadata.request_id].add(seq_id)
    seq_indexes_with_bonus_tokens = random.sample(range(batch_size), num_sequences_with_bonus_tokens)
    mask = torch.ones(batch_size, dtype=torch.bool, device='cuda')
    mask[seq_indexes_with_bonus_tokens] = False
    accepted_token_ids[mask, -1:] = -1
    worker = SpecDecodeWorker(draft_worker, target_worker, mock_spec_decode_sampler('rejection_sampler'), disable_logprobs=False, metrics_collector=metrics_collector)
    num_extra_sequence_ids = 10
    worker._seq_with_bonus_token_in_last_step = set(range(batch_size + num_extra_sequence_ids))
    worker._create_output_sampler_list(seq_group_metadata_list=seq_group_metadata_list, accepted_token_ids=accepted_token_ids, target_logprobs=target_token_logprobs, prompt_logprobs=None, k=k, stage_times=(0, 0, 0))
    expected_seq_ids_with_bonus_tokens = set([assigned_seq_ids[i] for i in seq_indexes_with_bonus_tokens])
    additional_sequence_ids = set(range(batch_size, batch_size + num_extra_sequence_ids))
    assert worker._seq_with_bonus_token_in_last_step == expected_seq_ids_with_bonus_tokens.union(additional_sequence_ids)
    assert worker._request_id_seq_id_mapping == expected_request_id_seq_ids_mapping
@torch.inference_mode()
def test_handle_finished_requests():
    batch_size = 32
    k = 3
    draft_worker = mock_worker(cls=MultiStepWorker)
    target_worker = mock_worker()
    metrics_collector = MagicMock(spec=AsyncMetricsCollector)
    worker = SpecDecodeWorker(draft_worker, target_worker, mock_spec_decode_sampler('rejection_sampler'), metrics_collector)
    worker._request_id_seq_id_mapping = {'request-1': {1, 2, 3}, 'request-2': {4, 5, 6, 7}, 'request-3': {8, 9}, 'request-4': {10, 11}}
    worker._seq_with_bonus_token_in_last_step = {1, 4, 5, 8, 9, 10}
    exception_secret = 'artificial stop'
    draft_worker.get_spec_proposals.side_effect = ValueError(exception_secret)
    seq_group_metadata_list, _, _ = create_batch(batch_size, k)
    execute_model_req = ExecuteModelRequest(seq_group_metadata_list=seq_group_metadata_list, num_lookahead_slots=k, finished_requests_ids=['request-1', 'request-3'])
    with pytest.raises(ValueError, match=exception_secret):
        worker.execute_model(execute_model_req=execute_model_req)
    assert worker._request_id_seq_id_mapping == {'request-2': {4, 5, 6, 7}, 'request-4': {10, 11}}
    assert worker._seq_with_bonus_token_in_last_step == {4, 5, 10}
@pytest.mark.parametrize('k', [3])
@pytest.mark.parametrize('batch_size', [2, 32])
@pytest.mark.parametrize('batch_composition', ['prefill_only', 'decode_only', 'mixed'])
@torch.inference_mode()
def test_chunked_prefill_flow(k: int, batch_size: int, batch_composition: str):
    vocab_size = 32000
    draft_worker = mock_worker(cls=MultiStepWorker)
    target_worker = mock_worker()
    metrics_collector = MagicMock(spec=AsyncMetricsCollector)
    worker = SpecDecodeWorker(draft_worker, target_worker, mock_spec_decode_sampler('rejection_sampler'), disable_logprobs=False, metrics_collector=metrics_collector)
    exception_secret = 'artificial stop'
    worker.scorer = mock_worker(BatchExpansionTop1Scorer)
    worker.scorer.score_proposals.side_effect = ValueError(exception_secret)
    decodes, _, _ = create_batch(batch_size, k)
    prefill, _, _ = create_batch(batch_size, k, prefill_chunk_size=4, seq_ids=list(range(batch_size, batch_size * 2)))
    if batch_composition == 'prefill_only':
        n_prefills = batch_size
    elif batch_composition == 'decode_only':
        n_prefills = 0
    else:
        n_prefills = random.randint(1, batch_size - 1)
    n_decodes = batch_size - n_prefills
    prefill = random.sample(prefill, n_prefills)
    decodes = random.sample(decodes, n_decodes)
    target_group_metadata_list = prefill + decodes
    execute_model_req = ExecuteModelRequest(seq_group_metadata_list=target_group_metadata_list, num_lookahead_slots=k if n_decodes > 0 else 0)
    target_token_ids = torch.randint(low=0, high=vocab_size, size=(1, batch_size * (k + 1)), dtype=torch.int64, device='cuda')
    target_token_probs = torch.rand(1, batch_size * (k + 1), vocab_size, dtype=torch.float32, device='cuda')
    target_token_logprobs = torch.rand(1, batch_size * (k + 1), vocab_size, dtype=torch.float32, device='cuda')
    target_output = create_sampler_output_list(target_token_ids, target_token_probs, target_token_logprobs)
    target_worker.execute_model.return_value = [target_output[0]]
    if not len(decodes):
        worker.execute_model(execute_model_req=execute_model_req)
        draft_worker.execute_model.assert_called_once_with(execute_model_req)
        target_worker.execute_model.assert_called_once_with(execute_model_req)
    else:
        with pytest.raises(ValueError, match=exception_secret):
            worker.execute_model(execute_model_req=execute_model_req)
        assert draft_worker.get_spec_proposals.call_count == 1
def test_correctly_load_weight_for_eagle():
    seed = 100
    block_size = 32
    num_gpu_blocks = 8096 // block_size
    target_worker = create_worker(Worker, 'JackFram/llama-68m', block_size, num_gpu_blocks, seed)
    draft_worker = create_worker(MultiStepWorker, 'abhigoyal/aphrodite-eagle-llama-68m-random', block_size, num_gpu_blocks, seed, model_runner_cls=TP1DraftModelRunner)
    spec_decode_sampler = mock_spec_decode_sampler('rejection_sampler')
    worker = SpecDecodeWorker(draft_worker, target_worker, spec_decode_sampler, disable_logprobs=False)
    worker.proposer_worker.maybe_load_lm_head_weight(target_worker.model_runner.model.lm_head.weight.data)
    assert torch.allclose(worker.proposer_worker.worker.model_runner.model.lm_head.weight.data, worker.scorer_worker.model_runner.model.lm_head.weight.data)