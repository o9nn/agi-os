import pytest
from aphrodite.common.config import CacheConfig, SchedulerConfig
from aphrodite.processing.scheduler import Scheduler
from aphrodite.common.sequence import SequenceGroup
from .utils import append_new_token, create_dummy_prompt_encoder_decoder, get_sequence_groups, schedule_and_update_computed_tokens
def test_scheduler_schedule_simple_encoder_decoder():
    block_size = 4
    num_seq_group = 4
    max_model_len = 16
    scheduler_config = SchedulerConfig('generate', max_num_batched_tokens=64, max_num_seqs=num_seq_group, max_model_len=max_model_len)
    cache_config = CacheConfig(block_size, 1.0, 1, 'auto')
    cache_config.num_cpu_blocks = 16
    cache_config.num_gpu_blocks = 16
    scheduler = Scheduler(scheduler_config, cache_config, None)
    running: list[SequenceGroup] = []
    req_id_list = []
    for i in range(num_seq_group):
        req_id = str(i)
        req_id_list.append(req_id)
        _, _, seq_group = create_dummy_prompt_encoder_decoder(req_id, block_size, block_size, block_size)
        scheduler.add_seq_group(seq_group)
        running.append(seq_group)
    num_tokens = block_size * num_seq_group
    seq_group_meta_list, out = schedule_and_update_computed_tokens(scheduler)
    assert all([req_id in scheduler.block_manager.cross_block_tables for req_id in req_id_list])
    assert set(get_sequence_groups(out)) == set(running)
    assert out.num_batched_tokens == num_tokens
    assert not out.blocks_to_copy and (not out.blocks_to_swap_in) and (not out.blocks_to_swap_out)
    assert len(seq_group_meta_list) == num_seq_group
    append_new_token(out, 1)
    seq_group_meta_list, out = schedule_and_update_computed_tokens(scheduler)
    assert all([not (seq_group_meta.encoder_seq_data is None or seq_group_meta.cross_block_table is None) for seq_group_meta in seq_group_meta_list])
    assert set(get_sequence_groups(out)) == set(running)
    assert out.num_batched_tokens == num_seq_group
    assert not out.blocks_to_copy and (not out.blocks_to_swap_in) and (not out.blocks_to_swap_out)
    assert len(seq_group_meta_list) == num_seq_group
    append_new_token(out, 1)
    for req_id in req_id_list:
        scheduler.abort_seq_group(req_id)
        assert req_id not in scheduler.block_manager.cross_block_tables