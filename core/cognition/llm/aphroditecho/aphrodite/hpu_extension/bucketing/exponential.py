import itertools
import logging
import math
import os
import numpy as np
from dataclasses import dataclass, field
from typing import List, Set, Tuple
from .common import WeakSingleton
logger = logging.getLogger(__name__)
@dataclass
class HPUExponentialBucketingGlobalState(metaclass=WeakSingleton):
    prompt_bs_bucket_cfg: Tuple[int, int, int, int] = field(init=False)
    decode_bs_bucket_cfg: Tuple[int, int, int, int] = field(init=False)
    prompt_seq_bucket_cfg: Tuple[int, int, int, int] = field(init=False)
    decode_block_bucket_cfg: Tuple[int, int, int, int] = field(init=False)
    prompt_buckets: List[Tuple[int, int]] = field(init=False)
    decode_buckets: List[Tuple[int, int]] = field(init=False)
class HPUExponentialBucketingContext(metaclass=WeakSingleton):
    global_state = HPUExponentialBucketingGlobalState()
    def __init__(self, max_num_seqs, max_num_prefill_seqs, block_size, max_num_batched_tokens, use_merged_prefill, max_model_len=None, max_prompt_seq=None, max_decode_seq=None):
        self.max_num_seqs = max_num_seqs
        self.max_num_prefill_seqs = max_num_prefill_seqs
        self.block_size = block_size
        self.max_num_batched_tokens = max_num_batched_tokens
        self.num_hpu_blocks = None
        self.max_model_len = max_model_len
        self.max_prompt_seq = max_prompt_seq
        self.max_decode_seq = max_decode_seq
        self._setup_buckets()
        self.generate_prompt_buckets()
    def _setup_buckets(self) -> None:
        default_max_prompt_seq = 1024
        default_max_decode_seq = 2048
        if self.max_model_len is None and self.max_prompt_seq is None:
            logger.warning(f'max_model_len and max_prompt_seq are not set. Using default value max_prompt_seq={default_max_prompt_seq}. This may cause issues.')
        if self.max_model_len is None and self.max_decode_seq is None:
            logger.warning(f'max_model_len and max_decode_seq are not set. Using default value max_decode_seq={default_max_decode_seq}. This may cause issues.')
        max_prompt_seq = next((item for item in [self.max_prompt_seq, self.max_model_len] if item is not None), default_max_prompt_seq)
        max_decode_seq = next((item for item in [self.max_decode_seq, self.max_model_len] if item is not None), default_max_decode_seq)
        max_blocks = max(self.block_size, self.max_num_seqs * max_decode_seq // self.block_size)
        prompt_bs_limit = math.ceil(math.log2(self.max_num_prefill_seqs)) + 1
        self.global_state.prompt_bs_bucket_cfg = read_bucket_settings('prompt', 'bs', min=1, step=1, limit=prompt_bs_limit, max=self.max_num_prefill_seqs)
        decode_bs_limit = math.ceil(math.log2(self.max_num_seqs)) + 1
        self.global_state.decode_bs_bucket_cfg = read_bucket_settings('decode', 'bs', min=1, step=1, limit=decode_bs_limit, max=self.max_num_seqs)
        max_prompt_seq_limit = math.ceil(math.log2(max_prompt_seq)) + 1
        self.global_state.prompt_seq_bucket_cfg = read_bucket_settings('prompt', 'seq', min=self.block_size, limit=max_prompt_seq_limit, step=self.block_size, max=max_prompt_seq)
        max_decode_block_limit = math.ceil(math.log2(max_blocks)) + 1
        self.global_state.decode_block_bucket_cfg = read_bucket_settings('decode', 'block', min=self.block_size, limit=max_decode_block_limit, step=self.block_size, max=max_blocks)
        msg = f'Prompt bucket config (min, step, max_warmup, limit) bs:{self.global_state.prompt_bs_bucket_cfg}, seq:{self.global_state.prompt_seq_bucket_cfg}'
        logger.info(msg)
        msg = f'Decode bucket config (min, step, max_warmup, limit) bs:{self.global_state.decode_bs_bucket_cfg}, block:{self.global_state.decode_block_bucket_cfg}'
        logger.info(msg)
    def generate_prompt_buckets(self):
        self.global_state.prompt_buckets, prompt_omitted_buckets = generate_prompt_buckets(self.global_state.prompt_bs_bucket_cfg, self.global_state.prompt_seq_bucket_cfg, self.max_num_batched_tokens, self.max_model_len)
        msg = f'Generated {len(self.global_state.prompt_buckets)} prompt buckets [bs, seq]: {list(sorted(self.global_state.prompt_buckets))}'
        logger.info(msg)
        msg = f'Omitted {len(prompt_omitted_buckets)} prompt buckets due to exceeded token budget (max_num_batched_tokens={self.max_num_batched_tokens})'
        logger.info(msg)
        msg = f'Omitted prompt buckets: {list(sorted(prompt_omitted_buckets))}'
        logger.info(msg)
    def generate_decode_buckets(self, max_blocks):
        self.global_state.decode_buckets = generate_decode_buckets(self.global_state.decode_bs_bucket_cfg, self.global_state.decode_block_bucket_cfg, max_blocks, self.max_model_len, self.block_size)
        logger.info(f'Generated {len(self.global_state.decode_buckets)} decode buckets [bs, total_blocks]: {list(sorted(self.global_state.decode_buckets))}')
    def get_max_prompt_shape(self):
        return (self.global_state.prompt_bs_bucket_cfg[-2], self.global_state.prompt_seq_bucket_cfg[-2])
    def get_padded_prompt_batch_size(self, batch_size):
        return find_bucket(self.prompt_buckets, batch_size, 0)
    def get_padded_decode_batch_size(self, batch_size):
        return find_bucket(self.decode_buckets, batch_size, 0)
    def get_padded_prompt_seq_len(self, seq_len):
        return find_bucket(self.prompt_buckets, seq_len, 1)
    def get_padded_decode_num_blocks(self, num_blocks):
        assert self.num_hpu_blocks is not None, 'num_hpu_blocks is not set'
        bucket_size = find_bucket(self.decode_buckets, num_blocks, 1)
        return min(bucket_size, self.num_hpu_blocks)
    def get_padded_batch_size(self, batch_size, is_prompt):
        if is_prompt:
            return self.get_padded_prompt_batch_size(batch_size)
        return self.get_padded_decode_batch_size(batch_size)
    def get_padded_seq_or_block(self, seq_or_block, is_prompt):
        if is_prompt:
            return self.get_padded_prompt_seq_len(seq_or_block)
        return self.get_padded_decode_num_blocks(seq_or_block)
    def get_closest_prompt_bucket(self, target):
        return get_closest_bucket(self.prompt_buckets, target)
    def get_closest_decode_bucket(self, target):
        return get_closest_bucket(self.decode_buckets, target)
    @property
    def prompt_buckets(self):
        return self.global_state.prompt_buckets
    @property
    def decode_buckets(self):
        return self.global_state.decode_buckets
    @classmethod
    def get_instance(cls):
        assert cls in cls._instances, 'Singleton instance not initialized'
        return type(cls)._instances[cls]
def read_bucket_settings(phase: str, dim: str, **defaults):
    params = ['min', 'step', 'max', 'limit']
    hidden_params = ['min', 'step', 'max']
    env_vars = [f'APHRODITE_{phase}_{dim}_BUCKET_{p}'.upper() for p in params]
    default_values = [defaults[p] for p in params]
    values = [int(d if p in hidden_params else os.environ.get(e, d)) for p, e, d in zip(params, env_vars, default_values)]
    for p, e, v, d in zip(params, env_vars, values, default_values):
        prefix = '[non-modifiable] ' if p in hidden_params else ''
        suffix = '' if p in hidden_params else ' (default: {})' % d
        logger_call = logger.debug if p in hidden_params else logger.info
        logger_call(f'{prefix}{e}={v}{suffix}')
    return values
def find_bucket(buckets, value, dim=None):
    if dim is not None:
        buckets = get_buckets_single_dim(buckets, dim)
    try:
        return next((p for p in sorted(buckets) if p >= value))
    except StopIteration:
        import pdb
        pdb.set_trace()
def get_buckets_single_dim(buckets, dim):
    return [b[dim] for b in buckets]
def get_closest_bucket(buckets, target):
    distances = [np.linalg.norm(b - target) for b in buckets]
    sorted_indices = sorted(range(len(distances)), key=lambda k: distances[k])
    is_valid_bucket = [b[0] >= target[0] and b[1] >= target[1] for b in buckets]
    return next((buckets[idx] for idx in sorted_indices if is_valid_bucket))
def generate_prompt_buckets(bs_bucket_config, seq_bucket_config, max_num_batched_tokens=None, max_model_len=None):
    buckets = list(itertools.product(warmup_range_with_limit(bs_bucket_config), warmup_range_with_limit(seq_bucket_config)))
    if len(buckets) == 0:
        msg = f'No buckets could be captured with following config (min, step, max_warmup): bs:{bs_bucket_config}, seq:{seq_bucket_config}'
        raise ValueError(msg)
    filtered_buckets = buckets
    if max_num_batched_tokens is not None:
        filtered_buckets = list(filter(lambda bucket: bucket[0] * bucket[1] <= max_num_batched_tokens and bucket[1] <= max_model_len, buckets))
        if len(filtered_buckets) == 0:
            min_bucket_bs, min_bucket_seq = min(buckets, key=lambda b: b[0] * b[1])
            min_reqd_budget = min_bucket_bs * min_bucket_seq
            msg = f'The current bucketing configuration (min, step, max_warmup): bs:{bs_bucket_config}, seq:{seq_bucket_config} cannot be used with specified max_num_batched_tokens ({max_num_batched_tokens}), as the smallest bucket ({min_reqd_budget}) would exceed token budget. Please increase max_num_batched_tokens or decrease bucket minimum. Ignoring max_num_batched_tokens at risk of out-of-memory errors.'
            logger.warning(msg)
            return (list(sorted(buckets, key=lambda b: (b[0] * b[1], b[1], b[0]))), [])
    captured_buckets = list(sorted(filtered_buckets, key=lambda b: (b[0] * b[1], b[1], b[0])))
    omitted_buckets = list(sorted([x for x in buckets if x not in filtered_buckets]))
    return (captured_buckets, omitted_buckets)
def generate_decode_buckets(bs_bucket_config, blocks_bucket_config, max_blocks, max_model_len, block_size, skip_invalid=False):
    buckets = []
    bs_buckets = warmup_range_with_limit(bs_bucket_config)
    tmp_blocks_bucket_config = blocks_bucket_config
    tmp_blocks_bucket_config = (*tmp_blocks_bucket_config[:2], max_blocks, tmp_blocks_bucket_config[-1])
    block_buckets = warmup_range_with_limit(tmp_blocks_bucket_config)
    last_bucket = max_blocks
    valid_blocks = set()
    if not skip_invalid:
        valid_blocks = set(((bs, x) for x in sorted(block_buckets) for bs in bs_buckets))
    else:
        for bs in bs_buckets:
            max_blocks_per_bs = min(bs * math.ceil(max_model_len / block_size), last_bucket)
            upper_bucket_bound = next((x for x in sorted(block_buckets) if x >= max_blocks_per_bs))
            valid_blocks = set(((bs, x) for x in sorted(block_buckets) if x <= upper_bucket_bound))
    buckets.extend(list(valid_blocks))
    return list(sorted(buckets, key=lambda b: (b[0] * b[1], b[1], b[0])))
def warmup_range_with_limit(config: Tuple[int, int, int, int], fill=True):
    bmin, bstep, bmax, num_buckets = config
    linear_buckets = set(np.arange(bmin, bmax + 1, step=bstep))
    assert num_buckets > 0, 'num_buckets must be a positive integer'
    if num_buckets == 1:
        return [bmax]
    buckets: Set[Tuple[int, int]] = set()
    for i in range(num_buckets):
        power_unpadded = bmin * np.float_power(bmax / bmin, 1.0 / float(num_buckets - 1) * i)
        bucket = math.ceil(power_unpadded / bstep) * bstep
        if fill and bucket in buckets:
            available_buckets = linear_buckets.difference(buckets)
            if len(available_buckets) == 0:
                break
            new_bucket = min(available_buckets, key=lambda x: abs(x - power_unpadded))
            buckets.add(new_bucket)
        else:
            buckets.add(bucket)
    return list(sorted(buckets))