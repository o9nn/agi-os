import torch
import torch.nn as nn
from loguru import logger
from aphrodite.common.config import LogprobsMode
from aphrodite.utils import is_pin_memory_available
from aphrodite.common.logger import log_once
from aphrodite.common.sampling_params import SamplerID
from aphrodite.v1.outputs import LogprobsTensors, SamplerOutput
from aphrodite.v1.sample.metadata import SamplingMetadata
from aphrodite.v1.sample.ops import SamplingOps
from aphrodite.v1.sample.ops.temperatures import apply_all_temperatures
from aphrodite.v1.sample.ops.topk_topp_sampler import TopKTopPSampler
_SAMPLING_EPS = 1e-05
DEFAULT_SAMPLER_ORDER = [SamplerID.DRY, SamplerID.PENALTIES, SamplerID.NO_REPEAT_NGRAM, SamplerID.TEMPERATURE, SamplerID.TOP_NSIGMA, SamplerID.TOP_P_TOP_K, SamplerID.TOP_A, SamplerID.MIN_P, SamplerID.TFS, SamplerID.ETA_CUTOFF, SamplerID.EPSILON_CUTOFF, SamplerID.TYPICAL_P, SamplerID.QUADRATIC, SamplerID.XTC]
class Sampler(nn.Module):
    def __init__(self, logprobs_mode: LogprobsMode='raw_logprobs'):
        super().__init__()
        self.topk_topp_sampler = TopKTopPSampler()
        self.sampling_ops = SamplingOps()
        self.pin_memory = is_pin_memory_available()
        self.logprobs_mode = logprobs_mode
    def forward(self, logits: torch.Tensor, sampling_metadata: SamplingMetadata) -> SamplerOutput:
        num_logprobs = sampling_metadata.max_num_logprobs
        if num_logprobs is not None:
            if self.logprobs_mode == 'raw_logprobs':
                raw_logprobs = self.compute_logprobs(logits)
            elif self.logprobs_mode == 'raw_logits':
                raw_logprobs = logits.clone()
        logits = logits.to(torch.float32)
        logits = self.sampling_ops.apply_allowed_token_ids(logits, sampling_metadata)
        logits = self.sampling_ops.apply_bad_words(logits, sampling_metadata)
        for processor in sampling_metadata.logitsprocs.non_argmax_invariant:
            logits = processor.apply(logits)
        logits = self._execute_samplers_in_order(logits, sampling_metadata)
        for processor in sampling_metadata.logitsprocs.argmax_invariant:
            logits = processor.apply(logits)
        if num_logprobs is not None:
            if self.logprobs_mode == 'processed_logprobs':
                raw_logprobs = self.compute_logprobs(logits)
            elif self.logprobs_mode == 'processed_logits':
                raw_logprobs = logits.clone()
        sampled = self.sample(logits, sampling_metadata)
        sampled = sampled.long()
        logprobs_tensors = None if num_logprobs is None else self.gather_logprobs(raw_logprobs, num_logprobs, token_ids=sampled)
        sampled = sampled.to(torch.int32)
        sampler_output = SamplerOutput(sampled_token_ids=sampled.unsqueeze(-1), logprobs_tensors=logprobs_tensors)
        return sampler_output
    def apply_temperature(self, logits: torch.Tensor, sampling_metadata: SamplingMetadata) -> torch.Tensor:
        return apply_all_temperatures(logits, sampling_metadata)
    def _execute_samplers_in_order(self, logits: torch.Tensor, sampling_metadata: SamplingMetadata) -> torch.Tensor:
        sampler_order = sampling_metadata.sampler_priority
        do_temp_last = sampling_metadata.temperature_last
        if sampler_order is None:
            sampler_order = []
            for sampler_id in DEFAULT_SAMPLER_ORDER:
                if sampler_id == SamplerID.TEMPERATURE and do_temp_last:
                    continue
                sampler_order.append(sampler_id)
                if sampler_id == SamplerID.XTC and do_temp_last:
                    sampler_order.append(SamplerID.TEMPERATURE)
        elif do_temp_last:
            log_once('WARNING', 'Both sampler_priority and temperature_last=True were specified. Using custom sampler_priority order and ignoring temperature_last.')
        logger.debug('Sampler execution order: ')
        for i, sampler_id in enumerate(sampler_order, 1):
            logger.debug(f'{i}. {sampler_id.name}')
        for sampler_id in sampler_order:
            if sampler_id == SamplerID.DRY and sampling_metadata.dry_multiplier is not None:
                logger.debug(f'Applying DRY with dry_multiplier: {sampling_metadata.dry_multiplier}')
                logits = self.sampling_ops.apply_dry(logits, sampling_metadata)
            elif sampler_id == SamplerID.PENALTIES and (not sampling_metadata.no_penalties):
                logger.debug('Applying penalties')
                logits = self.sampling_ops.apply_penalties(logits, sampling_metadata)
            elif sampler_id == SamplerID.NO_REPEAT_NGRAM and sampling_metadata.no_repeat_ngram_size is not None:
                logger.debug(f'Applying no_repeat_ngram with size: {sampling_metadata.no_repeat_ngram_size}')
                logits = self.sampling_ops.apply_no_repeat_ngram(logits, sampling_metadata)
            elif sampler_id == SamplerID.TEMPERATURE and sampling_metadata.temperature is not None:
                logger.debug(f'Applying temperature: {sampling_metadata.temperature}')
                logits = self.apply_temperature(logits, sampling_metadata)
            elif sampler_id == SamplerID.TOP_NSIGMA and sampling_metadata.top_nsigma is not None:
                logger.debug(f'Applying Top-Nsigma with nsigma: {sampling_metadata.top_nsigma}')
                logits = self.sampling_ops.apply_top_nsigma(logits, sampling_metadata)
            elif sampler_id == SamplerID.TOP_P_TOP_K:
                if sampling_metadata.top_k is not None:
                    logger.debug(f'Applying Top-k with top_k: {sampling_metadata.top_k}')
                    for i, top_k_val in enumerate(sampling_metadata.top_k):
                        if top_k_val < logits.size(-1):
                            top_k_values, _ = torch.topk(logits[i], int(top_k_val.item()), dim=-1)
                            top_k_threshold = top_k_values[-1] if top_k_values.numel() > 0 else -float('inf')
                            logits[i] = torch.where(logits[i] >= top_k_threshold, logits[i], torch.tensor(-float('inf'), device=logits.device, dtype=logits.dtype))
                if sampling_metadata.top_p is not None:
                    logger.debug(f'Applying Top-p with top_p: {sampling_metadata.top_p}')
                    for i, top_p_val in enumerate(sampling_metadata.top_p):
                        if top_p_val < 1.0:
                            sorted_logits, sorted_indices = torch.sort(logits[i], descending=True, dim=-1)
                            cumulative_probs = torch.softmax(sorted_logits, dim=-1).cumsum(dim=-1)
                            sorted_indices_to_remove = cumulative_probs > top_p_val
                            sorted_indices_to_remove[1:] = sorted_indices_to_remove[:-1].clone()
                            sorted_indices_to_remove[0] = 0
                            indices_to_remove = sorted_indices_to_remove.scatter(0, sorted_indices, sorted_indices_to_remove)
                            logits[i][indices_to_remove] = -float('inf')
            elif sampler_id == SamplerID.TOP_A and sampling_metadata.top_a is not None:
                logger.debug(f'Applying Top-a with top_a: {sampling_metadata.top_a}')
                logits = self.sampling_ops.apply_top_a(logits, sampling_metadata)
            elif sampler_id == SamplerID.TFS and sampling_metadata.tfs is not None:
                logger.debug(f'Applying TFS with tfs: {sampling_metadata.tfs}')
                logits = self.sampling_ops.apply_tfs(logits, sampling_metadata)
            elif sampler_id == SamplerID.ETA_CUTOFF and sampling_metadata.eta_cutoff is not None:
                logger.debug(f'Applying ETA Cutoff with eta_cutoff: {sampling_metadata.eta_cutoff}')
                logits = self.sampling_ops.apply_eta_cutoff(logits, sampling_metadata)
            elif sampler_id == SamplerID.EPSILON_CUTOFF and sampling_metadata.epsilon_cutoff is not None:
                logger.debug(f'Applying Epsilon Cutoff with epsilon_cutoff: {sampling_metadata.epsilon_cutoff}')
                logits = self.sampling_ops.apply_epsilon_cutoff(logits, sampling_metadata)
            elif sampler_id == SamplerID.TYPICAL_P and sampling_metadata.typical_p is not None:
                logger.debug(f'Applying Typical P with typical_p: {sampling_metadata.typical_p}')
                logits = self.sampling_ops.apply_typical_p(logits, sampling_metadata)
            elif sampler_id == SamplerID.QUADRATIC and sampling_metadata.quadratic_smoothing_factor is not None:
                logger.debug(f'Applying Quadratic with smoothing_factor: {sampling_metadata.quadratic_smoothing_factor}')
                logits = self.sampling_ops.apply_quadratic(logits, sampling_metadata)
            elif sampler_id == SamplerID.XTC and sampling_metadata.xtc_threshold is not None:
                logger.debug(f'Applying XTC with threshold: {sampling_metadata.xtc_threshold}')
                logits = self.sampling_ops.apply_xtc(logits, sampling_metadata)
        return logits
    def greedy_sample(self, logits: torch.Tensor) -> torch.Tensor:
        return logits.argmax(dim=-1).view(-1)
    def sample(self, logits: torch.Tensor, sampling_metadata: SamplingMetadata) -> torch.Tensor:
        assert not (sampling_metadata.all_greedy and sampling_metadata.all_random)
        if sampling_metadata.all_random:
            greedy_sampled = None
        else:
            greedy_sampled = self.greedy_sample(logits)
            if sampling_metadata.all_greedy:
                return greedy_sampled
        assert sampling_metadata.temperature is not None
        random_sampled = self.topk_topp_sampler(logits, sampling_metadata.generators, None, None)
        if sampling_metadata.skew is not None:
            probs = logits.softmax(dim=-1, dtype=torch.float32)
            probs = self.sampling_ops.apply_skew(probs, sampling_metadata)
            logits = torch.log(probs)
        if greedy_sampled is None:
            return random_sampled
        sampled = torch.where(sampling_metadata.temperature < _SAMPLING_EPS, greedy_sampled, random_sampled, out=greedy_sampled)
        return sampled
    def compute_logprobs(self, logits: torch.Tensor) -> torch.Tensor:
        return logits.log_softmax(dim=-1, dtype=torch.float32)
    def gather_logprobs(self, logprobs: torch.Tensor, num_logprobs: int, token_ids: torch.Tensor) -> LogprobsTensors:
        assert token_ids.dtype == torch.int64
        topk_logprobs, topk_indices = torch.topk(logprobs, num_logprobs, dim=-1)
        token_ids = token_ids.unsqueeze(-1)
        token_logprobs = logprobs.gather(-1, token_ids)
        token_ranks = batched_count_greater_than(logprobs, token_logprobs)
        indices = torch.cat((token_ids, topk_indices), dim=1)
        logprobs = torch.cat((token_logprobs, topk_logprobs), dim=1)
        indices = indices.to(torch.int32)
        return LogprobsTensors(indices, logprobs, token_ranks)