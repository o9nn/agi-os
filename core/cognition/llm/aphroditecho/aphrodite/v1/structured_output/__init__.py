from __future__ import annotations
import multiprocessing
from concurrent.futures import Future, ThreadPoolExecutor
from typing import TYPE_CHECKING, Optional
from aphrodite.common.config import AphroditeConfig
from aphrodite.reasoning import ReasoningParserManager
from aphrodite.transformers_utils.tokenizer_group import init_tokenizer_from_configs
from aphrodite.utils import LazyLoader
from aphrodite.v1.structured_output.backend_guidance import GuidanceBackend
from aphrodite.v1.structured_output.backend_types import StructuredOutputBackend, StructuredOutputGrammar
from aphrodite.v1.structured_output.backend_xgrammar import XgrammarBackend
if TYPE_CHECKING:
    import numpy as np
    import numpy.typing as npt
    import torch
    from aphrodite.reasoning import ReasoningParser
    from aphrodite.v1.request import Request
else:
    torch = LazyLoader('torch', globals(), 'torch')
class StructuredOutputManager:
    def __init__(self, aphrodite_config: AphroditeConfig):
        self.backend: Optional[StructuredOutputBackend] = None
        self.reasoner: Optional[ReasoningParser] = None
        self.aphrodite_config = aphrodite_config
        self._grammar_bitmask: Optional[torch.Tensor] = None
        self._full_mask = torch.tensor(-1, dtype=torch.int32)
        max_batch_size = self.aphrodite_config.scheduler_config.max_num_seqs
        self.fill_bitmask_parallel_threshold = 128
        if self.fill_bitmask_parallel_threshold < max_batch_size:
            self.fill_bitmask_parallel_batch_size = 16
            max_workers = max(1, min(multiprocessing.cpu_count() // 2, 8))
            self.executor_for_fillmask = ThreadPoolExecutor(max_workers=max_workers)
        if not self.aphrodite_config.model_config.skip_tokenizer_init:
            max_workers = max(1, (multiprocessing.cpu_count() + 1) // 2)
            self.executor = ThreadPoolExecutor(max_workers=max_workers)
            self.tokenizer = init_tokenizer_from_configs(model_config=self.aphrodite_config.model_config, scheduler_config=self.aphrodite_config.scheduler_config, lora_config=self.aphrodite_config.lora_config).get_lora_tokenizer(None)
            reasoning_backend = self.aphrodite_config.decoding_config.reasoning_backend
            if reasoning_backend:
                reasoner_cls = ReasoningParserManager.get_reasoning_parser(reasoning_backend)
                self.reasoner = reasoner_cls(tokenizer=self.tokenizer)
    def grammar_init(self, request: Request) -> None:
        if request.structured_output_request is None:
            return
        if TYPE_CHECKING:
            assert request.sampling_params is not None and request.sampling_params.guided_decoding is not None
        if self.backend is None:
            assert request.sampling_params is not None
            backend = request.sampling_params.guided_decoding.backend
            vocab_size = self.aphrodite_config.model_config.get_vocab_size()
            if backend == 'xgrammar':
                self.backend = XgrammarBackend(self.aphrodite_config, tokenizer=self.tokenizer, vocab_size=vocab_size)
            elif backend == 'guidance':
                self.backend = GuidanceBackend(self.aphrodite_config, tokenizer=self.tokenizer, vocab_size=vocab_size)
            elif backend == 'outlines':
                from aphrodite.v1.structured_output.backend_outlines import OutlinesBackend
                self.backend = OutlinesBackend(self.aphrodite_config, tokenizer=self.tokenizer, vocab_size=vocab_size)
            else:
                raise ValueError(f'Unsupported structured output backend: {backend}')
        grammar = self.executor.submit(self._async_create_grammar, request)
        request.structured_output_request.grammar = grammar
    def _async_create_grammar(self, request: Request) -> StructuredOutputGrammar:
        key = request.structured_output_request.structured_output_key
        request_type, grammar_spec = key
        assert self.backend is not None
        return self.backend.compile_grammar(request_type, grammar_spec)
    def _fill_bitmasks(self, batch: list[tuple[StructuredOutputGrammar, int, bool]]) -> None:
        assert self._grammar_bitmask is not None
        for grammar, index, apply_bitmask in batch:
            if apply_bitmask and (not grammar.is_terminated()):
                grammar.fill_bitmask(self._grammar_bitmask, index)
            else:
                self._grammar_bitmask[index].fill_(self._full_mask)
    def _async_submit_fill_bitmask(self, batch: list[tuple[StructuredOutputGrammar, int, bool]]) -> Future:
        return self.executor_for_fillmask.submit(self._fill_bitmasks, batch)
    def grammar_bitmask(self, requests: dict[str, Request], structured_output_request_ids: dict[str, int], scheduled_spec_decode_tokens: dict[str, list[int]]) -> Optional[npt.NDArray[np.int32]]:
        if not structured_output_request_ids:
            return None
        max_num_spec_tokens = 0
        if self.aphrodite_config.speculative_config is not None:
            max_num_spec_tokens = self.aphrodite_config.speculative_config.num_speculative_tokens
        if self._grammar_bitmask is None:
            assert self.backend is not None
            max_batch_size = self.aphrodite_config.scheduler_config.max_num_seqs
            self._grammar_bitmask = self.backend.allocate_token_bitmask(max_batch_size * (1 + max_num_spec_tokens))
        cumulative_index = 0
        ordered_seq = sorted(structured_output_request_ids.items(), key=lambda x: x[1])
        if len(ordered_seq) > self.fill_bitmask_parallel_threshold and max_num_spec_tokens == 0:
            promises = []
            batch = []
            for req_id, _ in ordered_seq:
                request = requests[req_id]
                structured_output_request = request.structured_output_request
                if TYPE_CHECKING:
                    assert structured_output_request is not None
                    assert structured_output_request.grammar is not None
                apply_bitmask = self.should_fill_bitmask(request)
                batch.append((structured_output_request.grammar, cumulative_index, apply_bitmask))
                if len(batch) == self.fill_bitmask_parallel_batch_size:
                    promises.append(self._async_submit_fill_bitmask(batch))
                    batch = []
                cumulative_index += 1
            if batch:
                promises.append(self._async_submit_fill_bitmask(batch))
            for promise in promises:
                promise.result()
        else:
            for req_id, _ in ordered_seq:
                request = requests[req_id]
                structured_output_request = request.structured_output_request
                if TYPE_CHECKING:
                    assert structured_output_request is not None
                    assert structured_output_request.grammar is not None
                apply_bitmask = self.should_fill_bitmask(request)
                state_advancements = 0
                req_tokens = scheduled_spec_decode_tokens.get(req_id, [])
                for i, token in enumerate(req_tokens + [None]):
                    self._fill_bitmasks([(structured_output_request.grammar, cumulative_index, apply_bitmask)])
                    if apply_bitmask and token is not None and (not structured_output_request.grammar.is_terminated()):
                        assert structured_output_request.grammar.accept_tokens(req_id, [token])
                        state_advancements += 1
                    cumulative_index += 1
                if state_advancements > 0:
                    structured_output_request.grammar.rollback(state_advancements)
        bitmask_tensor = self._grammar_bitmask
        if cumulative_index < bitmask_tensor.shape[0]:
            bitmask_tensor = bitmask_tensor[:cumulative_index]
        return bitmask_tensor.numpy()
    def should_fill_bitmask(self, request: Request) -> bool:
        if self.reasoner is not None:
            assert request.structured_output_request is not None
            if request.structured_output_request.reasoning_ended is None:
                request.structured_output_request.reasoning_ended = self.reasoner.is_reasoning_end(request.prompt_token_ids)
            return request.structured_output_request.reasoning_ended
        return True
    def should_advance(self, request: Request) -> bool:
        if not request.use_structured_output:
            return False
        if TYPE_CHECKING:
            assert request.structured_output_request is not None
            assert request.structured_output_request.grammar is not None
        if self.reasoner is not None:
            structured_req = request.structured_output_request
            if structured_req.reasoning_ended:
                return True
            if self.reasoner.is_reasoning_end(request.all_token_ids):
                structured_req.reasoning_ended = True
            return False
        else:
            return True
    def clear_backend(self) -> None:
        if self.backend is not None:
            self.backend.destroy()