from collections.abc import Sequence
from typing import Optional, Union
import regex as re
from transformers import PreTrainedTokenizerBase
from aphrodite.endpoints.openai.protocol import ChatCompletionRequest, DeltaMessage
from aphrodite.reasoning import ReasoningParser, ReasoningParserManager
@ReasoningParserManager.register_module('granite')
class GraniteReasoningParser(ReasoningParser):
    def __init__(self, tokenizer: PreTrainedTokenizerBase):
        super().__init__(tokenizer)
        self.think_start_expr = "(?:Here's|Here is) my thought process:"
        self.response_start_expr = "(?:Here's|Here is) my response:"
        self.reasoning_regex = re.compile(f'{self.think_start_expr}(.*?){self.response_start_expr}(.*)', re.DOTALL)
        self.valid_think_starts = ["Here's my thought process:", 'Here is my thought process:']
        self.valid_response_starts = ["Here's my response:", 'Here is my response:']
        self.seq_boundary_end = ':'
        self.seq_boundary_start = 'Here'
        self.longest_think_start = max((len(think_start) for think_start in self.valid_think_starts))
    def extract_reasoning_content(self, model_output: str, request: ChatCompletionRequest) -> tuple[Optional[str], Optional[str]]:
        re_match = self.reasoning_regex.findall(model_output)
        if not re_match:
            return (None, model_output)
        reasoning_content, response_content = re_match[0]
        if not response_content:
            return (reasoning_content, None)
        return (reasoning_content, response_content)
    def extract_reasoning_content_streaming(self, previous_text: str, current_text: str, delta_text: str, previous_token_ids: Sequence[int], current_token_ids: Sequence[int], delta_token_ids: Sequence[int]) -> Union[DeltaMessage, None]:
        reasoning_content, resp_seq_len, content = self._get_content_sections(current_text)
        if not reasoning_content:
            delta_message = self._get_delta_message_with_no_reasoning_bounds(current_text, delta_text)
        elif not content:
            delta_message = self._get_delta_message_with_no_response_bounds(current_text, reasoning_content, delta_text)
        else:
            assert resp_seq_len is not None
            delta_message = self._get_delta_message_with_both_bounds(delta_text, reasoning_content, content, current_text, resp_seq_len)
        if not delta_message.content and (not delta_message.reasoning_content):
            return None
        return delta_message
    def _is_reasoning_start_substr(self, text: str) -> bool:
        return any((think_start.startswith(text) for think_start in self.valid_think_starts))
    def _is_response_start_substr(self, text: str) -> bool:
        return any((response_start.startswith(text) for response_start in self.valid_response_starts))
    def _get_delta_message_with_no_reasoning_bounds(self, current_text: str, delta_text: str) -> DeltaMessage:
        prev_longest_length = len(current_text) - len(delta_text)
        is_substr = self._is_reasoning_start_substr(current_text)
        was_substr = self._is_reasoning_start_substr(current_text[:prev_longest_length])
        if was_substr and (not is_substr):
            return DeltaMessage(reasoning_content=None, content=current_text)
        if is_substr:
            return DeltaMessage(reasoning_content=None, content=None)
        return DeltaMessage(reasoning_content=None, content=delta_text)
    def _get_delta_message_with_no_response_bounds(self, current_text: str, reasoning_content: str, delta_text: str) -> DeltaMessage:
        ends_with_start_response_seq = any((current_text.endswith(response_start) for response_start in self.valid_response_starts))
        if reasoning_content is None or ends_with_start_response_seq:
            return DeltaMessage(reasoning_content=None, content=None)
        previous_text = reasoning_content[:-len(delta_text)]
        current_text = reasoning_content
        prev_idx = previous_text.rfind(self.seq_boundary_start)
        delta_idx = delta_text.rfind(self.seq_boundary_start)
        prev_was_substr = self._is_response_start_substr(previous_text[prev_idx:]) if prev_idx >= 0 else False
        delta_continues_substr = self._is_response_start_substr(current_text[prev_idx:]) if prev_idx >= 0 else False
        delta_new_substr = self._is_response_start_substr(delta_text[delta_idx:]) if delta_idx >= 0 else False
        if delta_continues_substr:
            return DeltaMessage(reasoning_content=None, content=None)
        if not prev_was_substr:
            if delta_new_substr:
                return DeltaMessage(reasoning_content=delta_text[:delta_idx], content=None)
            return DeltaMessage(reasoning_content=delta_text, content=None)
        elif delta_new_substr:
            reasoning_content = previous_text[prev_idx:] + delta_text[:delta_idx]
            return DeltaMessage(reasoning_content=reasoning_content, content=None)
        return DeltaMessage(reasoning_content=previous_text[prev_idx:] + delta_text, content=None)
    def _get_delta_message_with_both_bounds(self, delta_text: str, reasoning_content: str, response_content: str, current_text: str, response_seq_len: int) -> DeltaMessage:
        delta_content = delta_text[-len(response_content):]
        reasoning_end_idx = len(delta_text) - (len(response_content) + response_seq_len)
        if reasoning_end_idx < 0:
            delta_reasoning_content = None
        else:
            start_reasoning_content_idx = len(reasoning_content) + response_seq_len + len(response_content) - 1
            delta_offset = len(current_text) - len(delta_text)
            start_offset = start_reasoning_content_idx - delta_offset
            if start_offset < 0:
                start_offset = 0
            delta_reasoning_content = delta_text[start_offset:reasoning_end_idx]
        return DeltaMessage(reasoning_content=delta_reasoning_content, content=delta_content)
    def _get_content_sections(self, current_text: str) -> tuple[Optional[str], Optional[int], Optional[str]]:
        current_chunk_start = 0
        start_reasoning_content = None
        parsed_content = False
        delimiter_idxs = [idx for idx, char in enumerate(current_text) if char == self.seq_boundary_end]
        for current_chunk_end in delimiter_idxs:
            current_chunk = current_text[current_chunk_start:current_chunk_end]
            if start_reasoning_content is None:
                for think_start in self.valid_think_starts:
                    if current_chunk == think_start[:-1]:
                        start_reasoning_content = current_chunk_end + 1
                        current_chunk_start = current_chunk_end + 1
                        break
            elif not parsed_content:
                for response_start in self.valid_response_starts:
                    if current_chunk[-len(response_start) + 1:] == response_start[:-1]:
                        end_reasoning_content = current_chunk_end - len(response_start)
                        reasoning_content = current_text[start_reasoning_content:end_reasoning_content]
                        response_content = current_text[current_chunk_end + 1:]
                        return (reasoning_content, len(response_start), response_content)
        if start_reasoning_content and (not parsed_content):
            return (current_text[start_reasoning_content:], None, None)
        return (None, None, None)