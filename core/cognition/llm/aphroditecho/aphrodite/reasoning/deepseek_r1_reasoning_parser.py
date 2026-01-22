from collections.abc import Sequence
from typing import Optional, Union
from transformers import PreTrainedTokenizerBase
from aphrodite.endpoints.openai.protocol import ChatCompletionRequest, DeltaMessage
from aphrodite.reasoning import ReasoningParser, ReasoningParserManager
@ReasoningParserManager.register_module('deepseek_r1')
class DeepSeekR1ReasoningParser(ReasoningParser):
    start_token_id: int
    end_token_id: int
    start_token: str = '<think>'
    end_token: str = '</think>'
    def __init__(self, tokenizer: PreTrainedTokenizerBase):
        super().__init__(tokenizer)
        if not self.model_tokenizer:
            raise ValueError('The model tokenizer must be passed to the ReasoningParser constructor during construction.')
        self.start_token_id = self.vocab.get(self.start_token)
        self.end_token_id = self.vocab.get(self.end_token)
        if self.start_token_id is None or self.end_token_id is None:
            raise RuntimeError('DeepSeek R1 reasoning parser could not locate think start/end tokens in the tokenizer!')
    def is_reasoning_end(self, input_ids: list[int]) -> bool:
        return self.end_token_id in input_ids
    def extract_content_ids(self, input_ids: list[int]) -> list[int]:
        if self.end_token_id not in input_ids[:-1]:
            return []
        else:
            return input_ids[input_ids.index(self.end_token_id) + 1:]
    def extract_reasoning_content_streaming(self, previous_text: str, current_text: str, delta_text: str, previous_token_ids: Sequence[int], current_token_ids: Sequence[int], delta_token_ids: Sequence[int]) -> Union[DeltaMessage, None]:
        if len(delta_token_ids) == 1 and delta_token_ids[0] in [self.start_token_id, self.end_token_id]:
            return None
        if self.start_token_id in previous_token_ids:
            if self.end_token_id in delta_token_ids:
                end_index = delta_text.find(self.end_token)
                reasoning_content = delta_text[:end_index]
                content = delta_text[end_index + len(self.end_token):]
                return DeltaMessage(reasoning_content=reasoning_content, content=content if content else None)
            elif self.end_token_id in previous_token_ids:
                return DeltaMessage(content=delta_text)
            else:
                return DeltaMessage(reasoning_content=delta_text)
        elif self.start_token_id in delta_token_ids:
            if self.end_token_id in delta_token_ids:
                start_index = delta_text.find(self.start_token)
                end_index = delta_text.find(self.end_token)
                reasoning_content = delta_text[start_index + len(self.start_token):end_index]
                content = delta_text[end_index + len(self.end_token):]
                return DeltaMessage(reasoning_content=reasoning_content, content=content if content else None)
            else:
                return DeltaMessage(reasoning_content=delta_text)
        elif self.end_token_id in delta_token_ids:
            end_index = delta_text.find(self.end_token)
            reasoning_content = delta_text[:end_index]
            content = delta_text[end_index + len(self.end_token):]
            return DeltaMessage(reasoning_content=reasoning_content, content=content if content else None)
        elif self.end_token_id in previous_token_ids:
            return DeltaMessage(content=delta_text)
        else:
            return DeltaMessage(reasoning_content=delta_text)
    def extract_reasoning_content(self, model_output: str, request: ChatCompletionRequest) -> tuple[Optional[str], Optional[str]]:
        model_output_parts = model_output.partition(self.start_token)
        model_output = model_output_parts[2] if model_output_parts[1] else model_output_parts[0]
        if self.end_token not in model_output:
            return (model_output, None)
        else:
            reasoning_content, _, content = model_output.partition(self.end_token)
            final_content = content or None
            return (reasoning_content, final_content)