from collections.abc import Sequence
from typing import Optional, Union
import regex as re
from transformers import PreTrainedTokenizerBase
from aphrodite.endpoints.openai.protocol import ChatCompletionRequest, DeltaMessage
from aphrodite.reasoning import ReasoningParser, ReasoningParserManager
@ReasoningParserManager.register_module('step3')
class Step3ReasoningParser(ReasoningParser):
    def __init__(self, tokenizer: PreTrainedTokenizerBase):
        super().__init__(tokenizer)
        self.think_end_token = '</think>'
        self.reasoning_regex = re.compile(f'(.*?){self.think_end_token}', re.DOTALL)
        if not self.model_tokenizer:
            raise ValueError('The model tokenizer must be passed to the ReasoningParser constructor during construction.')
        self.think_end_token_id = self.vocab.get(self.think_end_token)
        if self.think_end_token_id is None:
            raise RuntimeError('Step3 reasoning parser could not locate think end token in the tokenizer!')
    def extract_reasoning_content_streaming(self, previous_text: str, current_text: str, delta_text: str, previous_token_ids: Sequence[int], current_token_ids: Sequence[int], delta_token_ids: Sequence[int]) -> Union[DeltaMessage, None]:
        if len(delta_token_ids) == 1 and delta_token_ids[0] == self.think_end_token_id:
            return None
        if self.think_end_token_id in delta_token_ids:
            end_index = delta_text.find(self.think_end_token)
            reasoning_content = delta_text[:end_index]
            content = delta_text[end_index + len(self.think_end_token):]
            return DeltaMessage(reasoning_content=reasoning_content, content=content if content else None)
        elif self.think_end_token_id in previous_token_ids:
            return DeltaMessage(content=delta_text)
        else:
            return DeltaMessage(reasoning_content=delta_text)
    def extract_reasoning_content(self, model_output: str, request: ChatCompletionRequest) -> tuple[Optional[str], Optional[str]]:
        if self.think_end_token not in model_output:
            return (model_output, None)
        else:
            end_index = model_output.find(self.think_end_token)
            reasoning_content = model_output[:end_index]
            content = model_output[end_index + len(self.think_end_token):]
            if len(content) == 0:
                content = None
            return (reasoning_content, content)
    def is_reasoning_end(self, input_ids: list[int]) -> bool:
        return self.think_end_token_id in input_ids
    def extract_content_ids(self, input_ids: list[int]) -> list[int]:
        if self.think_end_token_id not in input_ids[:-1]:
            return []
        else:
            return input_ids[input_ids.index(self.think_end_token_id) + 1:]