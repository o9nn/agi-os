from __future__ import annotations
import enum
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import TYPE_CHECKING
if TYPE_CHECKING:
    import torch
    from aphrodite.common.config import AphroditeConfig
    from aphrodite.transformers_utils.tokenizer import AnyTokenizer
class StructuredOutputOptions(enum.Enum):
    JSON = enum.auto()
    JSON_OBJECT = enum.auto()
    REGEX = enum.auto()
    GRAMMAR = enum.auto()
    CHOICE = enum.auto()
    STRUCTURAL_TAG = enum.auto()
StructuredOutputKey = tuple[StructuredOutputOptions, str]
class StructuredOutputGrammar(ABC):
    @abstractmethod
    def accept_tokens(self, request_id: str, tokens: list[int]) -> bool:
    @abstractmethod
    def validate_tokens(self, tokens: list[int]) -> list[int]:
    @abstractmethod
    def rollback(self, num_tokens: int) -> None:
    @abstractmethod
    def fill_bitmask(self, bitmask: torch.Tensor, batch_index: int) -> None:
    @abstractmethod
    def is_terminated(self) -> bool:
    @abstractmethod
    def reset(self):
@dataclass
class StructuredOutputBackend(ABC):
    aphrodite_config: AphroditeConfig
    tokenizer: AnyTokenizer
    vocab_size: int
    @abstractmethod
    def compile_grammar(self, request_type: StructuredOutputOptions, grammar_spec: str) -> StructuredOutputGrammar:
    @abstractmethod
    def allocate_token_bitmask(self, max_num_seqs: int) -> torch.Tensor:
    @abstractmethod
    def destroy(self):