from abc import ABC, abstractmethod
from collections.abc import Set
from dataclasses import dataclass, field
from typing import Optional
from loguru import logger
from aphrodite.lora.request import LoRARequest
class LoRAResolver(ABC):
    @abstractmethod
    async def resolve_lora(self, base_model_name: str, lora_name: str) -> Optional[LoRARequest]:
        pass
@dataclass
class _LoRAResolverRegistry:
    resolvers: dict[str, LoRAResolver] = field(default_factory=dict)
    def get_supported_resolvers(self) -> Set[str]:
        return self.resolvers.keys()
    def register_resolver(self, resolver_name: str, resolver: LoRAResolver) -> None:
        if resolver_name in self.resolvers:
            logger.warning('LoRA resolver {} is already registered, and will be overwritten by the new resolver instance {}.', resolver_name, resolver)
        self.resolvers[resolver_name] = resolver
    def get_resolver(self, resolver_name: str) -> LoRAResolver:
        if resolver_name not in self.resolvers:
            raise KeyError(f"LoRA resolver '{resolver_name}' not found. Available resolvers: {list(self.resolvers.keys())}")
        return self.resolvers[resolver_name]
LoRAResolverRegistry = _LoRAResolverRegistry()