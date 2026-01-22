from typing import Optional
import pytest
from aphrodite.lora.request import LoRARequest
from aphrodite.lora.resolver import LoRAResolver, LoRAResolverRegistry
class DummyLoRAResolver(LoRAResolver):
    async def resolve_lora(self, base_model_name: str, lora_name: str) -> Optional[LoRARequest]:
        if lora_name == 'test_lora':
            return LoRARequest(lora_name=lora_name, lora_path=f'/dummy/path/{base_model_name}/{lora_name}', lora_int_id=abs(hash(lora_name)))
        return None
def test_resolver_registry_registration():
    registry = LoRAResolverRegistry
    resolver = DummyLoRAResolver()
    registry.register_resolver('dummy', resolver)
    assert 'dummy' in registry.get_supported_resolvers()
    retrieved_resolver = registry.get_resolver('dummy')
    assert retrieved_resolver is resolver
def test_resolver_registry_duplicate_registration():
    registry = LoRAResolverRegistry
    resolver1 = DummyLoRAResolver()
    resolver2 = DummyLoRAResolver()
    registry.register_resolver('dummy', resolver1)
    registry.register_resolver('dummy', resolver2)
    assert registry.get_resolver('dummy') is resolver2
def test_resolver_registry_unknown_resolver():
    registry = LoRAResolverRegistry
    with pytest.raises(KeyError, match='not found'):
        registry.get_resolver('unknown_resolver')
@pytest.mark.asyncio
async def test_dummy_resolver_resolve():
    dummy_resolver = DummyLoRAResolver()
    base_model_name = 'base_model_test'
    lora_name = 'test_lora'
    result = await dummy_resolver.resolve_lora(base_model_name, lora_name)
    assert isinstance(result, LoRARequest)
    assert result.lora_name == lora_name
    assert result.lora_path == f'/dummy/path/{base_model_name}/{lora_name}'
    result = await dummy_resolver.resolve_lora(base_model_name, 'nonexistent_lora')
    assert result is None