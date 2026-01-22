from typing import TYPE_CHECKING, Optional
from aphrodite.common import envs
from aphrodite.distributed.kv_transfer.kv_connector.base import KVConnectorBaseType
from aphrodite.distributed.kv_transfer.kv_connector.factory import KVConnectorFactory
from aphrodite.distributed.kv_transfer.kv_connector.v1 import KVConnectorBase_V1, KVConnectorRole
if TYPE_CHECKING:
    from aphrodite.common.config import AphroditeConfig
_KV_CONNECTOR_AGENT: Optional[KVConnectorBaseType] = None
def get_kv_transfer_group() -> KVConnectorBaseType:
    assert _KV_CONNECTOR_AGENT is not None, 'disaggregated KV cache transfer parallel group is not initialized'
    return _KV_CONNECTOR_AGENT
def has_kv_transfer_group() -> bool:
    return _KV_CONNECTOR_AGENT is not None
def is_v1_kv_transfer_group(connector: Optional[KVConnectorBaseType]=None) -> bool:
    if connector is None:
        connector = _KV_CONNECTOR_AGENT
    if connector is None:
        return False
    return isinstance(connector, KVConnectorBase_V1)
def ensure_kv_transfer_initialized(aphrodite_config: 'AphroditeConfig') -> None:
    global _KV_CONNECTOR_AGENT
    if aphrodite_config.kv_transfer_config is None:
        return
    if aphrodite_config.kv_transfer_config.is_kv_transfer_instance and _KV_CONNECTOR_AGENT is None:
        if envs.APHRODITE_USE_V1:
            _KV_CONNECTOR_AGENT = KVConnectorFactory.create_connector(config=aphrodite_config, role=KVConnectorRole.WORKER)
        else:
            raise ValueError('V0 is no longer supported')