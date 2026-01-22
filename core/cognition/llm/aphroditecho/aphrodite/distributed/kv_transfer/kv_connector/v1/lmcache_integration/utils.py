from typing import TYPE_CHECKING, Union
import os
if TYPE_CHECKING:
    from aphrodite.multimodal.inputs import PlaceholderRange
import torch
from lmcache.config import LMCacheEngineConfig as Config
from lmcache.logging import init_logger
from lmcache.v1.config import LMCacheEngineConfig as V1Config
ENGINE_NAME = 'aphrodite-instance'
def is_false(value: str) -> bool:
    return value.lower() in ('false', '0', 'no', 'n', 'off')
def lmcache_get_config() -> Union[Config, V1Config]:
    if is_false(os.getenv('LMCACHE_USE_EXPERIMENTAL', 'True')):
        logger.warning('Detected LMCACHE_USE_EXPERIMENTAL is set to False. Using legacy configuration is deprecated and will be remove soon! Please set LMCACHE_USE_EXPERIMENTAL to True.')
        LMCacheEngineConfig = Config
    else:
        LMCacheEngineConfig = V1Config
    if 'LMCACHE_CONFIG_FILE' not in os.environ:
        logger.warn('No LMCache configuration file is set. Trying to read configurations from the environment variables.')
        logger.warn('You can set the configuration file through the environment variable: LMCACHE_CONFIG_FILE')
        config = LMCacheEngineConfig.from_env()
    else:
        config_file = os.environ['LMCACHE_CONFIG_FILE']
        logger.info(f'Loading LMCache config file {config_file}')
        config = LMCacheEngineConfig.from_file(config_file)
    return config
def hex_hash_to_int16(s: str) -> int:
    return int(s, 16) & 65535
def apply_mm_hashes_to_token_ids(token_ids: torch.Tensor, mm_hashes: list[str], mm_positions: list['PlaceholderRange']) -> torch.Tensor:
    n = token_ids.size(0)
    for hash_str, placeholder in zip(mm_hashes, mm_positions, strict=False):
        start, length = (placeholder.offset, placeholder.length)
        if start >= n:
            continue
        end = min(start + length, n)
        token_ids[start:end] = hex_hash_to_int16(hash_str)
    return token_ids
def create_lmcache_metadata(aphrodite_config=None, model_config=None, parallel_config=None, cache_config=None):
    from aphrodite.utils import get_kv_cache_torch_dtype
    from lmcache.config import LMCacheEngineMetadata
    config = lmcache_get_config()
    if aphrodite_config is not None:
        model_cfg = aphrodite_config.model_config
        parallel_cfg = aphrodite_config.parallel_config
        cache_cfg = aphrodite_config.cache_config
    else:
        model_cfg = model_config
        parallel_cfg = parallel_config
        cache_cfg = cache_config
    kv_dtype = get_kv_cache_torch_dtype(cache_cfg.cache_dtype, model_cfg.dtype)
    use_mla = False
    if hasattr(model_cfg, 'use_mla') and isinstance(model_cfg.use_mla, bool) and model_cfg.use_mla:
        use_mla = True
    num_layer = model_cfg.get_num_layers(parallel_cfg)
    chunk_size = config.chunk_size
    num_kv_head = model_cfg.get_num_kv_heads(parallel_cfg)
    head_size = model_cfg.get_head_size()
    kv_shape = (num_layer, 1 if use_mla else 2, chunk_size, num_kv_head, head_size)
    metadata = LMCacheEngineMetadata(model_cfg.model, parallel_cfg.world_size, parallel_cfg.rank, 'aphrodite', kv_dtype, kv_shape, use_mla)
    return (metadata, config)