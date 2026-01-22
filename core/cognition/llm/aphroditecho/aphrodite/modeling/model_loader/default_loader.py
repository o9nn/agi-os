import dataclasses
import glob
import os
import time
from collections.abc import Generator, Iterable
from typing import Optional, cast
import huggingface_hub
import torch
from loguru import logger
from torch import nn
from transformers.utils import SAFE_WEIGHTS_INDEX_NAME
from aphrodite.common import envs
from aphrodite.common.config import LoadConfig, ModelConfig
from aphrodite.modeling.model_loader.base_loader import BaseModelLoader
from aphrodite.modeling.model_loader.weight_utils import download_safetensors_index_file_from_hf, download_weights_from_hf, fastsafetensors_weights_iterator, filter_duplicate_safetensors_files, filter_files_not_needed_for_inference, get_lock, np_cache_weights_iterator, pt_weights_iterator, safetensors_weights_iterator
from aphrodite.platforms import current_platform
class DefaultModelLoader(BaseModelLoader):
    @dataclasses.dataclass
    class Source:
        model_or_path: str
        'The model ID or path.'
        revision: Optional[str]
        'The optional model revision.'
        prefix: str = ''
        'A prefix to prepend to all weights.'
        fall_back_to_pt: bool = True
        'Whether .pt weights can be used.'
        allow_patterns_overrides: Optional[list[str]] = None
        'If defined, weights will load exclusively using these patterns.'
    counter_before_loading_weights: float = 0.0
    counter_after_loading_weights: float = 0.0
    def __init__(self, load_config: LoadConfig):
        super().__init__(load_config)
        if load_config.model_loader_extra_config:
            raise ValueError(f'Model loader extra config is not supported for load format {load_config.load_format}')
    def _maybe_download_from_modelscope(self, model: str, revision: Optional[str]) -> Optional[str]:
        if envs.APHRODITE_USE_MODELSCOPE:
            from modelscope.hub.snapshot_download import snapshot_download
            with get_lock(model, self.load_config.download_dir):
                if not os.path.exists(model):
                    model_path = snapshot_download(model_id=model, cache_dir=self.load_config.download_dir, local_files_only=huggingface_hub.constants.HF_HUB_OFFLINE, revision=revision, ignore_file_pattern=self.load_config.ignore_patterns)
                else:
                    model_path = model
            return model_path
        return None
    def _prepare_weights(self, model_name_or_path: str, revision: Optional[str], fall_back_to_pt: bool, allow_patterns_overrides: Optional[list[str]]) -> tuple[str, list[str], bool]:
        model_name_or_path = self._maybe_download_from_modelscope(model_name_or_path, revision) or model_name_or_path
        is_local = os.path.isdir(model_name_or_path)
        load_format = self.load_config.load_format
        use_safetensors = False
        index_file = SAFE_WEIGHTS_INDEX_NAME
        if load_format == 'auto':
            allow_patterns = ['*.safetensors', '*.bin']
        elif load_format == 'safetensors' or load_format == 'fastsafetensors':
            use_safetensors = True
            allow_patterns = ['*.safetensors']
        elif load_format == 'mistral':
            use_safetensors = True
            allow_patterns = ['consolidated*.safetensors']
            index_file = 'consolidated.safetensors.index.json'
        elif load_format == 'pt':
            allow_patterns = ['*.pt']
        elif load_format == 'npcache':
            allow_patterns = ['*.bin']
        else:
            raise ValueError(f'Unknown load_format: {load_format}')
        if fall_back_to_pt:
            allow_patterns += ['*.pt']
        if allow_patterns_overrides is not None:
            allow_patterns = allow_patterns_overrides
        if not is_local:
            hf_folder = download_weights_from_hf(model_name_or_path, self.load_config.download_dir, allow_patterns, revision, ignore_patterns=self.load_config.ignore_patterns)
        else:
            hf_folder = model_name_or_path
        hf_weights_files: list[str] = []
        for pattern in allow_patterns:
            hf_weights_files += glob.glob(os.path.join(hf_folder, pattern))
            if len(hf_weights_files) > 0:
                if pattern == '*.safetensors':
                    use_safetensors = True
                break
        if use_safetensors:
            if not is_local:
                download_safetensors_index_file_from_hf(model_name_or_path, index_file, self.load_config.download_dir, revision)
            hf_weights_files = filter_duplicate_safetensors_files(hf_weights_files, hf_folder, index_file)
        else:
            hf_weights_files = filter_files_not_needed_for_inference(hf_weights_files)
        if len(hf_weights_files) == 0:
            raise RuntimeError(f'Cannot find any model weights with `{model_name_or_path}`')
        return (hf_folder, hf_weights_files, use_safetensors)
    def _get_weights_iterator_with_size(self, source: 'Source') -> tuple[Generator[tuple[str, torch.Tensor], None, None], int]:
        hf_folder, hf_weights_files, use_safetensors = self._prepare_weights(source.model_or_path, source.revision, source.fall_back_to_pt, source.allow_patterns_overrides)
        total_bytes = 0
        for file_path in hf_weights_files:
            if os.path.exists(file_path):
                total_bytes += os.path.getsize(file_path)
        if self.load_config.load_format == 'npcache':
            assert use_safetensors is False
            weights_iterator = np_cache_weights_iterator(source.model_or_path, self.load_config.download_dir, hf_folder, hf_weights_files, self.load_config.use_tqdm_on_load)
        elif use_safetensors:
            if self.load_config.load_format == 'fastsafetensors':
                weights_iterator = fastsafetensors_weights_iterator(hf_weights_files, self.load_config.use_tqdm_on_load)
            else:
                weights_iterator = safetensors_weights_iterator(hf_weights_files, self.load_config.use_tqdm_on_load)
        else:
            weights_iterator = pt_weights_iterator(hf_weights_files, self.load_config.use_tqdm_on_load, self.load_config.pt_load_map_location)
        if current_platform.is_tpu():
            import torch_xla.core.xla_model as xm
            def _xla_weights_iterator(iterator: Generator):
                for weights in iterator:
                    yield weights
                    xm.mark_step()
            weights_iterator = _xla_weights_iterator(weights_iterator)
        if self.counter_before_loading_weights == 0.0:
            self.counter_before_loading_weights = time.perf_counter()
        return (((source.prefix + name, tensor) for name, tensor in weights_iterator), total_bytes)
    def _get_weights_iterator(self, source: 'Source') -> Generator[tuple[str, torch.Tensor], None, None]:
        iterator, _ = self._get_weights_iterator_with_size(source)
        return iterator
    def get_all_weights(self, model_config: ModelConfig, model: nn.Module) -> tuple[Iterable[tuple[str, torch.Tensor]], int]:
        primary_weights = DefaultModelLoader.Source(model_config.model, model_config.revision, prefix='', fall_back_to_pt=getattr(model, 'fall_back_to_pt_during_load', True), allow_patterns_overrides=getattr(model, 'allow_patterns_overrides', None))
        primary_iterator, primary_bytes = self._get_weights_iterator_with_size(primary_weights)
        total_bytes = primary_bytes
        iterators = [primary_iterator]
        secondary_weights = cast(Iterable[DefaultModelLoader.Source], getattr(model, 'secondary_weights', ()))
        for source in secondary_weights:
            iterator, bytes_size = self._get_weights_iterator_with_size(source)
            iterators.append(iterator)
            total_bytes += bytes_size
        from itertools import chain
        return (chain.from_iterable(iterators), total_bytes)
    def download_model(self, model_config: ModelConfig) -> None:
        self._prepare_weights(model_config.model, model_config.revision, fall_back_to_pt=True, allow_patterns_overrides=None)
    def load_weights(self, model: nn.Module, model_config: ModelConfig) -> None:
        weights_to_load = {name for name, _ in model.named_parameters()}
        weights_iter, total_bytes = self.get_all_weights(model_config, model)
        from aphrodite.utils import tensor_progress_bar
        loaded_weights = model.load_weights(tensor_progress_bar(weights_iter, total_bytes, 'Loading model weights'))
        self.counter_after_loading_weights = time.perf_counter()
        logger.debug('Loading weights took {:.2f} seconds', self.counter_after_loading_weights - self.counter_before_loading_weights)
        if model_config.quantization is None and loaded_weights is not None:
            weights_not_loaded = weights_to_load - loaded_weights
            if weights_not_loaded:
                raise ValueError(f'Following weights were not initialized from checkpoint: {weights_not_loaded}')