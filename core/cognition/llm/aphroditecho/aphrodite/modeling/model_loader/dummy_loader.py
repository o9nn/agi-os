import torch.nn as nn
from aphrodite.common.config import LoadConfig, ModelConfig
from aphrodite.modeling.model_loader.base_loader import BaseModelLoader
from aphrodite.modeling.model_loader.weight_utils import initialize_dummy_weights
class DummyModelLoader(BaseModelLoader):
    def __init__(self, load_config: LoadConfig):
        super().__init__(load_config)
        if load_config.model_loader_extra_config:
            raise ValueError(f'Model loader extra config is not supported for load format {load_config.load_format}')
    def download_model(self, model_config: ModelConfig) -> None:
        pass
    def load_weights(self, model: nn.Module, model_config: ModelConfig) -> None:
        initialize_dummy_weights(model)