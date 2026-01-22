from fractions import Fraction
from typing import Callable, Optional, Union
import torch
from torch.nn import Parameter
from aphrodite.distributed import get_tensor_model_parallel_rank
from aphrodite.modeling.utils import _make_synced_weight_loader
__all__ = ['BaseAphroditeParameter', 'PackedAphroditeParameter', 'PerTensorScaleParameter', 'ModelWeightParameter', 'ChannelQuantScaleParameter', 'GroupQuantScaleParameter', 'PackedColumnParameter', 'RowAphroditeParameter']
class BaseAphroditeParameter(Parameter):
    def __new__(cls, data: torch.Tensor, **kwargs):
        return super().__new__(cls, data=data, requires_grad=False)
    def __init__(self, data: torch.Tensor, weight_loader: Callable):
        from aphrodite.platforms import current_platform
        if current_platform.is_tpu():
            weight_loader = _make_synced_weight_loader(weight_loader)
        self._weight_loader = weight_loader
    @property
    def weight_loader(self):
        return self._weight_loader
    def _is_1d_and_scalar(self, loaded_weight: torch.Tensor):
        cond1 = self.data.ndim == 1 and self.data.numel() == 1
        cond2 = loaded_weight.ndim == 0 and loaded_weight.numel() == 1
        return cond1 and cond2
    def _assert_and_load(self, loaded_weight: torch.Tensor):
        assert self.data.shape == loaded_weight.shape or self._is_1d_and_scalar(loaded_weight)
        self.data.copy_(loaded_weight)
    def load_column_parallel_weight(self, loaded_weight: torch.Tensor):
        self._assert_and_load(loaded_weight)
    def load_row_parallel_weight(self, loaded_weight: torch.Tensor):
        self._assert_and_load(loaded_weight)
    def load_merged_column_weight(self, loaded_weight: torch.Tensor, **kwargs):
        self._assert_and_load(loaded_weight)
    def load_qkv_weight(self, loaded_weight: torch.Tensor, **kwargs):
        self._assert_and_load(loaded_weight)
class _ColumnAphroditeParameter(BaseAphroditeParameter):
    def __init__(self, output_dim: int, **kwargs):
        self._output_dim = output_dim
        super().__init__(**kwargs)
    @property
    def output_dim(self):
        return self._output_dim
    def load_column_parallel_weight(self, loaded_weight: torch.Tensor):
        tp_rank = get_tensor_model_parallel_rank()
        shard_size = self.data.shape[self.output_dim]
        loaded_weight = loaded_weight.narrow(self.output_dim, tp_rank * shard_size, shard_size)
        assert self.data.shape == loaded_weight.shape
        self.data.copy_(loaded_weight)
    def load_merged_column_weight(self, loaded_weight: torch.Tensor, **kwargs):
        shard_offset = kwargs.get('shard_offset')
        shard_size = kwargs.get('shard_size')
        if isinstance(self, (PackedColumnParameter, PackedAphroditeParameter)) and self.packed_dim == self.output_dim:
            shard_size, shard_offset = self.adjust_shard_indexes_for_packing(shard_offset=shard_offset, shard_size=shard_size)
        param_data = self.data
        tp_rank = get_tensor_model_parallel_rank()
        param_data = param_data.narrow(self.output_dim, shard_offset, shard_size)
        loaded_weight = loaded_weight.narrow(self.output_dim, tp_rank * shard_size, shard_size)
        assert param_data.shape == loaded_weight.shape
        param_data.copy_(loaded_weight)
    def load_qkv_weight(self, loaded_weight: torch.Tensor, **kwargs):
        shard_offset = kwargs.get('shard_offset')
        shard_size = kwargs.get('shard_size')
        shard_id = kwargs.get('shard_id')
        num_heads = kwargs.get('num_heads')
        if isinstance(self, (PackedColumnParameter, PackedAphroditeParameter)) and self.output_dim == self.packed_dim:
            shard_size, shard_offset = self.adjust_shard_indexes_for_packing(shard_offset=shard_offset, shard_size=shard_size)
        param_data = self.data
        tp_rank = get_tensor_model_parallel_rank()
        shard_id = tp_rank if shard_id == 'q' else tp_rank // num_heads
        param_data = param_data.narrow(self.output_dim, shard_offset, shard_size)
        loaded_weight = loaded_weight.narrow(self.output_dim, shard_id * shard_size, shard_size)
        assert param_data.shape == loaded_weight.shape
        param_data.copy_(loaded_weight)
class RowAphroditeParameter(BaseAphroditeParameter):
    def __init__(self, input_dim: int, **kwargs):
        self._input_dim = input_dim
        super().__init__(**kwargs)
    @property
    def input_dim(self):
        return self._input_dim
    def load_row_parallel_weight(self, loaded_weight: torch.Tensor):
        tp_rank = get_tensor_model_parallel_rank()
        shard_size = self.data.shape[self.input_dim]
        loaded_weight = loaded_weight.narrow(self.input_dim, tp_rank * shard_size, shard_size)
        if len(loaded_weight.shape) == 0:
            loaded_weight = loaded_weight.reshape(1)
        assert self.data.shape == loaded_weight.shape
        self.data.copy_(loaded_weight)
class ModelWeightParameter(_ColumnAphroditeParameter, RowAphroditeParameter):
    pass
class GroupQuantScaleParameter(_ColumnAphroditeParameter, RowAphroditeParameter):
    pass
class ChannelQuantScaleParameter(_ColumnAphroditeParameter):
    pass
class PerTensorScaleParameter(BaseAphroditeParameter):
    def __init__(self, **kwargs):
        self.qkv_idxs = {'q': 0, 'k': 1, 'v': 2}
        super().__init__(**kwargs)
    def _shard_id_as_int(self, shard_id: Union[str, int]) -> int:
        if isinstance(shard_id, int):
            return shard_id
        assert isinstance(shard_id, str)
        assert shard_id in self.qkv_idxs
        return self.qkv_idxs[shard_id]
    def load_row_parallel_weight(self, *args, **kwargs):
        super().load_row_parallel_weight(*args, **kwargs)
    def load_merged_column_weight(self, *args, **kwargs):
        self._load_into_shard_id(*args, **kwargs)
    def load_qkv_weight(self, *args, **kwargs):
        self._load_into_shard_id(*args, **kwargs)
    def load_column_parallel_weight(self, *args, **kwargs):
        super().load_row_parallel_weight(*args, **kwargs)
    def _load_into_shard_id(self, loaded_weight: torch.Tensor, shard_id: Union[str, int], **kwargs):
        param_data = self.data
        shard_id = self._shard_id_as_int(shard_id)
        if len(loaded_weight.shape) != 0:
            assert loaded_weight.shape[0] == 1
            loaded_weight = loaded_weight[0]
        param_data = param_data[shard_id]
        assert param_data.shape == loaded_weight.shape
        param_data.copy_(loaded_weight)
class PackedColumnParameter(_ColumnAphroditeParameter):
    def __init__(self, packed_factor: Union[int, Fraction], packed_dim: int, marlin_tile_size: Optional[int]=None, bitblas_tile_size: Optional[int]=None, **kwargs):
        self._packed_factor = packed_factor
        self._packed_dim = packed_dim
        self._marlin_tile_size = marlin_tile_size
        self._bitblas_tile_size = bitblas_tile_size
        super().__init__(**kwargs)
    @property
    def packed_dim(self):
        return self._packed_dim
    @property
    def packed_factor(self):
        return self._packed_factor
    @property
    def marlin_tile_size(self):
        return self._marlin_tile_size
    @property
    def bitblas_tile_size(self):
        return self._bitblas_tile_size
    def adjust_shard_indexes_for_packing(self, shard_size, shard_offset):
        return _adjust_shard_indexes_for_packing(shard_size=shard_size, shard_offset=shard_offset, packed_factor=self.packed_factor, marlin_tile_size=self.marlin_tile_size, bitblas_tile_size=self.bitblas_tile_size)
class PackedAphroditeParameter(ModelWeightParameter):
    def __init__(self, packed_factor: Union[int, Fraction], packed_dim: int, marlin_tile_size: Optional[int]=None, bitblas_tile_size: Optional[int]=None, **kwargs):
        self._packed_factor = packed_factor
        self._packed_dim = packed_dim
        self._marlin_tile_size = marlin_tile_size
        self._bitblas_tile_size = bitblas_tile_size
        super().__init__(**kwargs)
    @property
    def packed_dim(self):
        return self._packed_dim
    @property
    def packed_factor(self):
        return self._packed_factor
    @property
    def marlin_tile_size(self):
        return self._marlin_tile_size
    @property
    def bitblas_tile_size(self):
        return self._bitblas_tile_size
    def adjust_shard_indexes_for_packing(self, shard_size, shard_offset):
        return _adjust_shard_indexes_for_packing(shard_size=shard_size, shard_offset=shard_offset, packed_factor=self.packed_factor, marlin_tile_size=self.marlin_tile_size, bitblas_tile_size=self.bitblas_tile_size)
class BlockQuantScaleParameter(_ColumnAphroditeParameter, RowAphroditeParameter):
    pass
def permute_param_layout_(param: BaseAphroditeParameter, input_dim: int, output_dim: int, **kwargs) -> BaseAphroditeParameter:
    curr_input_dim = getattr(param, 'input_dim', None)
    curr_output_dim = getattr(param, 'output_dim', None)
    if curr_input_dim is None or curr_output_dim is None:
        assert param.data.dim() == 2, 'permute_param_layout_ only supports 2D parameters when either input_dim or output_dim is not set'
    if curr_input_dim is None:
        assert curr_output_dim is not None, 'either input or output dim must be set'
        curr_input_dim = (curr_output_dim + 1) % 2
    if curr_output_dim is None:
        assert curr_input_dim is not None, 'either input or output dim must be set'
        curr_output_dim = (curr_input_dim + 1) % 2
    perm = [i for i in range(param.data.dim()) if i not in [curr_input_dim, curr_output_dim]]
    perm.insert(input_dim, curr_input_dim)
    perm.insert(output_dim, curr_output_dim)
    if 'packed_dim' in kwargs:
        assert hasattr(param, 'packed_dim') and param.packed_dim == perm[kwargs['packed_dim']], "permute_param_layout_ currently doesn't support repacking"
    param.data = param.data.permute(*perm)
    if hasattr(param, '_input_dim'):
        param._input_dim = input_dim
    if hasattr(param, '_output_dim'):
        param._output_dim = output_dim
    if 'packed_dim' in kwargs and hasattr(param, '_packed_dim'):
        param._packed_dim = kwargs['packed_dim']
    return param
def _adjust_shard_indexes_for_marlin(shard_size, shard_offset, marlin_tile_size):
    return (shard_size * marlin_tile_size, shard_offset * marlin_tile_size)
def _adjust_shard_indexes_for_bitblas(shard_size, shard_offset, bitblas_tile_size):
    return (shard_size // bitblas_tile_size, shard_offset // bitblas_tile_size)
def _adjust_shard_indexes_for_packing(shard_size, shard_offset, packed_factor, marlin_tile_size, bitblas_tile_size):
    shard_size = shard_size // packed_factor
    shard_offset = shard_offset // packed_factor
    if marlin_tile_size is not None:
        return _adjust_shard_indexes_for_marlin(shard_size=shard_size, shard_offset=shard_offset, marlin_tile_size=marlin_tile_size)
    elif bitblas_tile_size is not None:
        return _adjust_shard_indexes_for_bitblas(shard_size=shard_size, shard_offset=shard_offset, bitblas_tile_size=bitblas_tile_size)
    return (shard_size, shard_offset)