import itertools
from abc import abstractmethod
from typing import Any, Literal, Optional, Union
import torch
import torch.nn as nn
from loguru import logger
from torch.nn.parameter import Parameter, UninitializedParameter
from aphrodite.common import envs
from aphrodite.distributed import divide, get_tensor_model_parallel_rank, get_tensor_model_parallel_world_size, split_tensor_along_last_dim, tensor_model_parallel_all_gather, tensor_model_parallel_all_reduce
from aphrodite.modeling.layers.utils import dispatch_unquantized_gemm
from aphrodite.modeling.parameter import BaseAphroditeParameter, BlockQuantScaleParameter, PackedAphroditeParameter, PackedColumnParameter, PerTensorScaleParameter, RowAphroditeParameter
from aphrodite.modeling.utils import set_weight_attrs
from aphrodite.platforms import current_platform
from aphrodite.quantization.base_config import QuantizationConfig, QuantizeMethodBase
WEIGHT_LOADER_V2_SUPPORTED = ['CompressedTensorsLinearMethod', 'BitBLASLinearMethod', 'GPTQBitBLASLinearMethod', 'AWQMarlinLinearMethod', 'AWQLinearMethod', 'GPTQMarlinLinearMethod', 'Fp8LinearMethod', 'MarlinLinearMethod', 'QQQLinearMethod', 'GPTQMarlin24LinearMethod', 'TPUInt8LinearMethod', 'GPTQLinearMethod', 'FBGEMMFp8LinearMethod', 'ModelOptFp8LinearMethod', 'IPEXAWQLinearMethod', 'IPEXGPTQLinearMethod', 'HQQMarlinMethod', 'QuarkLinearMethod', 'ModelOptNvFp4LinearMethod']
def adjust_bitblas_shard(param, shard_size, shard_offset):
    bitblas_tile_size = getattr(param, 'bitblas_tile_size', None)
    if bitblas_tile_size is not None:
        return (shard_size // bitblas_tile_size, shard_offset // bitblas_tile_size)
    return (shard_size, shard_offset)
def adjust_marlin_shard(param, shard_size, shard_offset):
    marlin_tile_size = getattr(param, 'marlin_tile_size', None)
    if marlin_tile_size is None:
        return (shard_size, shard_offset)
    return (shard_size * marlin_tile_size, shard_offset * marlin_tile_size)
def adjust_bitsandbytes_4bit_shard(param: Parameter, shard_offsets: dict[str, tuple[int, int]], loaded_shard_id: str) -> tuple[int, int]:
    total, _ = shard_offsets['total']
    orig_offset, orig_size = shard_offsets[loaded_shard_id]
    quantized_total = param.data.shape[0]
    quantized_offset = orig_offset * quantized_total // total
    quantized_size = orig_size * quantized_total // total
    return (quantized_size, quantized_offset)
def adjust_scalar_to_fused_array(param, loaded_weight, shard_id):
    qkv_idxs = {'q': 0, 'k': 1, 'v': 2}
    if isinstance(shard_id, str):
        shard_id = qkv_idxs[shard_id]
    elif not isinstance(shard_id, int):
        raise ValueError(f'Unknown Shard Id {shard_id}')
    if len(loaded_weight.shape) != 0:
        assert loaded_weight.shape[0] == 1
        loaded_weight = loaded_weight[0]
    return (param[shard_id], loaded_weight)
def left_shift_bitsandbytes_4bit_shard(bnb_weight_attrs: dict[str, Any]):
    shard_offsets = bnb_weight_attrs['bnb_shard_offsets']
    offset_l = shard_offsets[:2]
    offset_r = shard_offsets[1:] - shard_offsets[1]
    quant_state_l = {0: bnb_weight_attrs['bnb_quant_state'][0]}
    quant_state_r = {i - 1: bnb_weight_attrs['bnb_quant_state'][i] for i in range(1, len(shard_offsets) - 1)}
    left = dict(bnb_shard_offsets=offset_l, bnb_quant_state=quant_state_l)
    right = dict(bnb_shard_offsets=offset_r, bnb_quant_state=quant_state_r)
    return (left, right)
class LinearMethodBase(QuantizeMethodBase):
    @abstractmethod
    def create_weights(self, layer: torch.nn.Module, input_size_per_partition: int, output_partition_sizes: list[int], input_size: int, output_size: int, params_dtype: torch.dtype, **extra_weight_attrs):
        raise NotImplementedError
    @abstractmethod
    def apply(self, layer: torch.nn.Module, x: torch.Tensor, bias: Optional[torch.Tensor]=None) -> torch.Tensor:
        raise NotImplementedError
class UnquantizedLinearMethod(LinearMethodBase):
    def create_weights(self, layer: torch.nn.Module, input_size_per_partition: int, output_partition_sizes: list[int], input_size: int, output_size: int, params_dtype: torch.dtype, **extra_weight_attrs):
        weight = Parameter(torch.empty(sum(output_partition_sizes), input_size_per_partition, dtype=params_dtype), requires_grad=False)
        set_weight_attrs(weight, {'input_dim': 1, 'output_dim': 0})
        layer.register_parameter('weight', weight)
        set_weight_attrs(weight, extra_weight_attrs)
    def process_weights_after_loading(self, layer: torch.nn.Module) -> None:
        if current_platform.is_cpu() and envs.APHRODITE_CPU_SGL_KERNEL:
            N, K = layer.weight.size()
            dtype = layer.weight.dtype
            if torch._C._cpu._is_amx_tile_supported() and dtype == torch.bfloat16 and (N % 32 == 0) and (K % 32 == 0):
                packed_weight = torch.ops._C.convert_weight_packed(layer.weight)
                assert packed_weight.size() == layer.weight.size()
                layer.weight.copy_(packed_weight)
                if layer.bias is not None:
                    layer.bias = Parameter(layer.bias.to(torch.float32), requires_grad=False)
                layer.use_cpu_sgl = True
            else:
                logger.warning('CPU SGL kernels require Intel AMX support, bfloat16 weight, IC and OC are divisible by 32.')
                layer.use_cpu_sgl = False
    def apply(self, layer: torch.nn.Module, x: torch.Tensor, bias: Optional[torch.Tensor]=None) -> torch.Tensor:
        return dispatch_unquantized_gemm()(layer, x, layer.weight, bias)
class LinearBase(torch.nn.Module):
    def __init__(self, input_size: int, output_size: int, skip_bias_add: bool=False, params_dtype: Optional[torch.dtype]=None, quant_config: Optional[QuantizationConfig]=None, prefix: str='', *, return_bias: bool=True):
        super().__init__()
        self.input_size = input_size
        self.output_size = output_size
        self.skip_bias_add = skip_bias_add
        if params_dtype is None:
            params_dtype = torch.get_default_dtype()
        self.params_dtype = params_dtype
        self.quant_config = quant_config
        self.prefix = prefix
        if quant_config is None:
            self.quant_method: Optional[QuantizeMethodBase] = UnquantizedLinearMethod()
        else:
            self.quant_method = quant_config.get_quant_method(self, prefix=prefix)
        self.return_bias = return_bias
    def forward(self, x: torch.Tensor) -> Union[torch.Tensor, tuple[torch.Tensor, Optional[Parameter]]]:
        raise NotImplementedError
class ReplicatedLinear(LinearBase):
    def __init__(self, input_size: int, output_size: int, bias: bool=True, skip_bias_add: bool=False, params_dtype: Optional[torch.dtype]=None, quant_config: Optional[QuantizationConfig]=None, prefix: str='', *, return_bias: bool=True):
        if hasattr(self, 'output_sizes'):
            self.output_partition_sizes = self.output_sizes
        else:
            self.output_partition_sizes = [output_size]
        super().__init__(input_size, output_size, skip_bias_add, params_dtype, quant_config, prefix=prefix, return_bias=return_bias)
        assert self.quant_method is not None
        self.quant_method.create_weights(self, self.input_size, self.output_partition_sizes, self.input_size, self.output_size, self.params_dtype, weight_loader=self.weight_loader)
        if bias:
            self.bias = Parameter(torch.empty(self.output_size, dtype=self.params_dtype))
            set_weight_attrs(self.bias, {'output_dim': 0, 'weight_loader': self.weight_loader})
        else:
            self.register_parameter('bias', None)
    def weight_loader(self, param: Parameter, loaded_weight: torch.Tensor):
        is_gguf_weight = getattr(param, 'is_gguf_weight', False)
        is_gguf_weight_type = getattr(param, 'is_gguf_weight_type', False)
        if is_gguf_weight_type:
            param.weight_type = loaded_weight.item()
        if is_gguf_weight and isinstance(param, UninitializedParameter):
            param.materialize(loaded_weight.shape, dtype=loaded_weight.dtype)
        if len(loaded_weight.shape) == 0:
            loaded_weight = loaded_weight.reshape(1)
        assert param.size() == loaded_weight.size(), f'Tried to load weights of size {loaded_weight.size()}to a parameter of size {param.size()}'
        param.data.copy_(loaded_weight)
    def forward(self, x: torch.Tensor) -> Union[torch.Tensor, tuple[torch.Tensor, Optional[Parameter]]]:
        bias = self.bias if not self.skip_bias_add else None
        assert self.quant_method is not None
        output = self.quant_method.apply(self, x, bias)
        output_bias = self.bias if self.skip_bias_add else None
        if not self.return_bias:
            return output
        return (output, output_bias)
    def extra_repr(self) -> str:
        s = f'in_features={self.input_size}'
        s += f', output_features={self.output_size}'
        s += f', bias={self.bias is not None}'
        return s
class MergedReplicatedLinear(ReplicatedLinear):
    def __init__(self, input_size: int, output_sizes: list[int], bias: bool=True, skip_bias_add: bool=False, params_dtype: Optional[torch.dtype]=None, quant_config: Optional[QuantizationConfig]=None, prefix: str='', *, return_bias: bool=True):
        self.output_sizes = output_sizes
        super().__init__(input_size, sum(output_sizes), bias, skip_bias_add, params_dtype, quant_config, prefix=prefix, return_bias=return_bias)
    def weight_loader(self, param: Union[Parameter, BaseAphroditeParameter], loaded_weight: torch.Tensor, loaded_shard_id: Optional[int]=None):
        assert loaded_shard_id is not None
        assert loaded_shard_id < len(self.output_sizes)
        if isinstance(param, BlockQuantScaleParameter):
            from aphrodite.quantization.fp8 import Fp8LinearMethod, Fp8MoEMethod
            assert self.quant_method is not None
            assert isinstance(self.quant_method, (Fp8LinearMethod, Fp8MoEMethod))
            weight_block_size = self.quant_method.quant_config.weight_block_size
            assert weight_block_size is not None
            block_n, _ = (weight_block_size[0], weight_block_size[1])
            shard_offset = (sum(self.output_sizes[:loaded_shard_id]) + block_n - 1) // block_n
            shard_size = (self.output_sizes[loaded_shard_id] + block_n - 1) // block_n
        elif isinstance(param, PerTensorScaleParameter):
            shard_offset = loaded_shard_id
            shard_size = 1
        else:
            shard_offset = sum(self.output_sizes[:loaded_shard_id])
            shard_size = self.output_sizes[loaded_shard_id]
        param[shard_offset:shard_offset + shard_size] = loaded_weight
class ColumnParallelLinear(LinearBase):
    def __init__(self, input_size: int, output_size: int, bias: bool=True, gather_output: bool=False, skip_bias_add: bool=False, params_dtype: Optional[torch.dtype]=None, quant_config: Optional[QuantizationConfig]=None, output_sizes: Optional[list[int]]=None, prefix: str='', *, return_bias: bool=True):
        self.tp_size = get_tensor_model_parallel_world_size()
        self.input_size_per_partition = input_size
        self.output_size_per_partition = divide(output_size, self.tp_size)
        self.output_partition_sizes = [self.output_size_per_partition]
        if hasattr(self, 'output_sizes'):
            self.output_partition_sizes = [divide(output_size, self.tp_size) for output_size in self.output_sizes]
        super().__init__(input_size, output_size, skip_bias_add, params_dtype, quant_config, prefix, return_bias=return_bias)
        self.gather_output = gather_output
        if output_sizes is None:
            output_sizes = [output_size]
        assert self.quant_method is not None
        self.quant_method.create_weights(layer=self, input_size_per_partition=self.input_size_per_partition, output_partition_sizes=self.output_partition_sizes, input_size=self.input_size, output_size=self.output_size, params_dtype=self.params_dtype, weight_loader=self.weight_loader_v2 if self.quant_method.__class__.__name__ in WEIGHT_LOADER_V2_SUPPORTED else self.weight_loader)
        if bias:
            self.bias = Parameter(torch.empty(self.output_size_per_partition, dtype=params_dtype))
            set_weight_attrs(self.bias, {'output_dim': 0, 'weight_loader': self.weight_loader})
        else:
            self.register_parameter('bias', None)
        self.tp_rank = get_tensor_model_parallel_rank()
    def weight_loader(self, param: Parameter, loaded_weight: torch.Tensor):
        output_dim = getattr(param, 'output_dim', None)
        is_sharded_weight = getattr(param, 'is_sharded_weight', False)
        use_bitsandbytes_4bit = getattr(param, 'use_bitsandbytes_4bit', False)
        is_sharded_weight = is_sharded_weight or use_bitsandbytes_4bit
        is_gguf_weight = getattr(param, 'is_gguf_weight', False)
        is_gguf_weight_type = getattr(param, 'is_gguf_weight_type', False)
        if is_gguf_weight_type:
            param.weight_type = loaded_weight.item()
        if is_gguf_weight and isinstance(param, UninitializedParameter):
            final_shape = list(loaded_weight.shape)
            if output_dim is not None:
                assert final_shape[output_dim] % self.tp_size == 0
                final_shape[output_dim] = final_shape[output_dim] // self.tp_size
            param.materialize(final_shape, dtype=loaded_weight.dtype)
        param_data = param.data
        if output_dim is not None and (not is_sharded_weight):
            shard_size = param_data.shape[output_dim]
            start_idx = self.tp_rank * shard_size
            loaded_weight = loaded_weight.narrow(output_dim, start_idx, shard_size)
        if len(loaded_weight.shape) == 0:
            loaded_weight = loaded_weight.reshape(1)
        assert param_data.shape == loaded_weight.shape
        param_data.copy_(loaded_weight)
    def weight_loader_v2(self, param: Parameter, loaded_weight: torch.Tensor):
        if len(loaded_weight.shape) == 0:
            assert loaded_weight.numel() == 1
            loaded_weight = loaded_weight.reshape(1)
        param.load_column_parallel_weight(loaded_weight=loaded_weight)
    def forward(self, input_) -> Union[torch.Tensor, tuple[torch.Tensor, Optional[Parameter]]]:
        bias = self.bias if not self.skip_bias_add else None
        assert self.quant_method is not None
        output_parallel = self.quant_method.apply(self, input_, bias)
        if self.gather_output:
            output = tensor_model_parallel_all_gather(output_parallel)
        else:
            output = output_parallel
        output_bias = self.bias if self.skip_bias_add else None
        if not self.return_bias:
            return output
        return (output, output_bias)
    def extra_repr(self) -> str:
        s = f'in_features={self.input_size}'
        s += f', output_features={self.output_size_per_partition}'
        s += f', bias={self.bias is not None}'
        s += f', tp_size={get_tensor_model_parallel_world_size()}'
        s += f', gather_output={self.gather_output}'
        return s
class MergedColumnParallelLinear(ColumnParallelLinear):
    def __init__(self, input_size: int, output_sizes: list[int], bias: bool=True, gather_output: bool=False, skip_bias_add: bool=False, params_dtype: Optional[torch.dtype]=None, quant_config: Optional[QuantizationConfig]=None, prefix: str='', *, return_bias: bool=True):
        self.output_sizes = output_sizes
        self.tp_size = get_tensor_model_parallel_world_size()
        self.tp_rank = get_tensor_model_parallel_rank()
        assert all((output_size % self.tp_size == 0 for output_size in output_sizes))
        super().__init__(input_size=input_size, output_size=sum(output_sizes), bias=bias, gather_output=gather_output, skip_bias_add=skip_bias_add, params_dtype=params_dtype, quant_config=quant_config, prefix=prefix, return_bias=return_bias)
    def weight_loader(self, param: Parameter, loaded_weight: torch.Tensor, loaded_shard_id: Optional[int]=None):
        is_gguf_weight = getattr(param, 'is_gguf_weight', False)
        is_gguf_weight_type = getattr(param, 'is_gguf_weight_type', False)
        if is_gguf_weight_type:
            if loaded_shard_id is not None:
                param.data[loaded_shard_id].copy_(loaded_weight)
                param.shard_weight_type[loaded_shard_id] = loaded_weight.item()
            else:
                param.shard_weight_type = {i: loaded_weight.item() for i, _ in enumerate(self.output_sizes)}
            return
        if is_gguf_weight:
            output_dim = getattr(param, 'output_dim', None)
            shard_size = loaded_weight.size(output_dim) // self.tp_size
            start_idx = self.tp_rank * shard_size
            if loaded_shard_id is not None:
                loaded_weight = loaded_weight.narrow(output_dim, start_idx, shard_size)
                param.shard_id.append(loaded_shard_id)
                param.shard_id_map[loaded_shard_id] = len(param.data_container)
                param.data_container.append(loaded_weight)
                return
        param_data = param.data
        output_dim = getattr(param, 'output_dim', None)
        is_metadata = getattr(param, 'is_metadata', False)
        needs_scalar_to_array = getattr(param, 'needs_scalar_to_array', False)
        if loaded_shard_id is None:
            if output_dim is None:
                if needs_scalar_to_array:
                    param_data, loaded_weight = adjust_scalar_to_fused_array(param_data, loaded_weight, 0)
                assert param_data.shape == loaded_weight.shape
                param_data.copy_(loaded_weight)
                return
            current_shard_offset = 0
            use_bitsandbytes_4bit = getattr(param, 'use_bitsandbytes_4bit', False)
            shard_offsets: list[tuple[int, int, int]] = []
            for i, output_size in enumerate(self.output_sizes):
                shard_offsets.append((i, current_shard_offset, output_size))
                current_shard_offset += output_size
            packed_dim = getattr(param, 'packed_dim', None)
            for shard_id, shard_offset, shard_size in shard_offsets:
                if packed_dim == output_dim:
                    shard_size = shard_size // param.pack_factor
                    shard_offset = shard_offset // param.pack_factor
                    shard_size, shard_offset = adjust_marlin_shard(param, shard_size, shard_offset)
                shard_size, shard_offset = adjust_bitblas_shard(param, shard_size, shard_offset)
                if use_bitsandbytes_4bit:
                    index = list(itertools.accumulate([0] + self.output_sizes))
                    orig_offsets = {str(i): (index[i], size) for i, size in enumerate(self.output_sizes)}
                    orig_offsets['total'] = (self.output_size, 0)
                    shard_size, shard_offset = adjust_bitsandbytes_4bit_shard(param, orig_offsets, str(shard_id))
                loaded_weight_shard = loaded_weight.narrow(output_dim, shard_offset, shard_size)
                self.weight_loader(param, loaded_weight_shard, shard_id)
            return
        assert loaded_shard_id < len(self.output_sizes)
        if output_dim is not None:
            shard_offset = sum(self.output_sizes[:loaded_shard_id]) // self.tp_size
            shard_size = self.output_sizes[loaded_shard_id] // self.tp_size
            packed_dim = getattr(param, 'packed_dim', None)
            if packed_dim == output_dim:
                shard_size = shard_size // param.pack_factor
                shard_offset = shard_offset // param.pack_factor
                shard_size, shard_offset = adjust_marlin_shard(param, shard_size, shard_offset)
            shard_size, shard_offset = adjust_bitblas_shard(param, shard_size, shard_offset)
            use_bitsandbytes_4bit = getattr(param, 'use_bitsandbytes_4bit', False)
            is_sharded_weight = getattr(param, 'is_sharded_weight', False)
            is_sharded_weight = is_sharded_weight or use_bitsandbytes_4bit
            if use_bitsandbytes_4bit:
                shard_size = loaded_weight.shape[output_dim]
                shard_offset = loaded_weight.shape[output_dim] * loaded_shard_id
            param_data = param_data.narrow(output_dim, shard_offset, shard_size)
            start_idx = self.tp_rank * shard_size
            if not is_sharded_weight:
                loaded_weight = loaded_weight.narrow(output_dim, start_idx, shard_size)
        elif is_metadata:
            shard_size = loaded_weight.shape[0]
            shard_offset = loaded_shard_id * shard_size
            param_data = param_data.narrow(0, shard_offset, shard_size)
        elif needs_scalar_to_array:
            param_data, loaded_weight = adjust_scalar_to_fused_array(param_data, loaded_weight, loaded_shard_id)
        else:
            ignore_warning = getattr(param, 'ignore_warning', False)
            if not ignore_warning:
                logger.warning('Loading a weight without `output_dim` attribute in MergedColumnParallelLinear, assume the weight is the same for all partitions.')
        assert param_data.shape == loaded_weight.shape
        param_data.copy_(loaded_weight)
    def _load_fused_module_from_checkpoint(self, param: BaseAphroditeParameter, loaded_weight: torch.Tensor):
        current_shard_offset = 0
        shard_offsets: list[tuple[int, int, int]] = []
        for i, output_size in enumerate(self.output_sizes):
            shard_offsets.append((i, current_shard_offset, output_size))
            current_shard_offset += output_size
        for shard_id, shard_offset, shard_size in shard_offsets:
            if isinstance(param, (PackedColumnParameter, PackedAphroditeParameter)) and param.packed_dim == param.output_dim:
                shard_size, shard_offset = param.adjust_shard_indexes_for_packing(shard_size=shard_size, shard_offset=shard_offset)
            loaded_weight_shard = loaded_weight.narrow(param.output_dim, shard_offset, shard_size)
            self.weight_loader_v2(param, loaded_weight_shard, shard_id)
    def weight_loader_v2(self, param: BaseAphroditeParameter, loaded_weight: torch.Tensor, loaded_shard_id: Optional[int]=None):
        if loaded_shard_id is None:
            if isinstance(param, PerTensorScaleParameter):
                param.load_merged_column_weight(loaded_weight=loaded_weight, shard_id=0)
                return
            elif type(param) in (RowAphroditeParameter, BaseAphroditeParameter):
                param.load_merged_column_weight(loaded_weight=loaded_weight)
                return
            self._load_fused_module_from_checkpoint(param, loaded_weight)
            return
        assert loaded_shard_id < len(self.output_sizes)
        tp_size = get_tensor_model_parallel_world_size()
        if isinstance(param, BlockQuantScaleParameter):
            from aphrodite.quantization.fp8 import Fp8LinearMethod, Fp8MoEMethod
            assert self.quant_method is not None
            assert isinstance(self.quant_method, (Fp8LinearMethod, Fp8MoEMethod))
            weight_block_size = self.quant_method.quant_config.weight_block_size
            assert weight_block_size is not None
            block_n, _ = (weight_block_size[0], weight_block_size[1])
            shard_offset = (sum(self.output_sizes[:loaded_shard_id]) + block_n - 1) // block_n // tp_size
            shard_size = (self.output_sizes[loaded_shard_id] + block_n - 1) // block_n // tp_size
        else:
            shard_offset = sum(self.output_sizes[:loaded_shard_id]) // tp_size
            shard_size = self.output_sizes[loaded_shard_id] // tp_size
        param.load_merged_column_weight(loaded_weight=loaded_weight, shard_id=loaded_shard_id, shard_offset=shard_offset, shard_size=shard_size)
class QKVParallelLinear(ColumnParallelLinear):
    def __init__(self, hidden_size: int, head_size: int, total_num_heads: int, total_num_kv_heads: Optional[int]=None, bias: bool=True, skip_bias_add: bool=False, params_dtype: Optional[torch.dtype]=None, quant_config: Optional[QuantizationConfig]=None, prefix: str='', *, return_bias: bool=True):
        self.hidden_size = hidden_size
        self.head_size = head_size
        self.total_num_heads = total_num_heads
        if total_num_kv_heads is None:
            total_num_kv_heads = total_num_heads
        self.total_num_kv_heads = total_num_kv_heads
        tp_size = get_tensor_model_parallel_world_size()
        self.num_heads = divide(self.total_num_heads, tp_size)
        if tp_size >= self.total_num_kv_heads:
            self.num_kv_heads = 1
            self.num_kv_head_replicas = divide(tp_size, self.total_num_kv_heads)
        else:
            self.num_kv_heads = divide(self.total_num_kv_heads, tp_size)
            self.num_kv_head_replicas = 1
        input_size = self.hidden_size
        output_size = (self.num_heads + 2 * self.num_kv_heads) * tp_size * self.head_size
        self.output_sizes = [self.num_heads * self.head_size * tp_size, self.num_kv_heads * self.head_size * tp_size, self.num_kv_heads * self.head_size * tp_size]
        super().__init__(input_size=input_size, output_size=output_size, bias=bias, gather_output=False, skip_bias_add=skip_bias_add, params_dtype=params_dtype, quant_config=quant_config, prefix=prefix, return_bias=return_bias)
    def _get_shard_offset_mapping(self, loaded_shard_id: str):
        shard_offset_mapping = {'q': 0, 'k': self.num_heads * self.head_size, 'v': (self.num_heads + self.num_kv_heads) * self.head_size, 'total': (self.num_heads + 2 * self.num_kv_heads) * self.head_size}
        return shard_offset_mapping.get(loaded_shard_id)
    def _get_shard_size_mapping(self, loaded_shard_id: str):
        shard_size_mapping = {'q': self.num_heads * self.head_size, 'k': self.num_kv_heads * self.head_size, 'v': self.num_kv_heads * self.head_size}
        return shard_size_mapping.get(loaded_shard_id)
    def _load_fused_module_from_checkpoint(self, param: BaseAphroditeParameter, loaded_weight: torch.Tensor):
        shard_offsets = [('q', 0, self.total_num_heads * self.head_size), ('k', self.total_num_heads * self.head_size, self.total_num_kv_heads * self.head_size), ('v', (self.total_num_heads + self.total_num_kv_heads) * self.head_size, self.total_num_kv_heads * self.head_size)]
        for shard_id, shard_offset, shard_size in shard_offsets:
            if isinstance(param, (PackedColumnParameter, PackedAphroditeParameter)) and param.packed_dim == param.output_dim:
                shard_size, shard_offset = param.adjust_shard_indexes_for_packing(shard_size=shard_size, shard_offset=shard_offset)
            loaded_weight_shard = loaded_weight.narrow(param.output_dim, shard_offset, shard_size)
            self.weight_loader_v2(param, loaded_weight_shard, shard_id)
    def weight_loader_v2(self, param: BaseAphroditeParameter, loaded_weight: torch.Tensor, loaded_shard_id: Optional[str]=None):
        if loaded_shard_id is None:
            if isinstance(param, PerTensorScaleParameter):
                param.load_qkv_weight(loaded_weight=loaded_weight, shard_id=0)
                return
            elif type(param) in (RowAphroditeParameter, BaseAphroditeParameter):
                param.load_qkv_weight(loaded_weight=loaded_weight)
                return
            self._load_fused_module_from_checkpoint(param, loaded_weight)
            return
        assert loaded_shard_id in ['q', 'k', 'v']
        shard_offset = self._get_shard_offset_mapping(loaded_shard_id)
        shard_size = self._get_shard_size_mapping(loaded_shard_id)
        if isinstance(param, BlockQuantScaleParameter):
            assert self.quant_method is not None
            assert hasattr(self.quant_method, 'quant_config')
            weight_block_size = self.quant_method.quant_config.weight_block_size
            block_n, _ = (weight_block_size[0], weight_block_size[1])
            shard_offset = (shard_offset + block_n - 1) // block_n
            shard_size = (shard_size + block_n - 1) // block_n
        param.load_qkv_weight(loaded_weight=loaded_weight, num_heads=self.num_kv_head_replicas, shard_id=loaded_shard_id, shard_offset=shard_offset, shard_size=shard_size)
    def weight_loader(self, param: Parameter, loaded_weight: torch.Tensor, loaded_shard_id: Optional[str]=None):
        is_gguf_weight = getattr(param, 'is_gguf_weight', False)
        is_gguf_weight_type = getattr(param, 'is_gguf_weight_type', False)
        if is_gguf_weight_type:
            idx_map = {'q': 0, 'k': 1, 'v': 2}
            if loaded_shard_id is not None:
                param.data[idx_map[loaded_shard_id]].copy_(loaded_weight)
                param.shard_weight_type[loaded_shard_id] = loaded_weight.item()
            else:
                param.shard_weight_type = {k: loaded_weight.item() for k in idx_map}
            return
        if is_gguf_weight:
            output_dim = getattr(param, 'output_dim', None)
            shard_size = loaded_weight.size(output_dim) // self.tp_size
            start_idx = self.tp_rank * shard_size
            if loaded_shard_id is not None:
                loaded_weight = loaded_weight.narrow(output_dim, start_idx, shard_size)
                param.shard_id.append(loaded_shard_id)
                param.shard_id_map[loaded_shard_id] = len(param.data_container)
                param.data_container.append(loaded_weight)
                return
        param_data = param.data
        output_dim = getattr(param, 'output_dim', None)
        is_metadata = getattr(param, 'is_metadata', False)
        needs_scalar_to_array = getattr(param, 'needs_scalar_to_array', False)
        if loaded_shard_id is None:
            if output_dim is None:
                if needs_scalar_to_array:
                    param_data, loaded_weight = adjust_scalar_to_fused_array(param_data, loaded_weight, 0)
                assert param_data.shape == loaded_weight.shape
                param_data.copy_(loaded_weight)
                return
            shard_offsets = [('q', 0, self.total_num_heads * self.head_size), ('k', self.total_num_heads * self.head_size, self.total_num_kv_heads * self.head_size), ('v', (self.total_num_heads + self.total_num_kv_heads) * self.head_size, self.total_num_kv_heads * self.head_size)]
            use_bitsandbytes_4bit = getattr(param, 'use_bitsandbytes_4bit', False)
            packed_dim = getattr(param, 'packed_dim', None)
            for shard_id, shard_offset, shard_size in shard_offsets:
                if packed_dim == output_dim:
                    shard_size = shard_size // param.pack_factor
                    shard_offset = shard_offset // param.pack_factor
                    shard_size, shard_offset = adjust_marlin_shard(param, shard_size, shard_offset)
                if use_bitsandbytes_4bit:
                    orig_qkv_offsets = {'q': (0, self.total_num_heads * self.head_size), 'k': (self.total_num_heads * self.head_size, self.total_num_kv_heads * self.head_size), 'v': ((self.total_num_heads + self.total_num_kv_heads) * self.head_size, self.total_num_kv_heads * self.head_size), 'total': ((self.total_num_heads + 2 * self.total_num_kv_heads) * self.head_size, 0)}
                    shard_size, shard_offset = adjust_bitsandbytes_4bit_shard(param, orig_qkv_offsets, shard_id)
                loaded_weight_shard = loaded_weight.narrow(output_dim, shard_offset, shard_size)
                self.weight_loader(param, loaded_weight_shard, shard_id)
            return
        assert loaded_shard_id in ['q', 'k', 'v']
        if output_dim is not None:
            if loaded_shard_id == 'q':
                shard_offset = 0
                shard_size = self.num_heads * self.head_size
            elif loaded_shard_id == 'k':
                shard_offset = self.num_heads * self.head_size
                shard_size = self.num_kv_heads * self.head_size
            elif loaded_shard_id == 'v':
                shard_offset = (self.num_heads + self.num_kv_heads) * self.head_size
                shard_size = self.num_kv_heads * self.head_size
            packed_dim = getattr(param, 'packed_dim', None)
            if packed_dim == output_dim:
                shard_size = shard_size // param.pack_factor
                shard_offset = shard_offset // param.pack_factor
                shard_size, shard_offset = adjust_marlin_shard(param, shard_size, shard_offset)
            use_bitsandbytes_4bit = getattr(param, 'use_bitsandbytes_4bit', False)
            is_sharded_weight = getattr(param, 'is_sharded_weight', False)
            is_sharded_weight = is_sharded_weight or use_bitsandbytes_4bit
            if use_bitsandbytes_4bit:
                orig_qkv_offsets = {'q': (0, self.num_heads * self.head_size), 'k': (self.num_heads * self.head_size, self.num_kv_heads * self.head_size), 'v': ((self.num_heads + self.num_kv_heads) * self.head_size, self.num_kv_heads * self.head_size), 'total': ((self.num_heads + 2 * self.num_kv_heads) * self.head_size, 0)}
                shard_size, shard_offset = adjust_bitsandbytes_4bit_shard(param, orig_qkv_offsets, loaded_shard_id)
            param_data = param_data.narrow(output_dim, shard_offset, shard_size)
            if loaded_shard_id == 'q':
                shard_id = self.tp_rank
            else:
                shard_id = self.tp_rank // self.num_kv_head_replicas
            start_idx = shard_id * shard_size
            if not is_sharded_weight:
                loaded_weight = loaded_weight.narrow(output_dim, start_idx, shard_size)
        elif is_metadata:
            shard_size = loaded_weight.shape[0]
            shard_index = ['q', 'k', 'v'].index(loaded_shard_id)
            param_data = param_data.narrow(0, shard_index * shard_size, shard_size)
        elif needs_scalar_to_array:
            param_data, loaded_weight = adjust_scalar_to_fused_array(param_data, loaded_weight, loaded_shard_id)
        else:
            ignore_warning = getattr(param, 'ignore_warning', False)
            if not ignore_warning:
                logger.warning('Loading a weight without `output_dim` attribute in QKVParallelLinear, assume the weight is the same for all partitions.')
        assert param_data.shape == loaded_weight.shape
        param_data.copy_(loaded_weight)
class RowParallelLinear(LinearBase):
    def __init__(self, input_size: int, output_size: int, bias: bool=True, input_is_parallel: bool=True, skip_bias_add: bool=False, params_dtype: Optional[torch.dtype]=None, reduce_results: bool=True, quant_config: Optional[QuantizationConfig]=None, prefix: str='', *, return_bias: bool=True):
        self.tp_rank = get_tensor_model_parallel_rank()
        self.tp_size = get_tensor_model_parallel_world_size()
        self.input_size_per_partition = divide(input_size, self.tp_size)
        self.output_size_per_partition = output_size
        self.output_partition_sizes = [output_size]
        super().__init__(input_size, output_size, skip_bias_add, params_dtype, quant_config, prefix, return_bias=return_bias)
        self.input_is_parallel = input_is_parallel
        self.reduce_results = reduce_results
        assert self.quant_method is not None
        self.quant_method.create_weights(layer=self, input_size_per_partition=self.input_size_per_partition, output_partition_sizes=self.output_partition_sizes, input_size=self.input_size, output_size=self.output_size, params_dtype=self.params_dtype, weight_loader=self.weight_loader_v2 if self.quant_method.__class__.__name__ in WEIGHT_LOADER_V2_SUPPORTED else self.weight_loader)
        if not reduce_results and (bias and (not skip_bias_add)):
            raise ValueError('When not reduce the results, adding bias to the results can lead to incorrect results')
        if bias:
            self.bias = Parameter(torch.empty(self.output_size, dtype=params_dtype))
            set_weight_attrs(self.bias, {'output_dim': 0, 'weight_loader': self.weight_loader})
        else:
            self.register_parameter('bias', None)
    def weight_loader(self, param: Parameter, loaded_weight: torch.Tensor):
        input_dim = getattr(param, 'input_dim', None)
        use_bitsandbytes_4bit = getattr(param, 'use_bitsandbytes_4bit', False)
        is_sharded_weight = getattr(param, 'is_sharded_weight', False)
        is_sharded_weight = is_sharded_weight or use_bitsandbytes_4bit
        is_gguf_weight = getattr(param, 'is_gguf_weight', False)
        is_gguf_weight_type = getattr(param, 'is_gguf_weight_type', False)
        if is_gguf_weight_type:
            param.weight_type = loaded_weight.item()
        if is_gguf_weight and isinstance(param, UninitializedParameter):
            weight_shape = list(loaded_weight.shape)
            if input_dim:
                weight_shape[input_dim] = weight_shape[input_dim] // self.tp_size
            param.materialize(tuple(weight_shape), dtype=loaded_weight.dtype)
        param_data = param.data
        if input_dim is not None and (not is_sharded_weight):
            shard_size = param_data.shape[input_dim]
            start_idx = self.tp_rank * shard_size
            loaded_weight = loaded_weight.narrow(input_dim, start_idx, shard_size)
        if len(loaded_weight.shape) == 0:
            loaded_weight = loaded_weight.reshape(1)
        assert param_data.shape == loaded_weight.shape
        param_data.copy_(loaded_weight)
    def weight_loader_v2(self, param: BaseAphroditeParameter, loaded_weight: torch.Tensor):
        if len(loaded_weight.shape) == 0:
            assert loaded_weight.numel() == 1
            loaded_weight = loaded_weight.reshape(1)
        param.load_row_parallel_weight(loaded_weight=loaded_weight)
    def forward(self, input_) -> Union[torch.Tensor, tuple[torch.Tensor, Optional[Parameter]]]:
        if self.input_is_parallel:
            input_parallel = input_
        else:
            tp_rank = get_tensor_model_parallel_rank()
            splitted_input = split_tensor_along_last_dim(input_, num_partitions=self.tp_size)
            input_parallel = splitted_input[tp_rank].contiguous()
        assert self.quant_method is not None
        bias_ = None if self.tp_rank > 0 or self.skip_bias_add else self.bias
        output_parallel = self.quant_method.apply(self, input_parallel, bias=bias_)
        if self.reduce_results and self.tp_size > 1:
            output = tensor_model_parallel_all_reduce(output_parallel)
        else:
            output = output_parallel
        output_bias = self.bias if self.skip_bias_add else None
        if not self.return_bias:
            return output
        return (output, output_bias)
    def extra_repr(self) -> str:
        s = f'input_features={self.input_size_per_partition}'
        s += f', output_features={self.output_size}'
        s += f', bias={self.bias is not None}'
        s += f', tp_size={self.tp_size}'
        s += f', reduce_results={self.reduce_results}'
        return s
class QKVCrossParallelLinear(LinearBase):
    def __init__(self, hidden_size: int, head_size: int, total_num_heads: int, total_num_kv_heads: Optional[int]=None, bias: bool=True, skip_bias_add: bool=False, params_dtype: Optional[torch.dtype]=None, quant_config: Optional[QuantizationConfig]=None, prefix: str=''):
        input_size = hidden_size
        output_size = (total_num_heads + (total_num_kv_heads or 0)) * head_size
        super().__init__(input_size=input_size, output_size=output_size, skip_bias_add=skip_bias_add, params_dtype=params_dtype, quant_config=quant_config, prefix=prefix)
        self.quant_config = quant_config
        placeholder_size = 0
        assert self.quant_method is not None
        self.quant_method.create_weights(self, placeholder_size, [placeholder_size], placeholder_size, placeholder_size, self.params_dtype, weight_loader=self.weight_loader)
        self.proj = dict()
        self.proj['q_proj_decoder'] = ColumnParallelLinear(input_size=hidden_size, output_size=total_num_heads * head_size, bias=bias, quant_config=quant_config, skip_bias_add=skip_bias_add, params_dtype=params_dtype, prefix=f'{prefix}.q_proj_decoder')
        self.proj['kv_proj_encoder'] = QKVParallelLinear(hidden_size=hidden_size, head_size=head_size, total_num_heads=0, total_num_kv_heads=total_num_kv_heads, bias=bias, quant_config=quant_config, skip_bias_add=skip_bias_add, params_dtype=params_dtype, prefix=f'{prefix}.kv_proj_encoder')
        self.q_size = self.q_proj_decoder.output_size_per_partition
        self.kv_size = self.kv_proj_encoder.num_kv_heads * head_size
        if bias:
            self.bias = torch.nn.Parameter()
            set_weight_attrs(self.bias, {'output_dim': 0, 'weight_loader': self.weight_loader})
        else:
            self.bias = None
    def process_weights_after_loading(self):
        for layer in self.proj.values():
            if self.quant_method is not None:
                self.quant_method.process_weights_after_loading(layer)
    @property
    def q_proj_decoder(self) -> ColumnParallelLinear:
        layer = self.proj['q_proj_decoder']
        for name, param in self.named_parameters():
            target_param = getattr(layer, name, None)
            if target_param is not None:
                self.sync_weight_attrs(param, target_param, mode='q_proj_decoder')
        return layer
    @property
    def kv_proj_encoder(self) -> QKVParallelLinear:
        layer = self.proj['kv_proj_encoder']
        for name, param in self.named_parameters():
            target_param = getattr(layer, name, None)
            if target_param is not None:
                self.sync_weight_attrs(param, target_param, mode='kv_proj_encoder')
        return layer
    def sync_weight_attrs(self, src_param: nn.Parameter, tgt_param: nn.Parameter, mode: Literal['q_proj_decoder', 'kv_proj_encoder']):
        missing_attrs_dict = {k: getattr(src_param, k) for k in set(vars(src_param).keys()) - set(vars(tgt_param).keys())}
        use_bitsandbytes_4bit = getattr(src_param, 'use_bitsandbytes_4bit', False)
        if missing_attrs_dict and use_bitsandbytes_4bit:
            q_proj_attrs, kv_proj_attrs = left_shift_bitsandbytes_4bit_shard(missing_attrs_dict)
            if mode == 'q_proj_decoder':
                set_weight_attrs(tgt_param, q_proj_attrs)
            elif mode == 'kv_proj_encoder':
                set_weight_attrs(tgt_param, kv_proj_attrs)
        else:
            set_weight_attrs(tgt_param, missing_attrs_dict)
    def _is_same_param(self, src_param: torch.nn.Parameter, map_param: torch.nn.Parameter) -> bool:
        key_to_ignore = ['weight_loader', '_weight_loader']
        has_same_type_name = type(src_param) is type(map_param)
        src_param_attrs = {k: v for k, v in src_param.__dict__.items() if k not in key_to_ignore}
        map_param_attrs = {k: v for k, v in map_param.__dict__.items() if k not in key_to_ignore}
        has_same_attrs = src_param_attrs == map_param_attrs
        return has_same_type_name and has_same_attrs
    def select_proj_params(self, layer: nn.Module, param: nn.Parameter) -> nn.Parameter:
        target_param_list = [v for _, v in layer.named_parameters() if self._is_same_param(param, v)]
        assert len(target_param_list) == 1
        target_param = target_param_list[0]
        return target_param
    def forward(self, decoder_hidden_states: torch.Tensor, encoder_hidden_states: torch.Tensor) -> tuple[torch.Tensor, ...]:
        q, _ = self.q_proj_decoder(decoder_hidden_states)
        if encoder_hidden_states is None:
            k = None
            v = None
        else:
            kv_enc, _ = self.kv_proj_encoder(encoder_hidden_states)
            k, v = kv_enc.split(self.kv_size, dim=-1)
        return (q, k, v)
    def weight_loader(self, param: torch.nn.Parameter, loaded_weight: torch.Tensor, loaded_shard_id: Optional[str]=None):
        layer = self.q_proj_decoder if loaded_shard_id == 'q' else self.kv_proj_encoder
        target_param = self.select_proj_params(layer, param)
        shard_id_args = (loaded_shard_id,) if loaded_shard_id != 'q' else ()
        if self.quant_method.__class__.__name__ in WEIGHT_LOADER_V2_SUPPORTED:
            layer.weight_loader_v2(target_param, loaded_weight, *shard_id_args)
        else:
            layer.weight_loader(target_param, loaded_weight, *shard_id_args)
    def extra_repr(self) -> str:
        s = f'in_features={self.input_size}'
        s += f', q_size={self.q_size}'
        s += f', kv_size={self.kv_size}'
        s += f', bias={self.bias is not None}'
        s += f', tp_size={get_tensor_model_parallel_world_size()}'
        s += ', gather_output=False'
        return s