import torch
from aphrodite.common.logger import log_once
from aphrodite.platforms import current_platform
from aphrodite.quantization.base_config import QuantizationConfig, QuantizeMethodBase
class BaseKVCacheMethod(QuantizeMethodBase):
    def __init__(self, quant_config: QuantizationConfig):
        self.quant_config = quant_config
    def create_weights(self, layer: torch.nn.Module):
        layer.q_scale = torch.nn.Parameter(torch.tensor(-1.0), requires_grad=False)
        layer.k_scale = torch.nn.Parameter(torch.tensor(-1.0), requires_grad=False)
        layer.v_scale = torch.nn.Parameter(torch.tensor(-1.0), requires_grad=False)
        layer.prob_scale = torch.nn.Parameter(torch.tensor(-1.0), requires_grad=False)
    def apply(self, layer: torch.nn.Module) -> torch.Tensor:
        raise RuntimeError(f'{self.__class__.__name__}.apply should not be called.')
    def process_weights_after_loading(self, layer: torch.nn.Module) -> None:
        if layer.kv_cache_dtype != 'auto' and (not layer.calculate_kv_scales):
            if layer.k_scale > 0.0 and layer.v_scale > 0.0:
                k_scale = layer.k_scale.to('cpu').tolist()
                v_scale = layer.v_scale.to('cpu').tolist()
                if current_platform.is_fp8_fnuz():
                    k_scale *= 2
                    v_scale *= 2
            elif layer.k_scale < 0.0 and layer.v_scale < 0.0:
                k_scale = 1.0
                v_scale = 1.0
            else:
                assert layer.k_scale > 0.0
                scale_to_duplicate = max(layer.k_scale, layer.v_scale)
                k_scale = scale_to_duplicate.to('cpu').tolist()
                v_scale = scale_to_duplicate.to('cpu').tolist()
                if current_platform.is_fp8_fnuz():
                    k_scale *= 2
                    v_scale *= 2
            if not isinstance(k_scale, float) or not isinstance(v_scale, float):
                raise ValueError('Only support per-tensor scaling factor for fp8 KV cache')
            if layer.q_scale < 0.0:
                log_once('WARNING', 'Checkpoint does not provide a q scaling factor. Setting it to k_scale. This only matters for the flash-attn backend.')
                layer._q_scale.copy_(k_scale)
            layer._k_scale.copy_(k_scale)
            layer._v_scale.copy_(v_scale)
            layer._k_scale_float = k_scale
            layer._v_scale_float = v_scale
            if k_scale == 1.0 and v_scale == 1.0 and ('e5m2' not in layer.kv_cache_dtype):
                log_once('WARNING', 'Using KV cache scaling factor 1.0 for fp8_e4m3. This may cause accuracy issues. Please make sure k/v_scale scaling factors are available in the fp8 checkpoint.')
        if layer.q_scale > 0.0:
            q_scale = layer.q_scale
            if current_platform.is_fp8_fnuz():
                q_scale *= 2
            layer.calculate_kv_scales = False
        else:
            q_scale = 1.0
        if layer.prob_scale > 0.0:
            prob_scale = layer.prob_scale
            if current_platform.is_fp8_fnuz():
                prob_scale *= 2
        else:
            prob_scale = 1.0
        is_singleton_float = lambda x: isinstance(x, float) or (isinstance(x, torch.Tensor) and x.numel() == 1 and x.is_floating_point())
        if not is_singleton_float(q_scale) or not is_singleton_float(prob_scale):
            raise ValueError('Only support per-tensor scaling factorfor fp8-quantized Q/prob')
        layer._q_scale.copy_(q_scale)
        layer._prob_scale.copy_(prob_scale)
        if layer.kv_cache_dtype == 'fp8' and (q_scale == 1.0 or prob_scale == 1.0):
            log_once('WARNING', 'Using uncalibrated q_scale {} and/or prob_scale {} with fp8 attention. This may cause accuracy issues. Please make sure q/prob scaling factors are available in the fp8 checkpoint.', q_scale, prob_scale)
        del layer.k_scale
        del layer.v_scale
        del layer.q_scale
        del layer.prob_scale