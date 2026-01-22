from pathlib import Path
import fire
import torch
from accelerate import infer_auto_device_map, init_empty_weights, load_checkpoint_in_model
from transformers import AutoConfig, AutoModelForCausalLM, AutoTokenizer
from aphrodite.kv_quant.calib_dataloader import get_calib_loaders
from aphrodite.kv_quant.calibration import CalibrationContext
from aphrodite.kv_quant.utils import collect_target_modules
LAYER_TYPE_MAP = {'InternLMForCausalLM': 'InternLMDecoderLayer', 'QWenLMHeadModel': 'QWenBlock', 'BaiChuanForCausalLM': 'DecoderLayer', 'LlamaForCausalLM': 'LlamaDecoderLayer'}
NORM_TYPE_MAP = {'InternLMForCausalLM': 'InternLMRMSNorm', 'QWenLMHeadModel': 'RMSNorm', 'BaiChuanForCausalLM': 'RMSNorm', 'LlamaForCausalLM': 'LlamaRMSNorm'}
def calibrate(model: str, calib_dataset: str='c4', calib_samples: int=128, calib_seqlen: int=2048, work_dir: str='./work_dir', device: str='cuda', dataset_path: str=None) -> None:
    assert calib_dataset in ['c4', 'ptb', 'wikitext2', 'pileval'], 'Support only `c4`, `ptb`, `wikitext2` or `pileval`.'
    tokenizer = AutoTokenizer.from_pretrained(model, use_fast=False, trust_remote_code=True)
    hf_config = AutoConfig.from_pretrained(model, trust_remote_code=True)
    checkpoint = hf_config._name_or_path
    with init_empty_weights():
        model = AutoModelForCausalLM.from_pretrained(model, torch_dtype=torch.float16, trust_remote_code=True)
        model.config.use_cache = False
    layer_type = LAYER_TYPE_MAP[type(model).__name__]
    norm_type = NORM_TYPE_MAP[type(model).__name__]
    decoder_layers = collect_target_modules(model, layer_type)
    device_map = infer_auto_device_map(model, no_split_module_classes=[layer_type])
    for name in device_map:
        if name in decoder_layers or 'lm_head' in name:
            device_map[name] = 'cpu'
        else:
            device_map[name] = 0
    load_checkpoint_in_model(model, checkpoint, device_map)
    print('Loading calibrate dataset ...')
    calib_loader, _ = get_calib_loaders(calib_dataset, tokenizer, nsamples=calib_samples, seqlen=calib_seqlen, path=dataset_path)
    calib_ctx = CalibrationContext(model, tokenizer, layer_type=layer_type, norm_type=norm_type, device=device)
    with calib_ctx:
        all_data = torch.cat([data if isinstance(data, torch.Tensor) else data[0] for data in calib_loader]).to(device)
        calib_ctx.calibrate(all_data)
    work_dir = Path(work_dir)
    work_dir.mkdir(parents=True, exist_ok=True)
    calib_ctx.export(work_dir)
if __name__ == '__main__':
    fire.Fire(calibrate)