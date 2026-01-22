import os
import re
from typing import Callable, List, Optional, Tuple, Type
import pytest
import torch
from transformers import AutoImageProcessor, AutoTokenizer
from aphrodite.common.sequence import SampleLogprobs
from aphrodite.common.utils import is_cpu, is_hip
from aphrodite.inputs import InputContext, LLMInputs
from aphrodite.modeling.models.phi3v import _IMAGE_TOKEN_ID
from aphrodite.multimodal import MultiModalRegistry
from aphrodite.multimodal.utils import rescale_image_size
from ....conftest import IMAGE_ASSETS, AphroditeRunner, HfRunner, PromptImageInput, _ImageAssets
from ...utils import build_model_context, check_logprobs_close
HF_IMAGE_PROMPTS = IMAGE_ASSETS.prompts({'stop_sign': "<|user|>\n<|image_1|>\nWhat's the content of the image?<|end|>\n<|assistant|>\n", 'cherry_blossom': '<|user|>\n<|image_1|>\nWhat is the season?<|end|>\n<|assistant|>\n'})
HF_MULTIIMAGE_IMAGE_PROMPT = '<|user|>\n<|image_1|>\n<|image_2|>\nDescribe these images.<|end|>\n<|assistant|>\n'
models = ['microsoft/Phi-3.5-vision-instruct']
def aphrodite_to_hf_output(aphrodite_output: Tuple[List[int], str, Optional[SampleLogprobs]], model: str):
    _, output_str, out_logprobs = aphrodite_output
    output_str_without_image = re.sub('(<\\|image_\\d+\\|>)+', '', output_str)
    assert output_str_without_image[0] == ' '
    output_str_without_image = output_str_without_image[1:]
    hf_output_str = output_str_without_image + '<|end|><|endoftext|>'
    tokenizer = AutoTokenizer.from_pretrained(model)
    hf_output_ids = tokenizer.encode(output_str_without_image)
    assert hf_output_ids[0] == 1
    hf_output_ids = hf_output_ids[1:]
    return (hf_output_ids, hf_output_str, out_logprobs)
target_dtype = 'half'
if is_cpu():
    target_dtype = 'bfloat16'
if is_hip():
    os.environ['APHRODITE_USE_TRITON_FLASH_ATTN'] = '0'
def run_test(hf_runner: Type[HfRunner], aphrodite_runner: Type[AphroditeRunner], inputs: List[Tuple[List[str], PromptImageInput]], model: str, *, dtype: str, max_tokens: int, num_logprobs: int, mm_limit: int, tensor_parallel_size: int, distributed_executor_backend: Optional[str]=None):
    with aphrodite_runner(model, max_model_len=4096, max_num_seqs=1, dtype=dtype, limit_mm_per_prompt={'image': mm_limit}, tensor_parallel_size=tensor_parallel_size, distributed_executor_backend=distributed_executor_backend, enforce_eager=True) as aphrodite_model:
        aphrodite_outputs_per_case = [aphrodite_model.generate_greedy_logprobs(prompts, max_tokens, num_logprobs=num_logprobs, images=images) for prompts, images in inputs]
    hf_model_kwargs = {'_attn_implementation': 'eager'}
    with hf_runner(model, dtype=dtype, model_kwargs=hf_model_kwargs) as hf_model:
        eos_token_id = hf_model.processor.tokenizer.eos_token_id
        hf_outputs_per_case = [hf_model.generate_greedy_logprobs_limit(prompts, max_tokens, num_logprobs=num_logprobs, images=images, eos_token_id=eos_token_id) for prompts, images in inputs]
    for hf_outputs, aphrodite_outputs in zip(hf_outputs_per_case, aphrodite_outputs_per_case):
        check_logprobs_close(outputs_0_lst=hf_outputs, outputs_1_lst=[aphrodite_to_hf_output(aphrodite_output, model) for aphrodite_output in aphrodite_outputs], name_0='hf', name_1='aphrodite')
@pytest.mark.parametrize('model', models)
@pytest.mark.parametrize('size_factors', [[], [1.0], [1.0, 1.0, 1.0], [0.25, 0.5, 1.0]])
@pytest.mark.parametrize('dtype', [target_dtype])
@pytest.mark.parametrize('max_tokens', [128])
@pytest.mark.parametrize('num_logprobs', [10])
def test_models(hf_runner, aphrodite_runner, image_assets, model, size_factors, dtype: str, max_tokens: int, num_logprobs: int) -> None:
    images = [asset.pil_image for asset in image_assets]
    inputs_per_image = [([prompt for _ in size_factors], [rescale_image_size(image, factor) for factor in size_factors]) for image, prompt in zip(images, HF_IMAGE_PROMPTS)]
    run_test(hf_runner, aphrodite_runner, inputs_per_image, model, dtype=dtype, max_tokens=max_tokens, num_logprobs=num_logprobs, mm_limit=1, tensor_parallel_size=1)
@pytest.mark.parametrize('model', models)
@pytest.mark.parametrize('dtype', [target_dtype])
def test_regression_7840(hf_runner, aphrodite_runner, image_assets, model, dtype) -> None:
    images = [asset.pil_image for asset in image_assets]
    inputs_regresion_7840 = [([prompt], [image]) for image, prompt in zip(images, HF_IMAGE_PROMPTS)]
    run_test(hf_runner, aphrodite_runner, inputs_regresion_7840, model, dtype=dtype, max_tokens=128, num_logprobs=10, mm_limit=1, tensor_parallel_size=1)
@pytest.mark.parametrize('model', models)
@pytest.mark.parametrize('size_factors', [[], [1.0], [1.0, 1.0, 1.0], [0.25, 0.5, 1.0]])
@pytest.mark.parametrize('dtype', [target_dtype])
@pytest.mark.parametrize('max_tokens', [128])
@pytest.mark.parametrize('num_logprobs', [10])
def test_multi_images_models(hf_runner, aphrodite_runner, image_assets, model, size_factors, dtype: str, max_tokens: int, num_logprobs: int) -> None:
    images = [asset.pil_image for asset in image_assets]
    inputs_per_case = [([HF_MULTIIMAGE_IMAGE_PROMPT for _ in size_factors], [[rescale_image_size(image, factor) for image in images] for factor in size_factors])]
    run_test(hf_runner, aphrodite_runner, inputs_per_case, model, dtype=dtype, max_tokens=max_tokens, num_logprobs=num_logprobs, mm_limit=2, tensor_parallel_size=1)
@pytest.fixture()
def input_processor_for_phi3v():
    from aphrodite.modeling.models.phi3v import input_processor_for_phi3v
    return input_processor_for_phi3v
@pytest.fixture()
def dummy_data_for_phi3v():
    from aphrodite.modeling.models.phi3v import dummy_data_for_phi3v
    return dummy_data_for_phi3v
@pytest.fixture()
def get_max_phi3v_image_tokens():
    from aphrodite.modeling.models.phi3v import get_max_phi3v_image_tokens
    return get_max_phi3v_image_tokens
@pytest.mark.parametrize('model', models)
@pytest.mark.parametrize('num_crops', [4, 16, None])
def test_input_mapper_override(model: str, image_assets: _ImageAssets, num_crops: Optional[int]):
    mm_processor_kwargs = {'num_crops': num_crops} if num_crops is not None else {}
    ctx = build_model_context(model_name=model, tokenizer_name=model, trust_remote_code=True, mm_processor_kwargs=mm_processor_kwargs)
    hf_processor = AutoImageProcessor.from_pretrained(model, trust_remote_code=True, **mm_processor_kwargs)
    mm_registry = MultiModalRegistry()
    mm_registry.init_mm_limits_per_prompt(ctx.model_config)
    image = image_assets[0].pil_image
    hf_result = hf_processor.preprocess(image, return_tensors='pt')
    aphrodite_result = mm_registry.map_input(ctx.model_config, {'image': image})
    assert torch.all(hf_result['image_sizes'] == aphrodite_result['image_sizes'])
    assert torch.all(hf_result['num_img_tokens'] == aphrodite_result['num_img_tokens'])
    assert torch.all(hf_result['pixel_values'] == aphrodite_result['pixel_values'])
    assert aphrodite_result['pixel_values'].shape[1] == hf_processor.num_crops + 1
@pytest.mark.parametrize('model', models)
@pytest.mark.parametrize('num_crops,expected_max_tokens', [(4, 781), (16, 2653)])
def test_max_tokens_override(get_max_phi3v_image_tokens: Callable, model: str, num_crops: int, expected_max_tokens: int):
    ctx = build_model_context(model_name=model, tokenizer_name=model, trust_remote_code=True, mm_processor_kwargs=None)
    actual_max_tokens = get_max_phi3v_image_tokens(InputContext(ctx.model_config), num_crops=num_crops)
    assert expected_max_tokens == actual_max_tokens
@pytest.mark.parametrize('model', models)
@pytest.mark.parametrize('num_crops,toks_per_img,num_imgs', [(4, 781, 1), (4, 781, 2), (16, 2653, 1), (16, 2653, 2)])
def test_dummy_data_override(dummy_data_for_phi3v: Callable, model: str, num_crops: int, toks_per_img: int, num_imgs: int):
    ctx = build_model_context(model_name=model, tokenizer_name=model, trust_remote_code=True, mm_processor_kwargs=None)
    sequence_data, _ = dummy_data_for_phi3v(ctx=ctx, seq_len=8192, mm_counts={'image': num_imgs}, num_crops=num_crops)
    img_tok_count = sequence_data.get_token_ids().count(_IMAGE_TOKEN_ID)
    assert img_tok_count == toks_per_img * num_imgs
@pytest.mark.parametrize('model', models)
@pytest.mark.parametrize('num_crops,expected_toks_per_img,num_imgs', [(4, 757, 1), (4, 757, 2), (16, 1921, 1), (16, 1921, 2)])
def test_input_processor_override(input_processor_for_phi3v: Callable, image_assets: _ImageAssets, model: str, num_crops: int, expected_toks_per_img: int, num_imgs: int):
    ctx = build_model_context(model_name=model, tokenizer_name=model, trust_remote_code=True)
    tokenizer = AutoTokenizer.from_pretrained(model)
    img_str = ''.join([f'<|image_{idx}|>\n' for idx in range(1, num_imgs + 1)])
    prompt = f'<|user|>\n{img_str}<|end|>\n<|assistant|>\n'
    images = [image_assets[0].pil_image] * num_imgs
    llm_inputs = LLMInputs(prompt_token_ids=tokenizer.encode(prompt), prompt=prompt, multi_modal_data={'image': images})
    proc_llm_inputs = input_processor_for_phi3v(ctx=ctx, llm_inputs=llm_inputs, num_crops=num_crops)
    img_tok_count = proc_llm_inputs['prompt_token_ids'].count(_IMAGE_TOKEN_ID)
    assert img_tok_count == expected_toks_per_img * num_imgs