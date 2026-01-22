import pathlib
from typing import Dict, List, Optional, Tuple, Type, Union
import pytest
import torch
from PIL.Image import Image
from aphrodite.inputs import InputContext, LLMInputs
from aphrodite.multimodal.base import MultiModalInputs
from aphrodite.multimodal.utils import cached_get_tokenizer, rescale_image_size
from ....conftest import IMAGE_ASSETS, AphroditeRunner, HfRunner, ImageAsset, PromptImageInput, _ImageAssets
from ...utils import build_model_context, check_logprobs_close
text_only_models = ['Qwen/Qwen-7B-Chat']
multimodal_models = ['Qwen/Qwen-VL']
HF_IMAGE_PROMPTS = IMAGE_ASSETS.prompts({'stop_sign': "Picture 1: <img></img>\nWhat's the content of the image?: ", 'cherry_blossom': 'Picture 1: <img></img>\nWhat is the season?: '})
HF_MULTIIMAGE_IMAGE_PROMPT = 'Picture 1: <img></img>\nPicture 2: <img></img>\nCan you compare these images?\n'
HF_MULTIIMAGE_IMAGE_PROMPT = 'Picture 1: <img></img>\nPicture 2: <img></img>\nDescribe the two images in detail.\n'
SAMPLE_IMAGE = IMAGE_ASSETS[0].pil_image
IMG_START_ID = 151857
IMG_END_ID = 151858
IMG_PAD_ID = 151859
TOKS_PER_IMG = 256
VIS_ENC_DIM = 4096
IMG_SIZE = 448
@pytest.fixture()
def input_mapper_for_qwen():
    from aphrodite.modeling.models.qwen import input_mapper_for_qwen
    return input_mapper_for_qwen
@pytest.fixture()
def input_processor_for_qwen():
    from aphrodite.modeling.models.qwen import input_processor_for_qwen
    return input_processor_for_qwen
@pytest.fixture()
def qwen_vl_context() -> InputContext:
    return build_model_context(model_name='Qwen/Qwen-VL', trust_remote_code=True)
@pytest.mark.parametrize('num_images', [1, 2])
def test_input_processor_valid_mm_data(input_processor_for_qwen, qwen_vl_context: InputContext, num_images: int):
    prompt = ''.join([f'Picture {num}: <img></img>\n' for num in range(1, num_images + 1)])
    inputs = LLMInputs(prompt=prompt, prompt_token_ids=None, multi_modal_data={'image': torch.rand(num_images, TOKS_PER_IMG, 4096)})
    proc_inputs = input_processor_for_qwen(qwen_vl_context, inputs)
    assert isinstance(proc_inputs, dict)
    proc_tokens = proc_inputs['prompt_token_ids']
    assert proc_tokens.count(IMG_START_ID) == num_images
    assert proc_tokens.count(IMG_END_ID) == num_images
    assert proc_tokens.count(IMG_PAD_ID) == num_images * TOKS_PER_IMG
@pytest.mark.parametrize('img_data,expected_shape', [(SAMPLE_IMAGE, (1, 3, IMG_SIZE, IMG_SIZE)), (2 * [SAMPLE_IMAGE], (2, 3, IMG_SIZE, IMG_SIZE)), (torch.rand((TOKS_PER_IMG, VIS_ENC_DIM)), (1, TOKS_PER_IMG, VIS_ENC_DIM)), (torch.rand((1, TOKS_PER_IMG, VIS_ENC_DIM)), (1, TOKS_PER_IMG, VIS_ENC_DIM)), (torch.rand((2, TOKS_PER_IMG, VIS_ENC_DIM)), (2, TOKS_PER_IMG, VIS_ENC_DIM))])
def test_input_mapper_valid_mm_data(input_mapper_for_qwen, qwen_vl_context: InputContext, img_data: Union[torch.Tensor, List[Image], Image], expected_shape: List[int]):
    mapped_img_data = input_mapper_for_qwen(qwen_vl_context, img_data)
    assert isinstance(mapped_img_data, MultiModalInputs)
    assert 'pixel_values' in mapped_img_data
    assert mapped_img_data['pixel_values'].shape == expected_shape
@pytest.mark.parametrize('mm_data', [{'image': torch.rand(5)}, {'image': torch.rand((5, 5, 5, 5, 5))}])
def test_input_processor_invalid_mm_data(input_processor_for_qwen, qwen_vl_context: InputContext, mm_data: Dict[str, torch.Tensor]):
    tokenizer = cached_get_tokenizer(qwen_vl_context.model_config.tokenizer, trust_remote_code=True)
    prompt = 'Picture 1: <img></img>\n'
    prompt_token_ids = tokenizer.encode(prompt)
    inputs = LLMInputs(prompt=prompt, prompt_token_ids=prompt_token_ids, multi_modal_data=mm_data)
    with pytest.raises(ValueError):
        input_processor_for_qwen(qwen_vl_context, inputs)
@pytest.mark.parametrize('img_data', [torch.rand((1, TOKS_PER_IMG + 10, VIS_ENC_DIM)), torch.rand((1, TOKS_PER_IMG, VIS_ENC_DIM + 10))])
def test_input_mapper_invalid_mm_data(input_mapper_for_qwen, qwen_vl_context: InputContext, img_data: Union[torch.Tensor, List[Image], Image]):
    with pytest.raises(ValueError):
        input_mapper_for_qwen(qwen_vl_context, img_data)
def get_prompt_with_path(tmp_path: pathlib.PosixPath, prompt: str, assets: Union[_ImageAssets, List[ImageAsset]]) -> str:
    assert prompt.count('<img></img>') == len(assets)
    for asset in assets:
        image_tmp_path = tmp_path / f'{asset.name}.jpg'
        asset.pil_image.save(image_tmp_path)
        prompt = prompt.replace('<img></img>', f'<img>{image_tmp_path}</img>', 1)
    return prompt
def run_test(hf_runner: Type[HfRunner], aphrodite_runner: Type[AphroditeRunner], inputs: List[Tuple[List[str], PromptImageInput]], model: str, *, dtype: str, max_tokens: int, num_logprobs: int, mm_limit: int, tensor_parallel_size: int, distributed_executor_backend: Optional[str]=None):
    with aphrodite_runner(model, max_model_len=1024, max_num_seqs=1, dtype=dtype, limit_mm_per_prompt={'image': mm_limit}, tensor_parallel_size=tensor_parallel_size, distributed_executor_backend=distributed_executor_backend, enforce_eager=True) as aphrodite_model:
        aphrodite_outputs_per_image = [aphrodite_model.generate_greedy_logprobs(prompts, max_tokens, num_logprobs=num_logprobs, images=images) for prompts, images in inputs]
    with hf_runner(model, dtype=dtype) as hf_model:
        hf_outputs_per_image = [hf_model.generate_greedy_logprobs_limit(prompts, max_tokens, num_logprobs=num_logprobs, images=images) for prompts, images in inputs]
    for hf_outputs, aphrodite_outputs in zip(hf_outputs_per_image, aphrodite_outputs_per_image):
        check_logprobs_close(outputs_0_lst=hf_outputs, outputs_1_lst=aphrodite_outputs, name_0='hf', name_1='aphrodite')
@pytest.mark.parametrize('model', multimodal_models)
@pytest.mark.parametrize('size_factors', [[], [1.0], [1.0, 1.0, 1.0], [0.25, 0.5, 1.0]])
@pytest.mark.parametrize('dtype', ['bfloat16'])
@pytest.mark.parametrize('max_tokens', [8])
@pytest.mark.parametrize('num_logprobs', [5])
def test_multimodal_models_single_image(tmp_path: pathlib.PosixPath, hf_runner: Type[HfRunner], aphrodite_runner: Type[AphroditeRunner], image_assets: _ImageAssets, model: str, size_factors: List[float], dtype: str, max_tokens: int, num_logprobs: int) -> None:
    images = [asset.pil_image for asset in image_assets]
    prompts = [get_prompt_with_path(tmp_path, prompt, [asset]) for prompt, asset in zip(HF_IMAGE_PROMPTS, image_assets)]
    inputs = [([prompt for _ in size_factors], [rescale_image_size(image, factor) for factor in size_factors]) for image, prompt in zip(images, prompts)]
    run_test(hf_runner, aphrodite_runner, inputs, model, dtype=dtype, max_tokens=max_tokens, num_logprobs=num_logprobs, mm_limit=1, tensor_parallel_size=1)
@pytest.mark.parametrize('model', multimodal_models)
@pytest.mark.parametrize('size_factors', [[], [1.0], [1.0, 1.0, 1.0], [0.25, 0.5, 1.0]])
@pytest.mark.parametrize('dtype', ['bfloat16'])
@pytest.mark.parametrize('max_tokens', [128])
@pytest.mark.parametrize('num_logprobs', [5])
def test_multimodal_models_multi_image(tmp_path: pathlib.PosixPath, hf_runner: Type[HfRunner], aphrodite_runner: Type[AphroditeRunner], image_assets: _ImageAssets, model: str, size_factors: List[float], dtype: str, max_tokens: int, num_logprobs: int) -> None:
    images = [asset.pil_image for asset in image_assets]
    prompt = get_prompt_with_path(tmp_path, HF_MULTIIMAGE_IMAGE_PROMPT, image_assets)
    inputs = [([prompt for _ in size_factors], [[rescale_image_size(image, factor) for image in images] for factor in size_factors])]
    run_test(hf_runner, aphrodite_runner, inputs, model, dtype=dtype, max_tokens=max_tokens, num_logprobs=num_logprobs, mm_limit=2, tensor_parallel_size=1)
@pytest.mark.parametrize('model', text_only_models)
@pytest.mark.parametrize('dtype', ['bfloat16'])
@pytest.mark.parametrize('max_tokens', [32])
@pytest.mark.parametrize('num_logprobs', [5])
def test_text_only_qwen_model_can_be_loaded_and_run(aphrodite_runner: Type[AphroditeRunner], example_prompts: List[str], model: str, *, dtype: str, max_tokens: int, num_logprobs: int):
    with aphrodite_runner(model, dtype=dtype) as aphrodite_model:
        aphrodite_model.generate_greedy_logprobs(example_prompts, max_tokens, num_logprobs=num_logprobs)