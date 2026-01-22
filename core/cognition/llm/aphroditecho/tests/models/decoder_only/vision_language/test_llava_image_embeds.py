from typing import List, Optional, Tuple, Type
import pytest
from transformers import AutoConfig, AutoModelForVision2Seq, AutoTokenizer
from aphrodite.common.sequence import SampleLogprobs
from ....conftest import IMAGE_ASSETS, AphroditeRunner, HfRunner, _ImageAssets
from ...utils import check_logprobs_close
HF_IMAGE_PROMPTS = IMAGE_ASSETS.prompts({'stop_sign': "USER: <image>\nWhat's the content of the image?\nASSISTANT:", 'cherry_blossom': 'USER: <image>\nWhat is the season?\nASSISTANT:'})
models = ['llava-hf/llava-1.5-7b-hf']
def aphrodite_to_hf_output(aphrodite_output: Tuple[List[int], str, Optional[SampleLogprobs]], model: str):
    output_ids, output_str, out_logprobs = aphrodite_output
    config = AutoConfig.from_pretrained(model)
    image_token_id = config.image_token_index
    tokenizer = AutoTokenizer.from_pretrained(model)
    eos_token_id = tokenizer.eos_token_id
    hf_output_ids = [token_id for idx, token_id in enumerate(output_ids) if token_id != image_token_id or output_ids[idx - 1] != image_token_id]
    assert output_str[0] == ' '
    hf_output_str = output_str[1:]
    if hf_output_ids[-1] == eos_token_id:
        hf_output_str = hf_output_str + tokenizer.decode(eos_token_id)
    return (hf_output_ids, hf_output_str, out_logprobs)
def run_test(hf_runner: Type[HfRunner], aphrodite_runner: Type[AphroditeRunner], image_assets: _ImageAssets, model: str, *, size_factors: List[float], dtype: str, max_tokens: int, num_logprobs: int, tensor_parallel_size: int, distributed_executor_backend: Optional[str]=None):
    aphrodite_images = [asset.image_embeds for asset in image_assets]
    hf_images = [asset.pil_image for asset in image_assets]
    aphrodite_inputs_per_image = [([prompt for _ in size_factors], [image for _ in size_factors]) for image, prompt in zip(aphrodite_images, HF_IMAGE_PROMPTS)]
    hf_inputs_per_image = [([prompt for _ in size_factors], [image for _ in size_factors]) for image, prompt in zip(hf_images, HF_IMAGE_PROMPTS)]
    with aphrodite_runner(model, dtype=dtype, tensor_parallel_size=tensor_parallel_size, distributed_executor_backend=distributed_executor_backend, enforce_eager=True) as aphrodite_model:
        aphrodite_outputs_per_image = [aphrodite_model.generate_greedy_logprobs(prompts, max_tokens, num_logprobs=num_logprobs, images=images) for prompts, images in aphrodite_inputs_per_image]
    with hf_runner(model, dtype=dtype, auto_cls=AutoModelForVision2Seq) as hf_model:
        hf_outputs_per_image = [hf_model.generate_greedy_logprobs_limit(prompts, max_tokens, num_logprobs=num_logprobs, images=images) for prompts, images in hf_inputs_per_image]
    for hf_outputs, aphrodite_outputs in zip(hf_outputs_per_image, aphrodite_outputs_per_image):
        check_logprobs_close(outputs_0_lst=hf_outputs, outputs_1_lst=[aphrodite_to_hf_output(aphrodite_output, model) for aphrodite_output in aphrodite_outputs], name_0='hf', name_1='aphrodite')
@pytest.mark.parametrize('model', models)
@pytest.mark.parametrize('size_factors', [[], [1.0], [1.0, 1.0, 1.0]])
@pytest.mark.parametrize('dtype', ['half'])
@pytest.mark.parametrize('max_tokens', [128])
@pytest.mark.parametrize('num_logprobs', [5])
def test_models(hf_runner, aphrodite_runner, image_assets, model, size_factors, dtype: str, max_tokens: int, num_logprobs: int) -> None:
    run_test(hf_runner, aphrodite_runner, image_assets, model, size_factors=size_factors, dtype=dtype, max_tokens=max_tokens, num_logprobs=num_logprobs, tensor_parallel_size=1)