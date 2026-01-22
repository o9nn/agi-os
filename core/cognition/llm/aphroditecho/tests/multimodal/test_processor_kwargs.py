from array import array
from typing import Mapping
from unittest.mock import patch
import pytest
import torch
from aphrodite.common.sequence import APHRODITE_TOKEN_ID_ARRAY_TYPE, SequenceData
from aphrodite.inputs import InputContext, LLMInputs
from aphrodite.inputs.registry import InputRegistry
from aphrodite.multimodal import MultiModalRegistry
from ..models.utils import build_model_context
DUMMY_MODEL_ID = 'facebook/opt-125m'
MULTIMODAL_MODEL_ID = 'microsoft/Phi-3.5-vision-instruct'
DEFAULT_NUM_CROPS = 4
NUM_CROPS_OVERRIDE = 16
@pytest.fixture
def use_processor_mock():
    def custom_processor(ctx: InputContext, llm_inputs: LLMInputs, *, num_crops=DEFAULT_NUM_CROPS):
        return num_crops
    with patch('aphrodite.inputs.registry.InputRegistry._get_model_input_processor', return_value=custom_processor):
        yield
@pytest.fixture
def use_dummy_data_mock():
    def custom_dummy_data_factory(self, ctx: InputContext, seq_len: int, mm_counts: Mapping[str, int], *, num_crops=DEFAULT_NUM_CROPS):
        seq_data = SequenceData(array(APHRODITE_TOKEN_ID_ARRAY_TYPE, [0] * num_crops))
        return (seq_data, None)
    with patch('aphrodite.inputs.registry.InputRegistry._default_dummy_data_factory', custom_dummy_data_factory):
        yield
def mm_model_cls():
    from aphrodite.modeling.models.phi3v import Phi3VForCausalLM
    return Phi3VForCausalLM
get_num_crops = lambda ctx, *, num_crops=DEFAULT_NUM_CROPS: num_crops
custom_mapper = lambda ctx, data, *, num_crops=DEFAULT_NUM_CROPS: {'pixel_values': torch.zeros(size=(1, num_crops + 1, 3, 336, 336))}
def test_default_processor_is_a_noop():
    dummy_registry = InputRegistry()
    ctx = build_model_context(DUMMY_MODEL_ID)
    processor = dummy_registry.create_input_processor(ctx.model_config)
    proc_inputs = LLMInputs(prompt_token_ids=[], prompt='')
    proc_outputs = processor(inputs=proc_inputs)
    assert proc_inputs is proc_outputs
def _get_num_crops_info(init_num_crops: int, inference_num_crops: int):
    init_kwargs = None if init_num_crops is None else {'num_crops': init_num_crops}
    inference_kwargs = None if inference_num_crops is None else {'num_crops': inference_num_crops}
    if inference_num_crops is not None:
        expected_seq_count = inference_num_crops
    elif init_num_crops is not None:
        expected_seq_count = init_num_crops
    else:
        expected_seq_count = DEFAULT_NUM_CROPS
    return (init_kwargs, inference_kwargs, expected_seq_count)
@pytest.mark.parametrize('init_num_crops,inference_num_crops', [(None, None), (NUM_CROPS_OVERRIDE, None), (DEFAULT_NUM_CROPS, NUM_CROPS_OVERRIDE)])
def test_input_processor_kwargs(use_processor_mock, init_num_crops, inference_num_crops):
    dummy_registry = InputRegistry()
    init_kwargs, inference_kwargs, expected_seq_count = _get_num_crops_info(init_num_crops, inference_num_crops)
    ctx = build_model_context(DUMMY_MODEL_ID, mm_processor_kwargs=init_kwargs)
    processor = dummy_registry.create_input_processor(ctx.model_config)
    num_crops_val = processor(LLMInputs(prompt_token_ids=[], prompt='', mm_processor_kwargs=inference_kwargs))
    assert num_crops_val == expected_seq_count
@pytest.mark.parametrize('mm_processor_kwargs', [{'does_not_exist': 100}, {'ctx': 'something bad'}])
def test_processor_with_sad_kwarg_overrides(use_processor_mock, mm_processor_kwargs):
    dummy_registry = InputRegistry()
    ctx = build_model_context(DUMMY_MODEL_ID, mm_processor_kwargs=mm_processor_kwargs)
    processor = dummy_registry.create_input_processor(ctx.model_config)
    num_crops_val = processor(LLMInputs(prompt_token_ids=[], prompt='', mm_processor_kwargs=mm_processor_kwargs))
    assert num_crops_val == DEFAULT_NUM_CROPS
@pytest.mark.parametrize('num_crops', [None, NUM_CROPS_OVERRIDE])
def test_dummy_data_kwarg_overrides(use_dummy_data_mock, num_crops):
    mm_processor_kwargs = None if num_crops is None else {'num_crops': num_crops}
    expected_seq_count = DEFAULT_NUM_CROPS if num_crops is None else num_crops
    dummy_registry = InputRegistry()
    ctx = build_model_context(DUMMY_MODEL_ID, mm_processor_kwargs=mm_processor_kwargs)
    mm_registry = MultiModalRegistry()
    mm_registry.init_mm_limits_per_prompt(ctx.model_config)
    seq_data, _ = dummy_registry.dummy_data_for_profiling(ctx.model_config, seq_len=-1, mm_registry=mm_registry)
    assert len(seq_data.prompt_token_ids) == expected_seq_count
@pytest.mark.parametrize('mm_processor_kwargs', [{'does_not_exist': 100}, {'ctx': 'something bad'}])
def test_dummy_data_with_sad_kwarg_overrides(use_dummy_data_mock, mm_processor_kwargs):
    dummy_registry = InputRegistry()
    ctx = build_model_context(DUMMY_MODEL_ID, mm_processor_kwargs=mm_processor_kwargs)
    mm_registry = MultiModalRegistry()
    mm_registry.init_mm_limits_per_prompt(ctx.model_config)
    seq_data, _ = dummy_registry.dummy_data_for_profiling(ctx.model_config, seq_len=-1, mm_registry=mm_registry)
    assert len(seq_data.prompt_token_ids) == DEFAULT_NUM_CROPS
@pytest.mark.parametrize('num_crops', [None, NUM_CROPS_OVERRIDE])
def test_max_tokens_kwarg_overrides(num_crops):
    mm_processor_kwargs = None if num_crops is None else {'num_crops': num_crops}
    expected_seq_count = DEFAULT_NUM_CROPS if num_crops is None else num_crops
    ctx = build_model_context(MULTIMODAL_MODEL_ID, trust_remote_code=True, mm_processor_kwargs=mm_processor_kwargs, limit_mm_per_prompt={'image': 1})
    mm_registry = MultiModalRegistry()
    mm_registry.init_mm_limits_per_prompt(ctx.model_config)
    with patch.object(mm_registry._get_plugin('image'), '_max_mm_tokens', {mm_model_cls(): get_num_crops}):
        max_multimodal_tokens = mm_registry.get_max_multimodal_tokens(ctx.model_config)
    assert expected_seq_count == max_multimodal_tokens
@pytest.mark.parametrize('mm_processor_kwargs', [{'does_not_exist': 100}, {'ctx': 'something bad'}])
def test_max_tokens_with_sad_kwarg_overrides(mm_processor_kwargs):
    ctx = build_model_context(MULTIMODAL_MODEL_ID, trust_remote_code=True, mm_processor_kwargs=mm_processor_kwargs, limit_mm_per_prompt={'image': 1})
    mm_registry = MultiModalRegistry()
    mm_registry.init_mm_limits_per_prompt(ctx.model_config)
    with patch.object(mm_registry._get_plugin('image'), '_max_mm_tokens', {mm_model_cls(): get_num_crops}):
        max_multimodal_tokens = mm_registry.get_max_multimodal_tokens(ctx.model_config)
    assert max_multimodal_tokens == DEFAULT_NUM_CROPS
@pytest.mark.parametrize('num_crops', [DEFAULT_NUM_CROPS, NUM_CROPS_OVERRIDE])
def test_default_mapper_with_processer_kwargs(image_assets, num_crops):
    ctx = build_model_context(MULTIMODAL_MODEL_ID, trust_remote_code=True, mm_processor_kwargs={'num_crops': num_crops}, limit_mm_per_prompt={'image': 1})
    mm_registry = MultiModalRegistry()
    mm_registry.init_mm_limits_per_prompt(ctx.model_config)
    image = image_assets[0].pil_image
    mm_inputs = {'image': image}
    mapped_inputs = mm_registry.map_input(ctx.model_config, mm_inputs)
    assert mapped_inputs['pixel_values'].shape[1] == num_crops + 1
@pytest.mark.parametrize('init_num_crops,inference_num_crops', [(None, None), (NUM_CROPS_OVERRIDE, None), (DEFAULT_NUM_CROPS, NUM_CROPS_OVERRIDE)])
def test_custom_mapper_kwarg_overrides(image_assets, init_num_crops, inference_num_crops):
    init_kwargs, inference_kwargs, expected_seq_count = _get_num_crops_info(init_num_crops, inference_num_crops)
    ctx = build_model_context(MULTIMODAL_MODEL_ID, trust_remote_code=True, mm_processor_kwargs=init_kwargs, limit_mm_per_prompt={'image': 1})
    mm_registry = MultiModalRegistry()
    mm_registry.init_mm_limits_per_prompt(ctx.model_config)
    image = image_assets[0].pil_image
    mm_inputs = {'image': image}
    mm_registry._get_plugin('image').register_input_mapper(custom_mapper)(mm_model_cls())
    mapped_inputs = mm_registry.map_input(ctx.model_config, mm_inputs, inference_kwargs)
    assert mapped_inputs['pixel_values'].shape[1] == expected_seq_count + 1
@pytest.mark.parametrize('mm_processor_kwargs', [{'does_not_exist': 100}, {'ctx': 'something bad'}])
def test_custom_mapper_with_sad_kwarg_overrides(image_assets, mm_processor_kwargs):
    ctx = build_model_context(MULTIMODAL_MODEL_ID, trust_remote_code=True, mm_processor_kwargs=mm_processor_kwargs, limit_mm_per_prompt={'image': 1})
    mm_registry = MultiModalRegistry()
    mm_registry.init_mm_limits_per_prompt(ctx.model_config)
    image = image_assets[0].pil_image
    mm_inputs = {'image': image}
    mm_registry._get_plugin('image').register_input_mapper(custom_mapper)(mm_model_cls())
    mapped_inputs = mm_registry.map_input(ctx.model_config, mm_inputs, mm_processor_kwargs=mm_processor_kwargs)
    assert mapped_inputs['pixel_values'].shape[1] == DEFAULT_NUM_CROPS + 1