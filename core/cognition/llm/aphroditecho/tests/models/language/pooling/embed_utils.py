from collections.abc import Sequence
from typing import Optional
import pytest
from tests.conftest import HfRunner
from tests.models.utils import EmbedModelInfo, check_embeddings_close, matryoshka_fy
def run_embedding_correctness_test(hf_model: 'HfRunner', inputs: list[str], aphrodite_outputs: Sequence[list[float]], dimensions: Optional[int]=None):
    hf_outputs = hf_model.encode(inputs)
    if dimensions:
        hf_outputs = matryoshka_fy(hf_outputs, dimensions)
    check_embeddings_close(embeddings_0_lst=hf_outputs, embeddings_1_lst=aphrodite_outputs, name_0='hf', name_1='aphrodite', tol=0.01)
def correctness_test_embed_models(hf_runner, aphrodite_runner, model_info: EmbedModelInfo, example_prompts, aphrodite_extra_kwargs=None, hf_model_callback=None):
    if not model_info.enable_test:
        pytest.skip('Skipping test.')
    example_prompts = [str(s).strip() for s in example_prompts]
    aphrodite_extra_kwargs = aphrodite_extra_kwargs or {}
    aphrodite_extra_kwargs['dtype'] = model_info.dtype
    with aphrodite_runner(model_info.name, runner='pooling', max_model_len=None, **aphrodite_extra_kwargs) as aphrodite_model:
        aphrodite_outputs = aphrodite_model.embed(example_prompts)
    with hf_runner(model_info.name, dtype='float32', is_sentence_transformer=True) as hf_model:
        if hf_model_callback is not None:
            hf_model_callback(hf_model)
        run_embedding_correctness_test(hf_model, example_prompts, aphrodite_outputs)