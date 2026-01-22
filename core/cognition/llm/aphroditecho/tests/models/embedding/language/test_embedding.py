import pytest
from ..utils import check_embeddings_close
MODELS = ['intfloat/e5-mistral-7b-instruct', 'BAAI/bge-base-en-v1.5', 'BAAI/bge-multilingual-gemma2']
ENCODER_ONLY = ['BAAI/bge-base-en-v1.5']
@pytest.mark.parametrize('model', MODELS)
@pytest.mark.parametrize('dtype', ['half'])
def test_models(monkeypatch, hf_runner, aphrodite_runner, example_prompts, model, dtype: str) -> None:
    if model in ENCODER_ONLY:
        monkeypatch.setenv('APHRODITE_ATTENTION_BACKEND', 'XFORMERS')
    example_prompts = [str(s).strip() for s in example_prompts]
    with hf_runner(model, dtype=dtype, is_sentence_transformer=True) as hf_model:
        hf_outputs = hf_model.encode(example_prompts)
    with aphrodite_runner(model, dtype=dtype, max_model_len=None) as aphrodite_model:
        aphrodite_outputs = aphrodite_model.encode(example_prompts)
    check_embeddings_close(embeddings_0_lst=hf_outputs, embeddings_1_lst=aphrodite_outputs, name_0='hf', name_1='aphrodite', tol=0.01)