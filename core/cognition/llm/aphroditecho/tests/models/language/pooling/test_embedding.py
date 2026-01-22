from typing import Optional
import pytest
from aphrodite.common.config import PoolerConfig
from aphrodite.platforms import current_platform
from ...utils import check_embeddings_close
@pytest.fixture(autouse=True)
def v1(run_with_both_engines):
    pass
@pytest.mark.parametrize('model', [pytest.param('BAAI/bge-multilingual-gemma2', marks=[pytest.mark.core_model]), pytest.param('intfloat/e5-mistral-7b-instruct', marks=[pytest.mark.core_model]), pytest.param('ssmits/Qwen2-7B-Instruct-embed-base', marks=[pytest.mark.skip_v0, pytest.mark.cpu_model]), pytest.param('BAAI/bge-base-en-v1.5', marks=[pytest.mark.core_model]), pytest.param('sentence-transformers/all-MiniLM-L12-v2'), pytest.param('intfloat/multilingual-e5-small'), pytest.param('Alibaba-NLP/gte-Qwen2-1.5B-instruct', marks=[pytest.mark.skip_v1]), pytest.param('sentence-transformers/stsb-roberta-base-v2', marks=[pytest.mark.skip_v1])])
def test_models(hf_runner, aphrodite_runner, example_prompts, model, monkeypatch) -> None:
    if model == 'BAAI/bge-multilingual-gemma2' and current_platform.is_rocm():
        monkeypatch.setenv('APHRODITE_USE_TRITON_FLASH_ATTN', 'False')
    aphrodite_extra_kwargs = {}
    if model == 'ssmits/Qwen2-7B-Instruct-embed-base':
        aphrodite_extra_kwargs['override_pooler_config'] = PoolerConfig(pooling_type='MEAN', normalize=False)
    max_model_len: Optional[int] = 512
    if model in ['sentence-transformers/all-MiniLM-L12-v2', 'sentence-transformers/stsb-roberta-base-v2']:
        max_model_len = None
    example_prompts = [str(s).strip() for s in example_prompts]
    with hf_runner(model, is_sentence_transformer=True) as hf_model:
        hf_outputs = hf_model.encode(example_prompts)
    with aphrodite_runner(model, runner='pooling', max_model_len=max_model_len, **aphrodite_extra_kwargs) as aphrodite_model:
        aphrodite_outputs = aphrodite_model.embed(example_prompts)
    check_embeddings_close(embeddings_0_lst=hf_outputs, embeddings_1_lst=aphrodite_outputs, name_0='hf', name_1='aphrodite', tol=0.01)