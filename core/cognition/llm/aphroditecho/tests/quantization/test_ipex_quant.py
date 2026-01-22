import pytest
from aphrodite.platforms import current_platform
MODELS = ['AMead10/Llama-3.2-1B-Instruct-AWQ', 'shuyuej/Llama-3.2-1B-Instruct-GPTQ']
DTYPE = ['bfloat16']
@pytest.mark.skipif(not current_platform.is_cpu() and (not current_platform.is_xpu()), reason='only supports Intel CPU/XPU backend.')
@pytest.mark.parametrize('model', MODELS)
@pytest.mark.parametrize('dtype', DTYPE)
def test_ipex_quant(aphrodite_runner, model, dtype):
    with aphrodite_runner(model, dtype=dtype) as llm:
        output = llm.generate_greedy(['The capital of France is'], max_tokens=32)
    assert output
    print(output)