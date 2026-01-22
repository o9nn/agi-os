from __future__ import annotations
import time
from dataclasses import dataclass
from typing import TYPE_CHECKING
import numpy as np
import pytest
from aphrodite.platforms import current_platform
from aphrodite.common.sampling_params import SamplingParams
from aphrodite.transformers_utils.tokenizer import get_tokenizer
if TYPE_CHECKING:
    from tests.conftest import AphroditeRunner
@dataclass
class TestParams:
    model: str
    num_prompts: int
    prefix_len: int
    decode_len: int
    expected_avg_time: float
    err_tol: float
TEST_PARAMS = [TestParams(model='Qwen/Qwen2.5-1.5B-Instruct', num_prompts=64, prefix_len=500, decode_len=50, expected_avg_time=1.7, err_tol=0.2)]
NUM_WARMUPS = 5
NUM_RUNS = 10
MAX_MODEL_LEN = 1024
MAX_NUM_SEQS = 32
GPU_UTIL = 0.9
@pytest.mark.skipif(not current_platform.is_tpu(), reason='This is a basic performance test for TPU only')
@pytest.mark.parametrize('params', TEST_PARAMS)
def test_perf(aphrodite_runner: type[AphroditeRunner], monkeypatch: pytest.MonkeyPatch, params: TestParams) -> None:
    tokenizer = get_tokenizer(params.model, tokenizer_mode='auto', trust_remote_code=True)
    prompts = []
    for i in range(params.num_prompts):
        prefix_token_ids = np.random.randint(0, tokenizer.vocab_size, size=params.prefix_len).tolist()
        prompt = tokenizer.decode(prefix_token_ids)
        prompts.append(prompt)
    print('-- Running: num_prompts = {} prefix_len = {} decode_len = {}'.format(len(prompts), params.prefix_len, params.decode_len))
    with monkeypatch.context() as m:
        m.setenv('APHRODITE_USE_V1', '1')
        sampling_params = SamplingParams(max_tokens=params.decode_len, temperature=1.0, min_p=0.0)
        with aphrodite_runner(params.model, max_num_batched_tokens=MAX_MODEL_LEN, max_model_len=MAX_MODEL_LEN, max_num_seqs=MAX_NUM_SEQS, gpu_memory_utilization=GPU_UTIL, enforce_eager=False, tensor_parallel_size=1) as aphrodite_model:
            print('  -- Warmup / Compile')
            for i in range(NUM_WARMUPS):
                _ = aphrodite_model.generate(prompts, sampling_params)
            print('  -- Benchmarking... ')
            times = []
            for i in range(NUM_RUNS):
                start_time = time.time()
                _ = aphrodite_model.generate(prompts, sampling_params)
                times.append(time.time() - start_time)
            avg_time = sum(times) / len(times)
            print('  -- avg_time = {}'.format(avg_time))
            print('  -- expected_avg_time = {} with err_tol = {}'.format(params.expected_avg_time, params.err_tol))
            diff = avg_time - params.expected_avg_time
            ok = diff < params.err_tol
            if diff < -params.err_tol:
                print('  !! WARNING !! Performance has improved by {}, it may be necessary to fine-tune the expected_avg_time = {}'.format(-diff, params.expected_avg_time))
            assert ok, ' !! ERROR !! Regression detected'