from itertools import cycle
import pytest
from aphrodite import SamplingParams
from .conftest import get_token_ids_from_llm_generator
@pytest.mark.parametrize('common_llm_kwargs', [{'model': 'facebook/opt-125m', 'enforce_eager': True, 'block_size': 16, 'num_gpu_blocks_override': 5 * (64 + 1)}])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [{}])
@pytest.mark.parametrize('baseline_llm_kwargs', [{}])
@pytest.mark.parametrize('test_llm_kwargs', [{'preemption_mode': 'swap'}, {'preemption_mode': 'recompute'}])
@pytest.mark.parametrize('batch_size', [10])
@pytest.mark.parametrize('seed', [1])
def test_block_manager_with_preemption(baseline_llm_generator, test_llm_generator, batch_size):
    output_len = 1024
    temperature = 0.0
    prompts = ['Hello, my name is', 'The president of the United States is', 'The capital of France is', 'The future of AI is']
    prompts = [prompt for prompt, _ in zip(cycle(prompts), range(batch_size))]
    sampling_params = SamplingParams(max_tokens=output_len, ignore_eos=True, temperature=temperature)
    baseline_token_ids = get_token_ids_from_llm_generator(baseline_llm_generator, prompts, sampling_params)
    test_token_ids = get_token_ids_from_llm_generator(test_llm_generator, prompts, sampling_params)
    for expected_token_ids, actual_token_ids in zip(baseline_token_ids, test_token_ids):
        assert expected_token_ids == actual_token_ids
    assert baseline_token_ids == test_token_ids
@pytest.mark.parametrize('common_llm_kwargs', [{'model': 'facebook/opt-125m', 'max_model_len': 160, 'enforce_eager': True}])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [{'block_size': 16, 'num_gpu_blocks_override': 2 * (8 + 1)}, {'block_size': 8, 'num_gpu_blocks_override': 2 * (16 + 2)}])
@pytest.mark.parametrize('baseline_llm_kwargs', [{'num_lookahead_slots': 0}])
@pytest.mark.parametrize('test_llm_kwargs', [{'num_lookahead_slots': 10, 'preemption_mode': 'swap'}, {'num_lookahead_slots': 10, 'preemption_mode': 'recompute'}])
@pytest.mark.parametrize('batch_size', [4])
@pytest.mark.parametrize('seed', [1])
def test_lookahead_greedy_equality_with_preemption(baseline_llm_generator, test_llm_generator, batch_size):
    output_len = 128
    temperature = 0.0
    prompts = ['Hello, my name is', 'The president of the United States is', 'The capital of France is', 'The future of AI is']
    prompts = [prompt for prompt, _ in zip(cycle(prompts), range(batch_size))]
    sampling_params = SamplingParams(max_tokens=output_len, ignore_eos=True, temperature=temperature)
    print('Getting token ids without lookahead scheduling')
    baseline_token_ids = get_token_ids_from_llm_generator(baseline_llm_generator, prompts, sampling_params)
    print('Getting token ids with lookahead scheduling')
    test_token_ids = get_token_ids_from_llm_generator(test_llm_generator, prompts, sampling_params)
    for expected_token_ids, actual_token_ids in zip(baseline_token_ids, test_token_ids):
        assert expected_token_ids == actual_token_ids
    assert baseline_token_ids == test_token_ids
@pytest.mark.parametrize('common_llm_kwargs', [{'model': 'facebook/opt-125m', 'enforce_eager': True, 'enable_chunked_prefill': True}])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [{'block_size': 16, 'max_num_batched_tokens': 2, 'max_num_seqs': 2}, {'block_size': 16, 'max_num_batched_tokens': 3, 'max_num_seqs': 2}, {'block_size': 16, 'max_num_batched_tokens': 256, 'max_num_seqs': 10}])
@pytest.mark.parametrize('baseline_llm_kwargs', [{}])
@pytest.mark.parametrize('test_llm_kwargs', [{'num_lookahead_slots': 0}, {'num_lookahead_slots': 5}])
@pytest.mark.parametrize('batch_size', [4])
@pytest.mark.parametrize('seed', [1])
def test_chunked_prefill_block_manager(baseline_llm_generator, test_llm_generator, batch_size):
    output_len = 32
    temperature = 0.0
    prompts = ['Hello, my name is', 'The president of the United States is', '1 + ' * 50 + ' 1 = ', 'The capital of France is', 'The future of AI is']
    prompts = [prompt for prompt, _ in zip(cycle(prompts), range(batch_size))]
    sampling_params = SamplingParams(max_tokens=output_len, ignore_eos=True, temperature=temperature)
    print('Getting token ids with BlockManager')
    baseline_token_ids = get_token_ids_from_llm_generator(baseline_llm_generator, prompts, sampling_params)
    print('Getting token ids with BlockManager, with lookahead slots.')
    test_token_ids = get_token_ids_from_llm_generator(test_llm_generator, prompts, sampling_params)
    for expected_token_ids, actual_token_ids in zip(baseline_token_ids, test_token_ids):
        assert expected_token_ids == actual_token_ids
    assert baseline_token_ids == test_token_ids
@pytest.mark.parametrize('common_llm_kwargs', [{'model': 'facebook/opt-125m', 'enforce_eager': True, 'block_size': 16, 'num_gpu_blocks_override': 5 * (64 + 1), 'enable_prefix_caching': True}])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [{}])
@pytest.mark.parametrize('baseline_llm_kwargs', [{}])
@pytest.mark.parametrize('test_llm_kwargs', [{'preemption_mode': 'swap'}, {'preemption_mode': 'recompute'}])
@pytest.mark.parametrize('batch_size', [10])
@pytest.mark.parametrize('seed', [1])
def test_block_manager_prefix_caching_enabled_with_preemption(baseline_llm_generator, test_llm_generator, batch_size):
    output_len = 1024
    temperature = 0.0
    prompts = ['Hello, my name is', 'The president of the United States is', 'The capital of France is', 'The future of AI is']
    prompts = [prompt for prompt, _ in zip(cycle(prompts), range(batch_size))]
    sampling_params = SamplingParams(max_tokens=output_len, ignore_eos=True, temperature=temperature)
    print('Getting token ids from block manager')
    baseline_token_ids = get_token_ids_from_llm_generator(baseline_llm_generator, prompts, sampling_params)
    print('Getting token ids from block manager, with preemption')
    test_token_ids = get_token_ids_from_llm_generator(test_llm_generator, prompts, sampling_params)
    for expected_token_ids, actual_token_ids in zip(baseline_token_ids, test_token_ids):
        assert expected_token_ids == actual_token_ids
    assert baseline_token_ids == test_token_ids
@pytest.mark.parametrize('common_llm_kwargs', [{'model': 'facebook/opt-125m', 'enforce_eager': True, 'block_size': 16, 'num_gpu_blocks_override': 5 * (64 + 1)}])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [{}])
@pytest.mark.parametrize('baseline_llm_kwargs', [{'enable_prefix_caching': False}])
@pytest.mark.parametrize('test_llm_kwargs', [{'enable_prefix_caching': True, 'preemption_mode': 'swap'}, {'enable_prefix_caching': True, 'preemption_mode': 'recompute'}])
@pytest.mark.parametrize('batch_size', [10])
@pytest.mark.parametrize('seed', [1])
def test_auto_prefix_caching_with_preemption(baseline_llm_generator, test_llm_generator, batch_size):
    output_len = 1024
    temperature = 0.0
    prompts = ['Hello, my name is', 'The president of the United States is', 'The capital of France is', 'The future of AI is']
    prompts = [prompt for prompt, _ in zip(cycle(prompts), range(batch_size))]
    sampling_params = SamplingParams(max_tokens=output_len, ignore_eos=True, temperature=temperature)
    print('Getting token ids with APC disabled')
    baseline_token_ids = get_token_ids_from_llm_generator(baseline_llm_generator, prompts, sampling_params)
    print('Getting token ids with APC enabled')
    test_token_ids = get_token_ids_from_llm_generator(test_llm_generator, prompts, sampling_params)
    for expected_token_ids, actual_token_ids in zip(baseline_token_ids, test_token_ids):
        assert expected_token_ids == actual_token_ids
    assert baseline_token_ids == test_token_ids
@pytest.mark.parametrize('common_llm_kwargs', [{'model': 'facebook/opt-125m', 'enforce_eager': True, 'max_model_len': 48, 'block_size': 16, 'num_gpu_blocks_override': 3}])
@pytest.mark.parametrize('per_test_common_llm_kwargs', [{}])
@pytest.mark.parametrize('baseline_llm_kwargs', [{'enable_prefix_caching': False}])
@pytest.mark.parametrize('test_llm_kwargs', [{'enable_prefix_caching': True}])
@pytest.mark.parametrize('seed', [1])
def test_auto_prefix_caching_after_evition_start(baseline_llm_generator, test_llm_generator):
    output_len = 10
    temperature = 0.0
    prompts = ['You are a helpful assistant. Please answer truthfully and write out your thinking step by step to be sure you get the right answer. If you make a mistake, attempt to correct it. who are you?', 'You are a helpful assistant. Please answer truthfully and write out your thinking step by step to be sure you get the right answer. You are helpful and harmless and you follow ethical guidelines. who are you?']
    sampling_params = SamplingParams(max_tokens=output_len, ignore_eos=True, temperature=temperature)
    print('Getting token ids with APC disabled')
    baseline_token_ids = get_token_ids_from_llm_generator(baseline_llm_generator, prompts, sampling_params)
    print('Getting token ids with APC enabled')
    test_token_ids = get_token_ids_from_llm_generator(test_llm_generator, prompts, sampling_params)
    for expected_token_ids, actual_token_ids in zip(baseline_token_ids, test_token_ids):
        assert expected_token_ids == actual_token_ids
    assert baseline_token_ids == test_token_ids