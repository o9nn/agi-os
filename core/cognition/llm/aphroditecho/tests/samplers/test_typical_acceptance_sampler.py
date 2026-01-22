import pytest
import torch
from aphrodite.modeling.layers.typical_acceptance_sampler import TypicalAcceptanceSampler
from aphrodite.modeling.utils import set_random_seed
CUDA_DEVICES = [f'cuda:{i}' for i in range(1)]
@pytest.fixture(scope='function', autouse=True)
def use_v0_only(monkeypatch):
    monkeypatch.setenv('APHRODITE_USE_V1', '0')
def get_zero_temperature_prob_dist(batch_size, k, vocab_size):
    target_probs = torch.rand(batch_size, k, vocab_size, dtype=torch.float32)
    probs = torch.rand(batch_size, k, vocab_size)
    _, zero_temperature_token_ids = torch.max(probs, dim=-1)
    target_probs = torch.zeros_like(probs).scatter_(-1, zero_temperature_token_ids.unsqueeze(-1), 1.0)
    return (target_probs, zero_temperature_token_ids)
def get_draft_token_ids(batch_size: int, k: int, vocab_size: int, token_ids_to_exclude: torch.Tensor):
    draft_token_ids = torch.empty(batch_size, k, dtype=torch.long)
    for i in range(batch_size):
        for j in range(k):
            while True:
                token_id = torch.randint(0, vocab_size, (1,)).item()
                if token_id != token_ids_to_exclude[i, j]:
                    draft_token_ids[i, j] = token_id
                    break
    return draft_token_ids
def get_acceptance_sampler(posterior_threshold: float=0.03, posterior_alpha: float=0.9, strict_mode: bool=False) -> TypicalAcceptanceSampler:
    return TypicalAcceptanceSampler(posterior_threshold, posterior_alpha, strict_mode)
@pytest.mark.parametrize('k', list(range(1, 6)))
@pytest.mark.parametrize('vocab_size', [30000, 50000])
@pytest.mark.parametrize('batch_size', list(range(1, 32)))
@pytest.mark.parametrize('device', CUDA_DEVICES)
@torch.inference_mode()
def test_no_crash_with_varying_dims(k: int, vocab_size: int, batch_size: int, device: str):
    torch.set_default_device(device)
    typical_acceptance_sampler = get_acceptance_sampler()
    typical_acceptance_sampler.init_gpu_tensors(device=device)
    target_with_bonus_probs = torch.rand(batch_size, k + 1, vocab_size, dtype=torch.float32)
    bonus_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, 1), dtype=torch.int64)
    draft_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, k), dtype=torch.int64)
    typical_acceptance_sampler(target_with_bonus_probs, bonus_token_ids, draft_probs=None, draft_token_ids=draft_token_ids)
@pytest.mark.parametrize('above_or_below_vocab_range', ['above', 'below'])
@pytest.mark.parametrize('which_token_ids', ['bonus_token_ids', 'draft_token_ids'])
@pytest.mark.parametrize('device', CUDA_DEVICES)
@torch.inference_mode()
def test_raises_when_vocab_oob(above_or_below_vocab_range: str, which_token_ids: str, device: str):
    k = 3
    batch_size = 5
    vocab_size = 30000
    torch.set_default_device(device)
    typical_acceptance_sampler = get_acceptance_sampler(strict_mode=True)
    typical_acceptance_sampler.init_gpu_tensors(device=device)
    target_with_bonus_probs = torch.rand(batch_size, k + 1, vocab_size, dtype=torch.float32)
    bonus_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, 1), dtype=torch.int64)
    draft_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, k), dtype=torch.int64)
    oob_token_ids = None
    if which_token_ids == 'bonus_token_ids':
        oob_token_ids = bonus_token_ids
    elif which_token_ids == 'draft_token_ids':
        oob_token_ids = draft_token_ids
    else:
        raise AssertionError()
    if above_or_below_vocab_range == 'above':
        rogue_token_id = vocab_size + 1
    elif above_or_below_vocab_range == 'below':
        rogue_token_id = -1
    else:
        raise AssertionError()
    oob_token_ids[0][0] = rogue_token_id
    with pytest.raises(AssertionError):
        typical_acceptance_sampler(target_with_bonus_probs, bonus_token_ids, draft_probs=None, draft_token_ids=draft_token_ids)
@pytest.mark.parametrize('seed', list(range(10)))
@pytest.mark.parametrize('device', CUDA_DEVICES)
@torch.inference_mode()
def test_uniform_target_distribution_accepts_all_tokens(seed: int, device: str):
    set_random_seed(seed)
    k = 3
    batch_size = 5
    vocab_size = 30000
    torch.set_default_device(device)
    typical_acceptance_sampler = get_acceptance_sampler(strict_mode=True)
    typical_acceptance_sampler.init_gpu_tensors(device=device)
    target_with_bonus_probs = torch.rand(batch_size, k + 1, vocab_size, dtype=torch.float32)
    draft_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, k), dtype=torch.int64)
    bonus_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, 1), dtype=torch.int64)
    output_token_ids = typical_acceptance_sampler(target_with_bonus_probs, bonus_token_ids, draft_probs=None, draft_token_ids=draft_token_ids)
    assert output_token_ids.shape[0] == batch_size
    assert output_token_ids.shape[1] == k + 1
    assert torch.all(output_token_ids[:, -1] == bonus_token_ids.squeeze())
    assert torch.all(output_token_ids[:, :k] == draft_token_ids)
@pytest.mark.parametrize('seed', list(range(10)))
@pytest.mark.parametrize('device', CUDA_DEVICES)
@torch.inference_mode()
def test_temperature_zero_target_distribution(seed: int, device: str):
    set_random_seed(seed)
    k = 3
    batch_size = 5
    vocab_size = 30000
    torch.set_default_device(device)
    typical_acceptance_sampler = get_acceptance_sampler(strict_mode=True)
    typical_acceptance_sampler.init_gpu_tensors(device=device)
    target_with_bonus_probs, zero_temperature_token_ids = get_zero_temperature_prob_dist(batch_size, k + 1, vocab_size)
    zero_temperature_token_ids = zero_temperature_token_ids[:, :-1]
    draft_token_ids = get_draft_token_ids(batch_size, k, vocab_size, zero_temperature_token_ids)
    bonus_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, 1), dtype=torch.int64)
    output_token_ids = typical_acceptance_sampler(target_with_bonus_probs, bonus_token_ids, draft_probs=None, draft_token_ids=draft_token_ids)
    assert output_token_ids.shape[0] == batch_size
    assert output_token_ids.shape[1] == k + 1
    assert torch.all(output_token_ids[:, -1] == -1)
    assert torch.all(output_token_ids[:, 0] == zero_temperature_token_ids[:, 0])
@pytest.mark.parametrize('seed', list(range(10)))
@pytest.mark.parametrize('device', CUDA_DEVICES)
@torch.inference_mode()
def test_mixed_target_distribution(seed: int, device: str):
    set_random_seed(seed)
    k = 3
    batch_size = 4
    vocab_size = 30000
    torch.set_default_device(device)
    typical_acceptance_sampler = get_acceptance_sampler(strict_mode=True)
    typical_acceptance_sampler.init_gpu_tensors(device=device)
    target_with_bonus_probs, zero_temperature_token_ids = get_zero_temperature_prob_dist(batch_size, k + 1, vocab_size)
    zero_temperature_token_ids = zero_temperature_token_ids[:, :-1]
    target_probs = target_with_bonus_probs[:, :-1]
    draft_token_ids = get_draft_token_ids(batch_size, k, vocab_size, zero_temperature_token_ids)
    uniform_probs = torch.rand(2, k, vocab_size, dtype=torch.float32)
    target_probs[[1, 3]] = uniform_probs
    bonus_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, 1), dtype=torch.int64)
    output_token_ids = typical_acceptance_sampler(target_with_bonus_probs, bonus_token_ids, draft_probs=None, draft_token_ids=draft_token_ids)
    assert output_token_ids.shape[0] == batch_size
    assert output_token_ids.shape[1] == k + 1
    assert torch.all(output_token_ids[[0, 2], 1:] == -1)
    assert torch.all(output_token_ids[[0, 2], 0] == zero_temperature_token_ids[[0, 2], 0])
    assert torch.all(output_token_ids[[1, 3], :-1] == draft_token_ids[[1, 3], :])
    assert torch.all(output_token_ids[[1, 3], -1] != -1)
@pytest.mark.parametrize('seed', list(range(10)))
@pytest.mark.parametrize('device', CUDA_DEVICES)
@torch.inference_mode()
def test_accept_tokens_partially(seed: int, device: str):
    set_random_seed(seed)
    k = 5
    batch_size = 1
    vocab_size = 30000
    torch.set_default_device(device)
    typical_acceptance_sampler = get_acceptance_sampler(strict_mode=True)
    typical_acceptance_sampler.init_gpu_tensors(device=device)
    target_with_bonus_probs, zero_temperature_token_ids = get_zero_temperature_prob_dist(batch_size, k + 1, vocab_size)
    zero_temperature_token_ids = zero_temperature_token_ids[:, :-1]
    draft_token_ids = zero_temperature_token_ids
    bonus_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, 1), dtype=torch.int64)
    output_token_ids = typical_acceptance_sampler(target_with_bonus_probs, bonus_token_ids, draft_probs=None, draft_token_ids=draft_token_ids)
    assert output_token_ids.shape[0] == batch_size
    assert output_token_ids.shape[1] == k + 1
    assert torch.all(output_token_ids[:, 0:-1] == draft_token_ids)
    assert torch.all(output_token_ids[:, -1] == bonus_token_ids)
    draft_token_ids_to_replace = get_draft_token_ids(batch_size, k, vocab_size, zero_temperature_token_ids)
    draft_token_ids = torch.cat((draft_token_ids[:, :2], draft_token_ids_to_replace[:, -3:]), dim=1)
    output_token_ids = typical_acceptance_sampler(target_with_bonus_probs, bonus_token_ids, draft_probs=None, draft_token_ids=draft_token_ids)
    assert output_token_ids.shape[0] == batch_size
    assert output_token_ids.shape[1] == k + 1
    assert torch.all(output_token_ids[:, :2] == draft_token_ids[:, :2])
    assert torch.all(output_token_ids[:, 2] == target_with_bonus_probs.argmax(-1)[:, 2])
    assert torch.all(output_token_ids[:, -3:] == -1)
@pytest.mark.parametrize('seed', list(range(1)))
@pytest.mark.parametrize('device', CUDA_DEVICES)
@torch.inference_mode()
def test_accept_tokens_set_non_default_posteriors(seed: int, device: str):
    set_random_seed(seed)
    k = 5
    batch_size = 1
    vocab_size = 30000
    torch.set_default_device(device)
    typical_acceptance_sampler = get_acceptance_sampler(strict_mode=True)
    typical_acceptance_sampler.init_gpu_tensors(device=device)
    target_probs, zero_temperature_token_ids = get_zero_temperature_prob_dist(batch_size, k + 1, vocab_size)
    zero_temperature_token_ids = zero_temperature_token_ids[:, :-1]
    target_probs[target_probs == 0] = 1e-05
    draft_token_ids = get_draft_token_ids(batch_size, k, vocab_size, zero_temperature_token_ids)
    bonus_token_ids = torch.randint(low=0, high=vocab_size, size=(batch_size, 1), dtype=torch.int64)
    output_token_ids = typical_acceptance_sampler(target_probs, bonus_token_ids, draft_probs=None, draft_token_ids=draft_token_ids)
    assert output_token_ids.shape[0] == batch_size
    assert output_token_ids.shape[1] == k + 1
    assert torch.all(output_token_ids[:, 1:-1] == -1)
    typical_acceptance_sampler = TypicalAcceptanceSampler(strict_mode=True, posterior_threshold=0.0, posterior_alpha=0.0)
    typical_acceptance_sampler.init_gpu_tensors(device=device)
    output_token_ids = typical_acceptance_sampler(target_probs, bonus_token_ids, draft_probs=None, draft_token_ids=draft_token_ids)
    assert output_token_ids.shape[0] == batch_size
    assert output_token_ids.shape[1] == k + 1
    assert torch.all(output_token_ids[:, 0:-1] == draft_token_ids)
    assert torch.all(output_token_ids[:, -1] == bonus_token_ids)
@pytest.mark.parametrize('seed', list(range(10)))
@pytest.mark.parametrize('device', CUDA_DEVICES)
@torch.inference_mode()
def test_get_recovered_token_ids(seed: int, device: str):
    set_random_seed(seed)
    k = 10
    batch_size = 5
    vocab_size = 30000
    torch.set_default_device(device)
    typical_acceptance_sampler = get_acceptance_sampler(strict_mode=True)
    typical_acceptance_sampler.init_gpu_tensors(device=device)
    target_probs = torch.rand(batch_size, k, vocab_size, dtype=torch.float32)
    expected_replacement_tokens = torch.argmax(target_probs, dim=-1)
    actual_replacement_tokens = typical_acceptance_sampler._get_recovered_token_ids(target_probs)
    assert torch.all(expected_replacement_tokens == actual_replacement_tokens)