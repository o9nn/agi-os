import torch
from torch import Generator
from aphrodite.v1.sample.ops.topk_topp_sampler import apply_top_k_top_p
DEVICE = 'cuda'
BATCH_SIZE = 1024
VOCAB_SIZE = 128 * 1024
def test_topk_impl_equivalance():
    with torch.device(DEVICE):
        generator = Generator(device=DEVICE).manual_seed(33)
        logits = torch.rand((BATCH_SIZE, VOCAB_SIZE), generator=generator)
        k = torch.randint(1, 10, (BATCH_SIZE,), generator=generator)
        k.masked_fill_(torch.randint(0, 2, (BATCH_SIZE,), generator=generator, dtype=bool), VOCAB_SIZE)
        result1 = apply_top_k_top_p(logits=logits.clone(), k=k, p=None)
        no_op_top_p = torch.tensor([1.0])
        result2 = apply_top_k_top_p(logits=logits.clone(), k=k, p=no_op_top_p)
        assert torch.allclose(result1, result2)