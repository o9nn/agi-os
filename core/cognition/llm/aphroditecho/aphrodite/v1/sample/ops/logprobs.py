import torch
from aphrodite.platforms import current_platform
@torch.compile(dynamic=True, backend=current_platform.simple_compile_backend)
def batched_count_greater_than(x: torch.Tensor, values: torch.Tensor) -> torch.Tensor:
    return (x >= values).sum(-1)