from typing import Optional
import numpy as np
from numba import jit
from aphrodite.common.config import AphroditeConfig
class NgramProposer:
    def __init__(self, aphrodite_config: AphroditeConfig):
        self.min_n = aphrodite_config.speculative_config.prompt_lookup_min
        self.max_n = aphrodite_config.speculative_config.prompt_lookup_max
        self.k = aphrodite_config.speculative_config.num_speculative_tokens
        self.max_model_len = aphrodite_config.model_config.max_model_len
        self.propose(np.zeros(1024, dtype=np.int32))
    def propose(self, context_token_ids: np.ndarray) -> Optional[np.ndarray]:
        k = min(self.k, self.max_model_len - context_token_ids.shape[0])
        if k <= 0:
            return None
        for n in range(self.max_n, self.min_n - 1, -1):
            result = _find_subarray_kmp(context_token_ids, n, k)
            if result is not None:
                return result
        return None
    def load_model(self, *args, **kwargs):
        pass
@jit(nopython=True)
def _kmp_lps_array(pattern: np.ndarray) -> np.ndarray:
    lps = np.zeros(len(pattern), dtype=np.int32)
    prev_lps = 0
    i = 1
    while i < len(pattern):
        if pattern[i] == pattern[prev_lps]:
            prev_lps += 1
            lps[i] = prev_lps
            i += 1
        elif prev_lps != 0:
            prev_lps = lps[prev_lps - 1]
        else:
            lps[i] = 0
            i += 1
    return lps
@jit(nopython=True)
def _find_subarray_kmp(context_token_ids: np.ndarray, n: int, k: int) -> Optional[np.ndarray]:
    context_len = context_token_ids.shape[0]
    assert n > 0
    pattern = context_token_ids[-n:]
    lps = _kmp_lps_array(pattern)
    i = 0
    j = 0
    while i < context_len - n:
        if context_token_ids[i] == pattern[j]:
            i += 1
            j += 1
            if j == n:
                return context_token_ids[i:i + k]
        elif j != 0:
            j = lps[j - 1]
        else:
            i += 1
    return None