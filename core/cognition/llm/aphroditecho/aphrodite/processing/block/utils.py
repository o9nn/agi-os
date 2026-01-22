from aphrodite.common.sequence import SequenceGroup
from aphrodite.utils import STR_NOT_IMPL_ENC_DEC_PREFIX_CACHE, STR_NOT_IMPL_ENC_DEC_SWA
def check_no_caching_or_swa_for_blockmgr_encdec(block_mgr, seq_group: SequenceGroup) -> None:
    if seq_group.is_encoder_decoder():
        if block_mgr.max_block_sliding_window is not None:
            raise NotImplementedError(STR_NOT_IMPL_ENC_DEC_SWA)
        if block_mgr.enable_caching:
            raise NotImplementedError(STR_NOT_IMPL_ENC_DEC_PREFIX_CACHE)