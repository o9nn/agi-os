from aphrodite.distributed import divide
class MambaStateShapeCalculator:
    @classmethod
    def mamba1_state_shape(cls, tp_world_size: int, intermediate_size: int, state_size: int, conv_kernel: int, use_v1: bool=True) -> tuple[tuple[int, int], tuple[int, int]]:
        conv_state_shape = (divide(intermediate_size, tp_world_size), conv_kernel - 1)
        temporal_state_shape = (divide(intermediate_size, tp_world_size), state_size)
        if use_v1:
            conv_state_shape = (conv_state_shape[1], conv_state_shape[0])
        return (conv_state_shape, temporal_state_shape)
    @classmethod
    def mamba2_state_shape(cls, tp_world_size: int, intermediate_size: int, n_groups: int, num_heads: int, head_dim: int, state_size: int, conv_kernel: int, use_v1: bool=True) -> tuple[tuple[int, int], tuple[int, int, int]]:
        n_groups = n_groups + cls.extra_groups_for_head_shards(n_groups, tp_world_size)
        conv_dim = intermediate_size + 2 * n_groups * state_size
        conv_state_shape = (conv_kernel - 1, divide(conv_dim, tp_world_size))
        if not use_v1:
            conv_state_shape = (conv_state_shape[1], conv_state_shape[0])
        temporal_state_shape = (divide(num_heads, tp_world_size), head_dim, state_size)
        return (conv_state_shape, temporal_state_shape)
    @classmethod
    def extra_groups_for_head_shards(cls, ngroups: int, tp_size: int):
        if ngroups % tp_size == 0:
            return 0
        return tp_size - ngroups