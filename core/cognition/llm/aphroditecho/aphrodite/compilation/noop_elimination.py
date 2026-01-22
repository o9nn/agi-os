from collections.abc import Iterable
from typing import Union
import torch.fx
from torch import SymInt
from loguru import logger
from .fx_utils import is_func
from .aphrodite_inductor_pass import AphroditeInductorPass
class NoOpEliminationPass(AphroditeInductorPass):
    def __call__(self, graph: torch.fx.Graph):
        self.begin()
        self.dump_graph(graph, 'before_noop_elimination')
        count = 0
        for node in graph.nodes:
            if is_func(node, torch.ops.aten.reshape.default):
                input = node.args[0]
                if is_func(input, torch.ops.aten.reshape.default):
                    node.update_arg(0, input.args[0])
                    if len(input.users) == 0:
                        graph.erase_node(input)
                        count += 1
                input, shape = node.args[:2]
                input_shape = input.meta['val'].shape
                if len(shape) != len(input_shape):
                    continue
                if shape.count(-1) > 1:
                    continue
                if self.all_dims_equivalent(shape, input_shape):
                    node.replace_all_uses_with(input)
                    graph.erase_node(node)
                    count += 1
            elif is_func(node, torch.ops.aten.slice.Tensor):
                input, dim_index, start, end = node.args[:4]
                input_shape = input.meta['val'].shape
                i_dim = input_shape[dim_index]
                if start == 0 and self.dims_equivalent(end, i_dim):
                    node.replace_all_uses_with(input)
                    graph.erase_node(node)
                    count += 1
            elif is_func(node, torch.ops.aten.slice_scatter.default):
                base, view, dim_index, start, end = node.args[:5]
                base_shape = base.meta['val'].shape
                view_shape = view.meta['val'].shape
                view_dim = view_shape[dim_index]
                if base_shape == view_shape and start == 0 and self.dims_equivalent(end, view_dim):
                    node.replace_all_uses_with(view)
                    graph.erase_node(node)
                    count += 1
        logger.debug('Removed {} no-op reshapes and slices', count)
        self.dump_graph(graph, 'after_noop_elimination')
        self.end_and_log()
    def all_dims_equivalent(self, dims: Iterable[Union[int, torch.fx.Node]], i_dims: Iterable[Union[int, SymInt]]):
        return all((self.dims_equivalent(s, i_s) for s, i_s in zip(dims, i_dims)))
    def dims_equivalent(self, dim: Union[int, torch.fx.Node], i_dim: Union[int, SymInt]) -> bool:
        if dim == i_dim or dim == -1:
            return True
        return isinstance(dim, torch.fx.Node) and dim.meta['val'] == i_dim