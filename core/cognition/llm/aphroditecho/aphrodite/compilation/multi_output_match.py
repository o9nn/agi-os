import abc
import operator
from abc import abstractmethod
from collections.abc import Iterable
from torch import fx
from torch._higher_order_ops.auto_functionalize import auto_functionalized
from torch._inductor import pattern_matcher as pm
from torch._ops import OpOverload
from torch.fx import Node
from aphrodite.compilation.fx_utils import find_auto_fn
class MultiOutputMatch(abc.ABC):
    def __init__(self, match: pm.Match):
        self.match = match
    @abstractmethod
    def process(self):
        raise NotImplementedError
    @property
    def nodes(self) -> list[fx.Node]:
        return self.match.nodes
    @property
    def graph(self) -> fx.Graph:
        return self.match.graph
    def find_auto_fn(self, op) -> fx.Node:
        return find_auto_fn(self.nodes, op)
    def inserting_after_match(self):
        for last_node_in_match in reversed(self.graph.nodes):
            if last_node_in_match in self.match.nodes:
                break
        else:
            raise ValueError('No nodes in graph')
        return self.graph.inserting_after(last_node_in_match)
    def insert_getitems(self, tuple_node: fx.Node, indices: Iterable[int]) -> tuple[fx.Node, ...]:
        with self.graph.inserting_after(tuple_node):
            return tuple((self.graph.call_function(operator.getitem, (tuple_node, idx)) for idx in indices))
    def insert_auto_fn(self, op: OpOverload, kwargs) -> Node:
        return self.graph.call_function(auto_functionalized, (op,), kwargs=kwargs)