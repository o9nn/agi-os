from copy import deepcopy
from typing import Callable, Union
from torch import fx
from aphrodite.compilation.inductor_pass import InductorPass
from aphrodite.common.config import get_current_aphrodite_config
class TestBackend:
    def __init__(self, *passes: Union[InductorPass, Callable[[fx.Graph], None]]):
        self.custom_passes = list(passes)
        compile_config = get_current_aphrodite_config().compilation_config
        self.inductor_config = compile_config.inductor_compile_config
        self.inductor_config['force_disable_caches'] = True
        self.inductor_config['post_grad_custom_post_pass'] = self.post_pass
    def __call__(self, graph: fx.GraphModule, example_inputs):
        self.graph_pre_compile = deepcopy(graph)
        from torch._inductor.compile_fx import compile_fx
        return compile_fx(graph, example_inputs, config_patches=self.inductor_config)
    def post_pass(self, graph: fx.Graph):
        self.graph_pre_pass = deepcopy(graph)
        for pass_ in self.custom_passes:
            pass_(graph)
        self.graph_post_pass = deepcopy(graph)
        self.final_graph = graph