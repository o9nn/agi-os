from torch import fx as fx
from aphrodite.common.config import AphroditeConfig
from aphrodite.platforms import current_platform
if current_platform.is_cuda_alike():
    from .fusion import FusionPass
    from .fusion_attn import AttnFusionPass
if current_platform.is_cuda():
    from .collective_fusion import AllReduceFusionPass, AsyncTPPass
from .activation_quant_fusion import ActivationQuantFusionPass
from .aphrodite_inductor_pass import AphroditeInductorPass
from .fix_functionalization import FixFunctionalizationPass
from .inductor_pass import CustomGraphPass, InductorPass, get_pass_context
from .noop_elimination import NoOpEliminationPass
from .sequence_parallelism import SequenceParallelismPass
class PostGradPassManager(CustomGraphPass):
    def __init__(self):
        self.passes: list[AphroditeInductorPass] = []
    def __call__(self, graph: fx.Graph):
        shape = get_pass_context().runtime_shape
        for pass_ in self.passes:
            if pass_.is_applicable_for_shape(shape):
                pass_(graph)
        self.fix_functionalization(graph)
    def configure(self, config: AphroditeConfig):
        self.pass_config = config.compilation_config.pass_config
        if self.pass_config.enable_noop:
            self.passes += [NoOpEliminationPass(config)]
        if self.pass_config.enable_sequence_parallelism:
            self.passes += [SequenceParallelismPass(config)]
            if self.pass_config.enable_async_tp:
                self.passes += [AsyncTPPass(config)]
        if self.pass_config.enable_fusion:
            self.passes += [FusionPass.instance(config)]
            self.passes += [ActivationQuantFusionPass(config)]
        if self.pass_config.enable_attn_fusion:
            self.passes += [AttnFusionPass(config)]
        if self.pass_config.enable_fi_allreduce_fusion:
            self.passes += [AllReduceFusionPass(config)]
        self.fix_functionalization = FixFunctionalizationPass(config)
    def add(self, pass_: InductorPass):
        assert isinstance(pass_, InductorPass)
        self.passes.append(pass_)
    def uuid(self):
        state = {'pass_config': self.pass_config.uuid(), 'passes': []}
        for pass_ in self.passes:
            state['passes'].append(pass_.uuid())
        state['passes'].append(self.fix_functionalization.uuid())
        return InductorPass.hash_dict(state)