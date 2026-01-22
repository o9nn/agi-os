from copy import deepcopy
from typing import TYPE_CHECKING, Optional
import msgspec
from aphrodite.common.sampling_params import RequestOutputKind
from aphrodite.tasks import PoolingTask
if TYPE_CHECKING:
    from aphrodite.common.config import ModelConfig
class PoolingParams(msgspec.Struct, omit_defaults=True, array_like=True):
    dimensions: Optional[int] = None
    normalize: Optional[bool] = None
    activation: Optional[bool] = None
    softmax: Optional[bool] = None
    step_tag_id: Optional[int] = None
    returned_token_ids: Optional[list[int]] = None
    task: Optional[PoolingTask] = None
    'Internal use only.'
    requires_token_ids: bool = False
    'Internal use only.'
    output_kind: RequestOutputKind = RequestOutputKind.FINAL_ONLY
    @property
    def all_parameters(self) -> list[str]:
        return ['dimensions', 'normalize', 'activation', 'softmax', 'step_tag_id', 'returned_token_ids']
    @property
    def valid_parameters(self):
        return {'embed': ['dimensions', 'normalize'], 'classify': ['activation'], 'score': ['activation'], 'encode': ['softmax', 'step_tag_id', 'returned_token_ids']}
    def clone(self) -> 'PoolingParams':
        return deepcopy(self)
    def verify(self, task: PoolingTask, model_config: Optional['ModelConfig']=None) -> None:
        if self.task is None:
            self.task = task
        elif self.task != task:
            msg = f'You cannot overwrite self.task={self.task!r} with task={task!r}!'
            raise ValueError(msg)
        self._merge_default_parameters(model_config)
        self._set_default_parameters(model_config)
        self._verify_valid_parameters()
    def _merge_default_parameters(self, model_config: Optional['ModelConfig']=None) -> None:
        if model_config is None:
            return
        pooler_config = model_config.pooler_config
        if pooler_config is None:
            return
        assert self.task is not None, 'task must be set'
        valid_parameters = self.valid_parameters[self.task]
        for k in valid_parameters:
            if getattr(pooler_config, k, None) is None:
                continue
            if getattr(self, k, None) is None:
                setattr(self, k, getattr(pooler_config, k))
    def _set_default_parameters(self, model_config: Optional['ModelConfig']):
        if self.task == 'embed':
            if self.normalize is None:
                self.normalize = True
            if self.dimensions is not None and model_config is not None:
                if not model_config.is_matryoshka:
                    raise ValueError(f'Model "{model_config.served_model_name}" does not support matryoshka representation, changing output dimensions will lead to poor results.')
                mds = model_config.matryoshka_dimensions
                if mds is not None:
                    if self.dimensions not in mds:
                        raise ValueError(f'Model "{model_config.served_model_name}" only supports {str(mds)} matryoshka dimensions, use other output dimensions will lead to poor results.')
                elif self.dimensions < 1:
                    raise ValueError('Dimensions must be greater than 0')
        elif self.task in ['classify', 'score']:
            if self.activation is None:
                self.activation = True
        elif self.task == 'encode':
            if self.softmax is None:
                self.softmax = True
        else:
            raise ValueError(f'Unknown pooling task: {self.task}')
    def _verify_valid_parameters(self):
        assert self.task is not None, 'task must be set'
        valid_parameters = self.valid_parameters[self.task]
        invalid_parameters = []
        for k in self.all_parameters:
            if k in valid_parameters:
                continue
            if getattr(self, k, None) is not None:
                invalid_parameters.append(k)
        if invalid_parameters:
            raise ValueError(f'Task {self.task} only supports {valid_parameters} parameters, does not support {invalid_parameters} parameters')
    def __repr__(self) -> str:
        return f'PoolingParams(task={self.task}, normalize={self.normalize}, dimensions={self.dimensions}, activation={self.activation}, softmax={self.softmax}, step_tag_id={self.step_tag_id}, returned_token_ids={self.returned_token_ids}, requires_token_ids={self.requires_token_ids})'
    def __post_init__(self) -> None:
        assert self.output_kind == RequestOutputKind.FINAL_ONLY, 'For pooling output_kind has to be FINAL_ONLY'