from abc import ABC, abstractmethod
from collections import UserDict, defaultdict
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from functools import partial
from itertools import accumulate
from typing import TYPE_CHECKING, Any, Literal, Optional, TypedDict, TypeVar, Union, cast, final
import numpy as np
from typing_extensions import NotRequired, TypeAlias
from aphrodite.common.jsontree import JSONTree, json_map_leaves
from aphrodite.utils import LazyLoader, full_groupby, is_list_of
if TYPE_CHECKING:
    import torch
    import torch.types
    from PIL.Image import Image
    from transformers.feature_extraction_utils import BatchFeature
    from .hasher import MultiModalHashDict
else:
    torch = LazyLoader('torch', globals(), 'torch')
_T = TypeVar('_T')
HfImageItem: TypeAlias = Union['Image', np.ndarray, 'torch.Tensor']
'\nA `transformers.image_utils.ImageInput` representing a single image\nitem, which can be passed to a HuggingFace `ImageProcessor`.\n'
HfVideoItem: TypeAlias = Union[list['Image'], np.ndarray, 'torch.Tensor', list[np.ndarray], list['torch.Tensor']]
'\nA `transformers.image_utils.VideoInput` representing a single video\nitem, which can be passed to a HuggingFace `VideoProcessor`.\n'
HfAudioItem: TypeAlias = Union[list[float], np.ndarray, 'torch.Tensor']
'\nRepresents a single audio\nitem, which can be passed to a HuggingFace `AudioProcessor`.\n'
ImageItem: TypeAlias = Union[HfImageItem, 'torch.Tensor']
'\nA `transformers.image_utils.ImageInput` representing a single image\nitem, which can be passed to a HuggingFace `ImageProcessor`.\n\nAlternatively, a 3-D tensor or batch of 2-D tensors,\nwhich are treated as image embeddings;\nthese are directly passed to the model without HF processing.\n'
VideoItem: TypeAlias = Union[HfVideoItem, 'torch.Tensor', tuple[HfVideoItem, dict[str, Any]]]
'\nA `transformers.video_utils.VideoInput` representing a single video item. \nThis can be passed to a HuggingFace `VideoProcessor` \nwith `transformers.video_utils.VideoMetadata`.\n\nAlternatively, a 3-D tensor or batch of 2-D tensors,\nwhich are treated as video embeddings;\nthese are directly passed to the model without HF processing.\n'
AudioItem: TypeAlias = Union[HfAudioItem, tuple[np.ndarray, float], 'torch.Tensor']
"\nRepresents a single audio\nitem, which can be passed to a HuggingFace `AudioProcessor`.\n\nAlternatively, a tuple `(audio, sampling_rate)`, where the sampling rate\nis different from that expected by the model;\nthese are resampled to the model's sampling rate before being processed by HF.\n\nAlternatively, a 3-D tensor or batch of 2-D tensors,\nwhich are treated as audio embeddings;\nthese are directly passed to the model without HF processing.\n"
ModalityData: TypeAlias = Union[_T, list[_T]]
'\nEither a single data item, or a list of data items.\n\nThe number of data items allowed per modality is restricted by\n`--limit-mm-per-prompt`.\n'
@final
class MultiModalDataBuiltins(TypedDict, total=False):
    image: ModalityData[ImageItem]
    'The input image(s).'
    video: ModalityData[VideoItem]
    'The input video(s).'
    audio: ModalityData[AudioItem]
    'The input audio(s).'
MultiModalDataDict: TypeAlias = Mapping[str, ModalityData[Any]]
'\nA dictionary containing an entry for each modality type to input.\n\nThe built-in modalities are defined by\n[`MultiModalDataBuiltins`][aphrodite.multimodal.inputs.MultiModalDataBuiltins].\n'
@dataclass(frozen=True)
class PlaceholderRange:
    offset: int
    'The start index of the placeholder in the prompt.'
    length: int
    'The length of the placeholder.'
    is_embed: Optional['torch.Tensor'] = None
    '\n    A boolean mask of shape `(length,)` indicating which positions\n    between `offset` and `offset + length` to assign embeddings to.\n    '
    def get_num_embeds(self) -> int:
        if self.is_embed is None:
            return self.length
        return int(self.is_embed.sum().item())
    def __eq__(self, other: object) -> bool:
        if not isinstance(other, self.__class__):
            return False
        if not (self.offset, self.length) == (other.offset, other.length):
            return False
        if self.is_embed is None:
            return other.is_embed is None
        if other.is_embed is None:
            return self.is_embed is None
        return nested_tensors_equal(self.is_embed, other.is_embed)
NestedTensors: TypeAlias = Union[list['NestedTensors'], list['torch.Tensor'], 'torch.Tensor', tuple['torch.Tensor', ...]]
'\nUses a list instead of a tensor if the dimensions of each element do not match.\n'
def nested_tensors_equal(a: NestedTensors, b: NestedTensors) -> bool:
    if isinstance(a, torch.Tensor):
        return isinstance(b, torch.Tensor) and torch.equal(a, b)
    elif isinstance(b, torch.Tensor):
        return isinstance(a, torch.Tensor) and torch.equal(b, a)
    if isinstance(a, list):
        return isinstance(b, list) and all((nested_tensors_equal(a_, b_) for a_, b_ in zip(a, b)))
    if isinstance(b, list):
        return isinstance(a, list) and all((nested_tensors_equal(b_, a_) for b_, a_ in zip(b, a)))
    return a == b
BatchedTensorInputs: TypeAlias = Mapping[str, NestedTensors]
'\nA dictionary containing nested tensors which have been batched via\n[`MultiModalKwargs.batch`][aphrodite.multimodal.inputs.MultiModalKwargs.batch].\n'
@dataclass(frozen=True)
class MultiModalFieldElem:
    modality: str
    '\n    The modality of the corresponding multi-modal item.\n    Each multi-modal item can consist of multiple keyword arguments.\n    '
    key: str
    '\n    The key of this field in\n    [`MultiModalKwargs`][aphrodite.multimodal.inputs.MultiModalKwargs],\n    i.e. the name of the keyword argument to be passed to the model.\n    '
    data: NestedTensors
    '\n    The tensor data of this field in\n    [`MultiModalKwargs`][aphrodite.multimodal.inputs.MultiModalKwargs],\n    i.e. the value of the keyword argument to be passed to the model.\n    '
    field: 'BaseMultiModalField'
    '\n    Defines how to combine the tensor data of this field with others\n    in order to batch multi-modal items together for model inference.\n    '
    def __eq__(self, other: object) -> bool:
        if not isinstance(other, self.__class__):
            return False
        return (self.modality, self.key) == (other.modality, other.key) and nested_tensors_equal(self.data, other.data) and (type(self.field) == type(other.field))
@dataclass(frozen=True)
class BaseMultiModalField(ABC):
    def _field_factory(self, *, modality: str, key: str):
        f = partial(MultiModalFieldElem, modality=modality, key=key, field=self)
        def factory(data: NestedTensors) -> MultiModalFieldElem:
            return f(data=data)
        return factory
    @abstractmethod
    def build_elems(self, modality: str, key: str, data: NestedTensors) -> Sequence[MultiModalFieldElem]:
        raise NotImplementedError
    @abstractmethod
    def _reduce_data(self, batch: list[NestedTensors]) -> NestedTensors:
        raise NotImplementedError
    def reduce_data(self, elems: list[MultiModalFieldElem]) -> NestedTensors:
        field_types = [type(item.field) for item in elems]
        if len(set(field_types)) > 1:
            raise ValueError(f'Cannot merge different field_types={field_types!r}')
        return self._reduce_data([item.data for item in elems])
@dataclass(frozen=True)
class MultiModalBatchedField(BaseMultiModalField):
    def build_elems(self, modality: str, key: str, data: NestedTensors) -> Sequence[MultiModalFieldElem]:
        field_factory = self._field_factory(modality=modality, key=key)
        return [field_factory(item) for item in data]
    def _reduce_data(self, batch: list[NestedTensors]) -> NestedTensors:
        if len(batch) > 0 and is_list_of(batch, torch.Tensor, check='all'):
            if len(batch) == 1:
                return batch[0].unsqueeze(0).contiguous()
            first_shape = batch[0].shape
            if all((elem.shape == first_shape for elem in batch)):
                return torch.stack(batch)
        return batch
@dataclass(frozen=True)
class MultiModalFlatField(BaseMultiModalField):
    slices: Union[Sequence[slice], Sequence[Sequence[slice]]]
    dim: int = 0
    def build_elems(self, modality: str, key: str, data: NestedTensors) -> Sequence[MultiModalFieldElem]:
        field_factory = self._field_factory(modality=modality, key=key)
        if not is_list_of(self.slices, slice, check='all'):
            assert isinstance(data, torch.Tensor), 'torch.Tensor is required for multiple slices'
        return [field_factory(data[cast(slice, s)]) for s in self.slices]
    def _reduce_data(self, batch: list[NestedTensors]) -> NestedTensors:
        if len(batch) > 0 and is_list_of(batch, torch.Tensor, check='all'):
            if len(batch) == 1:
                return batch[0].contiguous()
            def _expect_same_shape(tensor: torch.Tensor):
                return tensor.shape[:self.dim] + tensor.shape[self.dim + 1:]
            first_shape = _expect_same_shape(batch[0])
            if all((_expect_same_shape(elem) == first_shape for elem in batch)):
                return torch.concat(batch, dim=self.dim)
        assert self.dim == 0, 'dim == 0 is required for nested list'
        return [e for elem in batch for e in elem]
@dataclass(frozen=True)
class MultiModalSharedField(BaseMultiModalField):
    batch_size: int
    def build_elems(self, modality: str, key: str, data: NestedTensors) -> Sequence[MultiModalFieldElem]:
        field_factory = self._field_factory(modality=modality, key=key)
        return [field_factory(data)] * self.batch_size
    def _reduce_data(self, batch: list[NestedTensors]) -> NestedTensors:
        return batch[0]
class MultiModalFieldConfig:
    @staticmethod
    def batched(modality: str):
        return MultiModalFieldConfig(field=MultiModalBatchedField(), modality=modality)
    @staticmethod
    def flat(modality: str, slices: Union[Sequence[slice], Sequence[Sequence[slice]]], dim: int=0):
        return MultiModalFieldConfig(field=MultiModalFlatField(slices=slices, dim=dim), modality=modality)
    @staticmethod
    def flat_from_sizes(modality: str, size_per_item: 'torch.Tensor', dim: int=0):
        if size_per_item.ndim != 1:
            raise ValueError(f'size_per_item should be a 1-D tensor, but found shape: {size_per_item.shape}')
        slice_idxs = [0, *accumulate(size_per_item)]
        slices = [(slice(None, None, None),) * dim + (slice(slice_idxs[i], slice_idxs[i + 1]),) for i in range(len(size_per_item))]
        return MultiModalFieldConfig.flat(modality, slices, dim=dim)
    @staticmethod
    def shared(modality: str, batch_size: int):
        return MultiModalFieldConfig(field=MultiModalSharedField(batch_size), modality=modality)
    def __init__(self, field: BaseMultiModalField, modality: str) -> None:
        super().__init__()
        self.field = field
        self.modality = modality
    def build_elems(self, key: str, batch: NestedTensors) -> Sequence[MultiModalFieldElem]:
        return self.field.build_elems(self.modality, key, batch)
class MultiModalKwargsItem(UserDict[str, MultiModalFieldElem]):
    @staticmethod
    def from_elems(elems: Sequence[MultiModalFieldElem]):
        return MultiModalKwargsItem({elem.key: elem for elem in elems})
    @property
    def modality(self) -> str:
        modalities = {elem.modality for elem in self.data.values()}
        assert len(modalities) == 1, f'Found different modalities={modalities}'
        return next(iter(modalities))
class MultiModalKwargs(UserDict[str, NestedTensors]):
    @staticmethod
    def from_hf_inputs(hf_inputs: 'BatchFeature', config_by_key: Mapping[str, MultiModalFieldConfig]):
        elems_by_key = dict[str, Sequence[MultiModalFieldElem]]()
        keys_by_modality = defaultdict[str, set[str]](set)
        for key, config in config_by_key.items():
            batch = hf_inputs.get(key)
            if batch is not None:
                elems = config.build_elems(key, batch)
                if len(elems) > 0:
                    elems_by_key[key] = elems
                    keys_by_modality[config.modality].add(key)
        items = list[MultiModalKwargsItem]()
        for modality, keys in keys_by_modality.items():
            elems_in_modality = {k: elems_by_key[k] for k in keys}
            batch_sizes = {k: len(v) for k, v in elems_in_modality.items()}
            if len(set(batch_sizes.values())) > 1:
                raise ValueError(f'Cannot merge different batch sizes for modality={modality!r}! Found: batch_sizes={batch_sizes!r}')
            batch_size = next(iter(batch_sizes.values()))
            for item_idx in range(batch_size):
                elems = [v[item_idx] for v in elems_in_modality.values()]
                items.append(MultiModalKwargsItem.from_elems(elems))
        return MultiModalKwargs.from_items(items)
    @staticmethod
    def from_items(items: Sequence[MultiModalKwargsItem]):
        elems_by_key = defaultdict[str, list[MultiModalFieldElem]](list)
        for item in items:
            for key, elem in item.items():
                elems_by_key[key].append(elem)
        data = {key: elems[0].field.reduce_data(elems) for key, elems in elems_by_key.items() if len(elems) > 0}
        return MultiModalKwargs(data, items=items)
    def __init__(self, data: Mapping[str, NestedTensors], *, items: Optional[Sequence[MultiModalKwargsItem]]=None) -> None:
        super().__init__(data)
        items_by_modality = full_groupby(items or [], key=lambda x: x.modality)
        self._items_by_modality = dict(items_by_modality)
    @property
    def modalities(self):
        return self._items_by_modality.keys()
    @staticmethod
    def _try_stack(nested_tensors: NestedTensors, pin_memory: bool=False) -> NestedTensors:
        if isinstance(nested_tensors, torch.Tensor):
            return nested_tensors
        if isinstance(nested_tensors, np.ndarray):
            return torch.from_numpy(nested_tensors)
        if isinstance(nested_tensors, (int, float)):
            return torch.tensor(nested_tensors)
        stacked = [MultiModalKwargs._try_stack(t, pin_memory) for t in nested_tensors]
        if not is_list_of(stacked, torch.Tensor, check='all'):
            return stacked
        tensors_ = cast(list[torch.Tensor], stacked)
        if len(tensors_) == 1:
            return tensors_[0].unsqueeze(0).contiguous()
        if any((t.shape != tensors_[0].shape for t in tensors_)):
            return tensors_
        outputs = torch.empty(len(tensors_), *tensors_[0].shape, dtype=tensors_[0].dtype, device=tensors_[0].device, pin_memory=pin_memory)
        return torch.stack(tensors_, out=outputs)
    @staticmethod
    def batch(inputs_list: list['MultiModalKwargs'], pin_memory: bool=False) -> BatchedTensorInputs:
        if len(inputs_list) == 0:
            return {}
        item_lists = defaultdict[str, list[NestedTensors]](list)
        for inputs in inputs_list:
            for k, v in inputs.items():
                item_lists[k].append(v)
        return {k: MultiModalKwargs._try_stack(item_list, pin_memory) for k, item_list in item_lists.items()}
    @staticmethod
    def as_kwargs(batched_inputs: BatchedTensorInputs, *, device: torch.types.Device) -> BatchedTensorInputs:
        json_inputs = cast(JSONTree[torch.Tensor], batched_inputs)
        json_mapped = json_map_leaves(lambda x: x.to(device=device, non_blocking=True), json_inputs)
        return cast(BatchedTensorInputs, json_mapped)
    def __delitem__(self, key: str) -> None:
        super().__delitem__(key)
        for items in self._items_by_modality.values():
            for item in items:
                item.pop(key, None)
    def __eq__(self, other: object) -> bool:
        if not isinstance(other, self.__class__):
            return False
        if self._items_by_modality != other._items_by_modality:
            return False
        ks = self.keys()
        return ks == other.keys() and all((nested_tensors_equal(self[k], other[k]) for k in ks))
    def _validate_modality(self, method_name: str, modality: str) -> None:
        if not self._items_by_modality:
            raise RuntimeError(f'`{method_name}` is not supported when MultiModalKwargs is not initialized with `items`')
        if modality not in self._items_by_modality:
            available_modalities = set(self._items_by_modality.keys())
            raise KeyError(f'Modality {modality!r} not found. Available modalities: {available_modalities}')
    def get_item_count(self, modality: str) -> int:
        self._validate_modality('get_item_count', modality)
        return len(self._items_by_modality[modality])
    def get_item(self, modality: str, item_index: int) -> MultiModalKwargsItem:
        self._validate_modality('get_item', modality)
        return self._items_by_modality[modality][item_index]
    def get_items(self, modality: str) -> Sequence[MultiModalKwargsItem]:
        self._validate_modality('get_items', modality)
        return self._items_by_modality[modality]
MultiModalPlaceholderDict: TypeAlias = Mapping[str, Sequence[PlaceholderRange]]
'\nA dictionary containing placeholder ranges for each modality.\n'
class MultiModalInputs(TypedDict):
    type: Literal['multimodal']
    'The type of inputs.'
    prompt: str
    'The processed prompt text.'
    prompt_token_ids: list[int]
    'The processed token IDs which includes placeholder tokens.'
    token_type_ids: NotRequired[list[int]]
    'The token type IDs of the prompt.'
    mm_kwargs: MultiModalKwargs
    'Keyword arguments to be directly passed to the model after batching.'
    mm_hashes: Optional['MultiModalHashDict']
    'The hashes of the multi-modal data.'
    mm_placeholders: 'MultiModalPlaceholderDict'
    '\n    For each modality, information about the placeholder tokens in\n    `prompt_token_ids`.\n    '
    cache_salt: NotRequired[str]
    '\n    Optional cache salt to be used for prefix caching.\n    '
class MultiModalEncDecInputs(MultiModalInputs):
    encoder_prompt: str
    'The processed encoder prompt text.'
    encoder_prompt_token_ids: list[int]
    'The processed token IDs of the encoder prompt.'
    encoder_token_type_ids: NotRequired[list[int]]
    'The token type IDs of the encoder prompt.'