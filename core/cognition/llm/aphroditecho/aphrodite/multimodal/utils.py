from itertools import groupby
from pathlib import Path
from typing import TYPE_CHECKING, Any, Optional, TypeVar, Union
from urllib.parse import ParseResult, urlparse
import numpy as np
import numpy.typing as npt
import torch
from PIL import Image, UnidentifiedImageError
import aphrodite.common.envs as envs
from aphrodite.connections import HTTPConnection, global_http_connection
from aphrodite.distributed import get_tensor_model_parallel_rank, get_tensor_model_parallel_world_size, tensor_model_parallel_all_gather
from .audio import AudioMediaIO
from .base import MediaIO
from .image import ImageEmbeddingMediaIO, ImageMediaIO
from .inputs import PlaceholderRange
from .video import VideoMediaIO
_M = TypeVar('_M')
if TYPE_CHECKING:
    from .hasher import MultiModalHashDict
    from .inputs import MultiModalKwargs, MultiModalPlaceholderDict
else:
    MultiModalHashDict = Any
    MultiModalKwargs = Any
    MultiModalPlaceholderDict = Any
class MediaConnector:
    def __init__(self, media_io_kwargs: Optional[dict[str, dict[str, Any]]]=None, connection: HTTPConnection=global_http_connection, *, allowed_local_media_path: str='') -> None:
        super().__init__()
        self.media_io_kwargs: dict[str, dict[str, Any]] = media_io_kwargs if media_io_kwargs else {}
        self.connection = connection
        if allowed_local_media_path:
            allowed_local_media_path_ = Path(allowed_local_media_path)
            if not allowed_local_media_path_.exists():
                raise ValueError(f'Invalid `--allowed-local-media-path`: The path {allowed_local_media_path_} does not exist.')
            if not allowed_local_media_path_.is_dir():
                raise ValueError(f'Invalid `--allowed-local-media-path`: The path {allowed_local_media_path_} must be a directory.')
        else:
            allowed_local_media_path_ = None
        self.allowed_local_media_path = allowed_local_media_path_
    def _load_data_url(self, url_spec: ParseResult, media_io: MediaIO[_M]) -> _M:
        data_spec, data = url_spec.path.split(',', 1)
        media_type, data_type = data_spec.split(';', 1)
        if data_type != 'base64':
            msg = 'Only base64 data URLs are supported for now.'
            raise NotImplementedError(msg)
        return media_io.load_base64(media_type, data)
    def _load_file_url(self, url_spec: ParseResult, media_io: MediaIO[_M]) -> _M:
        allowed_local_media_path = self.allowed_local_media_path
        if allowed_local_media_path is None:
            raise RuntimeError('Cannot load local files without `--allowed-local-media-path`.')
        filepath = Path(url_spec.path)
        if allowed_local_media_path not in filepath.resolve().parents:
            raise ValueError(f'The file path {filepath} must be a subpath of `--allowed-local-media-path` {allowed_local_media_path}.')
        return media_io.load_file(filepath)
    def load_from_url(self, url: str, media_io: MediaIO[_M], *, fetch_timeout: Optional[int]=None) -> _M:
        url_spec = urlparse(url)
        if url_spec.scheme.startswith('http'):
            connection = self.connection
            data = connection.get_bytes(url, timeout=fetch_timeout)
            return media_io.load_bytes(data)
        if url_spec.scheme == 'data':
            return self._load_data_url(url_spec, media_io)
        if url_spec.scheme == 'file':
            return self._load_file_url(url_spec, media_io)
        msg = 'The URL must be either a HTTP, data or file URL.'
        raise ValueError(msg)
    async def load_from_url_async(self, url: str, media_io: MediaIO[_M], *, fetch_timeout: Optional[int]=None) -> _M:
        url_spec = urlparse(url)
        if url_spec.scheme.startswith('http'):
            connection = self.connection
            data = await connection.async_get_bytes(url, timeout=fetch_timeout)
            return media_io.load_bytes(data)
        if url_spec.scheme == 'data':
            return self._load_data_url(url_spec, media_io)
        if url_spec.scheme == 'file':
            return self._load_file_url(url_spec, media_io)
        msg = 'The URL must be either a HTTP, data or file URL.'
        raise ValueError(msg)
    def fetch_audio(self, audio_url: str) -> tuple[np.ndarray, Union[int, float]]:
        audio_io = AudioMediaIO(**self.media_io_kwargs.get('audio', {}))
        return self.load_from_url(audio_url, audio_io, fetch_timeout=envs.APHRODITE_AUDIO_FETCH_TIMEOUT)
    async def fetch_audio_async(self, audio_url: str) -> tuple[np.ndarray, Union[int, float]]:
        audio_io = AudioMediaIO(**self.media_io_kwargs.get('audio', {}))
        return await self.load_from_url_async(audio_url, audio_io, fetch_timeout=envs.APHRODITE_AUDIO_FETCH_TIMEOUT)
    def fetch_image(self, image_url: str, *, image_mode: str='RGB') -> Image.Image:
        image_io = ImageMediaIO(image_mode=image_mode, **self.media_io_kwargs.get('image', {}))
        try:
            return self.load_from_url(image_url, image_io, fetch_timeout=envs.APHRODITE_IMAGE_FETCH_TIMEOUT)
        except UnidentifiedImageError as e:
            raise ValueError(str(e)) from e
    async def fetch_image_async(self, image_url: str, *, image_mode: str='RGB') -> Image.Image:
        image_io = ImageMediaIO(image_mode=image_mode, **self.media_io_kwargs.get('image', {}))
        try:
            return await self.load_from_url_async(image_url, image_io, fetch_timeout=envs.APHRODITE_IMAGE_FETCH_TIMEOUT)
        except UnidentifiedImageError as e:
            raise ValueError(str(e)) from e
    def fetch_video(self, video_url: str, *, image_mode: str='RGB') -> tuple[npt.NDArray, dict[str, Any]]:
        image_io = ImageMediaIO(image_mode=image_mode, **self.media_io_kwargs.get('image', {}))
        video_io = VideoMediaIO(image_io, **self.media_io_kwargs.get('video', {}))
        return self.load_from_url(video_url, video_io, fetch_timeout=envs.APHRODITE_VIDEO_FETCH_TIMEOUT)
    async def fetch_video_async(self, video_url: str, *, image_mode: str='RGB') -> tuple[npt.NDArray, dict[str, Any]]:
        image_io = ImageMediaIO(image_mode=image_mode, **self.media_io_kwargs.get('image', {}))
        video_io = VideoMediaIO(image_io, **self.media_io_kwargs.get('video', {}))
        return await self.load_from_url_async(video_url, video_io, fetch_timeout=envs.APHRODITE_VIDEO_FETCH_TIMEOUT)
    def fetch_image_embedding(self, data: str) -> torch.Tensor:
        image_embedding_io = ImageEmbeddingMediaIO()
        return image_embedding_io.load_base64('', data)
def encode_audio_base64(audio: np.ndarray, sampling_rate: float) -> str:
    audio_io = AudioMediaIO()
    return audio_io.encode_base64((audio, sampling_rate))
def encode_image_base64(image: Image.Image, *, image_mode: str='RGB', format: str='JPEG') -> str:
    image_io = ImageMediaIO(image_mode=image_mode)
    return image_io.encode_base64(image, image_format=format)
def encode_video_base64(frames: npt.NDArray) -> str:
    image_io = ImageMediaIO()
    video_io = VideoMediaIO(image_io)
    return video_io.encode_base64(frames)
def merge_and_sort_multimodal_metadata(mm_positions: MultiModalPlaceholderDict, mm_hashes: Optional[MultiModalHashDict]) -> tuple[list[str], list[PlaceholderRange], Optional[list[str]]]:
    modalities = list(mm_positions.keys())
    assert len(modalities) > 0, 'No modalities found in the mm_positions.'
    if len(modalities) == 1:
        modality = modalities[0]
        placeholder_list = list(mm_positions[modality])
        return ([modality] * len(placeholder_list), placeholder_list, None if not mm_hashes else mm_hashes[modality])
    all_items = []
    for modality in modalities:
        placeholder_list = list(mm_positions[modality])
        hash_list: list[Optional[str]] = list(mm_hashes[modality]) if mm_hashes and modality in mm_hashes else [None] * len(placeholder_list)
        for placeholder, hash_value in zip(placeholder_list, hash_list):
            all_items.append((modality, placeholder, hash_value))
    all_items.sort(key=lambda x: x[1].offset)
    sorted_modalities = [item[0] for item in all_items]
    merged_placeholders = [item[1] for item in all_items]
    merged_hashes = [str(item[2]) for item in all_items] if mm_hashes is not None else None
    return (sorted_modalities, merged_placeholders, merged_hashes)
def group_mm_inputs_by_modality(mm_inputs: list[MultiModalKwargs]) -> list[list[MultiModalKwargs]]:
    if not mm_inputs:
        return []
    def modality_group_func(mm_input: MultiModalKwargs) -> Union[str, int]:
        if len(mm_input.modalities) > 1:
            return id(mm_input)
        elif len(mm_input.modalities) == 1:
            return list(mm_input.modalities)[0]
        else:
            return ''
    return [list(group) for _, group in groupby(mm_inputs, key=modality_group_func)]
def run_dp_sharded_vision_model(image_input: torch.Tensor, vision_model: torch.nn.Module) -> torch.Tensor:
    num_chunks = image_input.shape[0]
    mp_world_size = get_tensor_model_parallel_world_size()
    num_chunks_per_rank = (num_chunks + mp_world_size - 1) // mp_world_size
    num_padded_chunks = num_chunks_per_rank * mp_world_size - num_chunks
    pad = (0,) * (2 * (image_input.dim() - 1)) + (0, num_padded_chunks)
    image_input_padded = torch.nn.functional.pad(image_input, pad)
    rank = get_tensor_model_parallel_rank()
    image_input_per_rank = image_input_padded[rank * num_chunks_per_rank:(rank + 1) * num_chunks_per_rank, ...]
    vision_embeddings = vision_model(image_input_per_rank)
    vision_embeddings = tensor_model_parallel_all_gather(vision_embeddings, dim=0)
    vision_embeddings = vision_embeddings[:num_chunks, ...]
    return vision_embeddings
def fetch_audio(audio_url: str, audio_io_kwargs: Optional[dict[str, Any]]=None) -> tuple[np.ndarray, Union[int, float]]:
    media_io_kwargs = None if not audio_io_kwargs else {'audio': audio_io_kwargs}
    media_connector = MediaConnector(media_io_kwargs=media_io_kwargs)
    return media_connector.fetch_audio(audio_url)
def fetch_image(image_url: str, image_io_kwargs: Optional[dict[str, Any]]=None) -> Image.Image:
    media_io_kwargs = None if not image_io_kwargs else {'image': image_io_kwargs}
    media_connector = MediaConnector(media_io_kwargs=media_io_kwargs)
    return media_connector.fetch_image(image_url)
def fetch_video(video_url: str, video_io_kwargs: Optional[dict[str, Any]]=None) -> tuple[npt.NDArray, dict[str, Any]]:
    media_io_kwargs = None if not video_io_kwargs else {'video': video_io_kwargs}
    media_connector = MediaConnector(media_io_kwargs=media_io_kwargs)
    return media_connector.fetch_video(video_url)