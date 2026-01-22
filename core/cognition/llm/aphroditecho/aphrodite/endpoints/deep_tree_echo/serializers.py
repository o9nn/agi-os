import json
import hashlib
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Union, Callable
import msgspec
from msgspec import msgpack
try:
    import orjson
    ORJSON_AVAILABLE = True
except ImportError:
    ORJSON_AVAILABLE = False
try:
    import numpy as np
    NUMPY_AVAILABLE = True
except ImportError:
    NUMPY_AVAILABLE = False
class SerializationFormat:
    JSON = 'json'
    JSON_OPTIMIZED = 'json_optimized'
    BINARY = 'binary'
    MSGPACK = 'msgpack'
    DETERMINISTIC = 'deterministic'
@dataclass
class SerializationConfig:
    format: str = SerializationFormat.JSON_OPTIMIZED
    include_metadata: bool = True
    include_engine_integration: bool = False
    compress_arrays: bool = True
    max_array_inline_size: int = 256
    precision: int = 6
    sort_keys: bool = True
    validate_output: bool = False
    streaming_threshold: int = 1024 * 1024
@dataclass
class SerializationMetadata:
    format_used: str
    serialization_time_ms: float
    compressed_size: int
    original_size: int
    compression_ratio: float
    checksum: Optional[str] = None
    version: str = '1.0'
    timestamp: float = field(default_factory=time.time)
class BaseSerializer(ABC):
    def __init__(self, config: SerializationConfig):
        self.config = config
        self.metadata: Optional[SerializationMetadata] = None
    @abstractmethod
    def serialize(self, dtesn_result: Any) -> Union[bytes, str]:
        pass
    @abstractmethod
    def deserialize(self, data: Union[bytes, str]) -> Any:
        pass
    def _create_metadata(self, format_name: str, data: Union[bytes, str], original_size: int, serialization_time: float) -> SerializationMetadata:
        if isinstance(data, str):
            compressed_size = len(data.encode('utf-8'))
        else:
            compressed_size = len(data)
        compression_ratio = compressed_size / original_size if original_size > 0 else 1.0
        checksum = None
        if self.config.validate_output:
            if isinstance(data, str):
                checksum = hashlib.sha256(data.encode('utf-8')).hexdigest()[:16]
            else:
                checksum = hashlib.sha256(data).hexdigest()[:16]
        return SerializationMetadata(format_used=format_name, serialization_time_ms=serialization_time * 1000, compressed_size=compressed_size, original_size=original_size, compression_ratio=compression_ratio, checksum=checksum)
    def _sanitize_for_serialization(self, data: Any) -> Any:
        if isinstance(data, dict):
            result = {}
            for key, value in data.items():
                if key.startswith('_'):
                    continue
                result[key] = self._sanitize_for_serialization(value)
            return result
        elif isinstance(data, list):
            return [self._sanitize_for_serialization(item) for item in data]
        elif NUMPY_AVAILABLE and isinstance(data, np.ndarray):
            if data.size > self.config.max_array_inline_size:
                return {'_type': 'ndarray', '_shape': data.shape, '_dtype': str(data.dtype), '_size': data.size, '_data_ref': f"array_{hash(str(data.data.hex() if hasattr(data, 'data') else str(data)))}"}
            return data.tolist()
        elif hasattr(data, '__dict__'):
            try:
                return self._sanitize_for_serialization(data.__dict__)
            except:
                return str(data)
        elif callable(data):
            return f"<callable: {(data.__name__ if hasattr(data, '__name__') else 'unknown')}>"
        else:
            try:
                json.dumps(data)
                return data
            except (TypeError, ValueError):
                return str(data)
class OptimizedJSONSerializer(BaseSerializer):
    def serialize(self, dtesn_result: Any) -> str:
        start_time = time.time()
        data = self._optimize_dtesn_data(dtesn_result)
        if ORJSON_AVAILABLE:
            serialized = orjson.dumps(data, option=orjson.OPT_SORT_KEYS if self.config.sort_keys else 0).decode('utf-8')
        else:
            serialized = json.dumps(data, sort_keys=self.config.sort_keys, separators=(',', ':'), ensure_ascii=False)
        serialization_time = time.time() - start_time
        original_size = len(str(dtesn_result))
        self.metadata = self._create_metadata('json_optimized', serialized, original_size, serialization_time)
        return serialized
    def deserialize(self, data: str) -> Dict[str, Any]:
        if ORJSON_AVAILABLE:
            return orjson.loads(data)
        return json.loads(data)
    def _optimize_dtesn_data(self, dtesn_result: Any) -> Dict[str, Any]:
        if hasattr(dtesn_result, 'to_dict'):
            base_data = dtesn_result.to_dict()
        elif hasattr(dtesn_result, '__dict__'):
            base_data = dtesn_result.__dict__.copy()
        else:
            base_data = {'result': dtesn_result}
        optimized = {}
        core_fields = ['input', 'output', 'processing_time_ms']
        for field in core_fields:
            if field in base_data:
                optimized[field] = self._sanitize_for_serialization(base_data[field])
        if self.config.include_metadata:
            metadata_fields = ['membrane_layers', 'esn_state', 'bseries_computation']
            for field in metadata_fields:
                if field in base_data:
                    value = base_data[field]
                    if isinstance(value, dict) and len(str(value)) > 1000:
                        optimized[field] = self._summarize_large_structure(value)
                    else:
                        optimized[field] = self._sanitize_for_serialization(value)
        if self.config.include_engine_integration and 'engine_integration' in base_data:
            engine_data = base_data['engine_integration']
            if isinstance(engine_data, dict):
                optimized['engine_integration'] = {'engine_available': engine_data.get('engine_available', False), 'backend_active': engine_data.get('backend_integration', {}).get('pipeline_configured', False)}
        if self.config.include_metadata:
            optimized['_serialization'] = {'format': 'optimized_json', 'optimization_applied': True, 'fields_included': list(optimized.keys()), 'timestamp': time.time()}
        return optimized
    def _summarize_large_structure(self, data: Dict[str, Any]) -> Dict[str, Any]:
        summary = {'_summary': True, '_original_size': len(str(data)), '_keys': list(data.keys()) if isinstance(data, dict) else []}
        if isinstance(data, dict):
            sample_keys = list(data.keys())[:3]
            for key in sample_keys:
                value = data[key]
                if isinstance(value, (str, int, float, bool)) and len(str(value)) < 100:
                    summary[key] = value
                else:
                    summary[key] = f'<{type(value).__name__}: {len(str(value))} chars>'
        return summary
class BinarySerializer(BaseSerializer):
    def __init__(self, config: SerializationConfig):
        super().__init__(config)
        self.encoder = msgpack.Encoder()
        self.decoder = msgpack.Decoder()
    def serialize(self, dtesn_result: Any) -> bytes:
        start_time = time.time()
        data = self._prepare_for_binary(dtesn_result)
        serialized = self.encoder.encode(data)
        serialization_time = time.time() - start_time
        original_size = len(str(dtesn_result))
        self.metadata = self._create_metadata('binary_msgpack', serialized, original_size, serialization_time)
        return serialized
    def deserialize(self, data: bytes) -> Any:
        return self.decoder.decode(data)
    def _prepare_for_binary(self, dtesn_result: Any) -> Dict[str, Any]:
        if hasattr(dtesn_result, 'to_dict'):
            data = dtesn_result.to_dict()
        elif hasattr(dtesn_result, '__dict__'):
            data = dtesn_result.__dict__.copy()
        else:
            data = {'result': dtesn_result}
        return self._optimize_for_binary(data)
    def _optimize_for_binary(self, data: Any) -> Any:
        if isinstance(data, dict):
            return {k: self._optimize_for_binary(v) for k, v in data.items() if not k.startswith('_')}
        elif isinstance(data, list):
            return [self._optimize_for_binary(item) for item in data]
        elif NUMPY_AVAILABLE and isinstance(data, np.ndarray):
            return data
        else:
            return data
class DeterministicSerializer(BaseSerializer):
    def serialize(self, dtesn_result: Any) -> str:
        start_time = time.time()
        data = self._create_deterministic_data(dtesn_result)
        serialized = json.dumps(data, sort_keys=True, separators=(',', ':'), ensure_ascii=True)
        serialization_time = time.time() - start_time
        original_size = len(str(dtesn_result))
        self.metadata = self._create_metadata('deterministic', serialized, original_size, serialization_time)
        return serialized
    def deserialize(self, data: str) -> Dict[str, Any]:
        return json.loads(data)
    def _create_deterministic_data(self, dtesn_result: Any) -> Dict[str, Any]:
        if hasattr(dtesn_result, 'to_dict'):
            base_data = dtesn_result.to_dict()
        elif hasattr(dtesn_result, '__dict__'):
            base_data = dtesn_result.__dict__.copy()
        else:
            base_data = {'result': dtesn_result}
        deterministic_data = {}
        skip_fields = {'processing_time_ms', 'timestamp', '_timestamp', 'integration_timestamp', 'last_sync', '_created_at'}
        for key, value in base_data.items():
            if key in skip_fields:
                continue
            deterministic_data[key] = self._make_deterministic(value)
        deterministic_data['_deterministic'] = {'version': '1.0', 'fields_processed': sorted(deterministic_data.keys()), 'checksum': self._calculate_content_hash(deterministic_data)}
        return deterministic_data
    def _make_deterministic(self, value: Any) -> Any:
        if isinstance(value, dict):
            result = {}
            for k, v in value.items():
                if k in {'timestamp', '_timestamp', 'processing_time', 'last_sync'}:
                    continue
                result[k] = self._make_deterministic(v)
            return result
        elif isinstance(value, list):
            return [self._make_deterministic(item) for item in value]
        elif isinstance(value, float):
            return round(value, self.config.precision)
        elif NUMPY_AVAILABLE and isinstance(value, np.ndarray):
            return value.round(self.config.precision).tolist()
        else:
            return value
    def _calculate_content_hash(self, data: Dict[str, Any]) -> str:
        content = json.dumps(data, sort_keys=True, separators=(',', ':'))
        return hashlib.sha256(content.encode('utf-8')).hexdigest()[:16]
class SerializerFactory:
    _serializers: Dict[str, type] = {SerializationFormat.JSON: OptimizedJSONSerializer, SerializationFormat.JSON_OPTIMIZED: OptimizedJSONSerializer, SerializationFormat.BINARY: BinarySerializer, SerializationFormat.MSGPACK: BinarySerializer, SerializationFormat.DETERMINISTIC: DeterministicSerializer}
    @classmethod
    def create_serializer(cls, config: SerializationConfig) -> BaseSerializer:
        serializer_class = cls._serializers.get(config.format)
        if not serializer_class:
            raise ValueError(f'Unsupported serialization format: {config.format}')
        return serializer_class(config)
    @classmethod
    def register_serializer(cls, format_name: str, serializer_class: type):
        cls._serializers[format_name] = serializer_class
def serialize_dtesn_result(dtesn_result: Any, format: str=SerializationFormat.JSON_OPTIMIZED, **config_kwargs) -> Union[str, bytes]:
    config = SerializationConfig(format=format, **config_kwargs)
    serializer = SerializerFactory.create_serializer(config)
    return serializer.serialize(dtesn_result)
def get_serialization_metadata(serializer: BaseSerializer) -> Optional[SerializationMetadata]:
    return serializer.metadata
def benchmark_serialization(dtesn_result: Any, iterations: int=1000) -> Dict[str, Dict[str, float]]:
    formats = [SerializationFormat.JSON, SerializationFormat.JSON_OPTIMIZED, SerializationFormat.BINARY, SerializationFormat.DETERMINISTIC]
    results = {}
    for format_name in formats:
        config = SerializationConfig(format=format_name)
        serializer = SerializerFactory.create_serializer(config)
        for _ in range(10):
            serializer.serialize(dtesn_result)
        start_time = time.time()
        for _ in range(iterations):
            serializer.serialize(dtesn_result)
        elapsed = time.time() - start_time
        metadata = serializer.metadata
        results[format_name] = {'avg_time_ms': elapsed / iterations * 1000, 'compression_ratio': metadata.compression_ratio if metadata else 1.0, 'serialized_size': metadata.compressed_size if metadata else 0}
    return results