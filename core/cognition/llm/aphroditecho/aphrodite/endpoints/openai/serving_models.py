from asyncio import Lock
from collections import defaultdict
from dataclasses import dataclass
from http import HTTPStatus
from typing import Optional, Union
from loguru import logger
from aphrodite.common.config import ModelConfig
from aphrodite.utils import AtomicCounter
from aphrodite.endpoints.openai.protocol import ErrorResponse, LoadLoRAAdapterRequest, ModelCard, ModelList, ModelPermission, UnloadLoRAAdapterRequest
from aphrodite.engine.protocol import EngineClient
from aphrodite.lora.request import LoRARequest
from aphrodite.lora.resolver import LoRAResolver, LoRAResolverRegistry
@dataclass
class BaseModelPath:
    name: str
    model_path: str
@dataclass
class LoRAModulePath:
    name: str
    path: str
    base_model_name: Optional[str] = None
class OpenAIServingModels:
    def __init__(self, engine_client: EngineClient, model_config: ModelConfig, base_model_paths: list[BaseModelPath], *, lora_modules: Optional[list[LoRAModulePath]]=None):
        super().__init__()
        self.base_model_paths = base_model_paths
        self.max_model_len = model_config.max_model_len
        self.engine_client = engine_client
        self.model_config = model_config
        self.static_lora_modules = lora_modules
        self.lora_requests: dict[str, LoRARequest] = {}
        self.lora_id_counter = AtomicCounter(0)
        self.lora_resolvers: list[LoRAResolver] = []
        for lora_resolver_name in LoRAResolverRegistry.get_supported_resolvers():
            self.lora_resolvers.append(LoRAResolverRegistry.get_resolver(lora_resolver_name))
        self.lora_resolver_lock: dict[str, Lock] = defaultdict(Lock)
    async def init_static_loras(self):
        if self.static_lora_modules is None:
            return
        for lora in self.static_lora_modules:
            load_request = LoadLoRAAdapterRequest(lora_path=lora.path, lora_name=lora.name)
            load_result = await self.load_lora_adapter(request=load_request, base_model_name=lora.base_model_name)
            if isinstance(load_result, ErrorResponse):
                raise ValueError(load_result.message)
    def is_base_model(self, model_name) -> bool:
        return any((model.name == model_name for model in self.base_model_paths))
    def model_name(self, lora_request: Optional[LoRARequest]=None) -> str:
        if lora_request is not None:
            return lora_request.lora_name
        return self.base_model_paths[0].name
    async def show_available_models(self) -> ModelList:
        model_cards = [ModelCard(id=base_model.name, max_model_len=self.max_model_len, root=base_model.model_path, permission=[ModelPermission()]) for base_model in self.base_model_paths]
        lora_cards = [ModelCard(id=lora.lora_name, root=lora.local_path, parent=lora.base_model_name if lora.base_model_name else self.base_model_paths[0].name, permission=[ModelPermission()]) for lora in self.lora_requests.values()]
        model_cards.extend(lora_cards)
        return ModelList(data=model_cards)
    async def load_lora_adapter(self, request: LoadLoRAAdapterRequest, base_model_name: Optional[str]=None) -> Union[ErrorResponse, str]:
        lora_name = request.lora_name
        async with self.lora_resolver_lock[lora_name]:
            error_check_ret = await self._check_load_lora_adapter_request(request)
            if error_check_ret is not None:
                return error_check_ret
            lora_path = request.lora_path
            unique_id = self.lora_id_counter.inc(1)
            lora_request = LoRARequest(lora_name=lora_name, lora_int_id=unique_id, lora_path=lora_path)
            if base_model_name is not None and self.is_base_model(base_model_name):
                lora_request.base_model_name = base_model_name
            try:
                await self.engine_client.add_lora(lora_request)
            except Exception as e:
                error_type = 'BadRequestError'
                status_code = HTTPStatus.BAD_REQUEST
                if 'No adapter found' in str(e):
                    error_type = 'NotFoundError'
                    status_code = HTTPStatus.NOT_FOUND
                return create_error_response(message=str(e), err_type=error_type, status_code=status_code)
            self.lora_requests[lora_name] = lora_request
            logger.info("Loaded new LoRA adapter: name '{}', path '{}'", lora_name, lora_path)
            return f"Success: LoRA adapter '{lora_name}' added successfully."
    async def unload_lora_adapter(self, request: UnloadLoRAAdapterRequest) -> Union[ErrorResponse, str]:
        lora_name = request.lora_name
        async with self.lora_resolver_lock[lora_name]:
            error_check_ret = await self._check_unload_lora_adapter_request(request)
            if error_check_ret is not None:
                return error_check_ret
            del self.lora_requests[lora_name]
            logger.info("Removed LoRA adapter: name '{}'", lora_name)
            return f"Success: LoRA adapter '{lora_name}' removed successfully."
    async def _check_load_lora_adapter_request(self, request: LoadLoRAAdapterRequest) -> Optional[ErrorResponse]:
        if not request.lora_name or not request.lora_path:
            return create_error_response(message="Both 'lora_name' and 'lora_path' must be provided.", err_type='InvalidUserInput', status_code=HTTPStatus.BAD_REQUEST)
        if request.lora_name in self.lora_requests:
            return create_error_response(message=f"The lora adapter '{request.lora_name}' has already been loaded.", err_type='InvalidUserInput', status_code=HTTPStatus.BAD_REQUEST)
        return None
    async def _check_unload_lora_adapter_request(self, request: UnloadLoRAAdapterRequest) -> Optional[ErrorResponse]:
        if not request.lora_name:
            return create_error_response(message="'lora_name' needs to be provided to unload a LoRA adapter.", err_type='InvalidUserInput', status_code=HTTPStatus.BAD_REQUEST)
        if request.lora_name not in self.lora_requests:
            return create_error_response(message=f"The lora adapter '{request.lora_name}' cannot be found.", err_type='NotFoundError', status_code=HTTPStatus.NOT_FOUND)
        return None
    async def resolve_lora(self, lora_name: str) -> Union[LoRARequest, ErrorResponse]:
        async with self.lora_resolver_lock[lora_name]:
            if lora_name in self.lora_requests:
                return self.lora_requests[lora_name]
            base_model_name = self.model_config.model
            unique_id = self.lora_id_counter.inc(1)
            found_adapter = False
            for resolver in self.lora_resolvers:
                lora_request = await resolver.resolve_lora(base_model_name, lora_name)
                if lora_request is not None:
                    found_adapter = True
                    lora_request.lora_int_id = unique_id
                    try:
                        await self.engine_client.add_lora(lora_request)
                        self.lora_requests[lora_name] = lora_request
                        logger.info("Resolved and loaded LoRA adapter '{}' using {}", lora_name, resolver.__class__.__name__)
                        return lora_request
                    except BaseException as e:
                        logger.warning("Failed to load LoRA '{}' resolved by {}: {}. Trying next resolver.", lora_name, resolver.__class__.__name__, e)
                        continue
            if found_adapter:
                return create_error_response(message=f"LoRA adapter '{lora_name}' was found but could not be loaded.", err_type='BadRequestError', status_code=HTTPStatus.BAD_REQUEST)
            else:
                return create_error_response(message=f'LoRA adapter {lora_name} does not exist', err_type='NotFoundError', status_code=HTTPStatus.NOT_FOUND)
def create_error_response(message: str, err_type: str='BadRequestError', status_code: HTTPStatus=HTTPStatus.BAD_REQUEST) -> ErrorResponse:
    return ErrorResponse(message=message, type=err_type, code=status_code.value)