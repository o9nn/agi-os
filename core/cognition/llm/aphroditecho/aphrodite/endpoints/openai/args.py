import argparse
import json
import ssl
from collections.abc import Sequence
from dataclasses import field
from typing import Literal, Optional, Union
from pydantic.dataclasses import dataclass
import aphrodite.common.envs as envs
from aphrodite.common.config import config
from aphrodite.utils import FlexibleArgumentParser
from aphrodite.endpoints.chat_utils import ChatTemplateContentFormatOption, validate_chat_template
from aphrodite.endpoints.openai.serving_models import LoRAModulePath
from aphrodite.endpoints.openai.tool_parsers import ToolParserManager
from aphrodite.engine.args_tools import AsyncEngineArgs, optional_type
class LoRAParserAction(argparse.Action):
    def __call__(self, parser: argparse.ArgumentParser, namespace: argparse.Namespace, values: Optional[Union[str, Sequence[str]]], option_string: Optional[str]=None):
        if values is None:
            values = []
        if isinstance(values, str):
            raise TypeError('Expected values to be a list')
        lora_list: list[LoRAModulePath] = []
        for item in values:
            if item in [None, '']:
                continue
            if '=' in item and ',' not in item:
                name, path = item.split('=')
                lora_list.append(LoRAModulePath(name, path))
            else:
                try:
                    lora_dict = json.loads(item)
                    lora = LoRAModulePath(**lora_dict)
                    lora_list.append(lora)
                except json.JSONDecodeError:
                    parser.error(f'Invalid JSON format for --lora-modules: {item}')
                except TypeError as e:
                    parser.error(f'Invalid fields for --lora-modules: {item} - {str(e)}')
        setattr(namespace, self.dest, lora_list)
@config
@dataclass
class FrontendArgs:
    host: Optional[str] = None
    'Host name.'
    port: int = 2242
    'Port number.'
    uvicorn_log_level: Literal['debug', 'info', 'warning', 'error', 'critical', 'trace'] = 'info'
    'Log level for uvicorn.'
    disable_uvicorn_access_log: bool = False
    'Disable uvicorn access log.'
    allow_credentials: bool = False
    'Allow credentials.'
    allowed_origins: list[str] = field(default_factory=lambda: ['*'])
    'Allowed origins.'
    allowed_methods: list[str] = field(default_factory=lambda: ['*'])
    'Allowed methods.'
    allowed_headers: list[str] = field(default_factory=lambda: ['*'])
    'Allowed headers.'
    api_key: Optional[list[str]] = None
    'If provided, the server will require one of these keys to be presented\n    in the header.'
    lora_modules: Optional[list[LoRAModulePath]] = None
    'LoRA modules configurations in either \'name=path\' format or JSON format\n    or JSON list format. Example (old format): `\'name=path\'` Example (new\n    format): `{"name": "name", "path": "lora_path",\n    "base_model_name": "id"}`'
    chat_template: Optional[str] = None
    'The file path to the chat template, or the template in single-line form\n    for the specified model.'
    chat_template_content_format: ChatTemplateContentFormatOption = 'auto'
    'The format to render message content within a chat template.\n* "string" will render the content as a string. Example: `"Hello World"`\n* "openai" will render the content as a list of dictionaries, similar to OpenAI\nschema. Example: `[{"type": "text", "text": "Hello world!"}]`'
    response_role: str = 'assistant'
    'The role name to return if `request.add_generation_prompt=true`.'
    ssl_keyfile: Optional[str] = None
    'The file path to the SSL key file.'
    ssl_certfile: Optional[str] = None
    'The file path to the SSL cert file.'
    ssl_ca_certs: Optional[str] = None
    'The CA certificates file.'
    enable_ssl_refresh: bool = False
    'Refresh SSL Context when SSL certificate files change'
    ssl_cert_reqs: int = int(ssl.CERT_NONE)
    "Whether client certificate is required (see stdlib ssl module's)."
    root_path: Optional[str] = None
    'FastAPI root_path when app is behind a path based routing proxy.'
    middleware: list[str] = field(default_factory=lambda: [])
    "Additional ASGI middleware to apply to the app. We accept multiple\n    --middleware arguments. The value should be an import path. If a function\n    is provided, vLLM will add it to the server using\n    `@app.middleware('http')`. If a class is provided, vLLM will\n    add it to the server using `app.add_middleware()`."
    return_tokens_as_token_ids: bool = False
    "When `--max-logprobs` is specified, represents single tokens as\n    strings of the form 'token_id:{token_id}' so that tokens that are not\n    JSON-encodable can be identified."
    disable_frontend_multiprocessing: bool = False
    'If specified, will run the OpenAI frontend server in the same process as\n    the model serving engine.'
    enable_request_id_headers: bool = False
    'If specified, API server will add X-Request-Id header to responses.\n    Caution: this hurts performance at high QPS.'
    enable_auto_tool_choice: bool = False
    "If specified, exclude tool definitions in prompts when\n    tool_choice='none'."
    exclude_tools_when_tool_choice_none: bool = False
    'Enable auto tool choice for supported models. Use `--tool-call-parser`\n    to specify which parser to use.'
    tool_call_parser: Optional[str] = None
    "Select the tool call parser depending on the model that you're using.\n    This is used to parse the model-generated tool call into OpenAI API format.\n    Required for `--enable-auto-tool-choice`. You can choose any option from\n    the built-in parsers or register a plugin via `--tool-parser-plugin`."
    tool_parser_plugin: str = ''
    'Special the tool parser plugin write to parse the model-generated tool\n    into OpenAI API format, the name register in this plugin can be used in\n    `--tool-call-parser`.'
    tool_server: Optional[str] = None
    'Comma-separated list of host:port pairs (IPv4, IPv6, or hostname).\n    Examples: 127.0.0.1:8000, [::1]:8000, localhost:1234. Or `demo` for demo\n    purpose.'
    log_config_file: Optional[str] = envs.APHRODITE_LOGGING_CONFIG_PATH
    'Path to logging config JSON file for both aphrodite and uvicorn'
    max_log_len: Optional[int] = None
    'Max number of prompt characters or prompt ID numbers being printed in\n    log. The default of None means unlimited.'
    disable_fastapi_docs: bool = False
    "Disable FastAPI's OpenAPI schema, Swagger UI, and ReDoc endpoint."
    enable_prompt_tokens_details: bool = False
    'If set to True, enable prompt_tokens_details in usage.'
    enable_server_load_tracking: bool = False
    'If set to True, enable tracking server_load_metrics in the app state.'
    enable_force_include_usage: bool = False
    'If set to True, including usage on every request.'
    enable_tokenizer_info_endpoint: bool = False
    'Enable the /get_tokenizer_info endpoint. May expose chat\n    templates and other tokenizer configuration.'
    optimization_level: Literal['minimal', 'balanced', 'high'] = 'balanced'
    "Route optimization level for sub-100ms response times. \n    'minimal': Basic compression only, 'balanced': Caching + compression + preprocessing, \n    'high': Aggressive optimization with larger cache and shorter timeouts."
    enable_continuous_learning: bool = False
    'Enable server-side continuous learning from production interactions.\n    When enabled, the server will collect interaction data and apply incremental\n    model updates in the background. Disabled by default for production safety.'
    continuous_learning_interval: int = 60
    'Background learning interval in seconds. Controls how often the system\n    processes collected interactions for learning. Default: 60 seconds.'
    continuous_learning_min_interactions: int = 10
    'Minimum number of interactions required before triggering a learning cycle.\n    Helps ensure sufficient data quality. Default: 10 interactions.'
    continuous_learning_quality_threshold: float = 0.5
    'Quality threshold for filtering interactions used in learning.\n    Interactions with performance feedback below this threshold are excluded.\n    Range: -1.0 to 1.0. Default: 0.5.'
    continuous_learning_max_rate: float = 0.001
    'Maximum learning rate for production safety. Limits how aggressively\n    the model can be updated from production data. Default: 0.001.'
    continuous_learning_enable_rollback: bool = True
    'Enable automatic rollback on performance degradation. When enabled,\n    the system will revert to previous model state if learning causes issues.\n    Default: True (recommended for production).'
    @staticmethod
    def add_cli_args(parser: FlexibleArgumentParser) -> FlexibleArgumentParser:
        from aphrodite.engine.args_tools import get_kwargs
        frontend_kwargs = get_kwargs(FrontendArgs)
        frontend_kwargs['allowed_origins']['type'] = json.loads
        frontend_kwargs['allowed_methods']['type'] = json.loads
        frontend_kwargs['allowed_headers']['type'] = json.loads
        del frontend_kwargs['allowed_origins']['nargs']
        del frontend_kwargs['allowed_methods']['nargs']
        del frontend_kwargs['allowed_headers']['nargs']
        frontend_kwargs['lora_modules']['type'] = optional_type(str)
        frontend_kwargs['lora_modules']['action'] = LoRAParserAction
        frontend_kwargs['middleware']['action'] = 'append'
        frontend_kwargs['middleware']['type'] = str
        if 'nargs' in frontend_kwargs['middleware']:
            del frontend_kwargs['middleware']['nargs']
        frontend_kwargs['middleware']['default'] = []
        valid_tool_parsers = list(ToolParserManager.tool_parsers.keys())
        parsers_str = ','.join(valid_tool_parsers)
        frontend_kwargs['tool_call_parser']['metavar'] = f'{{{parsers_str}}} or name registered in --tool-parser-plugin'
        frontend_group = parser.add_argument_group(title='Frontend', description=FrontendArgs.__doc__)
        for key, value in frontend_kwargs.items():
            frontend_group.add_argument(f"--{key.replace('_', '-')}", **value)
        return parser
def make_arg_parser(parser: FlexibleArgumentParser) -> FlexibleArgumentParser:
    parser.add_argument('model_tag', type=str, nargs='?', help='The model tag to serve (optional if specified in config)')
    parser.add_argument('--headless', action='store_true', default=False, help='Run in headless mode. See multi-node data parallel documentation for more details.')
    parser.add_argument('--api-server-count', '-asc', type=int, default=1, help='How many API server processes to run.')
    parser.add_argument('--config', help='Read CLI options from a config file.')
    parser = FrontendArgs.add_cli_args(parser)
    parser = AsyncEngineArgs.add_cli_args(parser)
    return parser
def validate_parsed_serve_args(args: argparse.Namespace):
    if hasattr(args, 'subparser') and args.subparser != 'run':
        return
    validate_chat_template(args.chat_template)
    if args.enable_auto_tool_choice and (not args.tool_call_parser):
        raise TypeError('Error: --enable-auto-tool-choice requires --tool-call-parser')
def create_parser_for_docs() -> FlexibleArgumentParser:
    parser_for_docs = FlexibleArgumentParser(prog='-m aphrodite.endpoints.openai.api_server')
    return make_arg_parser(parser_for_docs)