import asyncio
import click
from click_default_group import DefaultGroup
from dataclasses import asdict
import io
import json
import os
from llm import Attachment, AsyncConversation, AsyncKeyModel, AsyncResponse, CancelToolCall, Collection, Conversation, Fragment, Response, Template, Tool, Toolbox, UnknownModelError, KeyModel, encode, get_async_model, get_default_model, get_default_embedding_model, get_embedding_models_with_aliases, get_embedding_model_aliases, get_embedding_model, get_plugins, get_tools, get_fragment_loaders, get_template_loaders, get_model, get_model_aliases, get_models_with_aliases, user_dir, set_alias, set_default_model, set_default_embedding_model, remove_alias
from llm.models import _BaseConversation, ChainResponse
from .migrations import migrate
from .plugins import pm, load_plugins
from .utils import ensure_fragment, extract_fenced_code_block, find_unused_key, has_plugin_prefix, instantiate_from_spec, make_schema_id, maybe_fenced_code, mimetype_from_path, mimetype_from_string, multi_schema, output_rows_as_json, resolve_schema_input, schema_dsl, schema_summary, token_usage_string, truncate_string
import base64
import httpx
import inspect
import pathlib
import pydantic
import re
import readline
from runpy import run_module
import shutil
import sqlite_utils
from sqlite_utils.utils import rows_from_file, Format
import sys
import textwrap
from typing import cast, Dict, Optional, Iterable, List, Union, Tuple, Type, Any
import warnings
import yaml
warnings.simplefilter('ignore', ResourceWarning)
DEFAULT_TEMPLATE = 'prompt: '
class FragmentNotFound(Exception):
    pass
def validate_fragment_alias(ctx, param, value):
    if not re.match('^[a-zA-Z0-9_-]+$', value):
        raise click.BadParameter('Fragment alias must be alphanumeric')
    return value
def resolve_fragments(db: sqlite_utils.Database, fragments: Iterable[str], allow_attachments: bool=False) -> List[Union[Fragment, Attachment]]:
    def _load_by_alias(fragment: str) -> Tuple[Optional[str], Optional[str]]:
        rows = list(db.query('\n                select content, source from fragments\n                left join fragment_aliases on fragments.id = fragment_aliases.fragment_id\n                where alias = :alias or hash = :alias limit 1\n                ', {'alias': fragment}))
        if rows:
            row = rows[0]
            return (row['content'], row['source'])
        return (None, None)
    resolved: List[Union[Fragment, Attachment]] = []
    for fragment in fragments:
        if fragment.startswith('http://') or fragment.startswith('https://'):
            client = httpx.Client(follow_redirects=True, max_redirects=3)
            response = client.get(fragment)
            response.raise_for_status()
            resolved.append(Fragment(response.text, fragment))
        elif fragment == '-':
            resolved.append(Fragment(sys.stdin.read(), '-'))
        elif has_plugin_prefix(fragment):
            prefix, rest = fragment.split(':', 1)
            loaders = get_fragment_loaders()
            if prefix not in loaders:
                raise FragmentNotFound('Unknown fragment prefix: {}'.format(prefix))
            loader = loaders[prefix]
            try:
                result = loader(rest)
                if not isinstance(result, list):
                    result = [result]
                if not allow_attachments and any((isinstance(r, Attachment) for r in result)):
                    raise FragmentNotFound('Fragment loader {} returned a disallowed attachment'.format(prefix))
                resolved.extend(result)
            except Exception as ex:
                raise FragmentNotFound('Could not load fragment {}: {}'.format(fragment, ex))
        else:
            content, source = _load_by_alias(fragment)
            if content is not None:
                resolved.append(Fragment(content, source))
            else:
                path = pathlib.Path(fragment)
                if path.exists():
                    resolved.append(Fragment(path.read_text(), str(path.resolve())))
                else:
                    raise FragmentNotFound(f"Fragment '{fragment}' not found")
    return resolved
def process_fragments_in_chat(db: sqlite_utils.Database, prompt: str) -> tuple[str, list[Fragment], list[Attachment]]:
    prompt_lines = []
    fragments = []
    attachments = []
    for line in prompt.splitlines():
        if line.startswith('!fragment '):
            try:
                fragment_strs = line.strip().removeprefix('!fragment ').split()
                fragments_and_attachments = resolve_fragments(db, fragments=fragment_strs, allow_attachments=True)
                fragments += [fragment for fragment in fragments_and_attachments if isinstance(fragment, Fragment)]
                attachments += [attachment for attachment in fragments_and_attachments if isinstance(attachment, Attachment)]
            except FragmentNotFound as ex:
                raise click.ClickException(str(ex))
        else:
            prompt_lines.append(line)
    return ('\n'.join(prompt_lines), fragments, attachments)
class AttachmentError(Exception):
    pass
def resolve_attachment(value):
    if value == '-':
        content = sys.stdin.buffer.read()
        mimetype = mimetype_from_string(content)
        if mimetype is None:
            raise AttachmentError('Could not determine mimetype of stdin')
        return Attachment(type=mimetype, path=None, url=None, content=content)
    if '://' in value:
        try:
            response = httpx.head(value)
            response.raise_for_status()
            mimetype = response.headers.get('content-type')
        except httpx.HTTPError as ex:
            raise AttachmentError(str(ex))
        return Attachment(type=mimetype, path=None, url=value, content=None)
    path = pathlib.Path(value)
    if not path.exists():
        raise AttachmentError(f'File {value} does not exist')
    path = path.resolve()
    mimetype = mimetype_from_path(str(path))
    if mimetype is None:
        raise AttachmentError(f'Could not determine mimetype of {value}')
    return Attachment(type=mimetype, path=str(path), url=None, content=None)
class AttachmentType(click.ParamType):
    name = 'attachment'
    def convert(self, value, param, ctx):
        try:
            return resolve_attachment(value)
        except AttachmentError as e:
            self.fail(str(e), param, ctx)
def resolve_attachment_with_type(value: str, mimetype: str) -> Attachment:
    if '://' in value:
        attachment = Attachment(mimetype, None, value, None)
    elif value == '-':
        content = sys.stdin.buffer.read()
        attachment = Attachment(mimetype, None, None, content)
    else:
        path = pathlib.Path(value)
        if not path.exists():
            raise click.BadParameter(f'File {value} does not exist')
        path = path.resolve()
        attachment = Attachment(mimetype, str(path), None, None)
    return attachment
def attachment_types_callback(ctx, param, values) -> List[Attachment]:
    collected = []
    for value, mimetype in values:
        collected.append(resolve_attachment_with_type(value, mimetype))
    return collected
def json_validator(object_name):
    def validator(ctx, param, value):
        if value is None:
            return value
        try:
            obj = json.loads(value)
            if not isinstance(obj, dict):
                raise click.BadParameter(f'{object_name} must be a JSON object')
            return obj
        except json.JSONDecodeError:
            raise click.BadParameter(f'{object_name} must be valid JSON')
    return validator
def schema_option(fn):
    click.option('schema_input', '--schema', help='JSON schema, filepath or ID')(fn)
    return fn
@click.group(cls=DefaultGroup, default='prompt', default_if_no_args=True, context_settings={'help_option_names': ['-h', '--help']})
@click.version_option()
def cli():
@cli.command(name='prompt')
@click.argument('prompt', required=False)
@click.option('-s', '--system', help='System prompt to use')
@click.option('model_id', '-m', '--model', help='Model to use', envvar='LLM_MODEL')
@click.option('-d', '--database', type=click.Path(readable=True, dir_okay=False), help='Path to log database')
@click.option('queries', '-q', '--query', multiple=True, help='Use first model matching these strings')
@click.option('attachments', '-a', '--attachment', type=AttachmentType(), multiple=True, help='Attachment path or URL or -')
@click.option('attachment_types', '--at', '--attachment-type', type=(str, str), multiple=True, callback=attachment_types_callback, help='\x08\nAttachment with explicit mimetype,\n--at image.jpg image/jpeg')
@click.option('tools', '-T', '--tool', multiple=True, help='Name of a tool to make available to the model')
@click.option('python_tools', '--functions', help='Python code block or file path defining functions to register as tools', multiple=True)
@click.option('tools_debug', '--td', '--tools-debug', is_flag=True, help='Show full details of tool executions', envvar='LLM_TOOLS_DEBUG')
@click.option('tools_approve', '--ta', '--tools-approve', is_flag=True, help='Manually approve every tool execution')
@click.option('chain_limit', '--cl', '--chain-limit', type=int, default=5, help='How many chained tool responses to allow, default 5, set 0 for unlimited')
@click.option('options', '-o', '--option', type=(str, str), multiple=True, help='key/value options for the model')
@schema_option
@click.option('--schema-multi', help='JSON schema to use for multiple results')
@click.option('fragments', '-f', '--fragment', multiple=True, help='Fragment (alias, URL, hash or file path) to add to the prompt')
@click.option('system_fragments', '--sf', '--system-fragment', multiple=True, help='Fragment to add to system prompt')
@click.option('-t', '--template', help='Template to use')
@click.option('-p', '--param', multiple=True, type=(str, str), help='Parameters for template')
@click.option('--no-stream', is_flag=True, help='Do not stream output')
@click.option('-n', '--no-log', is_flag=True, help="Don't log to database")
@click.option('--log', is_flag=True, help='Log prompt and response to the database')
@click.option('_continue', '-c', '--continue', is_flag=True, flag_value=-1, help='Continue the most recent conversation.')
@click.option('conversation_id', '--cid', '--conversation', help='Continue the conversation with the given ID.')
@click.option('--key', help='API key to use')
@click.option('--save', help='Save prompt with this template name')
@click.option('async_', '--async', is_flag=True, help='Run prompt asynchronously')
@click.option('-u', '--usage', is_flag=True, help='Show token usage')
@click.option('-x', '--extract', is_flag=True, help='Extract first fenced code block')
@click.option('extract_last', '--xl', '--extract-last', is_flag=True, help='Extract last fenced code block')
def prompt(prompt, system, model_id, database, queries, attachments, attachment_types, tools, python_tools, tools_debug, tools_approve, chain_limit, options, schema_input, schema_multi, fragments, system_fragments, template, param, no_stream, no_log, log, _continue, conversation_id, key, save, async_, usage, extract, extract_last):
    if log and no_log:
        raise click.ClickException('--log and --no-log are mutually exclusive')
    log_path = pathlib.Path(database) if database else logs_db_path()
    log_path.parent.mkdir(parents=True, exist_ok=True)
    db = sqlite_utils.Database(log_path)
    migrate(db)
    if queries and (not model_id):
        matches = []
        for model_with_aliases in get_models_with_aliases():
            if all((model_with_aliases.matches(q) for q in queries)):
                matches.append(model_with_aliases.model.model_id)
        if not matches:
            raise click.ClickException('No model found matching queries {}'.format(', '.join(queries)))
        model_id = min(matches, key=len)
    if schema_multi:
        schema_input = schema_multi
    schema = resolve_schema_input(db, schema_input, load_template)
    if schema_multi:
        schema = multi_schema(schema)
    model_aliases = get_model_aliases()
    def read_prompt():
        nonlocal prompt, schema
        stdin_prompt = None
        if not sys.stdin.isatty():
            stdin_prompt = sys.stdin.read()
        if stdin_prompt:
            bits = [stdin_prompt]
            if prompt:
                bits.append(prompt)
            prompt = ' '.join(bits)
        if prompt is None and (not save) and sys.stdin.isatty() and (not attachments) and (not attachment_types) and (not schema) and (not fragments):
            prompt = sys.stdin.read()
        return prompt
    if save:
        disallowed_options = []
        for option, var in (('--template', template), ('--continue', _continue), ('--cid', conversation_id)):
            if var:
                disallowed_options.append(option)
        if disallowed_options:
            raise click.ClickException('--save cannot be used with {}'.format(', '.join(disallowed_options)))
        path = template_dir() / f'{save}.yaml'
        to_save = {}
        if model_id:
            try:
                to_save['model'] = model_aliases[model_id].model_id
            except KeyError:
                raise click.ClickException("'{}' is not a known model".format(model_id))
        prompt = read_prompt()
        if prompt:
            to_save['prompt'] = prompt
        if system:
            to_save['system'] = system
        if param:
            to_save['defaults'] = dict(param)
        if extract:
            to_save['extract'] = True
        if extract_last:
            to_save['extract_last'] = True
        if schema:
            to_save['schema_object'] = schema
        if fragments:
            to_save['fragments'] = list(fragments)
        if system_fragments:
            to_save['system_fragments'] = list(system_fragments)
        if python_tools:
            to_save['functions'] = '\n\n'.join(python_tools)
        if tools:
            to_save['tools'] = list(tools)
        if attachments:
            to_save['attachments'] = [a.path or a.url for a in attachments if a.path or a.url]
        if attachment_types:
            to_save['attachment_types'] = [{'type': a.type, 'value': a.path or a.url} for a in attachment_types if a.path or a.url]
        if options:
            model = get_model(model_id or get_default_model())
            try:
                options_model = model.Options(**dict(options))
                to_save['options'] = {k: v for k, v in options_model.model_dump(mode='json').items() if v is not None}
            except pydantic.ValidationError as ex:
                raise click.ClickException(render_errors(ex.errors()))
        path.write_text(yaml.safe_dump(to_save, indent=4, default_flow_style=False, sort_keys=False), 'utf-8')
        return
    if template:
        params = dict(param)
        try:
            template_obj = load_template(template)
        except LoadTemplateError as ex:
            raise click.ClickException(str(ex))
        extract = template_obj.extract
        extract_last = template_obj.extract_last
        if template_obj.fragments:
            fragments = [*template_obj.fragments, *fragments]
        if template_obj.system_fragments:
            system_fragments = [*template_obj.system_fragments, *system_fragments]
        if template_obj.schema_object:
            schema = template_obj.schema_object
        if template_obj.tools:
            tools = [*template_obj.tools, *tools]
        if template_obj.functions and template_obj._functions_is_trusted:
            python_tools = [template_obj.functions, *python_tools]
        input_ = ''
        if template_obj.options:
            options = list(options)
            specified_options = dict(options)
            for option_name, option_value in template_obj.options.items():
                if option_name not in specified_options:
                    options.append((option_name, option_value))
        if 'input' in template_obj.vars():
            input_ = read_prompt()
        try:
            template_prompt, template_system = template_obj.evaluate(input_, params)
            if template_prompt:
                if prompt and 'input' not in template_obj.vars():
                    prompt = template_prompt + '\n' + prompt
                else:
                    prompt = template_prompt
            if template_system and (not system):
                system = template_system
        except Template.MissingVariables as ex:
            raise click.ClickException(str(ex))
        if model_id is None and template_obj.model:
            model_id = template_obj.model
        if template_obj.attachments:
            attachments = [resolve_attachment(a) for a in template_obj.attachments] + list(attachments)
        if template_obj.attachment_types:
            attachment_types = [resolve_attachment_with_type(at.value, at.type) for at in template_obj.attachment_types] + list(attachment_types)
    if extract or extract_last:
        no_stream = True
    conversation = None
    if conversation_id or _continue:
        try:
            conversation = load_conversation(conversation_id, async_=async_, database=database)
        except UnknownModelError as ex:
            raise click.ClickException(str(ex))
    if (conversation_tools := _get_conversation_tools(conversation, tools)):
        tools = conversation_tools
    if model_id is None:
        if conversation:
            model_id = conversation.model.model_id
        else:
            model_id = get_default_model()
    try:
        if async_:
            model = get_async_model(model_id)
        else:
            model = get_model(model_id)
    except UnknownModelError as ex:
        raise click.ClickException(ex)
    if conversation is None and (tools or python_tools):
        conversation = model.conversation()
    if conversation:
        conversation.model = model
    validated_options = {}
    if options:
        try:
            validated_options = dict(((key, value) for key, value in model.Options(**dict(options)) if value is not None))
        except pydantic.ValidationError as ex:
            raise click.ClickException(render_errors(ex.errors()))
    default_options = get_model_options(model.model_id)
    for key_, value in default_options.items():
        if key_ not in validated_options:
            validated_options[key_] = value
    kwargs = {}
    resolved_attachments = [*attachments, *attachment_types]
    should_stream = model.can_stream and (not no_stream)
    if not should_stream:
        kwargs['stream'] = False
    if isinstance(model, (KeyModel, AsyncKeyModel)):
        kwargs['key'] = key
    prompt = read_prompt()
    response = None
    try:
        fragments_and_attachments = resolve_fragments(db, fragments, allow_attachments=True)
        resolved_fragments = [fragment for fragment in fragments_and_attachments if isinstance(fragment, Fragment)]
        resolved_attachments.extend((attachment for attachment in fragments_and_attachments if isinstance(attachment, Attachment)))
        resolved_system_fragments = resolve_fragments(db, system_fragments)
    except FragmentNotFound as ex:
        raise click.ClickException(str(ex))
    prompt_method = model.prompt
    if conversation:
        prompt_method = conversation.prompt
    tool_implementations = _gather_tools(tools, python_tools)
    if tool_implementations:
        prompt_method = conversation.chain
        kwargs['options'] = validated_options
        kwargs['chain_limit'] = chain_limit
        if tools_debug:
            kwargs['after_call'] = _debug_tool_call
        if tools_approve:
            kwargs['before_call'] = _approve_tool_call
        kwargs['tools'] = tool_implementations
    else:
        kwargs.update(validated_options)
    try:
        if async_:
            async def inner():
                if should_stream:
                    response = prompt_method(prompt, attachments=resolved_attachments, system=system, schema=schema, fragments=resolved_fragments, system_fragments=resolved_system_fragments, **kwargs)
                    async for chunk in response:
                        print(chunk, end='')
                        sys.stdout.flush()
                    print('')
                else:
                    response = prompt_method(prompt, fragments=resolved_fragments, attachments=resolved_attachments, schema=schema, system=system, system_fragments=resolved_system_fragments, **kwargs)
                    text = await response.text()
                    if extract or extract_last:
                        text = extract_fenced_code_block(text, last=extract_last) or text
                    print(text)
                return response
            response = asyncio.run(inner())
        else:
            response = prompt_method(prompt, fragments=resolved_fragments, attachments=resolved_attachments, system=system, schema=schema, system_fragments=resolved_system_fragments, **kwargs)
            if should_stream:
                for chunk in response:
                    print(chunk, end='')
                    sys.stdout.flush()
                print('')
            else:
                text = response.text()
                if extract or extract_last:
                    text = extract_fenced_code_block(text, last=extract_last) or text
                print(text)
    except (ValueError, NotImplementedError) as ex:
        raise click.ClickException(str(ex))
    except Exception as ex:
        if getattr(sys, '_called_from_test', False) or os.environ.get('LLM_RAISE_ERRORS', None):
            raise
        raise click.ClickException(str(ex))
    if usage:
        if isinstance(response, ChainResponse):
            responses = response._responses
        else:
            responses = [response]
        for response_object in responses:
            click.echo(click.style('Token usage: {}'.format(response_object.token_usage()), fg='yellow', bold=True), err=True)
    if (logs_on() or log) and (not no_log):
        if isinstance(response, AsyncResponse):
            response = asyncio.run(response.to_sync_response())
        response.log_to_db(db)
@cli.command()
@click.option('-s', '--system', help='System prompt to use')
@click.option('model_id', '-m', '--model', help='Model to use', envvar='LLM_MODEL')
@click.option('_continue', '-c', '--continue', is_flag=True, flag_value=-1, help='Continue the most recent conversation.')
@click.option('conversation_id', '--cid', '--conversation', help='Continue the conversation with the given ID.')
@click.option('fragments', '-f', '--fragment', multiple=True, help='Fragment (alias, URL, hash or file path) to add to the prompt')
@click.option('system_fragments', '--sf', '--system-fragment', multiple=True, help='Fragment to add to system prompt')
@click.option('-t', '--template', help='Template to use')
@click.option('-p', '--param', multiple=True, type=(str, str), help='Parameters for template')
@click.option('options', '-o', '--option', type=(str, str), multiple=True, help='key/value options for the model')
@click.option('-d', '--database', type=click.Path(readable=True, dir_okay=False), help='Path to log database')
@click.option('--no-stream', is_flag=True, help='Do not stream output')
@click.option('--key', help='API key to use')
@click.option('tools', '-T', '--tool', multiple=True, help='Name of a tool to make available to the model')
@click.option('python_tools', '--functions', help='Python code block or file path defining functions to register as tools', multiple=True)
@click.option('tools_debug', '--td', '--tools-debug', is_flag=True, help='Show full details of tool executions', envvar='LLM_TOOLS_DEBUG')
@click.option('tools_approve', '--ta', '--tools-approve', is_flag=True, help='Manually approve every tool execution')
@click.option('chain_limit', '--cl', '--chain-limit', type=int, default=5, help='How many chained tool responses to allow, default 5, set 0 for unlimited')
def chat(system, model_id, _continue, conversation_id, fragments, system_fragments, template, param, options, no_stream, key, database, tools, python_tools, tools_debug, tools_approve, chain_limit):
    if sys.platform != 'win32':
        readline.parse_and_bind('\\e[D: backward-char')
        readline.parse_and_bind('\\e[C: forward-char')
    else:
        readline.parse_and_bind("bind -x '\\e[D: backward-char'")
        readline.parse_and_bind("bind -x '\\e[C: forward-char'")
    log_path = pathlib.Path(database) if database else logs_db_path()
    log_path.parent.mkdir(parents=True, exist_ok=True)
    db = sqlite_utils.Database(log_path)
    migrate(db)
    conversation = None
    if conversation_id or _continue:
        try:
            conversation = load_conversation(conversation_id, database=database)
        except UnknownModelError as ex:
            raise click.ClickException(str(ex))
    if (conversation_tools := _get_conversation_tools(conversation, tools)):
        tools = conversation_tools
    template_obj = None
    if template:
        params = dict(param)
        try:
            template_obj = load_template(template)
        except LoadTemplateError as ex:
            raise click.ClickException(str(ex))
        if model_id is None and template_obj.model:
            model_id = template_obj.model
        if template_obj.tools:
            tools = [*template_obj.tools, *tools]
        if template_obj.functions and template_obj._functions_is_trusted:
            python_tools = [template_obj.functions, *python_tools]
    if model_id is None:
        if conversation:
            model_id = conversation.model.model_id
        else:
            model_id = get_default_model()
    try:
        model = get_model(model_id)
    except KeyError:
        raise click.ClickException("'{}' is not a known model".format(model_id))
    if conversation is None:
        conversation = Conversation(model=model)
    else:
        conversation.model = model
    if tools_debug:
        conversation.after_call = _debug_tool_call
    if tools_approve:
        conversation.before_call = _approve_tool_call
    validated_options = get_model_options(model.model_id)
    if options:
        try:
            validated_options = dict(((key, value) for key, value in model.Options(**dict(options)) if value is not None))
        except pydantic.ValidationError as ex:
            raise click.ClickException(render_errors(ex.errors()))
    kwargs = {}
    if validated_options:
        kwargs['options'] = validated_options
    tool_functions = _gather_tools(tools, python_tools)
    if tool_functions:
        kwargs['chain_limit'] = chain_limit
        kwargs['tools'] = tool_functions
    should_stream = model.can_stream and (not no_stream)
    if not should_stream:
        kwargs['stream'] = False
    if key and isinstance(model, KeyModel):
        kwargs['key'] = key
    try:
        fragments_and_attachments = resolve_fragments(db, fragments, allow_attachments=True)
        argument_fragments = [fragment for fragment in fragments_and_attachments if isinstance(fragment, Fragment)]
        argument_attachments = [attachment for attachment in fragments_and_attachments if isinstance(attachment, Attachment)]
        argument_system_fragments = resolve_fragments(db, system_fragments)
    except FragmentNotFound as ex:
        raise click.ClickException(str(ex))
    click.echo('Chatting with {}'.format(model.model_id))
    click.echo("Type 'exit' or 'quit' to exit")
    click.echo("Type '!multi' to enter multiple lines, then '!end' to finish")
    click.echo("Type '!edit' to open your default editor and modify the prompt")
    click.echo("Type '!fragment <my_fragment> [<another_fragment> ...]' to insert one or more fragments")
    in_multi = False
    accumulated = []
    accumulated_fragments = []
    accumulated_attachments = []
    end_token = '!end'
    while True:
        prompt = click.prompt('', prompt_suffix='> ' if not in_multi else '')
        fragments = []
        attachments = []
        if argument_fragments:
            fragments += argument_fragments
            argument_fragments = []
        if argument_attachments:
            attachments = argument_attachments
            argument_attachments = []
        if prompt.strip().startswith('!multi'):
            in_multi = True
            bits = prompt.strip().split()
            if len(bits) > 1:
                end_token = '!end {}'.format(' '.join(bits[1:]))
            continue
        if prompt.strip() == '!edit':
            edited_prompt = click.edit()
            if edited_prompt is None:
                click.echo('Editor closed without saving.', err=True)
                continue
            prompt = edited_prompt.strip()
        if prompt.strip().startswith('!fragment '):
            prompt, fragments, attachments = process_fragments_in_chat(db, prompt)
        if in_multi:
            if prompt.strip() == end_token:
                prompt = '\n'.join(accumulated)
                fragments = accumulated_fragments
                attachments = accumulated_attachments
                in_multi = False
                accumulated = []
                accumulated_fragments = []
                accumulated_attachments = []
            else:
                if prompt:
                    accumulated.append(prompt)
                accumulated_fragments += fragments
                accumulated_attachments += attachments
                continue
        if template_obj:
            try:
                uses_input = 'input' in template_obj.vars()
                input_ = prompt if uses_input else ''
                template_prompt, template_system = template_obj.evaluate(input_, params)
            except Template.MissingVariables as ex:
                raise click.ClickException(str(ex))
            if template_system and (not system):
                system = template_system
            if template_prompt:
                if prompt and (not uses_input):
                    prompt = f'{template_prompt}\n{prompt}'
                else:
                    prompt = template_prompt
        if prompt.strip() in ('exit', 'quit'):
            break
        response = conversation.chain(prompt, fragments=[str(fragment) for fragment in fragments], system_fragments=[str(system_fragment) for system_fragment in argument_system_fragments], attachments=attachments, system=system, **kwargs)
        system = None
        argument_system_fragments = []
        for chunk in response:
            print(chunk, end='')
            sys.stdout.flush()
        response.log_to_db(db)
        print('')
def load_conversation(conversation_id: Optional[str], async_=False, database=None) -> Optional[_BaseConversation]:
    log_path = pathlib.Path(database) if database else logs_db_path()
    db = sqlite_utils.Database(log_path)
    migrate(db)
    if conversation_id is None:
        matches = list(db['conversations'].rows_where(order_by='id desc', limit=1))
        if matches:
            conversation_id = matches[0]['id']
        else:
            return None
    try:
        row = cast(sqlite_utils.db.Table, db['conversations']).get(conversation_id)
    except sqlite_utils.db.NotFoundError:
        raise click.ClickException('No conversation found with id={}'.format(conversation_id))
    conversation_class = AsyncConversation if async_ else Conversation
    response_class = AsyncResponse if async_ else Response
    conversation = conversation_class.from_row(row)
    for response in db['responses'].rows_where('conversation_id = ?', [conversation_id]):
        conversation.responses.append(response_class.from_row(db, response))
    return conversation
@cli.group(cls=DefaultGroup, default='list', default_if_no_args=True)
def keys():
@keys.command(name='list')
def keys_list():
    path = user_dir() / 'keys.json'
    if not path.exists():
        click.echo('No keys found')
        return
    keys = json.loads(path.read_text())
    for key in sorted(keys.keys()):
        if key != '// Note':
            click.echo(key)
@keys.command(name='path')
def keys_path_command():
    click.echo(user_dir() / 'keys.json')
@keys.command(name='get')
@click.argument('name')
def keys_get(name):
    path = user_dir() / 'keys.json'
    if not path.exists():
        raise click.ClickException('No keys found')
    keys = json.loads(path.read_text())
    try:
        click.echo(keys[name])
    except KeyError:
        raise click.ClickException("No key found with name '{}'".format(name))
@keys.command(name='set')
@click.argument('name')
@click.option('--value', prompt='Enter key', hide_input=True, help='Value to set')
def keys_set(name, value):
    default = {'// Note': 'This file stores secret API credentials. Do not share!'}
    path = user_dir() / 'keys.json'
    path.parent.mkdir(parents=True, exist_ok=True)
    if not path.exists():
        path.write_text(json.dumps(default))
        path.chmod(384)
    try:
        current = json.loads(path.read_text())
    except json.decoder.JSONDecodeError:
        current = default
    current[name] = value
    path.write_text(json.dumps(current, indent=2) + '\n')
@cli.group(cls=DefaultGroup, default='list', default_if_no_args=True)
def logs():
@logs.command(name='path')
def logs_path():
    click.echo(logs_db_path())
@logs.command(name='status')
def logs_status():
    path = logs_db_path()
    if not path.exists():
        click.echo('No log database found at {}'.format(path))
        return
    if logs_on():
        click.echo('Logging is ON for all prompts'.format())
    else:
        click.echo('Logging is OFF'.format())
    db = sqlite_utils.Database(path)
    migrate(db)
    click.echo('Found log database at {}'.format(path))
    click.echo('Number of conversations logged:\t{}'.format(db['conversations'].count))
    click.echo('Number of responses logged:\t{}'.format(db['responses'].count))
    click.echo('Database file size: \t\t{}'.format(_human_readable_size(path.stat().st_size)))
@logs.command(name='backup')
@click.argument('path', type=click.Path(dir_okay=True, writable=True))
def backup(path):
    logs_path = logs_db_path()
    path = pathlib.Path(path)
    db = sqlite_utils.Database(logs_path)
    try:
        db.execute('vacuum into ?', [str(path)])
    except Exception as ex:
        raise click.ClickException(str(ex))
    click.echo('Backed up {} to {}'.format(_human_readable_size(path.stat().st_size), path))
@logs.command(name='on')
def logs_turn_on():
    path = user_dir() / 'logs-off'
    if path.exists():
        path.unlink()
@logs.command(name='off')
def logs_turn_off():
    path = user_dir() / 'logs-off'
    path.touch()
LOGS_COLUMNS = '    responses.id,\n    responses.model,\n    responses.resolved_model,\n    responses.prompt,\n    responses.system,\n    responses.prompt_json,\n    responses.options_json,\n    responses.response,\n    responses.response_json,\n    responses.conversation_id,\n    responses.duration_ms,\n    responses.datetime_utc,\n    responses.input_tokens,\n    responses.output_tokens,\n    responses.token_details,\n    conversations.name as conversation_name,\n    conversations.model as conversation_model,\n    schemas.content as schema_json'
LOGS_SQL = '\nselect\n{columns}\nfrom\n    responses\nleft join schemas on responses.schema_id = schemas.id\nleft join conversations on responses.conversation_id = conversations.id{extra_where}\norder by {order_by}{limit}\n'
LOGS_SQL_SEARCH = '\nselect\n{columns}\nfrom\n    responses\nleft join schemas on responses.schema_id = schemas.id\nleft join conversations on responses.conversation_id = conversations.id\njoin responses_fts on responses_fts.rowid = responses.rowid\nwhere responses_fts match :query{extra_where}\norder by {order_by}{limit}\n'
ATTACHMENTS_SQL = '\nselect\n    response_id,\n    attachments.id,\n    attachments.type,\n    attachments.path,\n    attachments.url,\n    length(attachments.content) as content_length\nfrom attachments\njoin prompt_attachments\n    on attachments.id = prompt_attachments.attachment_id\nwhere prompt_attachments.response_id in ({})\norder by prompt_attachments."order"\n'
@logs.command(name='list')
@click.option('-n', '--count', type=int, default=None, help='Number of entries to show - defaults to 3, use 0 for all')
@click.option('-p', '--path', type=click.Path(readable=True, exists=True, dir_okay=False), help='Path to log database', hidden=True)
@click.option('-d', '--database', type=click.Path(readable=True, exists=True, dir_okay=False), help='Path to log database')
@click.option('-m', '--model', help='Filter by model or model alias')
@click.option('-q', '--query', help='Search for logs matching this string')
@click.option('fragments', '--fragment', '-f', help='Filter for prompts using these fragments', multiple=True)
@click.option('tools', '-T', '--tool', multiple=True, help='Filter for prompts with results from these tools')
@click.option('any_tools', '--tools', is_flag=True, help='Filter for prompts with results from any tools')
@schema_option
@click.option('--schema-multi', help='JSON schema used for multiple results')
@click.option('-l', '--latest', is_flag=True, help='Return latest results matching search query')
@click.option('--data', is_flag=True, help='Output newline-delimited JSON data for schema')
@click.option('--data-array', is_flag=True, help='Output JSON array of data for schema')
@click.option('--data-key', help='Return JSON objects from array in this key')
@click.option('--data-ids', is_flag=True, help='Attach corresponding IDs to JSON objects')
@click.option('-t', '--truncate', is_flag=True, help='Truncate long strings in output')
@click.option('-s', '--short', is_flag=True, help='Shorter YAML output with truncated prompts')
@click.option('-u', '--usage', is_flag=True, help='Include token usage')
@click.option('-r', '--response', is_flag=True, help='Just output the last response')
@click.option('-x', '--extract', is_flag=True, help='Extract first fenced code block')
@click.option('extract_last', '--xl', '--extract-last', is_flag=True, help='Extract last fenced code block')
@click.option('current_conversation', '-c', '--current', is_flag=True, flag_value=-1, help='Show logs from the current conversation')
@click.option('conversation_id', '--cid', '--conversation', help='Show logs for this conversation ID')
@click.option('--id-gt', help='Return responses with ID > this')
@click.option('--id-gte', help='Return responses with ID >= this')
@click.option('json_output', '--json', is_flag=True, help='Output logs as JSON')
@click.option('--expand', '-e', is_flag=True, help='Expand fragments to show their content')
def logs_list(count, path, database, model, query, fragments, tools, any_tools, schema_input, schema_multi, latest, data, data_array, data_key, data_ids, truncate, short, usage, response, extract, extract_last, current_conversation, conversation_id, id_gt, id_gte, json_output, expand):
    if database and (not path):
        path = database
    path = pathlib.Path(path or logs_db_path())
    if not path.exists():
        raise click.ClickException('No log database found at {}'.format(path))
    db = sqlite_utils.Database(path)
    migrate(db)
    if schema_multi:
        schema_input = schema_multi
    schema = resolve_schema_input(db, schema_input, load_template)
    if schema_multi:
        schema = multi_schema(schema)
    if short and (json_output or response):
        invalid = ' or '.join([flag[0] for flag in (('--json', json_output), ('--response', response)) if flag[1]])
        raise click.ClickException('Cannot use --short and {} together'.format(invalid))
    if response and (not current_conversation) and (not conversation_id):
        current_conversation = True
    if current_conversation:
        try:
            conversation_id = next(db.query('select conversation_id from responses order by id desc limit 1'))['conversation_id']
        except StopIteration:
            raise click.ClickException('No conversations found')
    if count is None:
        if conversation_id:
            count = 0
        else:
            count = 3
    model_id = None
    if model:
        try:
            model_id = get_model(model).model_id
        except UnknownModelError:
            model_id = model
    sql = LOGS_SQL
    order_by = 'responses.id desc'
    if query:
        sql = LOGS_SQL_SEARCH
        if not latest:
            order_by = 'responses_fts.rank desc'
    limit = ''
    if count is not None and count > 0:
        limit = ' limit {}'.format(count)
    sql_format = {'limit': limit, 'columns': LOGS_COLUMNS, 'extra_where': '', 'order_by': order_by}
    where_bits = []
    sql_params = {'model': model_id, 'query': query, 'conversation_id': conversation_id, 'id_gt': id_gt, 'id_gte': id_gte}
    if model_id:
        where_bits.append('responses.model = :model')
    if conversation_id:
        where_bits.append('responses.conversation_id = :conversation_id')
    if id_gt:
        where_bits.append('responses.id > :id_gt')
    if id_gte:
        where_bits.append('responses.id >= :id_gte')
    if fragments:
        fragment_hashes = [fragment.id() for fragment in resolve_fragments(db, fragments)]
        exists_clauses = []
        for i, fragment_hash in enumerate(fragment_hashes):
            exists_clause = f'\n            exists (\n                select 1 from prompt_fragments\n                where prompt_fragments.response_id = responses.id\n                and prompt_fragments.fragment_id in (\n                    select fragments.id from fragments\n                    where hash = :f{i}\n                )\n                union\n                select 1 from system_fragments\n                where system_fragments.response_id = responses.id\n                and system_fragments.fragment_id in (\n                    select fragments.id from fragments\n                    where hash = :f{i}\n                )\n            )\n            '
            exists_clauses.append(exists_clause)
            sql_params['f{}'.format(i)] = fragment_hash
        where_bits.append(' and '.join(exists_clauses))
    if any_tools:
        where_bits.append('\n            exists (\n              select 1\n                from tool_results\n              where\n                tool_results.response_id = responses.id\n            )\n        ')
    if tools:
        tools_by_name = get_tools()
        tool_clauses = []
        for i, tool_name in enumerate(tools):
            try:
                plugin_name = tools_by_name[tool_name].plugin
            except KeyError:
                raise click.ClickException(f'Unknown tool: {tool_name}')
            tool_clauses.append(f'\n            exists (\n              select 1\n                from tool_results\n                join tools on tools.id = tool_results.tool_id\n               where tool_results.response_id = responses.id\n                 and tools.name = :tool{i}\n                 and tools.plugin = :plugin{i}\n            )\n            ')
            sql_params[f'tool{i}'] = tool_name
            sql_params[f'plugin{i}'] = plugin_name
        where_bits.append(' and '.join(tool_clauses))
    schema_id = None
    if schema:
        schema_id = make_schema_id(schema)[0]
        where_bits.append('responses.schema_id = :schema_id')
        sql_params['schema_id'] = schema_id
    if where_bits:
        where_ = ' and ' if query else ' where '
        sql_format['extra_where'] = where_ + ' and '.join(where_bits)
    final_sql = sql.format(**sql_format)
    rows = list(db.query(final_sql, sql_params))
    if not query and (not data):
        rows.reverse()
    ids = [row['id'] for row in rows]
    attachments = list(db.query(ATTACHMENTS_SQL.format(','.join('?' * len(ids))), ids))
    attachments_by_id = {}
    for attachment in attachments:
        attachments_by_id.setdefault(attachment['response_id'], []).append(attachment)
    FRAGMENTS_SQL = '\n    select\n        {table}.response_id,\n        fragments.hash,\n        fragments.id as fragment_id,\n        fragments.content,\n        (\n            select json_group_array(fragment_aliases.alias)\n            from fragment_aliases\n            where fragment_aliases.fragment_id = fragments.id\n        ) as aliases\n    from {table}\n    join fragments on {table}.fragment_id = fragments.id\n    where {table}.response_id in ({placeholders})\n    order by {table}."order"\n    '
    prompt_fragments_by_id = {}
    system_fragments_by_id = {}
    for table, dictionary in (('prompt_fragments', prompt_fragments_by_id), ('system_fragments', system_fragments_by_id)):
        for fragment in db.query(FRAGMENTS_SQL.format(placeholders=','.join('?' * len(ids)), table=table), ids):
            dictionary.setdefault(fragment['response_id'], []).append(fragment)
    if data or data_array or data_key or data_ids:
        to_output = []
        for row in rows:
            response = row['response'] or ''
            try:
                decoded = json.loads(response)
                new_items = []
                if isinstance(decoded, dict) and data_key in decoded and all((isinstance(item, dict) for item in decoded[data_key])):
                    for item in decoded[data_key]:
                        new_items.append(item)
                else:
                    new_items.append(decoded)
                if data_ids:
                    for item in new_items:
                        item[find_unused_key(item, 'response_id')] = row['id']
                        item[find_unused_key(item, 'conversation_id')] = row['id']
                to_output.extend(new_items)
            except ValueError:
                pass
        for line in output_rows_as_json(to_output, nl=not data_array, compact=True):
            click.echo(line)
        return
    TOOLS_SQL = "\n    SELECT responses.id,\n    -- Tools related to this response\n    COALESCE(\n        (SELECT json_group_array(json_object(\n            'id', t.id,\n            'hash', t.hash,\n            'name', t.name,\n            'description', t.description,\n            'input_schema', json(t.input_schema)\n        ))\n        FROM tools t\n        JOIN tool_responses tr ON t.id = tr.tool_id\n        WHERE tr.response_id = responses.id\n        ),\n        '[]'\n    ) AS tools,\n    -- Tool calls for this response\n    COALESCE(\n        (SELECT json_group_array(json_object(\n            'id', tc.id,\n            'tool_id', tc.tool_id,\n            'name', tc.name,\n            'arguments', json(tc.arguments),\n            'tool_call_id', tc.tool_call_id\n        ))\n        FROM tool_calls tc\n        WHERE tc.response_id = responses.id\n        ),\n        '[]'\n    ) AS tool_calls,\n    -- Tool results for this response\n    COALESCE(\n        (SELECT json_group_array(json_object(\n            'id', tr.id,\n            'tool_id', tr.tool_id,\n            'name', tr.name,\n            'output', tr.output,\n            'tool_call_id', tr.tool_call_id,\n            'exception', tr.exception,\n            'attachments', COALESCE(\n                (SELECT json_group_array(json_object(\n                    'id', a.id,\n                    'type', a.type,\n                    'path', a.path,\n                    'url', a.url,\n                    'content', a.content\n                ))\n                FROM tool_results_attachments tra\n                JOIN attachments a ON tra.attachment_id = a.id\n                WHERE tra.tool_result_id = tr.id\n                ),\n                '[]'\n            )\n        ))\n        FROM tool_results tr\n        WHERE tr.response_id = responses.id\n        ),\n        '[]'\n    ) AS tool_results\n    FROM responses\n    where id in ({placeholders})\n    "
    tool_info_by_id = {row['id']: {'tools': json.loads(row['tools']), 'tool_calls': json.loads(row['tool_calls']), 'tool_results': json.loads(row['tool_results'])} for row in db.query(TOOLS_SQL.format(placeholders=','.join('?' * len(ids))), ids)}
    for row in rows:
        if truncate:
            row['prompt'] = truncate_string(row['prompt'] or '')
            row['response'] = truncate_string(row['response'] or '')
        for key in ('prompt_fragments', 'system_fragments'):
            row[key] = [{'hash': fragment['hash'], 'content': fragment['content'] if expand else truncate_string(fragment['content']), 'aliases': json.loads(fragment['aliases'])} for fragment in (prompt_fragments_by_id.get(row['id'], []) if key == 'prompt_fragments' else system_fragments_by_id.get(row['id'], []))]
        keys = list(row.keys())
        for key in keys:
            if key.endswith('_json') and row[key] is not None:
                if truncate:
                    del row[key]
                else:
                    row[key] = json.loads(row[key])
        row.update(tool_info_by_id[row['id']])
    output = None
    if json_output:
        for row in rows:
            row['attachments'] = [{k: v for k, v in attachment.items() if k != 'response_id'} for attachment in attachments_by_id.get(row['id'], [])]
        output = json.dumps(list(rows), indent=2)
    elif extract or extract_last:
        for row in rows:
            output = extract_fenced_code_block(row['response'], last=extract_last)
            if output is not None:
                break
    elif response:
        if rows:
            output = rows[-1]['response']
    if output is not None:
        click.echo(output)
    else:
        def _display_fragments(fragments, title):
            if not fragments:
                return
            if not expand:
                content = '\n'.join(['- {}'.format(fragment['hash']) for fragment in fragments])
            else:
                bits = []
                for fragment in fragments:
                    bits.append('<details><summary>{}</summary>\n{}\n</details>'.format(fragment['hash'], maybe_fenced_code(fragment['content'])))
                content = '\n'.join(bits)
            click.echo(f'\n### {title}\n\n{content}')
        current_system = None
        should_show_conversation = True
        for row in rows:
            if short:
                system = truncate_string(row['system'] or '', 120, normalize_whitespace=True)
                prompt = truncate_string(row['prompt'] or '', 120, normalize_whitespace=True, keep_end=True)
                cid = row['conversation_id']
                attachments = attachments_by_id.get(row['id'])
                obj = {'model': row['model'], 'datetime': row['datetime_utc'].split('.')[0], 'conversation': cid}
                if row['tool_calls']:
                    obj['tool_calls'] = ['{}({})'.format(tool_call['name'], json.dumps(tool_call['arguments'])) for tool_call in row['tool_calls']]
                if row['tool_results']:
                    obj['tool_results'] = ['{}: {}'.format(tool_result['name'], truncate_string(tool_result['output'])) for tool_result in row['tool_results']]
                if system:
                    obj['system'] = system
                if prompt:
                    obj['prompt'] = prompt
                if attachments:
                    items = []
                    for attachment in attachments:
                        details = {'type': attachment['type']}
                        if attachment.get('path'):
                            details['path'] = attachment['path']
                        if attachment.get('url'):
                            details['url'] = attachment['url']
                        items.append(details)
                    obj['attachments'] = items
                for key in ('prompt_fragments', 'system_fragments'):
                    obj[key] = [fragment['hash'] for fragment in row[key]]
                if usage and (row['input_tokens'] or row['output_tokens']):
                    usage_details = {'input': row['input_tokens'], 'output': row['output_tokens']}
                    if row['token_details']:
                        usage_details['details'] = json.loads(row['token_details'])
                    obj['usage'] = usage_details
                click.echo(yaml.dump([obj], sort_keys=False).strip())
                continue
            click.echo('# {}{}\n{}'.format(row['datetime_utc'].split('.')[0], '    conversation: {} id: {}'.format(row['conversation_id'], row['id']) if should_show_conversation else '', '\nModel: **{}**{}\n'.format(row['model'], ' (resolved: **{}**)'.format(row['resolved_model']) if row['resolved_model'] else '') if should_show_conversation else ''))
            if conversation_id:
                should_show_conversation = False
            click.echo('## Prompt\n\n{}'.format(row['prompt'] or '-- none --'))
            _display_fragments(row['prompt_fragments'], 'Prompt fragments')
            if row['system'] != current_system:
                if row['system'] is not None:
                    click.echo('\n## System\n\n{}'.format(row['system']))
                current_system = row['system']
            _display_fragments(row['system_fragments'], 'System fragments')
            if row['schema_json']:
                click.echo('\n## Schema\n\n```json\n{}\n```'.format(json.dumps(row['schema_json'], indent=2)))
            if row['tools']:
                click.echo('\n### Tools\n')
                for tool in row['tools']:
                    click.echo('- **{}**: `{}`<br>\n    {}<br>\n    Arguments: {}'.format(tool['name'], tool['hash'], tool['description'], json.dumps(tool['input_schema']['properties'])))
            if row['tool_results']:
                click.echo('\n### Tool results\n')
                for tool_result in row['tool_results']:
                    attachments = ''
                    for attachment in tool_result['attachments']:
                        desc = ''
                        if attachment.get('type'):
                            desc += attachment['type'] + ': '
                        if attachment.get('path'):
                            desc += attachment['path']
                        elif attachment.get('url'):
                            desc += attachment['url']
                        elif attachment.get('content'):
                            desc += f"<{attachment['content_length']:,} bytes>"
                        attachments += '\n    - {}'.format(desc)
                    click.echo('- **{}**: `{}`<br>\n{}{}{}'.format(tool_result['name'], tool_result['tool_call_id'], textwrap.indent(tool_result['output'], '    '), '<br>\n    **Error**: {}\n'.format(tool_result['exception']) if tool_result['exception'] else '', attachments))
            attachments = attachments_by_id.get(row['id'])
            if attachments:
                click.echo('\n### Attachments\n')
                for i, attachment in enumerate(attachments, 1):
                    if attachment['path']:
                        path = attachment['path']
                        click.echo('{}. **{}**: `{}`'.format(i, attachment['type'], path))
                    elif attachment['url']:
                        click.echo('{}. **{}**: {}'.format(i, attachment['type'], attachment['url']))
                    elif attachment['content_length']:
                        click.echo('{}. **{}**: `<{} bytes>`'.format(i, attachment['type'], f"{attachment['content_length']:,}"))
            response = row['response']
            if row['schema_json']:
                try:
                    parsed = json.loads(response)
                    response = '```json\n{}\n```'.format(json.dumps(parsed, indent=2))
                except ValueError:
                    pass
            click.echo('\n## Response\n')
            if row['tool_calls']:
                click.echo('### Tool calls\n')
                for tool_call in row['tool_calls']:
                    click.echo('- **{}**: `{}`<br>\n    Arguments: {}'.format(tool_call['name'], tool_call['tool_call_id'], json.dumps(tool_call['arguments'])))
                click.echo('')
            if response:
                click.echo('{}\n'.format(response))
            if usage:
                token_usage = token_usage_string(row['input_tokens'], row['output_tokens'], json.loads(row['token_details']) if row['token_details'] else None)
                if token_usage:
                    click.echo('## Token usage\n\n{}\n'.format(token_usage))
@cli.group(cls=DefaultGroup, default='list', default_if_no_args=True)
def models():
_type_lookup = {'number': 'float', 'integer': 'int', 'string': 'str', 'object': 'dict'}
@models.command(name='list')
@click.option('--options', is_flag=True, help='Show options for each model, if available')
@click.option('async_', '--async', is_flag=True, help='List async models')
@click.option('--schemas', is_flag=True, help='List models that support schemas')
@click.option('--tools', is_flag=True, help='List models that support tools')
@click.option('-q', '--query', multiple=True, help='Search for models matching these strings')
@click.option('model_ids', '-m', '--model', help='Specific model IDs', multiple=True)
def models_list(options, async_, schemas, tools, query, model_ids):
    models_that_have_shown_options = set()
    for model_with_aliases in get_models_with_aliases():
        if async_ and (not model_with_aliases.async_model):
            continue
        if query:
            if not all((model_with_aliases.matches(q) for q in query)):
                continue
        if model_ids:
            ids_and_aliases = set([model_with_aliases.model.model_id] + model_with_aliases.aliases)
            if not ids_and_aliases.intersection(model_ids):
                continue
        if schemas and (not model_with_aliases.model.supports_schema):
            continue
        if tools and (not model_with_aliases.model.supports_tools):
            continue
        extra_info = []
        if model_with_aliases.aliases:
            extra_info.append('aliases: {}'.format(', '.join(model_with_aliases.aliases)))
        model = model_with_aliases.model if not async_ else model_with_aliases.async_model
        output = str(model)
        if extra_info:
            output += ' ({})'.format(', '.join(extra_info))
        if options and model.Options.model_json_schema()['properties']:
            output += '\n  Options:'
            for name, field in model.Options.model_json_schema()['properties'].items():
                any_of = field.get('anyOf')
                if any_of is None:
                    any_of = [{'type': field.get('type', 'str')}]
                types = ', '.join([_type_lookup.get(item.get('type'), item.get('type', 'str')) for item in any_of if item.get('type') != 'null'])
                bits = ['\n    ', name, ': ', types]
                description = field.get('description', '')
                if description and model.__class__ not in models_that_have_shown_options:
                    wrapped = textwrap.wrap(description, 70)
                    bits.append('\n      ')
                    bits.extend('\n      '.join(wrapped))
                output += ''.join(bits)
            models_that_have_shown_options.add(model.__class__)
        if options and model.attachment_types:
            attachment_types = ', '.join(sorted(model.attachment_types))
            wrapper = textwrap.TextWrapper(width=min(max(shutil.get_terminal_size().columns, 30), 70), initial_indent='    ', subsequent_indent='    ')
            output += '\n  Attachment types:\n{}'.format(wrapper.fill(attachment_types))
        features = [] + (['streaming'] if model.can_stream else []) + (['schemas'] if model.supports_schema else []) + (['tools'] if model.supports_tools else []) + (['async'] if model_with_aliases.async_model else [])
        if options and features:
            output += '\n  Features:\n{}'.format('\n'.join(('  - {}'.format(feature) for feature in features)))
        if options and hasattr(model, 'needs_key') and model.needs_key:
            output += '\n  Keys:'
            if hasattr(model, 'needs_key') and model.needs_key:
                output += '\n    key: {}'.format(model.needs_key)
            if hasattr(model, 'key_env_var') and model.key_env_var:
                output += '\n    env_var: {}'.format(model.key_env_var)
        click.echo(output)
    if not query and (not options) and (not schemas) and (not model_ids):
        click.echo(f'Default: {get_default_model()}')
@models.command(name='default')
@click.argument('model', required=False)
def models_default(model):
    if not model:
        click.echo(get_default_model())
        return
    try:
        model = get_model(model)
        set_default_model(model.model_id)
    except KeyError:
        raise click.ClickException('Unknown model: {}'.format(model))
@cli.group(cls=DefaultGroup, default='list', default_if_no_args=True)
def templates():
@templates.command(name='list')
def templates_list():
    path = template_dir()
    pairs = []
    for file in path.glob('*.yaml'):
        name = file.stem
        try:
            template = load_template(name)
        except LoadTemplateError:
            continue
        text = []
        if template.system:
            text.append(f'system: {template.system}')
            if template.prompt:
                text.append(f' prompt: {template.prompt}')
        else:
            text = [template.prompt if template.prompt else '']
        pairs.append((name, ''.join(text).replace('\n', ' ')))
    try:
        max_name_len = max((len(p[0]) for p in pairs))
    except ValueError:
        return
    else:
        fmt = '{name:<' + str(max_name_len) + '} : {prompt}'
        for name, prompt in sorted(pairs):
            text = fmt.format(name=name, prompt=prompt)
            click.echo(display_truncated(text))
@templates.command(name='show')
@click.argument('name')
def templates_show(name):
    try:
        template = load_template(name)
    except LoadTemplateError:
        raise click.ClickException(f"Template '{name}' not found or invalid")
    click.echo(yaml.dump(dict(((k, v) for k, v in template.model_dump().items() if v is not None)), indent=4, default_flow_style=False))
@templates.command(name='edit')
@click.argument('name')
def templates_edit(name):
    path = template_dir() / f'{name}.yaml'
    if not path.exists():
        path.write_text(DEFAULT_TEMPLATE, 'utf-8')
    click.edit(filename=str(path))
    load_template(name)
@templates.command(name='path')
def templates_path():
    click.echo(template_dir())
@templates.command(name='loaders')
def templates_loaders():
    found = False
    for prefix, loader in get_template_loaders().items():
        found = True
        docs = 'Undocumented'
        if loader.__doc__:
            docs = textwrap.dedent(loader.__doc__).strip()
        click.echo(f'{prefix}:')
        click.echo(textwrap.indent(docs, '  '))
    if not found:
        click.echo('No template loaders found')
@cli.group(cls=DefaultGroup, default='list', default_if_no_args=True)
def schemas():
@schemas.command(name='list')
@click.option('-p', '--path', type=click.Path(readable=True, exists=True, dir_okay=False), help='Path to log database', hidden=True)
@click.option('-d', '--database', type=click.Path(readable=True, exists=True, dir_okay=False), help='Path to log database')
@click.option('queries', '-q', '--query', multiple=True, help='Search for schemas matching this string')
@click.option('--full', is_flag=True, help='Output full schema contents')
@click.option('json_', '--json', is_flag=True, help='Output as JSON')
@click.option('nl', '--nl', is_flag=True, help='Output as newline-delimited JSON')
def schemas_list(path, database, queries, full, json_, nl):
    if database and (not path):
        path = database
    path = pathlib.Path(path or logs_db_path())
    if not path.exists():
        raise click.ClickException('No log database found at {}'.format(path))
    db = sqlite_utils.Database(path)
    migrate(db)
    params = []
    where_sql = ''
    if queries:
        where_bits = ['schemas.content like ?' for _ in queries]
        where_sql += ' where {}'.format(' and '.join(where_bits))
        params.extend(('%{}%'.format(q) for q in queries))
    sql = '\n    select\n      schemas.id,\n      schemas.content,\n      max(responses.datetime_utc) as recently_used,\n      count(*) as times_used\n    from schemas\n    join responses\n      on responses.schema_id = schemas.id\n    {} group by responses.schema_id\n    order by recently_used\n    '.format(where_sql)
    rows = db.query(sql, params)
    if json_ or nl:
        for line in output_rows_as_json(rows, json_cols={'content'}, nl=nl):
            click.echo(line)
        return
    for row in rows:
        click.echo('- id: {}'.format(row['id']))
        if full:
            click.echo('  schema: |\n{}'.format(textwrap.indent(json.dumps(json.loads(row['content']), indent=2), '    ')))
        else:
            click.echo('  summary: |\n    {}'.format(schema_summary(json.loads(row['content']))))
        click.echo('  usage: |\n    {} time{}, most recently {}'.format(row['times_used'], 's' if row['times_used'] != 1 else '', row['recently_used']))
@schemas.command(name='show')
@click.argument('schema_id')
@click.option('-p', '--path', type=click.Path(readable=True, exists=True, dir_okay=False), help='Path to log database', hidden=True)
@click.option('-d', '--database', type=click.Path(readable=True, exists=True, dir_okay=False), help='Path to log database')
def schemas_show(schema_id, path, database):
    if database and (not path):
        path = database
    path = pathlib.Path(path or logs_db_path())
    if not path.exists():
        raise click.ClickException('No log database found at {}'.format(path))
    db = sqlite_utils.Database(path)
    migrate(db)
    try:
        row = db['schemas'].get(schema_id)
    except sqlite_utils.db.NotFoundError:
        raise click.ClickException('Invalid schema ID')
    click.echo(json.dumps(json.loads(row['content']), indent=2))
@schemas.command(name='dsl')
@click.argument('input')
@click.option('--multi', is_flag=True, help='Wrap in an array')
def schemas_dsl_debug(input, multi):
    schema = schema_dsl(input, multi)
    click.echo(json.dumps(schema, indent=2))
@cli.group(cls=DefaultGroup, default='list', default_if_no_args=True)
def tools():
@tools.command(name='list')
@click.argument('tool_defs', nargs=-1)
@click.option('json_', '--json', is_flag=True, help='Output as JSON')
@click.option('python_tools', '--functions', help='Python code block or file path defining functions to register as tools', multiple=True)
def tools_list(tool_defs, json_, python_tools):
    def introspect_tools(toolbox_class):
        methods = []
        for tool in toolbox_class.method_tools():
            methods.append({'name': tool.name, 'description': tool.description, 'arguments': tool.input_schema, 'implementation': tool.implementation})
        return methods
    if tool_defs:
        tools = {}
        for tool in _gather_tools(tool_defs, python_tools):
            if hasattr(tool, 'name'):
                tools[tool.name] = tool
            else:
                tools[tool.__class__.__name__] = tool
    else:
        tools = get_tools()
        if python_tools:
            for code_or_path in python_tools:
                for tool in _tools_from_code(code_or_path):
                    tools[tool.name] = tool
    output_tools = []
    output_toolboxes = []
    tool_objects = []
    toolbox_objects = []
    for name, tool in sorted(tools.items()):
        if isinstance(tool, Tool):
            tool_objects.append(tool)
            output_tools.append({'name': name, 'description': tool.description, 'arguments': tool.input_schema, 'plugin': tool.plugin})
        else:
            toolbox_objects.append(tool)
            output_toolboxes.append({'name': name, 'tools': [{'name': tool['name'], 'description': tool['description'], 'arguments': tool['arguments']} for tool in introspect_tools(tool)]})
    if json_:
        click.echo(json.dumps({'tools': output_tools, 'toolboxes': output_toolboxes}, indent=2))
    else:
        for tool in tool_objects:
            sig = '()'
            if tool.implementation:
                sig = str(inspect.signature(tool.implementation))
            click.echo('{}{}{}\n'.format(tool.name, sig, ' (plugin: {})'.format(tool.plugin) if tool.plugin else ''))
            if tool.description:
                click.echo(textwrap.indent(tool.description.strip(), '  ') + '\n')
        for toolbox in toolbox_objects:
            click.echo(toolbox.name + ':\n')
            for tool in toolbox.method_tools():
                sig = str(inspect.signature(tool.implementation)).replace('(self, ', '(').replace('(self)', '()')
                click.echo('  {}{}\n'.format(tool.name, sig))
                if tool.description:
                    click.echo(textwrap.indent(tool.description.strip(), '    ') + '\n')
@cli.group(cls=DefaultGroup, default='list', default_if_no_args=True)
def aliases():
@aliases.command(name='list')
@click.option('json_', '--json', is_flag=True, help='Output as JSON')
def aliases_list(json_):
    to_output = []
    for alias, model in get_model_aliases().items():
        if alias != model.model_id:
            to_output.append((alias, model.model_id, ''))
    for alias, embedding_model in get_embedding_model_aliases().items():
        if alias != embedding_model.model_id:
            to_output.append((alias, embedding_model.model_id, 'embedding'))
    if json_:
        click.echo(json.dumps({key: value for key, value, type_ in to_output}, indent=4))
        return
    max_alias_length = max((len(a) for a, _, _ in to_output))
    fmt = '{alias:<' + str(max_alias_length) + '} : {model_id}{type_}'
    for alias, model_id, type_ in to_output:
        click.echo(fmt.format(alias=alias, model_id=model_id, type_=f' ({type_})' if type_ else ''))
@aliases.command(name='set')
@click.argument('alias')
@click.argument('model_id', required=False)
@click.option('-q', '--query', multiple=True, help='Set alias for model matching these strings')
def aliases_set(alias, model_id, query):
    if not model_id:
        if not query:
            raise click.ClickException('You must provide a model_id or at least one -q option')
        found = None
        for model_with_aliases in get_models_with_aliases():
            if all((model_with_aliases.matches(q) for q in query)):
                found = model_with_aliases
                break
        if not found:
            raise click.ClickException('No model found matching query: ' + ', '.join(query))
        model_id = found.model.model_id
        set_alias(alias, model_id)
        click.echo(f"Alias '{alias}' set to model '{model_id}'", err=True)
    else:
        set_alias(alias, model_id)
@aliases.command(name='remove')
@click.argument('alias')
def aliases_remove(alias):
    try:
        remove_alias(alias)
    except KeyError as ex:
        raise click.ClickException(ex.args[0])
@aliases.command(name='path')
def aliases_path():
    click.echo(user_dir() / 'aliases.json')
@cli.group(cls=DefaultGroup, default='list', default_if_no_args=True)
def fragments():
@fragments.command(name='list')
@click.option('queries', '-q', '--query', multiple=True, help='Search for fragments matching these strings')
@click.option('--aliases', is_flag=True, help='Show only fragments with aliases')
@click.option('json_', '--json', is_flag=True, help='Output as JSON')
def fragments_list(queries, aliases, json_):
    db = sqlite_utils.Database(logs_db_path())
    migrate(db)
    params = {}
    param_count = 0
    where_bits = []
    if aliases:
        where_bits.append('fragment_aliases.alias is not null')
    for q in queries:
        param_count += 1
        p = f'p{param_count}'
        params[p] = q
        where_bits.append(f"\n            (fragments.hash = :{p} or fragment_aliases.alias = :{p}\n            or fragments.source like '%' || :{p} || '%'\n            or fragments.content like '%' || :{p} || '%')\n        ")
    where = '\n      and\n  '.join(where_bits)
    if where:
        where = ' where ' + where
    sql = '\n    select\n        fragments.hash,\n        json_group_array(fragment_aliases.alias) filter (\n            where\n            fragment_aliases.alias is not null\n        ) as aliases,\n        fragments.datetime_utc,\n        fragments.source,\n        fragments.content\n    from\n        fragments\n    left join\n        fragment_aliases on fragment_aliases.fragment_id = fragments.id\n    {where}\n    group by\n        fragments.id, fragments.hash, fragments.content, fragments.datetime_utc, fragments.source\n    order by fragments.datetime_utc\n    '.format(where=where)
    results = list(db.query(sql, params))
    for result in results:
        result['aliases'] = json.loads(result['aliases'])
    if json_:
        click.echo(json.dumps(results, indent=4))
    else:
        yaml.add_representer(str, lambda dumper, data: dumper.represent_scalar('tag:yaml.org,2002:str', data, style='|' if '\n' in data else None))
        for result in results:
            result['content'] = truncate_string(result['content'])
            click.echo(yaml.dump([result], sort_keys=False, width=sys.maxsize).strip())
@fragments.command(name='set')
@click.argument('alias', callback=validate_fragment_alias)
@click.argument('fragment')
def fragments_set(alias, fragment):
    db = sqlite_utils.Database(logs_db_path())
    migrate(db)
    try:
        resolved = resolve_fragments(db, [fragment])[0]
    except FragmentNotFound as ex:
        raise click.ClickException(str(ex))
    migrate(db)
    alias_sql = '\n    insert into fragment_aliases (alias, fragment_id)\n    values (:alias, :fragment_id)\n    on conflict(alias) do update set\n        fragment_id = excluded.fragment_id;\n    '
    with db.conn:
        fragment_id = ensure_fragment(db, resolved)
        db.conn.execute(alias_sql, {'alias': alias, 'fragment_id': fragment_id})
@fragments.command(name='show')
@click.argument('alias_or_hash')
def fragments_show(alias_or_hash):
    db = sqlite_utils.Database(logs_db_path())
    migrate(db)
    try:
        resolved = resolve_fragments(db, [alias_or_hash])[0]
    except FragmentNotFound as ex:
        raise click.ClickException(str(ex))
    click.echo(resolved)
@fragments.command(name='remove')
@click.argument('alias', callback=validate_fragment_alias)
def fragments_remove(alias):
    db = sqlite_utils.Database(logs_db_path())
    migrate(db)
    with db.conn:
        db.conn.execute('delete from fragment_aliases where alias = :alias', {'alias': alias})
@fragments.command(name='loaders')
def fragments_loaders():
    from llm import get_fragment_loaders
    found = False
    for prefix, loader in get_fragment_loaders().items():
        if found:
            click.echo('')
        found = True
        docs = 'Undocumented'
        if loader.__doc__:
            docs = textwrap.dedent(loader.__doc__).strip()
        click.echo(f'{prefix}:')
        click.echo(textwrap.indent(docs, '  '))
    if not found:
        click.echo('No fragment loaders found')
@cli.command(name='plugins')
@click.option('--all', help='Include built-in default plugins', is_flag=True)
@click.option('hooks', '--hook', help='Filter for plugins that implement this hook', multiple=True)
def plugins_list(all, hooks):
    plugins = get_plugins(all)
    hooks = set(hooks)
    if hooks:
        plugins = [plugin for plugin in plugins if hooks.intersection(plugin['hooks'])]
    click.echo(json.dumps(plugins, indent=2))
def display_truncated(text):
    console_width = shutil.get_terminal_size()[0]
    if len(text) > console_width:
        return text[:console_width - 3] + '...'
    else:
        return text
@cli.command()
@click.argument('packages', nargs=-1, required=False)
@click.option('-U', '--upgrade', is_flag=True, help='Upgrade packages to latest version')
@click.option('-e', '--editable', help='Install a project in editable mode from this path')
@click.option('--force-reinstall', is_flag=True, help='Reinstall all packages even if they are already up-to-date')
@click.option('--no-cache-dir', is_flag=True, help='Disable the cache')
@click.option('--pre', is_flag=True, help='Include pre-release and development versions')
def install(packages, upgrade, editable, force_reinstall, no_cache_dir, pre):
    args = ['pip', 'install']
    if upgrade:
        args += ['--upgrade']
    if editable:
        args += ['--editable', editable]
    if force_reinstall:
        args += ['--force-reinstall']
    if no_cache_dir:
        args += ['--no-cache-dir']
    if pre:
        args += ['--pre']
    args += list(packages)
    sys.argv = args
    run_module('pip', run_name='__main__')
@cli.command()
@click.argument('packages', nargs=-1, required=True)
@click.option('-y', '--yes', is_flag=True, help="Don't ask for confirmation")
def uninstall(packages, yes):
    sys.argv = ['pip', 'uninstall'] + list(packages) + (['-y'] if yes else [])
    run_module('pip', run_name='__main__')
@cli.command()
@click.argument('collection', required=False)
@click.argument('id', required=False)
@click.option('-i', '--input', type=click.Path(exists=True, readable=True, allow_dash=True), help='File to embed')
@click.option('-m', '--model', help='Embedding model to use', envvar='LLM_EMBEDDING_MODEL')
@click.option('--store', is_flag=True, help='Store the text itself in the database')
@click.option('-d', '--database', type=click.Path(file_okay=True, allow_dash=False, dir_okay=False, writable=True), envvar='LLM_EMBEDDINGS_DB')
@click.option('-c', '--content', help='Content to embed')
@click.option('--binary', is_flag=True, help='Treat input as binary data')
@click.option('--metadata', help='JSON object metadata to store', callback=json_validator('metadata'))
@click.option('format_', '-f', '--format', type=click.Choice(['json', 'blob', 'base64', 'hex']), help='Output format')
def embed(collection, id, input, model, store, database, content, binary, metadata, format_):
    if collection and (not id):
        raise click.ClickException('Must provide both collection and id')
    if store and (not collection):
        raise click.ClickException('Must provide collection when using --store')
    def get_db():
        if database:
            return sqlite_utils.Database(database)
        else:
            return sqlite_utils.Database(user_dir() / 'embeddings.db')
    collection_obj = None
    model_obj = None
    if collection:
        db = get_db()
        if Collection.exists(db, collection):
            collection_obj = Collection(collection, db)
            model_obj = collection_obj.model()
        else:
            if not model:
                model = get_default_embedding_model()
                if model is None:
                    raise click.ClickException('You need to specify an embedding model (no default model is set)')
            collection_obj = Collection(collection, db=db, model_id=model)
            model_obj = collection_obj.model()
    if model_obj is None:
        if model is None:
            model = get_default_embedding_model()
        try:
            model_obj = get_embedding_model(model)
        except UnknownModelError:
            raise click.ClickException('You need to specify an embedding model (no default model is set)')
    show_output = True
    if collection and format_ is None:
        show_output = False
    if not content:
        if not input or input == '-':
            input_source = sys.stdin.buffer if binary else sys.stdin
            content = input_source.read()
        else:
            mode = 'rb' if binary else 'r'
            with open(input, mode) as f:
                content = f.read()
    if not content:
        raise click.ClickException('No content provided')
    if collection_obj:
        embedding = collection_obj.embed(id, content, metadata=metadata, store=store)
    else:
        embedding = model_obj.embed(content)
    if show_output:
        if format_ == 'json' or format_ is None:
            click.echo(json.dumps(embedding))
        elif format_ == 'blob':
            click.echo(encode(embedding))
        elif format_ == 'base64':
            click.echo(base64.b64encode(encode(embedding)).decode('ascii'))
        elif format_ == 'hex':
            click.echo(encode(embedding).hex())
@cli.command()
@click.argument('collection')
@click.argument('input_path', type=click.Path(exists=True, dir_okay=False, allow_dash=True, readable=True), required=False)
@click.option('--format', type=click.Choice(['json', 'csv', 'tsv', 'nl']), help='Format of input file - defaults to auto-detect')
@click.option('--files', type=(click.Path(file_okay=False, dir_okay=True, allow_dash=False), str), multiple=True, help='Embed files in this directory - specify directory and glob pattern')
@click.option('encodings', '--encoding', help='Encodings to try when reading --files', multiple=True)
@click.option('--binary', is_flag=True, help='Treat --files as binary data')
@click.option('--sql', help='Read input using this SQL query')
@click.option('--attach', type=(str, click.Path(file_okay=True, dir_okay=False, allow_dash=False)), multiple=True, help='Additional databases to attach - specify alias and file path')
@click.option('--batch-size', type=int, help='Batch size to use when running embeddings')
@click.option('--prefix', help='Prefix to add to the IDs', default='')
@click.option('-m', '--model', help='Embedding model to use', envvar='LLM_EMBEDDING_MODEL')
@click.option('--prepend', help='Prepend this string to all content before embedding')
@click.option('--store', is_flag=True, help='Store the text itself in the database')
@click.option('-d', '--database', type=click.Path(file_okay=True, allow_dash=False, dir_okay=False, writable=True), envvar='LLM_EMBEDDINGS_DB')
def embed_multi(collection, input_path, format, files, encodings, binary, sql, attach, batch_size, prefix, model, prepend, store, database):
    if binary and (not files):
        raise click.UsageError('--binary must be used with --files')
    if binary and encodings:
        raise click.UsageError('--binary cannot be used with --encoding')
    if not input_path and (not sql) and (not files):
        raise click.UsageError('Either --sql or input path or --files is required')
    if files:
        if input_path or sql or format:
            raise click.UsageError('Cannot use --files with --sql, input path or --format')
    if database:
        db = sqlite_utils.Database(database)
    else:
        db = sqlite_utils.Database(user_dir() / 'embeddings.db')
    for alias, attach_path in attach:
        db.attach(alias, attach_path)
    try:
        collection_obj = Collection(collection, db=db, model_id=model or get_default_embedding_model())
    except ValueError:
        raise click.ClickException('You need to specify an embedding model (no default model is set)')
    expected_length = None
    if files:
        encodings = encodings or ('utf-8', 'latin-1')
        def count_files():
            i = 0
            for directory, pattern in files:
                for path in pathlib.Path(directory).glob(pattern):
                    i += 1
            return i
        def iterate_files():
            for directory, pattern in files:
                p = pathlib.Path(directory)
                if not p.exists() or not p.is_dir():
                    raise click.UsageError(f'Invalid directory: {directory}')
                for path in pathlib.Path(directory).glob(pattern):
                    if path.is_dir():
                        continue
                    relative = path.relative_to(directory)
                    content = None
                    if binary:
                        content = path.read_bytes()
                    else:
                        for encoding in encodings:
                            try:
                                content = path.read_text(encoding=encoding)
                            except UnicodeDecodeError:
                                continue
                    if content is None:
                        click.echo('Could not decode text in file {}'.format(path), err=True)
                    else:
                        yield {'id': str(relative), 'content': content}
        expected_length = count_files()
        rows = iterate_files()
    elif sql:
        rows = db.query(sql)
        count_sql = 'select count(*) as c from ({})'.format(sql)
        expected_length = next(db.query(count_sql))['c']
    else:
        def load_rows(fp):
            return rows_from_file(fp, Format[format.upper()] if format else None)[0]
        try:
            if input_path != '-':
                expected_length = 0
                with open(input_path, 'rb') as fp:
                    for _ in load_rows(fp):
                        expected_length += 1
            rows = load_rows(open(input_path, 'rb') if input_path != '-' else io.BufferedReader(sys.stdin.buffer))
        except json.JSONDecodeError as ex:
            raise click.ClickException(str(ex))
    with click.progressbar(rows, label='Embedding', show_percent=True, length=expected_length) as rows:
        def tuples() -> Iterable[Tuple[str, Union[bytes, str]]]:
            for row in rows:
                values = list(row.values())
                id: str = prefix + str(values[0])
                content: Optional[Union[bytes, str]] = None
                if binary:
                    content = cast(bytes, values[1])
                else:
                    content = ' '.join((v or '' for v in values[1:]))
                if prepend and isinstance(content, str):
                    content = prepend + content
                yield (id, content or '')
        embed_kwargs = {'store': store}
        if batch_size:
            embed_kwargs['batch_size'] = batch_size
        collection_obj.embed_multi(tuples(), **embed_kwargs)
@cli.command()
@click.argument('collection')
@click.argument('id', required=False)
@click.option('-i', '--input', type=click.Path(exists=True, readable=True, allow_dash=True), help='File to embed for comparison')
@click.option('-c', '--content', help='Content to embed for comparison')
@click.option('--binary', is_flag=True, help='Treat input as binary data')
@click.option('-n', '--number', type=int, default=10, help='Number of results to return')
@click.option('-p', '--plain', is_flag=True, help='Output in plain text format')
@click.option('-d', '--database', type=click.Path(file_okay=True, allow_dash=False, dir_okay=False, writable=True), envvar='LLM_EMBEDDINGS_DB')
@click.option('--prefix', help='Just IDs with this prefix', default='')
def similar(collection, id, input, content, binary, number, plain, database, prefix):
    if not id and (not content) and (not input):
        raise click.ClickException('Must provide content or an ID for the comparison')
    if database:
        db = sqlite_utils.Database(database)
    else:
        db = sqlite_utils.Database(user_dir() / 'embeddings.db')
    if not db['embeddings'].exists():
        raise click.ClickException('No embeddings table found in database')
    try:
        collection_obj = Collection(collection, db, create=False)
    except Collection.DoesNotExist:
        raise click.ClickException('Collection does not exist')
    if id:
        try:
            results = collection_obj.similar_by_id(id, number, prefix=prefix)
        except Collection.DoesNotExist:
            raise click.ClickException('ID not found in collection')
    else:
        if not content:
            if not input or input == '-':
                input_source = sys.stdin.buffer if binary else sys.stdin
                content = input_source.read()
            else:
                mode = 'rb' if binary else 'r'
                with open(input, mode) as f:
                    content = f.read()
        if not content:
            raise click.ClickException('No content provided')
        results = collection_obj.similar(content, number, prefix=prefix)
    for result in results:
        if plain:
            click.echo(f'{result.id} ({result.score})\n')
            if result.content:
                click.echo(textwrap.indent(result.content, '  '))
            if result.metadata:
                click.echo(textwrap.indent(json.dumps(result.metadata), '  '))
            click.echo('')
        else:
            click.echo(json.dumps(asdict(result)))
@cli.group(cls=DefaultGroup, default='list', default_if_no_args=True)
def embed_models():
@embed_models.command(name='list')
@click.option('-q', '--query', multiple=True, help='Search for embedding models matching these strings')
def embed_models_list(query):
    output = []
    for model_with_aliases in get_embedding_models_with_aliases():
        if query:
            if not all((model_with_aliases.matches(q) for q in query)):
                continue
        s = str(model_with_aliases.model)
        if model_with_aliases.aliases:
            s += ' (aliases: {})'.format(', '.join(model_with_aliases.aliases))
        output.append(s)
    click.echo('\n'.join(output))
@embed_models.command(name='default')
@click.argument('model', required=False)
@click.option('--remove-default', is_flag=True, help='Reset to specifying no default model')
def embed_models_default(model, remove_default):
    if not model and (not remove_default):
        default = get_default_embedding_model()
        if default is None:
            click.echo('<No default embedding model set>', err=True)
        else:
            click.echo(default)
        return
    try:
        if remove_default:
            set_default_embedding_model(None)
        else:
            model = get_embedding_model(model)
            set_default_embedding_model(model.model_id)
    except KeyError:
        raise click.ClickException('Unknown embedding model: {}'.format(model))
@cli.group(cls=DefaultGroup, default='list', default_if_no_args=True)
def collections():
@collections.command(name='path')
def collections_path():
    click.echo(user_dir() / 'embeddings.db')
@collections.command(name='list')
@click.option('-d', '--database', type=click.Path(file_okay=True, allow_dash=False, dir_okay=False, writable=True), envvar='LLM_EMBEDDINGS_DB', help='Path to embeddings database')
@click.option('json_', '--json', is_flag=True, help='Output as JSON')
def embed_db_collections(database, json_):
    database = database or user_dir() / 'embeddings.db'
    db = sqlite_utils.Database(str(database))
    if not db['collections'].exists():
        raise click.ClickException('No collections table found in {}'.format(database))
    rows = db.query('\n    select\n        collections.name,\n        collections.model,\n        count(embeddings.id) as num_embeddings\n    from\n        collections left join embeddings\n        on collections.id = embeddings.collection_id\n    group by\n        collections.name, collections.model\n    ')
    if json_:
        click.echo(json.dumps(list(rows), indent=4))
    else:
        for row in rows:
            click.echo('{}: {}'.format(row['name'], row['model']))
            click.echo('  {} embedding{}'.format(row['num_embeddings'], 's' if row['num_embeddings'] != 1 else ''))
@collections.command(name='delete')
@click.argument('collection')
@click.option('-d', '--database', type=click.Path(file_okay=True, allow_dash=False, dir_okay=False, writable=True), envvar='LLM_EMBEDDINGS_DB', help='Path to embeddings database')
def collections_delete(collection, database):
    database = database or user_dir() / 'embeddings.db'
    db = sqlite_utils.Database(str(database))
    try:
        collection_obj = Collection(collection, db, create=False)
    except Collection.DoesNotExist:
        raise click.ClickException('Collection does not exist')
    collection_obj.delete()
@models.group(cls=DefaultGroup, default='list', default_if_no_args=True)
def options():
@options.command(name='list')
def options_list():
    options = get_all_model_options()
    if not options:
        click.echo('No default options set for any models.', err=True)
        return
    for model_id, model_options in options.items():
        click.echo(f'{model_id}:')
        for key, value in model_options.items():
            click.echo(f'  {key}: {value}')
@options.command(name='show')
@click.argument('model')
def options_show(model):
    import llm
    try:
        model_obj = llm.get_model(model)
        model_id = model_obj.model_id
    except llm.UnknownModelError:
        model_id = model
    options = get_model_options(model_id)
    if not options:
        click.echo(f"No default options set for model '{model_id}'.", err=True)
        return
    for key, value in options.items():
        click.echo(f'{key}: {value}')
@options.command(name='set')
@click.argument('model')
@click.argument('key')
@click.argument('value')
def options_set(model, key, value):
    import llm
    try:
        model_obj = llm.get_model(model)
        model_id = model_obj.model_id
        try:
            test_options = {key: value}
            model_obj.Options(**test_options)
        except pydantic.ValidationError as ex:
            raise click.ClickException(render_errors(ex.errors()))
    except llm.UnknownModelError:
        model_id = model
    set_model_option(model_id, key, value)
    click.echo(f'Set default option {key}={value} for model {model_id}', err=True)
@options.command(name='clear')
@click.argument('model')
@click.argument('key', required=False)
def options_clear(model, key):
    import llm
    try:
        model_obj = llm.get_model(model)
        model_id = model_obj.model_id
    except llm.UnknownModelError:
        model_id = model
    cleared_keys = []
    if not key:
        cleared_keys = list(get_model_options(model_id).keys())
        for key_ in cleared_keys:
            clear_model_option(model_id, key_)
    else:
        cleared_keys.append(key)
        clear_model_option(model_id, key)
    if cleared_keys:
        if len(cleared_keys) == 1:
            click.echo(f"Cleared option '{cleared_keys[0]}' for model {model_id}")
        else:
            click.echo(f"Cleared {', '.join(cleared_keys)} options for model {model_id}")
def template_dir():
    path = user_dir() / 'templates'
    path.mkdir(parents=True, exist_ok=True)
    return path
def logs_db_path():
    return user_dir() / 'logs.db'
def get_history(chat_id):
    if chat_id is None:
        return (None, [])
    log_path = logs_db_path()
    db = sqlite_utils.Database(log_path)
    migrate(db)
    if chat_id == -1:
        last_row = list(db['logs'].rows_where(order_by='-id', limit=1))
        if last_row:
            chat_id = last_row[0].get('chat_id') or last_row[0].get('id')
        else:
            return (None, [])
    rows = db['logs'].rows_where('id = ? or chat_id = ?', [chat_id, chat_id], order_by='id')
    return (chat_id, rows)
def render_errors(errors):
    output = []
    for error in errors:
        output.append(', '.join(error['loc']))
        output.append('  ' + error['msg'])
    return '\n'.join(output)
load_plugins()
pm.hook.register_commands(cli=cli)
def _human_readable_size(size_bytes):
    if size_bytes == 0:
        return '0B'
    size_name = ('B', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB')
    i = 0
    while size_bytes >= 1024 and i < len(size_name) - 1:
        size_bytes /= 1024.0
        i += 1
    return '{:.2f}{}'.format(size_bytes, size_name[i])
def logs_on():
    return not (user_dir() / 'logs-off').exists()
def get_all_model_options() -> dict:
    path = user_dir() / 'model_options.json'
    if not path.exists():
        return {}
    try:
        options = json.loads(path.read_text())
    except json.JSONDecodeError:
        return {}
    return options
def get_model_options(model_id: str) -> dict:
    path = user_dir() / 'model_options.json'
    if not path.exists():
        return {}
    try:
        options = json.loads(path.read_text())
    except json.JSONDecodeError:
        return {}
    return options.get(model_id, {})
def set_model_option(model_id: str, key: str, value: Any) -> None:
    path = user_dir() / 'model_options.json'
    if path.exists():
        try:
            options = json.loads(path.read_text())
        except json.JSONDecodeError:
            options = {}
    else:
        options = {}
    if model_id not in options:
        options[model_id] = {}
    options[model_id][key] = value
    path.write_text(json.dumps(options, indent=2))
def clear_model_option(model_id: str, key: str) -> None:
    path = user_dir() / 'model_options.json'
    if not path.exists():
        return
    try:
        options = json.loads(path.read_text())
    except json.JSONDecodeError:
        return
    if model_id not in options:
        return
    if key in options[model_id]:
        del options[model_id][key]
        if not options[model_id]:
            del options[model_id]
    path.write_text(json.dumps(options, indent=2))
class LoadTemplateError(ValueError):
    pass
def _parse_yaml_template(name, content):
    try:
        loaded = yaml.safe_load(content)
    except yaml.YAMLError as ex:
        raise LoadTemplateError('Invalid YAML: {}'.format(str(ex)))
    if isinstance(loaded, str):
        return Template(name=name, prompt=loaded)
    loaded['name'] = name
    try:
        return Template(**loaded)
    except pydantic.ValidationError as ex:
        msg = 'A validation error occurred:\n'
        msg += render_errors(ex.errors())
        raise LoadTemplateError(msg)
def load_template(name: str) -> Template:
    if name.startswith('https://') or name.startswith('http://'):
        response = httpx.get(name)
        try:
            response.raise_for_status()
        except httpx.HTTPStatusError as ex:
            raise LoadTemplateError('Could not load template {}: {}'.format(name, ex))
        return _parse_yaml_template(name, response.text)
    potential_path = pathlib.Path(name)
    if has_plugin_prefix(name) and (not potential_path.exists()):
        prefix, rest = name.split(':', 1)
        loaders = get_template_loaders()
        if prefix not in loaders:
            raise LoadTemplateError('Unknown template prefix: {}'.format(prefix))
        loader = loaders[prefix]
        try:
            return loader(rest)
        except Exception as ex:
            raise LoadTemplateError('Could not load template {}: {}'.format(name, ex))
    if potential_path.exists():
        path = potential_path
    else:
        path = template_dir() / f'{name}.yaml'
    if not path.exists():
        raise LoadTemplateError(f'Invalid template: {name}')
    content = path.read_text()
    template_obj = _parse_yaml_template(name, content)
    template_obj._functions_is_trusted = True
    return template_obj
def _tools_from_code(code_or_path: str) -> List[Tool]:
    if '\n' not in code_or_path and code_or_path.endswith('.py'):
        try:
            code_or_path = pathlib.Path(code_or_path).read_text()
        except FileNotFoundError:
            raise click.ClickException('File not found: {}'.format(code_or_path))
    namespace: Dict[str, Any] = {}
    tools = []
    try:
        exec(code_or_path, namespace)
    except SyntaxError as ex:
        raise click.ClickException('Error in --functions definition: {}'.format(ex))
    for name, value in namespace.items():
        if callable(value) and (not name.startswith('_')):
            tools.append(Tool.function(value))
    return tools
def _debug_tool_call(_, tool_call, tool_result):
    click.echo(click.style('\nTool call: {}({})'.format(tool_call.name, tool_call.arguments), fg='yellow', bold=True), err=True)
    output = ''
    attachments = ''
    if tool_result.attachments:
        attachments += '\nAttachments:\n'
        for attachment in tool_result.attachments:
            attachments += f'  {repr(attachment)}\n'
    try:
        output = json.dumps(json.loads(tool_result.output), indent=2)
    except ValueError:
        output = tool_result.output
    output += attachments
    click.echo(click.style(textwrap.indent(output, '  ') + ('\n' if not tool_result.exception else ''), fg='green', bold=True), err=True)
    if tool_result.exception:
        click.echo(click.style('  Exception: {}'.format(tool_result.exception), fg='red', bold=True), err=True)
def _approve_tool_call(_, tool_call):
    click.echo(click.style('Tool call: {}({})'.format(tool_call.name, tool_call.arguments), fg='yellow', bold=True), err=True)
    if not click.confirm('Approve tool call?'):
        raise CancelToolCall('User cancelled tool call')
def _gather_tools(tool_specs: List[str], python_tools: List[str]) -> List[Union[Tool, Type[Toolbox]]]:
    tools: List[Union[Tool, Type[Toolbox]]] = []
    if python_tools:
        for code_or_path in python_tools:
            tools.extend(_tools_from_code(code_or_path))
    registered_tools = get_tools()
    registered_classes = dict(((key, value) for key, value in registered_tools.items() if inspect.isclass(value)))
    bad_tools = [tool for tool in tool_specs if tool.split('(')[0] not in registered_tools]
    if bad_tools:
        raise click.ClickException('Tool(s) {} not found. Available tools: {}'.format(', '.join(bad_tools), ', '.join(registered_tools.keys())))
    for tool_spec in tool_specs:
        if not tool_spec[0].isupper():
            tools.append(registered_tools[tool_spec])
        else:
            tools.append(instantiate_from_spec(registered_classes, tool_spec))
    return tools
def _get_conversation_tools(conversation, tools):
    if conversation and (not tools) and conversation.responses:
        initial_tools = conversation.responses[0].prompt.tools
        if initial_tools:
            return [tool.name for tool in initial_tools if tool.plugin]