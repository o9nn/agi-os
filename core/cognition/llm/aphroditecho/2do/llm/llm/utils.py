import click
import hashlib
import httpx
import itertools
import json
import pathlib
import puremagic
import re
import sqlite_utils
import textwrap
from typing import Any, List, Dict, Optional, Tuple, Type
import os
import threading
import time
from typing import Final
from ulid import ULID
MIME_TYPE_FIXES = {'audio/wave': 'audio/wav'}
class Fragment(str):
    def __new__(cls, content, *args, **kwargs):
        return super().__new__(cls, content)
    def __init__(self, content, source=''):
        self.source = source
    def id(self):
        return hashlib.sha256(self.encode('utf-8')).hexdigest()
def mimetype_from_string(content) -> Optional[str]:
    try:
        type_ = puremagic.from_string(content, mime=True)
        return MIME_TYPE_FIXES.get(type_, type_)
    except puremagic.PureError:
        return None
def mimetype_from_path(path) -> Optional[str]:
    try:
        type_ = puremagic.from_file(path, mime=True)
        return MIME_TYPE_FIXES.get(type_, type_)
    except puremagic.PureError:
        return None
def dicts_to_table_string(headings: List[str], dicts: List[Dict[str, str]]) -> List[str]:
    max_lengths = [len(h) for h in headings]
    for d in dicts:
        for i, h in enumerate(headings):
            if h in d and len(str(d[h])) > max_lengths[i]:
                max_lengths[i] = len(str(d[h]))
    res = []
    res.append('    '.join((h.ljust(max_lengths[i]) for i, h in enumerate(headings))))
    for d in dicts:
        row = []
        for i, h in enumerate(headings):
            row.append(str(d.get(h, '')).ljust(max_lengths[i]))
        res.append('    '.join(row))
    return res
def remove_dict_none_values(d):
    if not isinstance(d, dict):
        return d
    new_dict = {}
    for key, value in d.items():
        if value is not None:
            if isinstance(value, dict):
                nested = remove_dict_none_values(value)
                if nested:
                    new_dict[key] = nested
            elif isinstance(value, list):
                new_dict[key] = [remove_dict_none_values(v) for v in value]
            else:
                new_dict[key] = value
    return new_dict
class _LogResponse(httpx.Response):
    def iter_bytes(self, *args, **kwargs):
        for chunk in super().iter_bytes(*args, **kwargs):
            click.echo(chunk.decode(), err=True)
            yield chunk
class _LogTransport(httpx.BaseTransport):
    def __init__(self, transport: httpx.BaseTransport):
        self.transport = transport
    def handle_request(self, request: httpx.Request) -> httpx.Response:
        response = self.transport.handle_request(request)
        return _LogResponse(status_code=response.status_code, headers=response.headers, stream=response.stream, extensions=response.extensions)
def _no_accept_encoding(request: httpx.Request):
    request.headers.pop('accept-encoding', None)
def _log_response(response: httpx.Response):
    request = response.request
    click.echo(f'Request: {request.method} {request.url}', err=True)
    click.echo('  Headers:', err=True)
    for key, value in request.headers.items():
        if key.lower() == 'authorization':
            value = '[...]'
        if key.lower() == 'cookie':
            value = value.split('=')[0] + '=...'
        click.echo(f'    {key}: {value}', err=True)
    click.echo('  Body:', err=True)
    try:
        request_body = json.loads(request.content)
        click.echo(textwrap.indent(json.dumps(request_body, indent=2), '    '), err=True)
    except json.JSONDecodeError:
        click.echo(textwrap.indent(request.content.decode(), '    '), err=True)
    click.echo(f'Response: status_code={response.status_code}', err=True)
    click.echo('  Headers:', err=True)
    for key, value in response.headers.items():
        if key.lower() == 'set-cookie':
            value = value.split('=')[0] + '=...'
        click.echo(f'    {key}: {value}', err=True)
    click.echo('  Body:', err=True)
def logging_client() -> httpx.Client:
    return httpx.Client(transport=_LogTransport(httpx.HTTPTransport()), event_hooks={'request': [_no_accept_encoding], 'response': [_log_response]})
def simplify_usage_dict(d):
    def remove_empty_and_zero(obj):
        if isinstance(obj, dict):
            cleaned = {k: remove_empty_and_zero(v) for k, v in obj.items() if v != 0 and v != {}}
            return {k: v for k, v in cleaned.items() if v is not None and v != {}}
        return obj
    return remove_empty_and_zero(d) or {}
def token_usage_string(input_tokens, output_tokens, token_details) -> str:
    bits = []
    if input_tokens is not None:
        bits.append(f"{format(input_tokens, ',')} input")
    if output_tokens is not None:
        bits.append(f"{format(output_tokens, ',')} output")
    if token_details:
        bits.append(json.dumps(token_details))
    return ', '.join(bits)
def extract_fenced_code_block(text: str, last: bool=False) -> Optional[str]:
    pattern = re.compile('(?m)^(?P<fence>`{3,})(?P<lang>\\w+)?\\n(?P<code>.*?)^(?P=fence)[ ]*(?=\\n|$)', re.DOTALL)
    matches = list(pattern.finditer(text))
    if matches:
        match = matches[-1] if last else matches[0]
        return match.group('code')
    return None
def make_schema_id(schema: dict) -> Tuple[str, str]:
    schema_json = json.dumps(schema, separators=(',', ':'))
    schema_id = hashlib.blake2b(schema_json.encode(), digest_size=16).hexdigest()
    return (schema_id, schema_json)
def output_rows_as_json(rows, nl=False, compact=False, json_cols=()):
    current_iter, next_iter = itertools.tee(rows, 2)
    next(next_iter, None)
    first = True
    for row, next_row in itertools.zip_longest(current_iter, next_iter):
        is_last = next_row is None
        for col in json_cols:
            row[col] = json.loads(row[col])
        if nl:
            yield json.dumps(row)
        elif compact:
            yield '{firstchar}{serialized}{maybecomma}{lastchar}'.format(firstchar='[' if first else ' ', serialized=json.dumps(row), maybecomma=',' if not is_last else '', lastchar=']' if is_last else '')
        else:
            yield '{firstchar}{serialized}{maybecomma}{lastchar}'.format(firstchar='[\n' if first else '', serialized=textwrap.indent(json.dumps(row, indent=2), '  '), maybecomma=',' if not is_last else '', lastchar='\n]' if is_last else '')
        first = False
    if first and (not nl):
        yield '[]'
def resolve_schema_input(db, schema_input, load_template):
    if not schema_input:
        return
    if schema_input.strip().startswith('t:'):
        name = schema_input.strip()[2:]
        schema_object = None
        try:
            template = load_template(name)
            schema_object = template.schema_object
        except ValueError:
            raise click.ClickException('Invalid template: {}'.format(name))
        if not schema_object:
            raise click.ClickException("Template '{}' has no schema".format(name))
        return template.schema_object
    if schema_input.strip().startswith('{'):
        try:
            return json.loads(schema_input)
        except ValueError:
            pass
    if ' ' in schema_input.strip() or ',' in schema_input:
        return schema_dsl(schema_input)
    path = pathlib.Path(schema_input)
    if path.exists():
        try:
            return json.loads(path.read_text())
        except ValueError:
            raise click.ClickException('Schema file contained invalid JSON')
    try:
        row = db['schemas'].get(schema_input)
        return json.loads(row['content'])
    except (sqlite_utils.db.NotFoundError, ValueError):
        raise click.BadParameter('Invalid schema')
def schema_summary(schema: dict) -> str:
    if not schema or not isinstance(schema, dict):
        return ''
    schema_type = schema.get('type', '')
    if schema_type == 'object':
        props = schema.get('properties', {})
        prop_summaries = []
        for name, prop_schema in props.items():
            prop_type = prop_schema.get('type', '')
            if prop_type == 'array':
                items = prop_schema.get('items', {})
                items_summary = schema_summary(items)
                prop_summaries.append(f'{name}: [{items_summary}]')
            elif prop_type == 'object':
                nested_summary = schema_summary(prop_schema)
                prop_summaries.append(f'{name}: {nested_summary}')
            else:
                prop_summaries.append(name)
        return '{' + ', '.join(prop_summaries) + '}'
    elif schema_type == 'array':
        items = schema.get('items', {})
        return schema_summary(items)
    return ''
def schema_dsl(schema_dsl: str, multi: bool=False) -> Dict[str, Any]:
    type_mapping = {'int': 'integer', 'float': 'number', 'bool': 'boolean', 'str': 'string'}
    json_schema: Dict[str, Any] = {'type': 'object', 'properties': {}, 'required': []}
    if '\n' in schema_dsl:
        fields = [field.strip() for field in schema_dsl.split('\n') if field.strip()]
    else:
        fields = [field.strip() for field in schema_dsl.split(',') if field.strip()]
    for field in fields:
        if ':' in field:
            field_info, description = field.split(':', 1)
            description = description.strip()
        else:
            field_info = field
            description = ''
        field_parts = field_info.strip().split()
        field_name = field_parts[0].strip()
        field_type = 'string'
        if len(field_parts) > 1:
            type_indicator = field_parts[1].strip()
            if type_indicator in type_mapping:
                field_type = type_mapping[type_indicator]
        json_schema['properties'][field_name] = {'type': field_type}
        if description:
            json_schema['properties'][field_name]['description'] = description
        json_schema['required'].append(field_name)
    if multi:
        return multi_schema(json_schema)
    else:
        return json_schema
def multi_schema(schema: dict) -> dict:
    return {'type': 'object', 'properties': {'items': {'type': 'array', 'items': schema}}, 'required': ['items']}
def find_unused_key(item: dict, key: str) -> str:
    while key in item:
        key += '_'
    return key
def truncate_string(text: str, max_length: int=100, normalize_whitespace: bool=False, keep_end: bool=False) -> str:
    if not text:
        return text
    if normalize_whitespace:
        text = re.sub('\\s+', ' ', text)
    if len(text) <= max_length:
        return text
    min_keep_end_length = 9
    if keep_end and max_length >= min_keep_end_length:
        cutoff = (max_length - 5) // 2
        return text[:cutoff] + '... ' + text[-cutoff:]
    else:
        return text[:max_length - 3] + '...'
def ensure_fragment(db, content):
    sql = "\n    insert into fragments (hash, content, datetime_utc, source)\n    values (:hash, :content, datetime('now'), :source)\n    on conflict(hash) do nothing\n    "
    hash_id = hashlib.sha256(content.encode('utf-8')).hexdigest()
    source = None
    if isinstance(content, Fragment):
        source = content.source
    with db.conn:
        db.execute(sql, {'hash': hash_id, 'content': content, 'source': source})
        return list(db.query('select id from fragments where hash = :hash', {'hash': hash_id}))[0]['id']
def ensure_tool(db, tool):
    sql = '\n    insert into tools (hash, name, description, input_schema, plugin)\n    values (:hash, :name, :description, :input_schema, :plugin)\n    on conflict(hash) do nothing\n    '
    with db.conn:
        db.execute(sql, {'hash': tool.hash(), 'name': tool.name, 'description': tool.description, 'input_schema': json.dumps(tool.input_schema), 'plugin': tool.plugin})
        return list(db.query('select id from tools where hash = :hash', {'hash': tool.hash()}))[0]['id']
def maybe_fenced_code(content: str) -> str:
    is_code = False
    if content.count('<') > 10:
        is_code = True
    if not is_code:
        lines = content.splitlines()
        if len(lines) > 3:
            num_short = sum((1 for line in lines if len(line) < 120))
            if num_short / len(lines) > 0.9:
                is_code = True
    if is_code:
        num_backticks = 3
        while '`' * num_backticks in content:
            num_backticks += 1
        content = '\n' + '`' * num_backticks + '\n' + content.strip() + '\n' + '`' * num_backticks
    return content
_plugin_prefix_re = re.compile('^[a-zA-Z0-9_-]+:')
def has_plugin_prefix(value: str) -> bool:
    return bool(_plugin_prefix_re.match(value))
def _parse_kwargs(arg_str: str) -> Dict[str, Any]:
    tokens = []
    buf = []
    depth = 0
    in_string = False
    string_char = ''
    escape = False
    for ch in arg_str:
        if in_string:
            buf.append(ch)
            if escape:
                escape = False
            elif ch == '\\':
                escape = True
            elif ch == string_char:
                in_string = False
        elif ch in '"\'':
            in_string = True
            string_char = ch
            buf.append(ch)
        elif ch in '{[(':
            depth += 1
            buf.append(ch)
        elif ch in '}])':
            depth -= 1
            buf.append(ch)
        elif ch == ',' and depth == 0:
            tokens.append(''.join(buf).strip())
            buf = []
        else:
            buf.append(ch)
    if buf:
        tokens.append(''.join(buf).strip())
    kwargs: Dict[str, Any] = {}
    for token in tokens:
        if not token:
            continue
        if '=' not in token:
            raise ValueError(f"Invalid keyword spec segment: '{token}'")
        key, value_str = token.split('=', 1)
        key = key.strip()
        value_str = value_str.strip()
        try:
            value = json.loads(value_str)
        except json.JSONDecodeError as e:
            raise ValueError(f"Value for '{key}' is not valid JSON: {value_str}") from e
        kwargs[key] = value
    return kwargs
def instantiate_from_spec(class_map: Dict[str, Type], spec: str):
    m = re.fullmatch('\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*(?:\\((.*)\\))?\\s*$', spec)
    if not m:
        raise ValueError(f"Invalid spec string: '{spec}'")
    class_name, arg_body = (m.group(1), (m.group(2) or '').strip())
    if class_name not in class_map:
        raise ValueError(f"Unknown class '{class_name}'")
    cls = class_map[class_name]
    if arg_body == '':
        return cls()
    if arg_body.lstrip().startswith('{'):
        try:
            kw = json.loads(arg_body)
        except json.JSONDecodeError as e:
            raise ValueError('Argument JSON object is not valid JSON') from e
        if not isinstance(kw, dict):
            raise ValueError('Top-level JSON must be an object when using {} form')
        return cls(**kw)
    if re.match('\\s*(["\\[\\d\\-]|true|false|null)', arg_body, re.I):
        try:
            positional_value = json.loads(arg_body)
        except json.JSONDecodeError as e:
            raise ValueError('Positional argument must be valid JSON') from e
        return cls(positional_value)
    kwargs = _parse_kwargs(arg_body)
    return cls(**kwargs)
NANOSECS_IN_MILLISECS = 1000000
TIMESTAMP_LEN = 6
RANDOMNESS_LEN = 10
_lock: Final = threading.Lock()
_last: Optional[bytes] = None
def monotonic_ulid() -> ULID:
    global _last
    now_ms = time.time_ns() // NANOSECS_IN_MILLISECS
    with _lock:
        if _last is None:
            _last = _fresh(now_ms)
            return ULID(_last)
        last_ms = int.from_bytes(_last[:TIMESTAMP_LEN], 'big')
        if now_ms == last_ms:
            rand_int = int.from_bytes(_last[TIMESTAMP_LEN:], 'big') + 1
            if rand_int >= 1 << RANDOMNESS_LEN * 8:
                raise OverflowError('Randomness overflow: > 2**80 ULIDs requested in one millisecond!')
            randomness = rand_int.to_bytes(RANDOMNESS_LEN, 'big')
            _last = _last[:TIMESTAMP_LEN] + randomness
            return ULID(_last)
        _last = _fresh(now_ms)
        return ULID(_last)
def _fresh(ms: int) -> bytes:
    timestamp = int.to_bytes(ms, TIMESTAMP_LEN, 'big')
    randomness = os.urandom(RANDOMNESS_LEN)
    return timestamp + randomness