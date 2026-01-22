import re
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass
from fastapi import Request
from fastapi.responses import HTMLResponse, JSONResponse, Response
from fastapi.templating import Jinja2Templates
import xml.etree.ElementTree as ET
import json
from xml.dom import minidom
@dataclass
class AcceptedType:
    media_type: str
    quality: float
    params: Dict[str, str]
    @classmethod
    def from_accept_entry(cls, accept_entry: str) -> 'AcceptedType':
        parts = [p.strip() for p in accept_entry.split(';')]
        media_type = parts[0]
        params = {}
        quality = 1.0
        for param in parts[1:]:
            if '=' in param:
                key, value = param.split('=', 1)
                key = key.strip()
                value = value.strip()
                if key == 'q':
                    try:
                        quality = float(value)
                    except ValueError:
                        quality = 1.0
                else:
                    params[key] = value
        return cls(media_type=media_type, quality=quality, params=params)
class ContentNegotiator:
    SUPPORTED_TYPES = {'application/json': 'json', 'text/html': 'html', 'application/xhtml+xml': 'html', 'application/xml': 'xml', 'text/xml': 'xml', '*/*': 'json'}
    def __init__(self):
        self._accept_pattern = re.compile('([^,;]+)(?:;([^,]*))?')
    def parse_accept_header(self, accept_header: str) -> List[AcceptedType]:
        if not accept_header:
            return [AcceptedType('application/json', 1.0, {})]
        accepted_types = []
        for entry in accept_header.split(','):
            entry = entry.strip()
            if entry:
                try:
                    accepted_type = AcceptedType.from_accept_entry(entry)
                    accepted_types.append(accepted_type)
                except Exception:
                    continue
        accepted_types.sort(key=lambda x: (-x.quality, x.media_type != '*/*'))
        return accepted_types
    def negotiate_content_type(self, request: Request) -> str:
        accept_header = request.headers.get('accept', 'application/json')
        accepted_types = self.parse_accept_header(accept_header)
        for accepted_type in accepted_types:
            media_type = accepted_type.media_type.lower()
            if media_type in self.SUPPORTED_TYPES:
                return self.SUPPORTED_TYPES[media_type]
            if '/' in media_type:
                type_part, subtype_part = media_type.split('/', 1)
                if subtype_part == '*':
                    for supported_type, format_name in self.SUPPORTED_TYPES.items():
                        if supported_type.startswith(type_part + '/'):
                            return format_name
        return 'json'
    def wants_html(self, request: Request) -> bool:
        return self.negotiate_content_type(request) == 'html'
    def wants_xml(self, request: Request) -> bool:
        return self.negotiate_content_type(request) == 'xml'
    def wants_json(self, request: Request) -> bool:
        return self.negotiate_content_type(request) == 'json'
class XMLResponseGenerator:
    @staticmethod
    def dict_to_xml(data: Dict, root_name: str='response') -> str:
        root = ET.Element(root_name)
        def _add_to_element(parent: ET.Element, key: str, value):
            tag_name = re.sub('[^a-zA-Z0-9_-]', '_', str(key))
            if tag_name[0].isdigit():
                tag_name = 'item_' + tag_name
            element = ET.SubElement(parent, tag_name)
            if isinstance(value, dict):
                for k, v in value.items():
                    _add_to_element(element, k, v)
            elif isinstance(value, (list, tuple)):
                for i, item in enumerate(value):
                    _add_to_element(element, f'item_{i}', item)
            elif value is None:
                element.set('null', 'true')
            else:
                element.text = str(value)
        for key, value in data.items():
            _add_to_element(root, key, value)
        xml_str = ET.tostring(root, encoding='unicode')
        try:
            dom = minidom.parseString(xml_str)
            return dom.toprettyxml(indent='  ').split('\n', 1)[1]
        except Exception:
            return xml_str
class MultiFormatResponse:
    def __init__(self, negotiator: ContentNegotiator=None):
        self.negotiator = negotiator or ContentNegotiator()
        self.xml_generator = XMLResponseGenerator()
    def create_response(self, data: Dict, request: Request, templates: Optional[Jinja2Templates]=None, template_name: Optional[str]=None, xml_root: str='response') -> Response:
        content_type = self.negotiator.negotiate_content_type(request)
        if content_type == 'html':
            if not templates or not template_name:
                return JSONResponse(data)
            return templates.TemplateResponse(template_name, {'request': request, 'data': data})
        elif content_type == 'xml':
            xml_content = self.xml_generator.dict_to_xml(data, xml_root)
            return Response(content=f'<?xml version="1.0" encoding="UTF-8"?>\n{xml_content}', media_type='application/xml', headers={'Content-Type': 'application/xml; charset=utf-8'})
        else:
            return JSONResponse(data)
content_negotiator = ContentNegotiator()
multi_format_response = MultiFormatResponse(content_negotiator)
def wants_html(request: Request) -> bool:
    return content_negotiator.wants_html(request)
def wants_xml(request: Request) -> bool:
    return content_negotiator.wants_xml(request)
def create_negotiated_response(data: Dict, request: Request, templates: Optional[Jinja2Templates]=None, template_name: Optional[str]=None, xml_root: str='response') -> Response:
    return multi_format_response.create_response(data=data, request=request, templates=templates, template_name=template_name, xml_root=xml_root)