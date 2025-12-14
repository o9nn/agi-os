"""
Content negotiation for Deep Tree Echo endpoints.

Implements server-side content type negotiation supporting JSON, HTML, and XML formats
with proper HTTP Accept header parsing including quality factors (q-values).
"""

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
    """Represents a media type from Accept header with quality factor."""
    media_type: str
    quality: float
    params: Dict[str, str]

    @classmethod
    def from_accept_entry(cls, accept_entry: str) -> "AcceptedType":
        """Parse a single Accept header entry."""
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
    """
    Advanced content negotiation supporting multiple response formats.
    
    Supports JSON, HTML, and XML with proper HTTP content negotiation
    including quality factors and efficient content type switching.
    """
    
    # Supported content types in order of server preference
    SUPPORTED_TYPES = {
        'application/json': 'json',
        'text/html': 'html',
        'application/xhtml+xml': 'html',
        'application/xml': 'xml',
        'text/xml': 'xml',
        '*/*': 'json'  # Default fallback
    }
    
    def __init__(self):
        self._accept_pattern = re.compile(r'([^,;]+)(?:;([^,]*))?')
    
    def parse_accept_header(self, accept_header: str) -> List[AcceptedType]:
        """Parse Accept header into list of AcceptedType objects, sorted by quality."""
        if not accept_header:
            return [AcceptedType('application/json', 1.0, {})]
        
        accepted_types = []
        
        # Split by comma and parse each entry
        for entry in accept_header.split(','):
            entry = entry.strip()
            if entry:
                try:
                    accepted_type = AcceptedType.from_accept_entry(entry)
                    accepted_types.append(accepted_type)
                except Exception:
                    # Skip malformed entries
                    continue
        
        # Sort by quality factor (descending) and then by specificity
        accepted_types.sort(key=lambda x: (-x.quality, x.media_type != '*/*'))
        
        return accepted_types
    
    def negotiate_content_type(self, request: Request) -> str:
        """
        Determine the best content type to return based on Accept header.
        
        Returns one of: 'json', 'html', 'xml'
        """
        accept_header = request.headers.get('accept', 'application/json')
        accepted_types = self.parse_accept_header(accept_header)
        
        # Find the best match
        for accepted_type in accepted_types:
            media_type = accepted_type.media_type.lower()
            
            # Check for exact match
            if media_type in self.SUPPORTED_TYPES:
                return self.SUPPORTED_TYPES[media_type]
            
            # Check for wildcard patterns
            if '/' in media_type:
                type_part, subtype_part = media_type.split('/', 1)
                if subtype_part == '*':
                    # Look for any matching main type
                    for supported_type, format_name in self.SUPPORTED_TYPES.items():
                        if supported_type.startswith(type_part + '/'):
                            return format_name
        
        # Default to JSON if no match found
        return 'json'
    
    def wants_html(self, request: Request) -> bool:
        """Backward compatibility method - check if client wants HTML."""
        return self.negotiate_content_type(request) == 'html'
    
    def wants_xml(self, request: Request) -> bool:
        """Check if client wants XML response."""
        return self.negotiate_content_type(request) == 'xml'
    
    def wants_json(self, request: Request) -> bool:
        """Check if client wants JSON response."""
        return self.negotiate_content_type(request) == 'json'


class XMLResponseGenerator:
    """Generates XML responses from Python data structures."""
    
    @staticmethod
    def dict_to_xml(data: Dict, root_name: str = "response") -> str:
        """Convert dictionary to XML string with proper formatting."""
        root = ET.Element(root_name)
        
        def _add_to_element(parent: ET.Element, key: str, value):
            """Recursively add data to XML element."""
            # Sanitize key to be valid XML tag name
            tag_name = re.sub(r'[^a-zA-Z0-9_-]', '_', str(key))
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
        
        # Add all data to root
        for key, value in data.items():
            _add_to_element(root, key, value)
        
        # Convert to string with pretty formatting
        xml_str = ET.tostring(root, encoding='unicode')
        
        # Pretty print using minidom
        try:
            dom = minidom.parseString(xml_str)
            return dom.toprettyxml(indent="  ").split('\n', 1)[1]  # Remove XML declaration line
        except Exception:
            # Fallback to basic XML if pretty printing fails
            return xml_str


class MultiFormatResponse:
    """
    Helper class to generate appropriate responses based on content negotiation.
    
    Supports JSON, HTML (via templates), and XML formats with efficient switching.
    """
    
    def __init__(self, negotiator: ContentNegotiator = None):
        self.negotiator = negotiator or ContentNegotiator()
        self.xml_generator = XMLResponseGenerator()
    
    def create_response(
        self,
        data: Dict,
        request: Request,
        templates: Optional[Jinja2Templates] = None,
        template_name: Optional[str] = None,
        xml_root: str = "response"
    ) -> Response:
        """
        Create appropriate response based on content negotiation.
        
        Args:
            data: Response data as dictionary
            request: FastAPI request object  
            templates: Jinja2Templates instance (required for HTML)
            template_name: Template file name (required for HTML)
            xml_root: Root element name for XML responses
            
        Returns:
            Response object with appropriate content type
        """
        content_type = self.negotiator.negotiate_content_type(request)
        
        if content_type == 'html':
            if not templates or not template_name:
                # Fallback to JSON if HTML requirements not met
                return JSONResponse(data)
            return templates.TemplateResponse(
                template_name,
                {"request": request, "data": data}
            )
        
        elif content_type == 'xml':
            xml_content = self.xml_generator.dict_to_xml(data, xml_root)
            return Response(
                content=f'<?xml version="1.0" encoding="UTF-8"?>\n{xml_content}',
                media_type="application/xml",
                headers={"Content-Type": "application/xml; charset=utf-8"}
            )
        
        else:  # Default to JSON
            return JSONResponse(data)


# Global instances for use in route handlers
content_negotiator = ContentNegotiator()
multi_format_response = MultiFormatResponse(content_negotiator)


# Backward compatibility functions
def wants_html(request: Request) -> bool:
    """Backward compatibility wrapper for existing wants_html usage."""
    return content_negotiator.wants_html(request)


def wants_xml(request: Request) -> bool:
    """Check if client wants XML response."""
    return content_negotiator.wants_xml(request)


def create_negotiated_response(
    data: Dict,
    request: Request, 
    templates: Optional[Jinja2Templates] = None,
    template_name: Optional[str] = None,
    xml_root: str = "response"
) -> Response:
    """
    Convenient function to create content-negotiated response.
    
    This is the main function that route handlers should use for
    multi-format response generation.
    """
    return multi_format_response.create_response(
        data=data,
        request=request,
        templates=templates,
        template_name=template_name,
        xml_root=xml_root
    )