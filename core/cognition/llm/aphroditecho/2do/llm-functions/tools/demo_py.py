import os
from typing import List, Literal, Optional
def run(string: str, string_enum: Literal['foo', 'bar'], boolean: bool, integer: int, number: float, array: List[str], string_optional: Optional[str]=None, array_optional: Optional[List[str]]=None):
    output = f'string: {string}\nstring_enum: {string_enum}\nstring_optional: {string_optional}\nboolean: {boolean}\ninteger: {integer}\nnumber: {number}\narray: {array}\narray_optional: {array_optional}'
    for key, value in os.environ.items():
        if key.startswith('LLM_'):
            output = f'{output}\n{key}: {value}'
    return output