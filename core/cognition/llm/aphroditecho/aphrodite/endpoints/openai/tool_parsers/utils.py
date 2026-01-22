import json
from json import JSONDecodeError, JSONDecoder
from typing import Any
import partial_json_parser
from partial_json_parser.core.options import Allow
def find_common_prefix(s1: str, s2: str) -> str:
    prefix = ''
    min_length = min(len(s1), len(s2))
    for i in range(0, min_length):
        if s1[i] == s2[i]:
            prefix += s1[i]
        else:
            break
    return prefix
def find_common_suffix(s1: str, s2: str) -> str:
    suffix = ''
    min_length = min(len(s1), len(s2))
    for i in range(1, min_length + 1):
        if s1[-i] == s2[-i] and (not s1[-i].isalnum()):
            suffix = s1[-i] + suffix
        else:
            break
    return suffix
def extract_intermediate_diff(curr: str, old: str) -> str:
    suffix = find_common_suffix(curr, old)
    old = old[::-1].replace(suffix[::-1], '', 1)[::-1]
    prefix = find_common_prefix(curr, old)
    diff = curr
    if len(suffix):
        diff = diff[::-1].replace(suffix[::-1], '', 1)[::-1]
    if len(prefix):
        diff = diff.replace(prefix, '', 1)
    return diff
def find_all_indices(string: str, substring: str) -> list[int]:
    indices = []
    index = -1
    while True:
        index = string.find(substring, index + 1)
        if index == -1:
            break
        indices.append(index)
    return indices
def partial_json_loads(input_str: str, flags: Allow) -> tuple[Any, int]:
    try:
        return (partial_json_parser.loads(input_str, flags), len(input_str))
    except JSONDecodeError as e:
        if 'Extra data' in e.msg:
            dec = JSONDecoder()
            return dec.raw_decode(input_str)
        raise
def is_complete_json(input_str: str) -> bool:
    try:
        json.loads(input_str)
        return True
    except JSONDecodeError:
        return False
def consume_space(i: int, s: str) -> int:
    while i < len(s) and s[i].isspace():
        i += 1
    return i