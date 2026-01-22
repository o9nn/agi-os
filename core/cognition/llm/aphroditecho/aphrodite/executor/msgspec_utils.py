from array import array
from typing import Any, Type
from aphrodite.common.sequence import APHRODITE_TOKEN_ID_ARRAY_TYPE
def encode_hook(obj: Any) -> Any:
    if isinstance(obj, array):
        assert obj.typecode == APHRODITE_TOKEN_ID_ARRAY_TYPE, f"Aphrodite array type should use '{APHRODITE_TOKEN_ID_ARRAY_TYPE}' type. Given array has a type code of {obj.typecode}."
        return obj.tobytes()
def decode_hook(type: Type, obj: Any) -> Any:
    if type is array:
        deserialized = array(APHRODITE_TOKEN_ID_ARRAY_TYPE)
        deserialized.frombytes(obj)
        return deserialized