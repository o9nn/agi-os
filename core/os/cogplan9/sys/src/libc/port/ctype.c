#include <u.h>
#include <libc.h>
#include <ctype.h>
uchar	_ctype[256] =
{
_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
_C,	_S|_C,	_S|_C,	_S|_C,	_S|_C,	_S|_C,	_C,	_C,
_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
_S|_B,	_P,	_P,	_P,	_P,	_P,	_P,	_P,
_P,	_P,	_P,	_P,	_P,	_P,	_P,	_P,
_N|_X,	_N|_X,	_N|_X,	_N|_X,	_N|_X,	_N|_X,	_N|_X,	_N|_X,
_N|_X,	_N|_X,	_P,	_P,	_P,	_P,	_P,	_P,
_P,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U,
_U,	_U,	_U,	_U,	_U,	_U,	_U,	_U,
_U,	_U,	_U,	_U,	_U,	_U,	_U,	_U,
_U,	_U,	_U,	_P,	_P,	_P,	_P,	_P,
_P,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L,
_L,	_L,	_L,	_L,	_L,	_L,	_L,	_L,
_L,	_L,	_L,	_L,	_L,	_L,	_L,	_L,
_L,	_L,	_L,	_P,	_P,	_P,	_P,	_C,
};