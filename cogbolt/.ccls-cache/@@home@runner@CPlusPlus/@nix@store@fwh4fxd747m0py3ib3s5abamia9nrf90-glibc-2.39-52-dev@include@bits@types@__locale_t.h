#ifndef _BITS_TYPES___LOCALE_T_H
#define _BITS_TYPES___LOCALE_T_H 1
struct __locale_struct
{
struct __locale_data *__locales[13];
const unsigned short int *__ctype_b;
const int *__ctype_tolower;
const int *__ctype_toupper;
const char *__names[13];
};
typedef struct __locale_struct *__locale_t;
#endif