#ifndef READ_ATOMESE_H
#define READ_ATOMESE_H
#include "link-includes.h"
#ifdef HAVE_ATOMESE
Dictionary dictionary_create_from_atomese(const char *lang);
#endif
#endif