#ifndef MAILIMAP_PRINT_H
#define MAILIMAP_PRINT_H
#ifdef __cplusplus
extern "C" {
#endif
#include "mailimap_types.h"
void mailimap_response_print(struct mailimap_response * resp);
void mailimap_greeting_print(struct mailimap_greeting * greeting);
#ifdef __cplusplus
}
#endif
#endif