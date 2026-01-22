#ifndef _PRUNE_H
#define _PRUNE_H
#include "api-types.h"
#include "link-includes.h"
unsigned int pp_and_power_prune(Sentence, Tracon_sharing *, unsigned int,
Parse_Options, unsigned int *[2]);
bool optional_gap_collapse(Sentence, int, int);
#endif