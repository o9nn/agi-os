#ifndef LAME_PSYMODEL_H
#define LAME_PSYMODEL_H
#include "l3side.h"
int L3psycho_anal( lame_global_flags *gfc,
const sample_t *buffer[2], int gr,
FLOAT8 *ms_ratio,
FLOAT8 *ms_ratio_next,
III_psy_ratio ratio[2][2],
III_psy_ratio MS_ratio[2][2],
FLOAT8 pe[2], FLOAT8 pe_MS[2], FLOAT8 ener[2],
int blocktype_d[2]);
int L3psycho_anal_ns( lame_global_flags *gfc,
const sample_t *buffer[2], int gr,
FLOAT8 *ms_ratio,
FLOAT8 *ms_ratio_next,
III_psy_ratio ratio[2][2],
III_psy_ratio MS_ratio[2][2],
FLOAT8 pe[2], FLOAT8 pe_MS[2], FLOAT8 ener[2],
int blocktype_d[2]);
int psymodel_init(lame_global_flags *gfp);
#endif