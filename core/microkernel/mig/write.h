#ifndef	_WRITE_H
#define	_WRITE_H
#include <stdio.h>
#include "statement.h"
extern void WriteUserHeader(FILE *file, const statement_t *stats);
extern void WriteServerHeader(FILE *file, const statement_t *stats);
extern void WriteInternalHeader(FILE *file, const statement_t *stats);
extern void WriteUser(FILE *file, const statement_t *stats);
extern void WriteUserIndividual(const statement_t *stats);
extern void WriteServer(FILE *file, const statement_t *stats);
extern void WriteRoutineList(FILE *file, const statement_t *stats);
extern void WriteSubsystemServerRoutine(FILE *file, const char *typeModifier);
#endif