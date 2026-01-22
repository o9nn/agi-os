#ifndef IMAP_SEQSET_H
#define IMAP_SEQSET_H
#include "seq-range-array.h"
int imap_seq_set_parse(const char *str, ARRAY_TYPE(seq_range) *dest);
int imap_seq_set_nostar_parse(const char *str, ARRAY_TYPE(seq_range) *dest);
int imap_seq_range_parse(const char *str, uint32_t *seq1_r, uint32_t *seq2_r);
#endif