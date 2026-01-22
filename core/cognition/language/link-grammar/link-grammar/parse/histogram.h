#ifndef _HISTOGRAM_H_
#define _HISTOGRAM_H_
#include <stdint.h>
#include <inttypes.h>
#define PARSE_NUM_OVERFLOW (1<<24)
typedef int32_t count_t;
typedef int64_t w_count_t;
#define PERFORM_COUNT_HISTOGRAMMING 0
#if PERFORM_COUNT_HISTOGRAMMING
#define COUNT_FMT PRId64
#define NUM_BINS 12
typedef struct
{
short base;
w_count_t total;
w_count_t bin[NUM_BINS];
w_count_t overrun;
} Count_bin;
typedef Count_bin w_Count_bin;
Count_bin hist_zero(void);
Count_bin hist_one(void);
void hist_accum(Count_bin* sum, float, const Count_bin*);
void hist_accumv(Count_bin* sum, float, const Count_bin);
void hist_prod(Count_bin* prod, const Count_bin*, const Count_bin*);
void hist_muladd(Count_bin* prod, const Count_bin*, float, const Count_bin*);
void hist_muladdv(Count_bin* prod, const Count_bin*, float, const Count_bin);
static inline w_count_t hist_total(Count_bin* tot) { return tot->total; }
w_count_t hist_cut_total(Count_bin* tot, count_t min_total);
float hist_cost_cutoff(Count_bin*, count_t count);
#else
#define COUNT_FMT PRId32
typedef count_t Count_bin;
typedef w_count_t w_Count_bin;
static inline count_t hist_zero(void) { return 0; }
static inline count_t hist_one(void) { return 1; }
#define hist_accum(sum, cost, a) (*(sum) += *(a))
#define hist_accumv(sum, cost, a) (*(sum) += (a))
#define hist_prod(prod, a, b) (*(prod) = (*a) * (*b))
#define hist_muladd(prod, a, cost, b) (*(prod) += (*a) * (*b))
#define hist_muladdv(prod, a, cost, b) (*(prod) += (*a) * (b))
#define hist_total(tot) (*tot)
#define hist_cut_total(tot, min_total) (*tot)
static inline float hist_cost_cutoff(count_t* tot, count_t count) { return 1.0e38f; }
#endif
#endif