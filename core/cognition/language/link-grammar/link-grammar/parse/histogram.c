#include <math.h>
#include "histogram.h"
#if PERFORM_COUNT_HISTOGRAMMING
Count_bin hist_zero(void)
{
static Count_bin zero
= {0, 0, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 0};
return zero;
}
Count_bin hist_one(void)
{
static Count_bin one
= {0, 1, {1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 0};
return one;
}
#define BIN_WIDTH 0.334
void hist_accum(Count_bin* sum, float cost, const Count_bin* a)
{
unsigned int i;
unsigned int start;
if (0 == a->total) return;
sum->total += a->total;
start = (unsigned int) floor (cost / BIN_WIDTH);
if (0 == sum->bin[0])
{
sum->base = start;
start = 0;
}
for (i = start; i < NUM_BINS; i++)
{
sum->bin[i] += a->bin[i-start];
}
for (i = NUM_BINS-start; i < NUM_BINS; i++)
{
sum->overrun += a->bin[i];
}
sum->overrun += a->overrun;
}
void hist_accumv(Count_bin* sum, float cost, const Count_bin a)
{
hist_accum(sum, cost, &a);
}
void hist_prod(Count_bin* prod, const Count_bin* a, const Count_bin* b)
{
unsigned int i, k;
if (0 == a->total || 0 == b->total) return;
prod->total = a->total * b->total;
#ifdef SLOW_BUT_SIMPLE
for (i = 0; i < NUM_BINS; i++) prod->bin[i] = 0;
prod->overrun = 0;
for (i = 0; i < NUM_BINS; i++)
{
for (j = 0; j < NUM_BINS; j++)
{
if (i+j < NUM_BINS)
prod->bin[i+j] += a->bin[i] * b->bin[j];
else
prod->overrun += a->bin[i] * b->bin[j];
}
prod->overrun += a->bin[i] * b->overrun;
prod->overrun += a->overrun * b->bin[i];
}
prod->overrun += a->overrun * b->overrun;
#else
prod->overrun = 0;
for (k = 0; k < NUM_BINS; k++)
{
prod->bin[k] = 0;
for (i = 0; i <= k; i++)
{
prod->bin[k] += a->bin[i] * b->bin[k-i];
}
prod->overrun += a->bin[k] * b->overrun;
prod->overrun += a->overrun * b->bin[k];
}
for (k = NUM_BINS; k < 2 * NUM_BINS - 1; k++)
{
for (i = k - NUM_BINS + 1; i < NUM_BINS; i++)
{
prod->overrun += a->bin[i] * b->bin[k-i];
}
}
prod->overrun += a->overrun * b->overrun;
#endif
}
void hist_muladd(Count_bin* acc, const Count_bin* a, float cost, const Count_bin* b)
{
Count_bin tmp = hist_zero();
hist_prod(&tmp, a, b);
hist_accum(acc, cost, &tmp);
}
void hist_muladdv(Count_bin* acc, const Count_bin* a, float cost, const Count_bin b)
{
hist_muladd(acc, a, cost, &b);
}
float hist_cost_cutoff(Count_bin* hist, int count)
{
int i;
w_count_t cnt = 0;
for (i=0; i<NUM_BINS; i++)
{
cnt += hist->bin[i];
if (count <= cnt)
return ((float) i + hist->base) * BIN_WIDTH;
}
return 1.0e38;
}
w_count_t hist_cut_total(Count_bin* hist, int min_total)
{
int i;
w_count_t cnt = 0;
for (i=0; i<NUM_BINS; i++)
{
cnt += hist->bin[i];
if (min_total <= cnt) return cnt;
}
return hist->total;
}
#endif