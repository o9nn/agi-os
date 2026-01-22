#ifndef HURDCOG_PERFORMANCE_OPTIMIZER_H
#define HURDCOG_PERFORMANCE_OPTIMIZER_H
#ifdef __cplusplus
extern "C" {
#endif
int performance_optimizer_init(void);
int performance_optimizer_start(void);
int performance_optimizer_stop(void);
int performance_optimizer_benchmark(void);
void performance_optimizer_cleanup(void);
#ifdef __cplusplus
}
#endif
#endif