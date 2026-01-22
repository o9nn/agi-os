#ifndef _HACK_WAIT_H_
#define _HACK_WAIT_H_
#include <pthread.h>
struct wait_queue
{
struct task_struct *task;
struct wait_queue *next;
};
struct select_table_elt
{
pthread_cond_t *dependent_condition;
struct select_table_elt *next;
};
typedef struct select_table_struct
{
pthread_cond_t master_condition;
struct select_table_elt *head;
} select_table;
#endif