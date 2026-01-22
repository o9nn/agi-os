#ifndef _BITS_ATOMIC_WIDE_COUNTER_H
#define _BITS_ATOMIC_WIDE_COUNTER_H
typedef union
{
__extension__ unsigned long long int __value64;
struct
{
unsigned int __low;
unsigned int __high;
} __value32;
} __atomic_wide_counter;
#endif