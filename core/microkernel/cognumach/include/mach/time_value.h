#ifndef	_MACH_TIME_VALUE_H_
#define	_MACH_TIME_VALUE_H_
#include <mach/machine/vm_types.h>
struct rpc_time_value {
rpc_long_integer_t seconds;
integer_t microseconds;
};
struct time_value {
long_integer_t	seconds;
integer_t	microseconds;
};
typedef	struct time_value	time_value_t;
#ifdef KERNEL
typedef struct rpc_time_value rpc_time_value_t;
#else
typedef struct time_value rpc_time_value_t;
#endif
struct time_value64 {
int64_t seconds;
int64_t nanoseconds;
};
typedef struct time_value64 time_value64_t;
static __inline__ rpc_time_value_t convert_time_value_to_user(time_value_t tv)
{
rpc_time_value_t user = {.seconds = tv.seconds, .microseconds = tv.microseconds};
return user;
}
static __inline__ time_value_t convert_time_value_from_user(rpc_time_value_t tv)
{
time_value_t kernel = {.seconds = tv.seconds, .microseconds = tv.microseconds};
return kernel;
}
#define	TIME_MICROS_MAX	(1000000)
#define	TIME_NANOS_MAX	(1000000000)
#define time_value_assert(val)			\
assert(0 <= (val)->microseconds && (val)->microseconds < TIME_MICROS_MAX);
#define time_value64_assert(val)			\
assert(0 <= (val)->nanoseconds && (val)->nanoseconds < TIME_NANOS_MAX);
#define	time_value_add_usec(val, micros)		\
do {						\
time_value_assert(val);				\
if (((val)->microseconds += (micros))		\
>= TIME_MICROS_MAX) {			\
(val)->microseconds -= TIME_MICROS_MAX;	\
(val)->seconds++;				\
}						\
time_value_assert(val);				\
} while(0)
#define	time_value64_add_nanos(val, nanos)		\
do {						\
time_value64_assert(val);			\
if (((val)->nanoseconds += (nanos))		\
>= TIME_NANOS_MAX) {			\
(val)->nanoseconds -= TIME_NANOS_MAX;	\
(val)->seconds++;				\
}						\
time_value64_assert(val);			\
} while(0)
#define	time_value64_sub_nanos(val, nanos)		\
do {						\
time_value64_assert(val);			\
if (((val)->nanoseconds -= (nanos)) < 0) {	\
(val)->nanoseconds += TIME_NANOS_MAX;	\
(val)->seconds--;				\
}						\
time_value64_assert(val);			\
} while(0)
#define	time_value_add(result, addend) 				\
do {							\
time_value_assert(addend);					\
(result)->seconds += (addend)->seconds;			\
time_value_add_usec(result, (addend)->microseconds);	\
} while(0)
#define	time_value64_add(result, addend) 			\
do {							\
time_value64_assert(addend);				\
(result)->seconds += (addend)->seconds;			\
time_value64_add_nanos(result, (addend)->nanoseconds);	\
} while(0)
#define	time_value64_sub(result, subtrahend) 			\
do {							\
time_value64_assert(subtrahend);				\
(result)->seconds -= (subtrahend)->seconds;			\
time_value64_sub_nanos(result, (subtrahend)->nanoseconds);	\
} while(0)
#define time_value64_init(tv)					\
do {							\
(tv)->seconds = 0;				\
(tv)->nanoseconds = 0;				\
} while(0)
#define TIME_VALUE64_TO_TIME_VALUE(tv64, tv) 				\
do {								\
(tv)->seconds = (tv64)->seconds;			\
(tv)->microseconds = (tv64)->nanoseconds / 1000;	\
} while(0)
#define TIME_VALUE_TO_TIME_VALUE64(tv, tv64) 				\
do {								\
(tv64)->seconds = (tv)->seconds;			\
(tv64)->nanoseconds = (tv)->microseconds * 1000;	\
} while(0)
typedef struct mapped_time_value {
integer_t seconds;
integer_t microseconds;
integer_t check_seconds;
struct time_value64 time_value;
int64_t check_seconds64;
struct time_value64 uptime_value;
int64_t check_upseconds64;
} mapped_time_value_t;
#define TIME_VALUE_TO_TIMESPEC(tv, ts)                                  \
do {                                                             \
(ts)->tv_sec = (tv)->seconds;                                   \
(ts)->tv_nsec = (tv)->microseconds * 1000;                      \
} while(0)
#define TIMESPEC_TO_TIME_VALUE(tv, ts)                                  \
do {                                                             \
(tv)->seconds = (ts)->tv_sec;                                   \
(tv)->microseconds = (ts)->tv_nsec / 1000;                      \
} while(0)
#define TIME_VALUE64_TO_TIMESPEC(tv, ts)                                \
do {                                                             \
(ts)->tv_sec = (tv)->seconds;                                   \
(ts)->tv_nsec = (tv)->nanoseconds;                              \
} while(0)
#define TIMESPEC_TO_TIME_VALUE64(tv, ts)                                \
do {                                                             \
(tv)->seconds = (ts)->tv_sec;                                   \
(tv)->nanoseconds = (ts)->tv_nsec;                              \
} while(0)
#endif