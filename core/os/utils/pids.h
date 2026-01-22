#ifndef __PIDS_H__
#define __PIDS_H__
extern error_t add_fn_pids (pid_t **pids, mach_msg_type_number_t *num_pids,
unsigned id,
error_t (*pids_fn)(process_t proc, pid_t id,
pid_t **pids,
mach_msg_type_number_t *num_pids));
extern error_t add_pid (pid_t **pids, mach_msg_type_number_t *num_pids,
pid_t pid);
struct pids_argp_params
{
pid_t **pids;
mach_msg_type_number_t *num_pids;
int parse_pid_args;
};
extern struct argp pids_argp;
#endif