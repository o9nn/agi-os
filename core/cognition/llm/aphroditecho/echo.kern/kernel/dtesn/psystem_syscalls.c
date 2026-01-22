#define _GNU_SOURCE
#include "include/dtesn/psystem.h"
#include <sys/syscall.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/init.h>
#include <linux/slab.h>
#include <linux/uaccess.h>
#include <linux/mutex.h>
#include <linux/sched.h>
#define __NR_sys_membrane_create      500
#define __NR_sys_membrane_evolve      501
#define __NR_sys_membrane_get_state   502
#define __NR_sys_membrane_destroy     503
#define __NR_sys_membrane_communicate 504
static dtesn_psystem_t *g_kernel_psystem = NULL;
static DEFINE_MUTEX(g_kernel_psystem_mutex);
static bool g_psystem_module_initialized = false;
static int validate_user_pointer(const void __user *ptr, size_t size);
static int copy_multiset_from_user(dtesn_psystem_multiset_t *dest,
const void __user *src,
size_t size);
static int copy_multiset_to_user(void __user *dest,
const dtesn_psystem_multiset_t *src,
size_t size);
static int validate_user_pointer(const void __user *ptr, size_t size) {
if (!ptr || size == 0) {
return -EFAULT;
}
if (!access_ok(VERIFY_READ, ptr, size)) {
return -EFAULT;
}
return 0;
}
static int copy_multiset_from_user(dtesn_psystem_multiset_t *dest,
const void __user *src,
size_t size) {
if (!dest || !src || size < sizeof(dtesn_psystem_multiset_t)) {
return -EINVAL;
}
memset(dest, 0, sizeof(dtesn_psystem_multiset_t));
mutex_init(&dest->lock);
if (copy_from_user(dest, src, sizeof(dtesn_psystem_multiset_t))) {
return -EFAULT;
}
dest->objects = NULL;
return 0;
}
static int copy_multiset_to_user(void __user *dest,
const dtesn_psystem_multiset_t *src,
size_t size) {
if (!dest || !src || size < sizeof(dtesn_psystem_multiset_t)) {
return -EINVAL;
}
dtesn_psystem_multiset_t user_copy = *src;
user_copy.objects = NULL;
if (copy_to_user(dest, &user_copy, sizeof(dtesn_psystem_multiset_t))) {
return -EFAULT;
}
return sizeof(dtesn_psystem_multiset_t);
}
SYSCALL_DEFINE6(sys_membrane_create,
uint32_t, parent_id,
const void __user *, rules,
uint32_t, rule_count,
int, membrane_type,
const char __user *, label,
uint32_t, neuron_count) {
char kernel_label[DTESN_PSYSTEM_MAX_SYMBOL_LEN];
uint32_t membrane_id;
int ret;
if (membrane_type < 0 || membrane_type > DTESN_MEMBRANE_ELEMENTARY) {
return -EINVAL;
}
if (neuron_count == 0 || neuron_count > 10000) {
return -EINVAL;
}
if (label) {
ret = strncpy_from_user(kernel_label, label,
DTESN_PSYSTEM_MAX_SYMBOL_LEN - 1);
if (ret < 0) {
return ret;
}
kernel_label[DTESN_PSYSTEM_MAX_SYMBOL_LEN - 1] = '\0';
} else {
strcpy(kernel_label, "membrane");
}
mutex_lock(&g_kernel_psystem_mutex);
if (!g_kernel_psystem) {
mutex_unlock(&g_kernel_psystem_mutex);
return -ENODEV;
}
membrane_id = dtesn_membrane_create(g_kernel_psystem,
(dtesn_membrane_type_t)membrane_type,
kernel_label,
parent_id,
neuron_count);
if (membrane_id == 0) {
mutex_unlock(&g_kernel_psystem_mutex);
return -ENOMEM;
}
if (rules && rule_count > 0) {
}
mutex_unlock(&g_kernel_psystem_mutex);
return (long)membrane_id;
}
SYSCALL_DEFINE2(sys_membrane_evolve,
uint32_t, membrane_id,
uint32_t, steps) {
int total_rules_applied = 0;
int ret;
if (membrane_id == 0 || steps == 0 || steps > 1000000) {
return -EINVAL;
}
mutex_lock(&g_kernel_psystem_mutex);
if (!g_kernel_psystem) {
mutex_unlock(&g_kernel_psystem_mutex);
return -ENODEV;
}
for (uint32_t step = 0; step < steps; step++) {
ret = dtesn_membrane_evolve(g_kernel_psystem, membrane_id);
if (ret < 0) {
break;
}
total_rules_applied += ret;
if (ret == 0) {
break;
}
if (step % 100 == 0) {
cond_resched();
}
}
mutex_unlock(&g_kernel_psystem_mutex);
if (ret < 0) {
return ret;
}
return total_rules_applied;
}
SYSCALL_DEFINE3(sys_membrane_get_state,
uint32_t, membrane_id,
void __user *, buffer,
size_t, buffer_size) {
dtesn_psystem_membrane_t *membrane;
void *kernel_buffer;
size_t state_size;
int ret;
if (membrane_id == 0) {
return -EINVAL;
}
ret = validate_user_pointer(buffer, buffer_size);
if (ret < 0) {
return ret;
}
mutex_lock(&g_kernel_psystem_mutex);
if (!g_kernel_psystem) {
mutex_unlock(&g_kernel_psystem_mutex);
return -ENODEV;
}
membrane = NULL;
for (uint32_t i = 0; i < g_kernel_psystem->membrane_count; i++) {
if (g_kernel_psystem->membranes[i]->membrane_id == membrane_id) {
membrane = g_kernel_psystem->membranes[i];
break;
}
}
if (!membrane || membrane->is_dissolved) {
mutex_unlock(&g_kernel_psystem_mutex);
return -ENOENT;
}
state_size = sizeof(dtesn_psystem_membrane_t);
if (buffer_size < state_size) {
mutex_unlock(&g_kernel_psystem_mutex);
return -ENOSPC;
}
kernel_buffer = kmalloc(state_size, GFP_KERNEL);
if (!kernel_buffer) {
mutex_unlock(&g_kernel_psystem_mutex);
return -ENOMEM;
}
memcpy(kernel_buffer, membrane, sizeof(dtesn_psystem_membrane_t));
dtesn_psystem_membrane_t *user_membrane =
(dtesn_psystem_membrane_t *)kernel_buffer;
user_membrane->children_ids = NULL;
user_membrane->rules = NULL;
user_membrane->objects.objects = NULL;
if (copy_to_user(buffer, kernel_buffer, state_size)) {
kfree(kernel_buffer);
mutex_unlock(&g_kernel_psystem_mutex);
return -EFAULT;
}
kfree(kernel_buffer);
mutex_unlock(&g_kernel_psystem_mutex);
return state_size;
}
SYSCALL_DEFINE1(sys_membrane_destroy,
uint32_t, membrane_id) {
int ret;
if (membrane_id == 0) {
return -EINVAL;
}
mutex_lock(&g_kernel_psystem_mutex);
if (!g_kernel_psystem) {
mutex_unlock(&g_kernel_psystem_mutex);
return -ENODEV;
}
ret = dtesn_membrane_destroy(g_kernel_psystem, membrane_id);
mutex_unlock(&g_kernel_psystem_mutex);
return ret;
}
SYSCALL_DEFINE4(sys_membrane_communicate,
uint32_t, src_id,
uint32_t, dst_id,
const void __user *, objects,
size_t, objects_size) {
dtesn_psystem_multiset_t kernel_objects;
int ret;
if (src_id == 0 || dst_id == 0 || src_id == dst_id) {
return -EINVAL;
}
ret = validate_user_pointer(objects, objects_size);
if (ret < 0) {
return ret;
}
ret = copy_multiset_from_user(&kernel_objects, objects, objects_size);
if (ret < 0) {
return ret;
}
mutex_lock(&g_kernel_psystem_mutex);
if (!g_kernel_psystem) {
mutex_unlock(&g_kernel_psystem_mutex);
return -ENODEV;
}
ret = dtesn_membrane_communicate(g_kernel_psystem, src_id, dst_id,
&kernel_objects);
mutex_unlock(&g_kernel_psystem_mutex);
dtesn_multiset_destroy(&kernel_objects);
return ret;
}
static int __init dtesn_psystem_syscalls_init(void) {
int ret;
printk(KERN_INFO "DTESN P-System: Initializing syscalls interface\n");
ret = dtesn_psystem_init();
if (ret < 0) {
printk(KERN_ERR "DTESN P-System: Failed to initialize subsystem\n");
return ret;
}
mutex_lock(&g_kernel_psystem_mutex);
g_kernel_psystem = dtesn_psystem_create("kernel_psystem", 0);
if (!g_kernel_psystem) {
mutex_unlock(&g_kernel_psystem_mutex);
printk(KERN_ERR "DTESN P-System: Failed to create kernel instance\n");
return -ENOMEM;
}
g_psystem_module_initialized = true;
mutex_unlock(&g_kernel_psystem_mutex);
printk(KERN_INFO "DTESN P-System: Syscalls interface initialized successfully\n");
return 0;
}
static void __exit dtesn_psystem_syscalls_exit(void) {
printk(KERN_INFO "DTESN P-System: Cleaning up syscalls interface\n");
mutex_lock(&g_kernel_psystem_mutex);
if (g_kernel_psystem) {
dtesn_psystem_destroy(g_kernel_psystem);
g_kernel_psystem = NULL;
}
g_psystem_module_initialized = false;
mutex_unlock(&g_kernel_psystem_mutex);
dtesn_psystem_shutdown();
printk(KERN_INFO "DTESN P-System: Syscalls interface cleanup complete\n");
}
long dtesn_psystem_get_global_stats(void __user *stats) {
dtesn_psystem_stats_t kernel_stats;
int ret;
if (!stats) {
return -EINVAL;
}
ret = validate_user_pointer(stats, sizeof(dtesn_psystem_stats_t));
if (ret < 0) {
return ret;
}
mutex_lock(&g_kernel_psystem_mutex);
if (!g_kernel_psystem) {
mutex_unlock(&g_kernel_psystem_mutex);
return -ENODEV;
}
ret = dtesn_psystem_get_stats(g_kernel_psystem, &kernel_stats);
if (ret < 0) {
mutex_unlock(&g_kernel_psystem_mutex);
return ret;
}
mutex_unlock(&g_kernel_psystem_mutex);
if (copy_to_user(stats, &kernel_stats, sizeof(dtesn_psystem_stats_t))) {
return -EFAULT;
}
return 0;
}
long dtesn_psystem_validate_hierarchy(void) {
bool is_valid;
mutex_lock(&g_kernel_psystem_mutex);
if (!g_kernel_psystem) {
mutex_unlock(&g_kernel_psystem_mutex);
return -ENODEV;
}
is_valid = dtesn_psystem_validate_a000081(g_kernel_psystem);
mutex_unlock(&g_kernel_psystem_mutex);
return is_valid ? 1 : 0;
}
EXPORT_SYMBOL(dtesn_psystem_get_global_stats);
EXPORT_SYMBOL(dtesn_psystem_validate_hierarchy);
module_init(dtesn_psystem_syscalls_init);
module_exit(dtesn_psystem_syscalls_exit);
MODULE_LICENSE("GPL");
MODULE_AUTHOR("Echo.Kern Development Team");
MODULE_DESCRIPTION("DTESN P-System Membrane Computing System Calls Interface");
MODULE_VERSION("1.0");
MODULE_INFO(kernel_version, "5.0+");
MODULE_INFO(performance_targets, "evolution:10us, rules:1us, communication:5us");
MODULE_INFO(oeis_compliance, "A000081 unlabeled rooted trees");