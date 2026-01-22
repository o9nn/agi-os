#ifndef _HURD_REFCOUNT_H_
#define _HURD_REFCOUNT_H_
#ifdef REFCOUNT_DEFINE_EI
#define REFCOUNT_EI
#else
#define REFCOUNT_EI __extern_inline
#endif
#include <assert-backtrace.h>
#include <limits.h>
#include <stdint.h>
typedef unsigned int refcount_t;
REFCOUNT_EI void
refcount_init (refcount_t *ref, unsigned int references)
{
assert_backtrace (references > 0 || !"references must not be zero!");
*ref = references;
}
REFCOUNT_EI unsigned int
refcount_unsafe_ref (refcount_t *ref)
{
unsigned int r;
r = __atomic_add_fetch (ref, 1, __ATOMIC_RELAXED);
assert_backtrace (r != UINT_MAX || !"refcount overflowed!");
return r;
}
REFCOUNT_EI unsigned int
refcount_ref (refcount_t *ref)
{
unsigned int r;
r = refcount_unsafe_ref (ref);
assert_backtrace (r != 1 || !"refcount detected use-after-free!");
return r;
}
REFCOUNT_EI unsigned int
refcount_deref (refcount_t *ref)
{
unsigned int r;
r = __atomic_sub_fetch (ref, 1, __ATOMIC_RELAXED);
assert_backtrace (r != UINT_MAX || !"refcount underflowed!");
return r;
}
REFCOUNT_EI unsigned int
refcount_references (refcount_t *ref)
{
return __atomic_load_n (ref, __ATOMIC_RELAXED);
}
typedef union _references refcounts_t;
struct references {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define REFCOUNT_REFERENCES(_hard, _weak) \
(struct references) { .hard = (_hard), .weak = (_weak) }
uint32_t hard;
uint32_t weak;
#else
#define REFCOUNT_REFERENCES(_hard, _weak) \
(struct references) { .weak = (_weak), .hard = (_hard) }
uint32_t weak;
uint32_t hard;
#endif
};
union _references {
struct references references;
uint64_t value;
};
REFCOUNT_EI void
refcounts_init (refcounts_t *ref, uint32_t hard, uint32_t weak)
{
assert_backtrace ((hard != 0 || weak != 0)
|| !"references must not both be zero!");
ref->references = REFCOUNT_REFERENCES (hard, weak);
}
REFCOUNT_EI void
refcounts_unsafe_ref (refcounts_t *ref, struct references *result)
{
const union _references op = { .references = REFCOUNT_REFERENCES (1, 0) };
union _references r;
r.value = __atomic_add_fetch (&ref->value, op.value, __ATOMIC_RELAXED);
assert_backtrace (r.references.hard != UINT32_MAX
|| !"refcount overflowed!");
if (result)
*result = r.references;
}
REFCOUNT_EI void
refcounts_ref (refcounts_t *ref, struct references *result)
{
struct references r;
refcounts_unsafe_ref (ref, &r);
assert_backtrace (! (r.hard == 1 && r.weak == 0)
|| !"refcount detected use-after-free!");
if (result)
*result = r;
}
REFCOUNT_EI void
refcounts_deref (refcounts_t *ref, struct references *result)
{
const union _references op = { .references = REFCOUNT_REFERENCES (1, 0) };
union _references r;
r.value = __atomic_sub_fetch (&ref->value, op.value, __ATOMIC_RELAXED);
assert_backtrace (r.references.hard != UINT32_MAX
|| !"refcount underflowed!");
if (result)
*result = r.references;
}
REFCOUNT_EI void
refcounts_promote (refcounts_t *ref, struct references *result)
{
const union _references op =
{ .references = REFCOUNT_REFERENCES (1, ~0U) };
union _references r;
r.value = __atomic_add_fetch (&ref->value, op.value, __ATOMIC_RELAXED);
assert_backtrace (r.references.hard != UINT32_MAX
|| !"refcount overflowed!");
assert_backtrace (r.references.weak != UINT32_MAX
|| !"refcount underflowed!");
if (result)
*result = r.references;
}
REFCOUNT_EI void
refcounts_demote (refcounts_t *ref, struct references *result)
{
const union _references op = { .references = REFCOUNT_REFERENCES (~0U, 0) };
union _references r;
r.value = __atomic_add_fetch (&ref->value, op.value, __ATOMIC_RELAXED);
assert_backtrace (r.references.hard != UINT32_MAX
|| !"refcount underflowed!");
assert_backtrace (r.references.weak != UINT32_MAX
|| !"refcount overflowed!");
if (result)
*result = r.references;
}
REFCOUNT_EI void
refcounts_unsafe_ref_weak (refcounts_t *ref, struct references *result)
{
const union _references op = { .references = REFCOUNT_REFERENCES (0, 1) };
union _references r;
r.value = __atomic_add_fetch (&ref->value, op.value, __ATOMIC_RELAXED);
assert_backtrace (r.references.weak != UINT32_MAX
|| !"refcount overflowed!");
if (result)
*result = r.references;
}
REFCOUNT_EI void
refcounts_ref_weak (refcounts_t *ref, struct references *result)
{
struct references r;
refcounts_unsafe_ref_weak (ref, &r);
assert_backtrace (! (r.hard == 0 && r.weak == 1)
|| !"refcount detected use-after-free!");
if (result)
*result = r;
}
REFCOUNT_EI void
refcounts_deref_weak (refcounts_t *ref, struct references *result)
{
const union _references op = { .references = REFCOUNT_REFERENCES (0, 1) };
union _references r;
r.value = __atomic_sub_fetch (&ref->value, op.value, __ATOMIC_RELAXED);
assert_backtrace (r.references.weak != UINT32_MAX
|| !"refcount underflowed!");
if (result)
*result = r.references;
}
REFCOUNT_EI void
refcounts_references (refcounts_t *ref, struct references *result)
{
union _references r;
r.value =__atomic_load_n (&ref->value, __ATOMIC_RELAXED);
*result = r.references;
}
REFCOUNT_EI uint32_t
refcounts_hard_references (refcounts_t *ref)
{
struct references result;
refcounts_references (ref, &result);
return result.hard;
}
REFCOUNT_EI uint32_t
refcounts_weak_references (refcounts_t *ref)
{
struct references result;
refcounts_references (ref, &result);
return result.weak;
}
#endif