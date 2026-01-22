#ifndef _LINUX_STDDEF_H
#define _LINUX_STDDEF_H
#ifndef __always_inline
#define __always_inline __inline__
#endif
#define __struct_group(TAG, NAME, ATTRS, MEMBERS...) \
union { \
struct { MEMBERS } ATTRS; \
struct TAG { MEMBERS } ATTRS NAME; \
} ATTRS
#ifdef __cplusplus
#define __DECLARE_FLEX_ARRAY(T, member)	\
T member[0]
#else
#define __DECLARE_FLEX_ARRAY(TYPE, NAME)	\
struct { \
struct { } __empty_ ## NAME; \
TYPE NAME[]; \
}
#endif
#ifndef __counted_by
#define __counted_by(m)
#endif
#endif