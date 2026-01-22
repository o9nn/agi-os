#ifndef __XEN_PUBLIC_XEN_COMPAT_H__
#define __XEN_PUBLIC_XEN_COMPAT_H__
#define __XEN_LATEST_INTERFACE_VERSION__ 0x00030209
#if defined(__XEN__) || defined(__XEN_TOOLS__)
#define __XEN_INTERFACE_VERSION__ __XEN_LATEST_INTERFACE_VERSION__
#elif !defined(__XEN_INTERFACE_VERSION__)
#define __XEN_INTERFACE_VERSION__ 0x00000000
#endif
#if __XEN_INTERFACE_VERSION__ > __XEN_LATEST_INTERFACE_VERSION__
#error "These header files do not support the requested interface version."
#endif
#endif