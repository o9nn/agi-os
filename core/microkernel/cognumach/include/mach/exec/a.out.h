#ifndef _MACH_A_OUT_
#define _MACH_A_OUT_
struct exec
{
unsigned long a_magic;
unsigned long a_text;
unsigned long a_data;
unsigned long a_bss;
unsigned long a_syms;
unsigned long a_entry;
unsigned long a_trsize;
unsigned long a_drsize;
};
struct nlist {
long n_strx;
unsigned char n_type;
char n_other;
short n_desc;
unsigned long n_value;
};
#define OMAGIC 0407
#define NMAGIC 0410
#define ZMAGIC 0413
#define QMAGIC 0314
#define N_GETMAGIC(ex) \
( (ex).a_magic & 0xffff )
#define N_GETMAGIC_NET(ex) \
(ntohl((ex).a_magic) & 0xffff)
#define N_BADMAG(ex) \
(N_GETMAGIC(ex) != OMAGIC && N_GETMAGIC(ex) != NMAGIC && \
N_GETMAGIC(ex) != ZMAGIC && N_GETMAGIC(ex) != QMAGIC && \
N_GETMAGIC_NET(ex) != OMAGIC && N_GETMAGIC_NET(ex) != NMAGIC && \
N_GETMAGIC_NET(ex) != ZMAGIC && N_GETMAGIC_NET(ex) != QMAGIC)
#endif