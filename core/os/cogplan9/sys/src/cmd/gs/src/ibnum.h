#ifndef ibnum_INCLUDED
#  define ibnum_INCLUDED
#define BYTE_SWAP_IEEE_NATIVE_REALS 1
#define bt_num_array_value 149
#define num_int32 0
#define num_int16 32
#define num_float 48
#define num_float_IEEE num_float
#define num_float_native (num_float + 1)
#define num_msb 0
#define num_lsb 128
#define num_is_lsb(format) ((format) >= num_lsb)
#define num_is_valid(format) (((format) & 127) <= 49)
#define num_array 256
extern const byte enc_num_bytes[];
#define enc_num_bytes_values\
4, 4, 2, 4, 0, 0, 0, 0,\
4, 4, 2, 4, 0, 0, 0, 0,\
sizeof(ref)
#define encoded_number_bytes(format)\
(enc_num_bytes[(format) >> 4])
int num_array_format(const ref *);
uint num_array_size(const ref *, int);
int num_array_get(const gs_memory_t *mem, const ref *, int, uint, ref *);
int sdecode_number(const byte *, int, ref *);
int sdecodeshort(const byte *, int);
uint sdecodeushort(const byte *, int);
long sdecodelong(const byte *, int);
float sdecodefloat(const byte *, int);
#endif