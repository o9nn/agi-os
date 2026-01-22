#ifndef gsbittab_INCLUDED
#  define gsbittab_INCLUDED
#define btab2_(v0,v2,v1)\
v0,v1+v0,v2+v0,v2+v1+v0
#define bit_table_2(v0,v2,v1) btab2_(v0,v2,v1)
#define btab4_(v0,v8,v4,v2,v1)\
btab2_(v0,v2,v1), btab2_(v4+v0,v2,v1),\
btab2_(v8+v0,v2,v1), btab2_(v8+v4+v0,v2,v1)
#define bit_table_4(v0,v8,v4,v2,v1) btab4_(v0,v8,v4,v2,v1)
#define btab6_(v0,v20,v10,v8,v4,v2,v1)\
btab4_(v0,v8,v4,v2,v1), btab4_(v10+v0,v8,v4,v2,v1),\
btab4_(v20+v0,v8,v4,v2,v1), btab4_(v20+v10+v0,v8,v4,v2,v1)
#define bit_table_6(v0,v20,v10,v8,v4,v2,v1) btab6_(v0,v20,v10,v8,v4,v2,v1)
#define bit_table_8(v0,v80,v40,v20,v10,v8,v4,v2,v1)\
btab6_(v0,v20,v10,v8,v4,v2,v1), btab6_(v40+v0,v20,v10,v8,v4,v2,v1),\
btab6_(v80+v0,v20,v10,v8,v4,v2,v1), btab6_(v80+v40+v0,v20,v10,v8,v4,v2,v1)
extern const byte byte_reverse_bits[256];
extern const byte byte_right_mask[9];
extern const byte byte_count_bits[256];
extern const byte
byte_bit_run_length_0[256], byte_bit_run_length_1[256],
byte_bit_run_length_2[256], byte_bit_run_length_3[256],
byte_bit_run_length_4[256], byte_bit_run_length_5[256],
byte_bit_run_length_6[256], byte_bit_run_length_7[256];
extern const byte *const byte_bit_run_length[8];
extern const byte *const byte_bit_run_length_neg[8];
extern const byte byte_acegbdfh_to_abcdefgh[256];
#endif