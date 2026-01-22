#ifndef gscedata_INCLUDED
# define gscedata_INCLUDED
#define NUM_LEN_BITS 5
#define N(len,offset) (((offset) << NUM_LEN_BITS) + (len))
#define N_LEN(e) ((e) & ((1 << NUM_LEN_BITS) - 1))
#define N_OFFSET(e) ((e) >> NUM_LEN_BITS)
extern const char gs_c_known_encoding_chars[];
extern const int gs_c_known_encoding_total_chars;
extern const int gs_c_known_encoding_max_length;
extern const ushort gs_c_known_encoding_offsets[];
extern const int gs_c_known_encoding_count;
extern const ushort *const gs_c_known_encodings[];
extern const ushort *const gs_c_known_encodings_reverse[];
extern const ushort gs_c_known_encoding_lengths[];
extern const ushort gs_c_known_encoding_reverse_lengths[];
#endif