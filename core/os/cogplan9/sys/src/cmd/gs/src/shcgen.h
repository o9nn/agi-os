#ifndef shcgen_INCLUDED
#  define shcgen_INCLUDED
int hc_compute(hc_definition * def, const long *freqs, gs_memory_t * mem);
int hc_bytes_from_definition(byte * dbytes, const hc_definition * def);
void hc_sizes_from_bytes(hc_definition * def, const byte * dbytes, int num_bytes);
void hc_definition_from_bytes(hc_definition * def, const byte * dbytes);
void hc_make_encoding(hce_code * encode, const hc_definition * def);
uint hc_sizeof_decoding(const hc_definition * def, int initial_bits);
void hc_make_decoding(hcd_code * decode, const hc_definition * def,
int initial_bits);
#endif