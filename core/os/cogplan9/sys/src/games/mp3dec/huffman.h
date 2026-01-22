# ifndef LIBMAD_HUFFMAN_H
# define LIBMAD_HUFFMAN_H
struct huffquad {
unsigned char final;
struct {
unsigned char bits;
unsigned short offset;
} ptr;
struct {
unsigned char hlen;
unsigned char v, w, x, y;
} value;
};
struct huffpair {
unsigned char final;
struct {
unsigned char bits;
unsigned short offset;
} ptr;
struct {
unsigned char hlen;
unsigned char x;
unsigned char y;
} value;
};
struct hufftable {
struct huffpair const *table;
unsigned short linbits;
unsigned short startbits;
};
extern struct huffquad const *const mad_huff_quad_table[2];
extern struct hufftable const mad_huff_pair_table[32];
# endif