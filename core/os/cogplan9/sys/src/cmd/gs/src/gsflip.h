#ifndef gsflip_INCLUDED
#  define gsflip_INCLUDED
extern int image_flip_planes(byte * buffer, const byte ** planes,
int offset, int nbytes,
int num_planes, int bits_per_sample);
#endif