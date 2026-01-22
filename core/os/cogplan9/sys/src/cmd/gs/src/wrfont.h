#ifndef wrfont_INCLUDED
#define wrfont_INCLUDED
#include "stdpre.h"
typedef struct WRF_output_
{
unsigned char* m_pos;
long m_limit;
long m_count;
bool m_encrypt;
unsigned short m_key;
} WRF_output;
void WRF_init(WRF_output* a_output,unsigned char* a_buffer,long a_buffer_size);
void WRF_wbyte(WRF_output* a_output,unsigned char a_byte);
void WRF_wtext(WRF_output* a_output,const unsigned char* a_string,long a_length);
void WRF_wstring(WRF_output* a_output,const char* a_string);
void WRF_wfloat(WRF_output* a_output,double a_float);
void WRF_wint(WRF_output* a_output,long a_int);
#endif