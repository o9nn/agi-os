#include "wrfont.h"
#include "write_t1.h"
#include <assert.h>
static void write_word_entry(FAPI_font* a_fapi_font,WRF_output* a_output,const char* a_name,int a_index,int a_divisor)
{
short x;
WRF_wbyte(a_output,'/');
WRF_wstring(a_output,a_name);
WRF_wbyte(a_output,' ');
x = a_fapi_font->get_word(a_fapi_font,a_index,0);
x = (short)(x / a_divisor);
WRF_wint(a_output,x);
WRF_wstring(a_output," def\n");
}
static void write_array_entry_with_count(FAPI_font* a_fapi_font,WRF_output* a_output,const char* a_name,int a_index,int a_count,int a_divisor)
{
int i;
assert(a_count >= 0);
if (a_count == 0)
return;
WRF_wbyte(a_output,'/');
WRF_wstring(a_output,a_name);
WRF_wstring(a_output," [");
for (i = 0; i < a_count; i++)
{
short x = a_fapi_font->get_word(a_fapi_font,a_index,i);
x = (short)(x / a_divisor);
WRF_wint(a_output,x);
WRF_wbyte(a_output,(byte)(i == a_count - 1 ? ']' : ' '));
}
WRF_wstring(a_output," def\n");
}
static void write_array_entry(FAPI_font* a_fapi_font,WRF_output* a_output,const char* a_name,int a_index,int a_divisor)
{
int count = a_fapi_font->get_word(a_fapi_font,a_index - 1,0);
write_array_entry_with_count(a_fapi_font,a_output,a_name,a_index,count,a_divisor);
}
static void write_subrs(FAPI_font* a_fapi_font,WRF_output* a_output)
{
int i;
int count = a_fapi_font->get_word(a_fapi_font,FAPI_FONT_FEATURE_Subrs_count,0);
assert(count >= 0);
if (count == 0)
return;
WRF_wstring(a_output,"/Subrs ");
WRF_wint(a_output,count);
WRF_wstring(a_output," array\n");
for (i = 0; i < count; i++)
{
long length = a_fapi_font->get_subr(a_fapi_font,i,0,0);
long buffer_size;
WRF_wstring(a_output,"dup ");
WRF_wint(a_output,i);
WRF_wbyte(a_output,' ');
WRF_wint(a_output,length);
WRF_wstring(a_output," RD ");
buffer_size = a_output->m_limit - a_output->m_count;
if (buffer_size >= length)
{
a_fapi_font->get_subr(a_fapi_font,i,a_output->m_pos,(ushort)length);
WRF_wtext(a_output,a_output->m_pos,length);
}
else
a_output->m_count += length;
WRF_wstring(a_output," NP\n");
}
WRF_wstring(a_output,"ND\n");
}
static void write_private_dictionary(FAPI_font* a_fapi_font,WRF_output* a_output)
{
assert(!a_output->m_encrypt);
a_output->m_encrypt = true;
WRF_wstring(a_output,"XXXX");
WRF_wstring(a_output,"dup /Private 8 dict dup begin\n");
WRF_wstring(a_output,"/MinFeature {16 16} def\n");
WRF_wstring(a_output,"/password 5839 def\n");
WRF_wstring(a_output,"/lenIV -1 def\n");
write_word_entry(a_fapi_font,a_output,"BlueFuzz",FAPI_FONT_FEATURE_BlueFuzz,16);
WRF_wstring(a_output,"/BlueScale ");
WRF_wfloat(a_output,a_fapi_font->get_long(a_fapi_font,FAPI_FONT_FEATURE_BlueScale,0) / 65536.0);
WRF_wstring(a_output," def\n");
write_word_entry(a_fapi_font,a_output,"BlueShift",FAPI_FONT_FEATURE_BlueShift,16);
write_array_entry(a_fapi_font,a_output,"BlueValues",FAPI_FONT_FEATURE_BlueValues,16);
write_array_entry(a_fapi_font,a_output,"OtherBlues",FAPI_FONT_FEATURE_OtherBlues,16);
write_array_entry(a_fapi_font,a_output,"FamilyBlues",FAPI_FONT_FEATURE_FamilyBlues,16);
write_array_entry(a_fapi_font,a_output,"FamilyOtherBlues",FAPI_FONT_FEATURE_FamilyOtherBlues,16);
write_word_entry(a_fapi_font,a_output,"ForceBold",FAPI_FONT_FEATURE_ForceBold,1);
write_array_entry_with_count(a_fapi_font,a_output,"StdHW",FAPI_FONT_FEATURE_StdHW,1,16);
write_array_entry_with_count(a_fapi_font,a_output,"StdVW",FAPI_FONT_FEATURE_StdVW,1,16);
write_array_entry(a_fapi_font,a_output,"StemSnapH",FAPI_FONT_FEATURE_StemSnapH,16);
write_array_entry(a_fapi_font,a_output,"StemSnapV",FAPI_FONT_FEATURE_StemSnapV,16);
write_subrs(a_fapi_font,a_output);
}
static void write_main_dictionary(FAPI_font* a_fapi_font,WRF_output* a_output)
{
int i;
WRF_wstring(a_output,"5 dict begin\n");
WRF_wstring(a_output,"/FontType 1 def\n");
WRF_wstring(a_output,"/FontMatrix [");
for (i = 0; i < 6; i++)
{
WRF_wfloat(a_output,a_fapi_font->get_float(a_fapi_font,FAPI_FONT_FEATURE_FontMatrix,i));
WRF_wbyte(a_output,(byte)(i == 5 ? ']' : ' '));
}
WRF_wbyte(a_output,'\n');
WRF_wstring(a_output,"/Encoding StandardEncoding def\n");
WRF_wstring(a_output,"/FontBBox {");
for (i = 0; i < 4; i++)
{
short x = a_fapi_font->get_word(a_fapi_font,FAPI_FONT_FEATURE_FontBBox,i);
WRF_wint(a_output,x);
WRF_wbyte(a_output,(byte)(i == 3 ? '}' : ' '));
}
WRF_wbyte(a_output,'\n');
WRF_wstring(a_output,"currentdict end\ncurrentfile eexec\n");
write_private_dictionary(a_fapi_font,a_output);
}
long FF_serialize_type1_font(FAPI_font* a_fapi_font,unsigned char* a_buffer,long a_buffer_size)
{
WRF_output output;
WRF_init(&output,a_buffer,a_buffer_size);
WRF_wstring(&output,"%!PS-AdobeFont-1\n");
write_main_dictionary(a_fapi_font,&output);
return output.m_count;
}