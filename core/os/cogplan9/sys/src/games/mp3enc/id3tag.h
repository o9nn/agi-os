#ifndef LAME_ID3_H
#define LAME_ID3_H
#include "lame.h"
struct id3tag_spec
{
int flags;
const char *title;
const char *artist;
const char *album;
int year;
const char *comment;
int track;
int genre;
};
extern int id3tag_write_v2(lame_global_flags *gfp);
extern int id3tag_write_v1(lame_global_flags *gfp);
#endif