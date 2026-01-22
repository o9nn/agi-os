#ifndef __FHANDLE_H__
#define __FHANDLE_H__
union diskfs_fhandle
{
unsigned char bytes[28];
struct
{
int pad1;
int cache_id;
unsigned int gen;
} data;
};
#endif