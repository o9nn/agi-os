#include "get_audio.h"
#define MAX_NAME_SIZE 1000
extern sound_file_format input_format;
extern int swapbytes;
extern int silent;
extern int brhist;
extern int mp3_delay;
extern int mp3_delay_set;
extern float update_interval;
#define Min(A, B) ((A) < (B) ? (A) : (B))
#define Max(A, B) ((A) > (B) ? (A) : (B))
#define MAX_U_32_NUM 0xFFFFFFFF