#ifndef _DEVICE_AUDIO_STATUS_H_
#define _DEVICE_AUDIO_STATUS_H_
#define AUDIO_MIN_GAIN (0)
#define AUDIO_MAX_GAIN (255)
#define AUDIO_ENCODING_ULAW (1)
#define AUDIO_ENCODING_ALAW (2)
#define AUDIO_MIKE 1
#define AUDIO_SPEAKER 1
#define AUDIO_HEADPHONE 2
struct audio_prinfo {
unsigned int sample_rate;
unsigned int channels;
unsigned int precision;
unsigned int encoding;
unsigned int gain;
unsigned int port;
unsigned int seek;
unsigned int ispare[3];
unsigned int samples;
unsigned int eof;
unsigned char pause;
unsigned char error;
unsigned char waiting;
unsigned char cspare[3];
unsigned char open;
unsigned char active;
};
struct audio_info {
struct audio_prinfo play;
struct audio_prinfo record;
unsigned int monitor_gain;
unsigned int blocksize;
unsigned int hiwat;
unsigned int lowat;
unsigned int backlog;
};
typedef struct audio_info audio_info_t;
#define AUDIO_INITINFO(p)\
(void)memset((void *)(p), 0xff, sizeof(struct audio_info))
#define AUDIO_GETINFO _IOR('A', 21, audio_info_t)
#define AUDIO_SETINFO _IOWR('A', 22, audio_info_t)
#define AUDIO_DRAIN _IO('A', 23)
#define AUDIO_FLUSH _IO('A', 24)
#define AUDIO_WSEEK _IOR('A', 25, unsigned int)
#define AUDIO_RERROR _IOR('A', 26, int)
#define AUDIO_WERROR _IOR('A', 27, int)
struct mapreg {
unsigned short mr_x[8];
unsigned short mr_r[8];
unsigned short mr_gx;
unsigned short mr_gr;
unsigned short mr_ger;
unsigned short mr_stgr;
unsigned short mr_ftgr;
unsigned short mr_atgr;
unsigned char mr_mmr1;
unsigned char mr_mmr2;
};
#define AUDIO_GETMAP _IOR('A', 27, struct mapreg)
#define AUDIO_SETMAP _IOW('A', 28, struct mapreg)
struct audio_ioctl {
short control;
unsigned char data[46];
};
#define AUDIOGETREG _IOWR('i',1,struct audio_ioctl)
#define AUDIOSETREG _IOW('i',2,struct audio_ioctl)
#endif