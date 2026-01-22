#ifndef _LINUX_VIDEO_DECODER_H
#define _LINUX_VIDEO_DECODER_H
struct video_decoder_capability {
__u32 flags;
#define VIDEO_DECODER_PAL 1
#define VIDEO_DECODER_NTSC 2
#define VIDEO_DECODER_SECAM 4
#define VIDEO_DECODER_AUTO 8
#define VIDEO_DECODER_CCIR 16
int inputs;
int outputs;
};
#define DECODER_STATUS_GOOD 1
#define DECODER_STATUS_COLOR 2
#define DECODER_STATUS_PAL 4
#define DECODER_STATUS_NTSC 8
#define DECODER_STATUS_SECAM 16
#define DECODER_GET_CAPABILITIES _IOR('d', 1, struct video_decoder_capability)
#define DECODER_GET_STATUS _IOR('d', 2, int)
#define DECODER_SET_NORM _IOW('d', 3, int)
#define DECODER_SET_INPUT _IOW('d', 4, int)
#define DECODER_SET_OUTPUT _IOW('d', 5, int)
#define DECODER_ENABLE_OUTPUT _IOW('d', 6, int)
#define DECODER_SET_PICTURE _IOW('d', 7, struct video_picture)
#define DECODER_DUMP _IO('d', 192)
#endif