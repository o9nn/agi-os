#ifndef _LINUX_VIDEO_ENCODER_H
#define _LINUX_VIDEO_ENCODER_H
struct video_encoder_capability {
__u32	flags;
#define	VIDEO_ENCODER_PAL	1
#define	VIDEO_ENCODER_NTSC	2
#define	VIDEO_ENCODER_SECAM	4
#define	VIDEO_ENCODER_CCIR	16
int	inputs;
int	outputs;
};
#define	ENCODER_GET_CAPABILITIES _IOR('e', 1, struct video_encoder_capability)
#define	ENCODER_SET_NORM	_IOW('e', 2, int)
#define	ENCODER_SET_INPUT	_IOW('e', 3, int)
#define	ENCODER_SET_OUTPUT	_IOW('e', 4, int)
#define	ENCODER_ENABLE_OUTPUT	_IOW('e', 5, int)
#endif