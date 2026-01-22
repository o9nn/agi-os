#ifndef LAME_TIMESTATUS_H
#define LAME_TIMESTATUS_H
void timestatus_klemm(const lame_global_flags *gfp);
void timestatus ( int samp_rate,
int frameNum,
int totalframes,
int           framesize);
void timestatus_finish(void);
void decoder_progress(const lame_global_flags *gfp,const mp3data_struct *);
void decoder_progress_finish(const lame_global_flags *gfp);
#endif