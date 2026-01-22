#ifndef AWE_VOICE_H
#define AWE_VOICE_H
#ifndef SAMPLE_TYPE_AWE32
#define SAMPLE_TYPE_AWE32 0x20
#endif
#ifndef _PATCHKEY
#define _PATCHKEY(id) ((id<<8)|0xfd)
#endif
typedef struct awe_patch_info {
short key;
#define AWE_PATCH _PATCHKEY(0x07)
short device_no;
unsigned short sf_id;
short optarg;
int len;
short type;
#define AWE_LOAD_INFO 0
#define AWE_LOAD_DATA 1
#define AWE_OPEN_PATCH 2
#define AWE_CLOSE_PATCH 3
#define AWE_UNLOAD_PATCH 4
#define AWE_REPLACE_DATA 5
#define AWE_MAP_PRESET 6
#define AWE_PROBE_DATA 8
#define AWE_LOAD_CHORUS_FX 0x10
#define AWE_LOAD_REVERB_FX 0x11
short reserved;
#if defined(AWE_COMPAT_030) && AWE_COMPAT_030
char data[0];
#endif
} awe_patch_info;
#define AWE_PATCH_INFO_SIZE sizeof(awe_patch_info)
#define AWE_PATCH_NAME_LEN 32
typedef struct _awe_open_parm {
unsigned short type;
#define AWE_PAT_TYPE_MISC 0
#define AWE_PAT_TYPE_GM 1
#define AWE_PAT_TYPE_GS 2
#define AWE_PAT_TYPE_MT32 3
#define AWE_PAT_TYPE_XG 4
#define AWE_PAT_TYPE_SFX 5
#define AWE_PAT_TYPE_GUS 6
#define AWE_PAT_TYPE_MAP 7
#define AWE_PAT_LOCKED 0x100
#define AWE_PAT_SHARED 0x200
short reserved;
char name[AWE_PATCH_NAME_LEN];
} awe_open_parm;
#define AWE_OPEN_PARM_SIZE sizeof(awe_open_parm)
typedef struct _awe_voice_parm {
unsigned short moddelay;
unsigned short modatkhld;
unsigned short moddcysus;
unsigned short modrelease;
short modkeyhold, modkeydecay;
unsigned short voldelay;
unsigned short volatkhld;
unsigned short voldcysus;
unsigned short volrelease;
short volkeyhold, volkeydecay;
unsigned short lfo1delay;
unsigned short lfo2delay;
unsigned short pefe;
unsigned short fmmod;
unsigned short tremfrq;
unsigned short fm2frq2;
unsigned char cutoff;
unsigned char filterQ;
unsigned char chorus;
unsigned char reverb;
unsigned short reserved[4];
} awe_voice_parm;
typedef struct _awe_voice_parm_block {
unsigned short moddelay;
unsigned char modatk, modhld;
unsigned char moddcy, modsus;
unsigned char modrel, moddummy;
short modkeyhold, modkeydecay;
unsigned short voldelay;
unsigned char volatk, volhld;
unsigned char voldcy, volsus;
unsigned char volrel, voldummy;
short volkeyhold, volkeydecay;
unsigned short lfo1delay;
unsigned short lfo2delay;
unsigned char env1fc, env1pit;
unsigned char lfo1fc, lfo1pit;
unsigned char lfo1freq, lfo1vol;
unsigned char lfo2freq, lfo2pit;
unsigned char cutoff;
unsigned char filterQ;
unsigned char chorus;
unsigned char reverb;
unsigned short reserved[4];
} awe_voice_parm_block;
#define AWE_VOICE_PARM_SIZE 48
typedef struct _awe_voice_info {
unsigned short sf_id;
unsigned short sample;
int start, end;
int loopstart, loopend;
short rate_offset;
unsigned short mode;
#define AWE_MODE_ROMSOUND 0x8000
#define AWE_MODE_STEREO 1
#define AWE_MODE_LOOPING 2
#define AWE_MODE_NORELEASE 4
#define AWE_MODE_INIT_PARM 8
short root;
short tune;
char low, high;
char vellow, velhigh;
char fixkey, fixvel;
char pan, fixpan;
short exclusiveClass;
unsigned char amplitude;
unsigned char attenuation;
short scaleTuning;
awe_voice_parm parm;
short index;
} awe_voice_info;
#define AWE_VOICE_INFO_SIZE sizeof(awe_voice_info)
typedef struct _awe_voice_rec_hdr {
unsigned char bank;
unsigned char instr;
char nvoices;
char write_mode;
#define AWE_WR_APPEND 0
#define AWE_WR_EXCLUSIVE 1
#define AWE_WR_REPLACE 2
} awe_voice_rec_hdr;
#define AWE_VOICE_REC_SIZE sizeof(awe_voice_rec_hdr)
typedef struct _awe_voice_rec_patch {
awe_patch_info patch;
awe_voice_rec_hdr hdr;
awe_voice_info info;
} awe_voice_rec_patch;
#if defined(AWE_COMPAT_030) && AWE_COMPAT_030
#define AWE_INFOARRAY_SIZE 0
#else
#define AWE_INFOARRAY_SIZE 1
#endif
typedef struct _awe_voice_rec {
unsigned char bank;
unsigned char instr;
short nvoices;
awe_voice_info info[AWE_INFOARRAY_SIZE];
} awe_voice_rec;
typedef struct awe_sample_info {
unsigned short sf_id;
unsigned short sample;
int start, end;
int loopstart, loopend;
int size;
short checksum_flag;
unsigned short mode_flags;
#define AWE_SAMPLE_8BITS 1
#define AWE_SAMPLE_UNSIGNED 2
#define AWE_SAMPLE_NO_BLANK 4
#define AWE_SAMPLE_SINGLESHOT 8
#define AWE_SAMPLE_BIDIR_LOOP 16
#define AWE_SAMPLE_STEREO_LEFT 32
#define AWE_SAMPLE_STEREO_RIGHT 64
#define AWE_SAMPLE_REVERSE_LOOP 128
unsigned int checksum;
#if defined(AWE_COMPAT_030) && AWE_COMPAT_030
unsigned short data[0];
#endif
} awe_sample_info;
#define AWE_SAMPLE_INFO_SIZE sizeof(awe_sample_info)
typedef struct awe_voice_map {
int map_bank, map_instr, map_key;
int src_bank, src_instr, src_key;
} awe_voice_map;
#define AWE_VOICE_MAP_SIZE sizeof(awe_voice_map)
#define _AWE_DEBUG_MODE 0x00
#define _AWE_REVERB_MODE 0x01
#define _AWE_CHORUS_MODE 0x02
#define _AWE_REMOVE_LAST_SAMPLES 0x03
#define _AWE_INITIALIZE_CHIP 0x04
#define _AWE_SEND_EFFECT 0x05
#define _AWE_TERMINATE_CHANNEL 0x06
#define _AWE_TERMINATE_ALL 0x07
#define _AWE_INITIAL_VOLUME 0x08
#define _AWE_INITIAL_ATTEN _AWE_INITIAL_VOLUME
#define _AWE_RESET_CHANNEL 0x09
#define _AWE_CHANNEL_MODE 0x0a
#define _AWE_DRUM_CHANNELS 0x0b
#define _AWE_MISC_MODE 0x0c
#define _AWE_RELEASE_ALL 0x0d
#define _AWE_NOTEOFF_ALL 0x0e
#define _AWE_CHN_PRESSURE 0x0f
#define _AWE_EQUALIZER 0x11
#define _AWE_MODE_FLAG 0x80
#define _AWE_COOKED_FLAG 0x40
#define _AWE_MODE_VALUE_MASK 0x3F
#define _AWE_SET_CMD(p,dev,voice,cmd,p1,p2) \
{((char*)(p))[0] = SEQ_PRIVATE;\
((char*)(p))[1] = dev;\
((char*)(p))[2] = _AWE_MODE_FLAG|(cmd);\
((char*)(p))[3] = voice;\
((unsigned short*)(p))[2] = p1;\
((unsigned short*)(p))[3] = p2;}
#define _AWE_CMD(dev, voice, cmd, p1, p2) \
{_SEQ_NEEDBUF(8);\
_AWE_SET_CMD(_seqbuf + _seqbufptr, dev, voice, cmd, p1, p2);\
_SEQ_ADVBUF(8);}
#define _AWE_CMD_NOW(seqfd,dev,voice,cmd,p1,p2) \
{struct seq_event_rec tmp;\
_AWE_SET_CMD(&tmp, dev, voice, cmd, p1, p2);\
ioctl(seqfd, SNDCTL_SEQ_OUTOFBAND, &tmp);}
#define AWE_DEBUG_MODE(dev,p1) _AWE_CMD(dev, 0, _AWE_DEBUG_MODE, p1, 0)
#define AWE_REVERB_MODE(dev,p1) _AWE_CMD(dev, 0, _AWE_REVERB_MODE, p1, 0)
#define AWE_CHORUS_MODE(dev,p1) _AWE_CMD(dev, 0, _AWE_CHORUS_MODE, p1, 0)
#define AWE_RESET_CHANNEL(dev,ch) _AWE_CMD(dev, ch, _AWE_RESET_CHANNEL, 0, 0)
#define AWE_RESET_CONTROL(dev,ch) _AWE_CMD(dev, ch, _AWE_RESET_CHANNEL, 1, 0)
#define AWE_SEND_EFFECT(dev,voice,type,value) _AWE_CMD(dev,voice,_AWE_SEND_EFFECT,type,value)
#define AWE_ADD_EFFECT(dev,voice,type,value) _AWE_CMD(dev,voice,_AWE_SEND_EFFECT,((type)|0x80),value)
#define AWE_UNSET_EFFECT(dev,voice,type) _AWE_CMD(dev,voice,_AWE_SEND_EFFECT,((type)|0x40),0)
#define AWE_SEND_LAYER_EFFECT(dev,voice,layer,type,value) _AWE_CMD(dev,voice,_AWE_SEND_EFFECT,((layer+1)<<8|(type)),value)
#define AWE_ADD_LAYER_EFFECT(dev,voice,layer,type,value) _AWE_CMD(dev,voice,_AWE_SEND_EFFECT,((layer+1)<<8|(type)|0x80),value)
#define AWE_UNSET_LAYER_EFFECT(dev,voice,layer,type) _AWE_CMD(dev,voice,_AWE_SEND_EFFECT,((layer+1)<<8|(type)|0x40),0)
#define AWE_TERMINATE_CHANNEL(dev,voice) _AWE_CMD(dev,voice,_AWE_TERMINATE_CHANNEL,0,0)
#define AWE_TERMINATE_ALL(dev) _AWE_CMD(dev, 0, _AWE_TERMINATE_ALL, 0, 0)
#define AWE_RELEASE_ALL(dev) _AWE_CMD(dev, 0, _AWE_RELEASE_ALL, 0, 0)
#define AWE_NOTEOFF_ALL(dev) _AWE_CMD(dev, 0, _AWE_NOTEOFF_ALL, 0, 0)
#define AWE_INITIAL_VOLUME(dev,atten) _AWE_CMD(dev, 0, _AWE_INITIAL_VOLUME, atten, 0)
#define AWE_INITIAL_ATTEN AWE_INITIAL_VOLUME
#define AWE_SET_ATTEN(dev,atten) _AWE_CMD(dev, 0, _AWE_INITIAL_VOLUME, atten, 1)
#define AWE_SET_CHANNEL_MODE(dev,mode) _AWE_CMD(dev, 0, _AWE_CHANNEL_MODE, mode, 0)
#define AWE_PLAY_INDIRECT 0
#define AWE_PLAY_MULTI 1
#define AWE_PLAY_DIRECT 2
#define AWE_PLAY_MULTI2 3
#define AWE_DRUM_CHANNELS(dev,channels) _AWE_CMD(dev, 0, _AWE_DRUM_CHANNELS, ((channels) & 0xffff), ((channels) >> 16))
#define AWE_EQUALIZER(dev,bass,treble) _AWE_CMD(dev, 0, _AWE_EQUALIZER, bass, treble)
#define AWE_REMOVE_LAST_SAMPLES(seqfd,dev) _AWE_CMD_NOW(seqfd, dev, 0, _AWE_REMOVE_LAST_SAMPLES, 0, 0)
#define AWE_INITIALIZE_CHIP(seqfd,dev) _AWE_CMD_NOW(seqfd, dev, 0, _AWE_INITIALIZE_CHIP, 0, 0)
#define AWE_MISC_MODE(dev,mode,value) _AWE_CMD(dev, 0, _AWE_MISC_MODE, mode, value)
#define AWE_EXCLUSIVE_SOUND(dev,mode) AWE_MISC_MODE(dev,AWE_MD_EXCLUSIVE_SOUND,mode)
#define AWE_SET_GUS_BANK(dev,bank) AWE_MISC_MODE(dev,AWE_MD_GUS_BANK,bank)
#define AWE_REALTIME_PAN(dev,mode) AWE_MISC_MODE(dev,AWE_MD_REALTIME_PAN,mode)
#define AWE_KEY_PRESSURE(dev,ch,note,vel) SEQ_START_NOTE(dev,ch,(note)+128,vel)
#define AWE_CHN_PRESSURE(dev,ch,vel) _AWE_CMD(dev,ch,_AWE_CHN_PRESSURE,vel,0)
#define AWE_REVERB_ROOM1 0
#define AWE_REVERB_ROOM2 1
#define AWE_REVERB_ROOM3 2
#define AWE_REVERB_HALL1 3
#define AWE_REVERB_HALL2 4
#define AWE_REVERB_PLATE 5
#define AWE_REVERB_DELAY 6
#define AWE_REVERB_PANNINGDELAY 7
#define AWE_REVERB_PREDEFINED 8
#define AWE_REVERB_NUMBERS 32
typedef struct awe_reverb_fx_rec {
unsigned short parms[28];
} awe_reverb_fx_rec;
#define AWE_CHORUS_1 0
#define AWE_CHORUS_2 1
#define AWE_CHORUS_3 2
#define AWE_CHORUS_4 3
#define AWE_CHORUS_FEEDBACK 4
#define AWE_CHORUS_FLANGER 5
#define AWE_CHORUS_SHORTDELAY 6
#define AWE_CHORUS_SHORTDELAY2 7
#define AWE_CHORUS_PREDEFINED 8
#define AWE_CHORUS_NUMBERS 32
typedef struct awe_chorus_fx_rec {
unsigned short feedback;
unsigned short delay_offset;
unsigned short lfo_depth;
unsigned int delay;
unsigned int lfo_freq;
} awe_chorus_fx_rec;
enum {
AWE_MD_EXCLUSIVE_OFF,
AWE_MD_EXCLUSIVE_ON,
AWE_MD_VERSION,
AWE_MD_EXCLUSIVE_SOUND,
AWE_MD_REALTIME_PAN,
AWE_MD_GUS_BANK,
AWE_MD_KEEP_EFFECT,
AWE_MD_ZERO_ATTEN,
AWE_MD_CHN_PRIOR,
AWE_MD_MOD_SENSE,
AWE_MD_DEF_PRESET,
AWE_MD_DEF_BANK,
AWE_MD_DEF_DRUM,
AWE_MD_TOGGLE_DRUM_BANK,
AWE_MD_NEW_VOLUME_CALC,
AWE_MD_CHORUS_MODE,
AWE_MD_REVERB_MODE,
AWE_MD_BASS_LEVEL,
AWE_MD_TREBLE_LEVEL,
AWE_MD_DEBUG_MODE,
AWE_MD_PAN_EXCHANGE,
AWE_MD_END,
};
enum {
AWE_FX_ENV1_DELAY,
AWE_FX_ENV1_ATTACK,
AWE_FX_ENV1_HOLD,
AWE_FX_ENV1_DECAY,
AWE_FX_ENV1_RELEASE,
AWE_FX_ENV1_SUSTAIN,
AWE_FX_ENV1_PITCH,
AWE_FX_ENV1_CUTOFF,
AWE_FX_ENV2_DELAY,
AWE_FX_ENV2_ATTACK,
AWE_FX_ENV2_HOLD,
AWE_FX_ENV2_DECAY,
AWE_FX_ENV2_RELEASE,
AWE_FX_ENV2_SUSTAIN,
AWE_FX_LFO1_DELAY,
AWE_FX_LFO1_FREQ,
AWE_FX_LFO1_VOLUME,
AWE_FX_LFO1_PITCH,
AWE_FX_LFO1_CUTOFF,
AWE_FX_LFO2_DELAY,
AWE_FX_LFO2_FREQ,
AWE_FX_LFO2_PITCH,
AWE_FX_INIT_PITCH,
AWE_FX_CHORUS,
AWE_FX_REVERB,
AWE_FX_CUTOFF,
AWE_FX_FILTERQ,
AWE_FX_SAMPLE_START,
AWE_FX_LOOP_START,
AWE_FX_LOOP_END,
AWE_FX_COARSE_SAMPLE_START,
AWE_FX_COARSE_LOOP_START,
AWE_FX_COARSE_LOOP_END,
AWE_FX_ATTEN,
AWE_FX_END,
};
#endif