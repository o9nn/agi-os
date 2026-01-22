#ifndef __wavefront_h__
#define __wavefront_h__
#if (!defined(__GNUC__) && !defined(__GNUG__))
You will not be able to compile this file correctly without gcc, because
it is necessary to pack the "wavefront_alias" structure to a size
of 22 bytes, corresponding to 16-bit alignment (as would have been
the case on the original platform, MS-DOS). If this is not done,
then WavePatch-format files cannot be read/written correctly.
The method used to do this here ("__attribute__((packed)") is
completely compiler dependent.
All other wavefront_* types end up aligned to 32 bit values and
still have the same (correct) size.
#else
#endif __GNUC__
#ifndef NUM_MIDIKEYS
#define NUM_MIDIKEYS 128
#endif  NUM_MIDIKEYS
#ifndef NUM_MIDICHANNELS
#define NUM_MIDICHANNELS 16
#endif  NUM_MIDICHANNELS
typedef short INT16;
typedef unsigned short UINT16;
typedef int INT32;
typedef unsigned int UINT32;
typedef char CHAR8;
typedef unsigned char UCHAR8;
#define WFC_DEBUG_DRIVER                0
#define WFC_FX_IOCTL                    1
#define WFC_PATCH_STATUS                2
#define WFC_PROGRAM_STATUS              3
#define WFC_SAMPLE_STATUS               4
#define WFC_DISABLE_INTERRUPTS          5
#define WFC_ENABLE_INTERRUPTS           6
#define WFC_INTERRUPT_STATUS            7
#define WFC_ROMSAMPLES_RDONLY           8
#define WFC_IDENTIFY_SLOT_TYPE          9
#define WFC_DOWNLOAD_SAMPLE		0x80
#define WFC_DOWNLOAD_BLOCK		0x81
#define WFC_DOWNLOAD_MULTISAMPLE	0x82
#define WFC_DOWNLOAD_SAMPLE_ALIAS	0x83
#define WFC_DELETE_SAMPLE		0x84
#define WFC_REPORT_FREE_MEMORY		0x85
#define WFC_DOWNLOAD_PATCH		0x86
#define WFC_DOWNLOAD_PROGRAM		0x87
#define WFC_SET_SYNTHVOL		0x89
#define WFC_SET_NVOICES			0x8B
#define WFC_DOWNLOAD_DRUM		0x90
#define WFC_GET_SYNTHVOL		0x92
#define WFC_GET_NVOICES			0x94
#define WFC_DISABLE_CHANNEL		0x9A
#define WFC_ENABLE_CHANNEL		0x9B
#define WFC_MISYNTH_OFF			0x9D
#define WFC_MISYNTH_ON			0x9E
#define WFC_FIRMWARE_VERSION		0x9F
#define WFC_GET_NSAMPLES		0xA0
#define WFC_DISABLE_DRUM_PROGRAM	0xA2
#define WFC_UPLOAD_PATCH		0xA3
#define WFC_UPLOAD_PROGRAM		0xA4
#define WFC_SET_TUNING			0xA6
#define WFC_GET_TUNING			0xA7
#define WFC_VMIDI_ON			0xA8
#define WFC_VMIDI_OFF			0xA9
#define WFC_MIDI_STATUS			0xAA
#define WFC_GET_CHANNEL_STATUS		0xAB
#define WFC_DOWNLOAD_SAMPLE_HEADER	0xAC
#define WFC_UPLOAD_SAMPLE_HEADER	0xAD
#define WFC_UPLOAD_MULTISAMPLE		0xAE
#define WFC_UPLOAD_SAMPLE_ALIAS		0xAF
#define WFC_IDENTIFY_SAMPLE_TYPE	0xB0
#define WFC_DOWNLOAD_EDRUM_PROGRAM	0xB1
#define WFC_UPLOAD_EDRUM_PROGRAM	0xB2
#define WFC_SET_EDRUM_CHANNEL		0xB3
#define WFC_INSTOUT_LEVELS		0xB4
#define WFC_PEAKOUT_LEVELS		0xB5
#define WFC_REPORT_CHANNEL_PROGRAMS	0xB6
#define WFC_HARDWARE_VERSION		0xCF
#define WFC_UPLOAD_SAMPLE_PARAMS	0xD7
#define WFC_DOWNLOAD_OS			0xF1
#define WFC_NOOP                        0xFF
#define WF_MAX_SAMPLE   512
#define WF_MAX_PATCH    256
#define WF_MAX_PROGRAM  128
#define WF_SECTION_MAX  44
#define WF_PROGRAM_BYTES 32
#define WF_PATCH_BYTES 132
#define WF_SAMPLE_BYTES 27
#define WF_SAMPLE_HDR_BYTES 25
#define WF_ALIAS_BYTES 25
#define WF_DRUM_BYTES 9
#define WF_MSAMPLE_BYTES 259
#define WF_ACK     0x80
#define WF_DMA_ACK 0x81
#define WF_MIDI_VIRTUAL_ENABLED 0x1
#define WF_MIDI_VIRTUAL_IS_EXTERNAL 0x2
#define WF_MIDI_IN_TO_SYNTH_DISABLED 0x4
#define WF_SYNTH_SLOT         0
#define WF_INTERNAL_MIDI_SLOT 1
#define WF_EXTERNAL_MIDI_SLOT 2
#define WF_EXTERNAL_SWITCH  0xFD
#define WF_INTERNAL_SWITCH  0xF9
#define WF_DEBUG_CMD 0x1
#define WF_DEBUG_DATA 0x2
#define WF_DEBUG_LOAD_PATCH 0x4
#define WF_DEBUG_IO 0x8
#define WF_WAVEPATCH_VERSION     120;
#define WF_MAX_COMMENT           64
#define WF_NUM_LAYERS            4
#define WF_NAME_LENGTH           32
#define WF_SOURCE_LENGTH         260
#define BankFileID     "Bank"
#define DrumkitFileID  "DrumKit"
#define ProgramFileID  "Program"
struct wf_envelope
{
UCHAR8 attack_time:7;
UCHAR8 Unused1:1;
UCHAR8 decay1_time:7;
UCHAR8 Unused2:1;
UCHAR8 decay2_time:7;
UCHAR8 Unused3:1;
UCHAR8 sustain_time:7;
UCHAR8 Unused4:1;
UCHAR8 release_time:7;
UCHAR8 Unused5:1;
UCHAR8 release2_time:7;
UCHAR8 Unused6:1;
CHAR8 attack_level;
CHAR8 decay1_level;
CHAR8 decay2_level;
CHAR8 sustain_level;
CHAR8 release_level;
UCHAR8 attack_velocity:7;
UCHAR8 Unused7:1;
UCHAR8 volume_velocity:7;
UCHAR8 Unused8:1;
UCHAR8 keyboard_scaling:7;
UCHAR8 Unused9:1;
};
typedef struct wf_envelope wavefront_envelope;
struct wf_lfo
{
UCHAR8 sample_number;
UCHAR8 frequency:7;
UCHAR8 Unused1:1;
UCHAR8 am_src:4;
UCHAR8 fm_src:4;
CHAR8 fm_amount;
CHAR8 am_amount;
CHAR8 start_level;
CHAR8 end_level;
UCHAR8 ramp_delay:7;
UCHAR8 wave_restart:1;
UCHAR8 ramp_time:7;
UCHAR8 Unused2:1;
};
typedef struct wf_lfo wavefront_lfo;
struct wf_patch
{
INT16  frequency_bias;
UCHAR8 amplitude_bias:7;
UCHAR8 Unused1:1;
UCHAR8 portamento:7;
UCHAR8 Unused2:1;
UCHAR8 sample_number;
UCHAR8 pitch_bend:4;
UCHAR8 sample_msb:1;
UCHAR8 Unused3:3;
UCHAR8 mono:1;
UCHAR8 retrigger:1;
UCHAR8 nohold:1;
UCHAR8 restart:1;
UCHAR8 filterconfig:2;
UCHAR8 reuse:1;
UCHAR8 reset_lfo:1;
UCHAR8 fm_src2:4;
UCHAR8 fm_src1:4;
CHAR8 fm_amount1;
CHAR8 fm_amount2;
UCHAR8 am_src:4;
UCHAR8 Unused4:4;
CHAR8 am_amount;
UCHAR8 fc1_mode:4;
UCHAR8 fc2_mode:4;
CHAR8 fc1_mod_amount;
CHAR8 fc1_keyboard_scaling;
CHAR8 fc1_bias;
CHAR8 fc2_mod_amount;
CHAR8 fc2_keyboard_scaling;
CHAR8 fc2_bias;
UCHAR8 randomizer:7;
UCHAR8 Unused5:1;
struct wf_envelope envelope1;
struct wf_envelope envelope2;
struct wf_lfo lfo1;
struct wf_lfo lfo2;
};
typedef struct wf_patch wavefront_patch;
struct wf_layer
{
UCHAR8 patch_number;
UCHAR8 mix_level:7;
UCHAR8 mute:1;
UCHAR8 split_point:7;
UCHAR8 play_below:1;
UCHAR8 pan_mod_src:2;
UCHAR8 pan_or_mod:1;
UCHAR8 pan:4;
UCHAR8 split_type:1;
};
typedef struct wf_layer wavefront_layer;
struct wf_program
{
struct wf_layer layer[WF_NUM_LAYERS];
};
typedef struct wf_program wavefront_program;
struct wf_sample_offset
{
INT32 Fraction:4;
INT32 Integer:20;
INT32 Unused:8;
};
typedef struct wf_sample_offset wavefront_sample_offset;
#define WF_ST_SAMPLE      0
#define WF_ST_MULTISAMPLE 1
#define WF_ST_ALIAS       2
#define WF_ST_EMPTY       3
#define WF_ST_DRUM        4
#define WF_ST_PROGRAM     5
#define WF_ST_PATCH       6
#define WF_ST_SAMPLEHDR   7
#define WF_ST_MASK        0xf
#define WF_SLOT_USED      0x80
#define WF_SLOT_FILLED    0x40
#define WF_SLOT_ROM       0x20
#define WF_SLOT_MASK      0xf0
#define WF_CH_MONO  0
#define WF_CH_LEFT  1
#define WF_CH_RIGHT 2
#define LINEAR_16BIT 0
#define WHITE_NOISE  1
#define LINEAR_8BIT  2
#define MULAW_8BIT   3
#define WF_SAMPLE_IS_8BIT(smpl) ((smpl)->SampleResolution&2)
#define WF_SET_CHANNEL(samp,chn) \
(samp)->Unused1 = chn & 0x1; \
(samp)->Unused2 = chn & 0x2; \
(samp)->Unused3 = chn & 0x4
#define WF_GET_CHANNEL(samp) \
(((samp)->Unused3 << 2)|((samp)->Unused2<<1)|(samp)->Unused1)
typedef struct wf_sample {
struct wf_sample_offset sampleStartOffset;
struct wf_sample_offset loopStartOffset;
struct wf_sample_offset loopEndOffset;
struct wf_sample_offset sampleEndOffset;
INT16 FrequencyBias;
UCHAR8 SampleResolution:2;
UCHAR8 Unused1:1;
UCHAR8 Loop:1;
UCHAR8 Bidirectional:1;
UCHAR8 Unused2:1;
UCHAR8 Reverse:1;
UCHAR8 Unused3:1;
} wavefront_sample;
typedef struct wf_multisample {
INT16 NumberOfSamples;
INT16 SampleNumber[NUM_MIDIKEYS];
} wavefront_multisample;
typedef struct wf_alias {
INT16 OriginalSample __attribute__ ((packed));
struct wf_sample_offset sampleStartOffset __attribute__ ((packed));
struct wf_sample_offset loopStartOffset __attribute__ ((packed));
struct wf_sample_offset sampleEndOffset __attribute__ ((packed));
struct wf_sample_offset loopEndOffset __attribute__ ((packed));
INT16  FrequencyBias __attribute__ ((packed));
UCHAR8 SampleResolution:2  __attribute__ ((packed));
UCHAR8 Unused1:1  __attribute__ ((packed));
UCHAR8 Loop:1 __attribute__ ((packed));
UCHAR8 Bidirectional:1  __attribute__ ((packed));
UCHAR8 Unused2:1 __attribute__ ((packed));
UCHAR8 Reverse:1 __attribute__ ((packed));
UCHAR8 Unused3:1 __attribute__ ((packed));
UCHAR8 sixteen_bit_padding __attribute__ ((packed));
} wavefront_alias;
typedef struct wf_drum {
UCHAR8 PatchNumber;
UCHAR8 MixLevel:7;
UCHAR8 Unmute:1;
UCHAR8 Group:4;
UCHAR8 Unused1:4;
UCHAR8 PanModSource:2;
UCHAR8 PanModulated:1;
UCHAR8 PanAmount:4;
UCHAR8 Unused2:1;
} wavefront_drum;
typedef struct wf_drumkit {
struct wf_drum drum[NUM_MIDIKEYS];
} wavefront_drumkit;
typedef struct wf_channel_programs {
UCHAR8 Program[NUM_MIDICHANNELS];
} wavefront_channel_programs;
#define WF_CHANNEL_STATUS(ch,wcp) (wcp)[(ch/7)] & (1<<((ch)%7))
typedef union wf_any {
wavefront_sample s;
wavefront_multisample ms;
wavefront_alias a;
wavefront_program pr;
wavefront_patch p;
wavefront_drum d;
} wavefront_any;
typedef struct wf_patch_info {
INT16   key;
UINT16  devno;
UCHAR8  subkey;
#define WAVEFRONT_FIND_FREE_SAMPLE_SLOT 999
UINT16  number;
UINT32  size;
wavefront_any *hdrptr;
UINT16 *dataptr;
wavefront_any hdr;
} wavefront_patch_info;
#define WF_MAX_READ sizeof(wavefront_multisample)
#define WF_MAX_WRITE sizeof(wavefront_multisample)
typedef struct wavefront_control {
int cmd;
char status;
unsigned char rbuf[WF_MAX_READ];
unsigned char wbuf[WF_MAX_WRITE];
} wavefront_control;
#define WFCTL_WFCMD    0x1
#define WFCTL_LOAD_SPP 0x2
#define WF_MOD_LFO1      0
#define WF_MOD_LFO2      1
#define WF_MOD_ENV1      2
#define WF_MOD_ENV2      3
#define WF_MOD_KEYBOARD  4
#define WF_MOD_LOGKEY    5
#define WF_MOD_VELOCITY  6
#define WF_MOD_LOGVEL    7
#define WF_MOD_RANDOM    8
#define WF_MOD_PRESSURE  9
#define WF_MOD_MOD_WHEEL 10
#define WF_MOD_1         WF_MOD_MOD_WHEEL
#define WF_MOD_BREATH    11
#define WF_MOD_2         WF_MOD_BREATH
#define WF_MOD_FOOT      12
#define WF_MOD_4         WF_MOD_FOOT
#define WF_MOD_VOLUME    13
#define WF_MOD_7         WF_MOD_VOLUME
#define WF_MOD_PAN       14
#define WF_MOD_10        WF_MOD_PAN
#define WF_MOD_EXPR      15
#define WF_MOD_11        WF_MOD_EXPR
typedef struct wf_fx_info {
int request;
int data[4];
} wavefront_fx_info;
#define WFFX_SETOUTGAIN		        0
#define WFFX_SETSTEREOOUTGAIN		1
#define WFFX_SETREVERBIN1GAIN		2
#define WFFX_SETREVERBIN2GAIN		3
#define WFFX_SETREVERBIN3GAIN		4
#define WFFX_SETCHORUSINPORT		5
#define WFFX_SETREVERBIN1PORT		6
#define WFFX_SETREVERBIN2PORT		7
#define WFFX_SETREVERBIN3PORT		8
#define WFFX_SETEFFECTPORT		9
#define WFFX_SETAUXPORT		        10
#define WFFX_SETREVERBTYPE		11
#define WFFX_SETREVERBDELAY		12
#define WFFX_SETCHORUSLFO		13
#define WFFX_SETCHORUSPMD		14
#define WFFX_SETCHORUSAMD		15
#define WFFX_SETEFFECT		        16
#define WFFX_SETBASEALL		        17
#define WFFX_SETREVERBALL		18
#define WFFX_SETCHORUSALL		20
#define WFFX_SETREVERBDEF		22
#define WFFX_SETCHORUSDEF		23
#define WFFX_DELAYSETINGAIN		24
#define WFFX_DELAYSETFBGAIN	        25
#define WFFX_DELAYSETFBLPF		26
#define WFFX_DELAYSETGAIN		27
#define WFFX_DELAYSETTIME		28
#define WFFX_DELAYSETFBTIME		29
#define WFFX_DELAYSETALL		30
#define WFFX_DELAYSETDEF		32
#define WFFX_SDELAYSETINGAIN		33
#define WFFX_SDELAYSETFBGAIN		34
#define WFFX_SDELAYSETFBLPF		35
#define WFFX_SDELAYSETGAIN		36
#define WFFX_SDELAYSETTIME		37
#define WFFX_SDELAYSETFBTIME		38
#define WFFX_SDELAYSETALL		39
#define WFFX_SDELAYSETDEF		41
#define WFFX_DEQSETINGAIN		42
#define WFFX_DEQSETFILTER		43
#define WFFX_DEQSETALL		        44
#define WFFX_DEQSETDEF		        46
#define WFFX_MUTE		        47
#define WFFX_FLANGESETBALANCE	        48
#define WFFX_FLANGESETDELAY		49
#define WFFX_FLANGESETDWFFX_TH		50
#define WFFX_FLANGESETFBGAIN		51
#define WFFX_FLANGESETINGAIN		52
#define WFFX_FLANGESETLFO		53
#define WFFX_FLANGESETALL		54
#define WFFX_FLANGESETDEF		56
#define WFFX_PITCHSETSHIFT		57
#define WFFX_PITCHSETBALANCE		58
#define WFFX_PITCHSETALL		59
#define WFFX_PITCHSETDEF		61
#define WFFX_SRSSETINGAIN		62
#define WFFX_SRSSETSPACE		63
#define WFFX_SRSSETCENTER		64
#define WFFX_SRSSETGAIN		        65
#define WFFX_SRSSETMODE	        	66
#define WFFX_SRSSETDEF		        68
#define WFFX_MEMSET              69
#endif __wavefront_h__