#ifndef	_LINUX_CDROM_H
#define	_LINUX_CDROM_H
#define CD_MINS             74
#define CD_SECS             60
#define CD_FRAMES           75
#define CD_SYNC_SIZE        12
#define CD_HEAD_SIZE         4
#define CD_SUBHEAD_SIZE      8
#define CD_XA_HEAD        (CD_HEAD_SIZE+CD_SUBHEAD_SIZE)
#define CD_XA_SYNC_HEAD   (CD_SYNC_SIZE+CD_XA_HEAD)
#define CD_FRAMESIZE      2048
#define CD_FRAMESIZE_RAW  2352
#define CD_FRAMESIZE_RAW1 (CD_FRAMESIZE_RAW-CD_SYNC_SIZE)
#define CD_FRAMESIZE_RAW0 (CD_FRAMESIZE_RAW-CD_SYNC_SIZE-CD_HEAD_SIZE)
#define CD_FRAMESIZE_RAWER 2646
#define CD_EDC_SIZE         4
#define CD_ZERO_SIZE        8
#define CD_ECC_SIZE       276
#define CD_XA_TAIL        (CD_EDC_SIZE+CD_ECC_SIZE)
#define CD_FRAMESIZE_SUB    96
#define CD_MSF_OFFSET      150
#define CD_CHUNK_SIZE       24
#define CD_NUM_OF_CHUNKS    98
#define CD_FRAMESIZE_XA CD_FRAMESIZE_RAW1
#define CD_BLOCK_OFFSET    CD_MSF_OFFSET
struct cdrom_blk
{
unsigned from;
unsigned short len;
};
struct cdrom_msf
{
u_char	cdmsf_min0;
u_char	cdmsf_sec0;
u_char	cdmsf_frame0;
u_char	cdmsf_min1;
u_char	cdmsf_sec1;
u_char	cdmsf_frame1;
};
struct cdrom_ti
{
u_char	cdti_trk0;
u_char	cdti_ind0;
u_char	cdti_trk1;
u_char	cdti_ind1;
};
struct cdrom_tochdr
{
u_char	cdth_trk0;
u_char	cdth_trk1;
};
struct cdrom_msf0
{
u_char	minute;
u_char	second;
u_char	frame;
};
union cdrom_addr
{
struct cdrom_msf0	msf;
int			lba;
};
struct cdrom_tocentry
{
u_char	cdte_track;
u_char	cdte_adr	:4;
u_char	cdte_ctrl	:4;
u_char	cdte_format;
union cdrom_addr cdte_addr;
u_char	cdte_datamode;
};
#define	CDROM_LBA 0x01
#define	CDROM_MSF 0x02
#define	CDROM_DATA_TRACK	0x04
#define	CDROM_LEADOUT	0xAA
struct cdrom_subchnl
{
u_char	cdsc_format;
u_char	cdsc_audiostatus;
u_char	cdsc_adr:	4;
u_char	cdsc_ctrl:	4;
u_char	cdsc_trk;
u_char	cdsc_ind;
union cdrom_addr cdsc_absaddr;
union cdrom_addr cdsc_reladdr;
};
struct cdrom_mcn {
u_char medium_catalog_number[14];
};
#define	CDROM_AUDIO_INVALID	0x00
#define	CDROM_AUDIO_PLAY	0x11
#define	CDROM_AUDIO_PAUSED	0x12
#define	CDROM_AUDIO_COMPLETED	0x13
#define	CDROM_AUDIO_ERROR	0x14
#define	CDROM_AUDIO_NO_STATUS	0x15
struct cdrom_volctrl
{
u_char	channel0;
u_char	channel1;
u_char	channel2;
u_char	channel3;
};
struct cdrom_read
{
int	cdread_lba;
caddr_t	cdread_bufaddr;
int	cdread_buflen;
};
struct cdrom_read_audio
{
union cdrom_addr addr;
u_char addr_format;
int nframes;
u_char *buf;
};
struct cdrom_multisession
{
union cdrom_addr addr;
u_char xa_flag;
u_char addr_format;
};
#ifdef FIVETWELVE
#define	CDROM_MODE1_SIZE	512
#else
#define	CDROM_MODE1_SIZE	2048
#endif
#define	CDROM_MODE2_SIZE	2336
#define CDROMPAUSE		0x5301
#define CDROMRESUME		0x5302
#define CDROMPLAYMSF		0x5303
#define CDROMPLAYTRKIND		0x5304
#define CDROMREADTOCHDR		0x5305
#define CDROMREADTOCENTRY	0x5306
#define CDROMSTOP		0x5307
#define CDROMSTART		0x5308
#define CDROMEJECT		0x5309
#define CDROMVOLCTRL		0x530a
#define CDROMSUBCHNL		0x530b
#define CDROMREADMODE2		0x530c
#define CDROMREADMODE1		0x530d
#define CDROMREADAUDIO		0x530e
#define CDROMEJECT_SW		0x530f
#define CDROMMULTISESSION	0x5310
#define CDROM_GET_UPC		0x5311
#define CDROMRESET		0x5312
#define CDROMVOLREAD		0x5313
#define CDROMREADRAW		0x5314
#define CDROMREADCOOKED		0x5315
#define CDROMSEEK		0x5316
#define CDROMPLAYBLK		0x5317
#define CDROMREADALL		0x5318
#define CDROMCLOSETRAY		0x5319
#define CDROMLOADFROMSLOT	0x531a
#define SCMD_READ_TOC		0x43
#define SCMD_PLAYAUDIO_MSF	0x47
#define SCMD_PLAYAUDIO_TI	0x48
#define SCMD_PAUSE_RESUME	0x4B
#define SCMD_READ_SUBCHANNEL	0x42
#define SCMD_PLAYAUDIO10	0x45
#define SCMD_READ_HEADER	0x44
#define SCMD_PLAYAUDIO12	0xA5
#define SCMD_PLAYTRACK_REL12	0xA9
#define SCMD_CD_PLAYBACK_CONTROL 0xC9
#define SCMD_CD_PLAYBACK_STATUS 0xC4
struct scsi_capacity
{
u_long	capacity;
u_long	lbasize;
};
#define ERR_RECOVERY_PARMS	0x01
#define DISCO_RECO_PARMS	0x02
#define FORMAT_PARMS		0x03
#define GEOMETRY_PARMS		0x04
#define CERTIFICATION_PARMS	0x06
#define CACHE_PARMS		0x38
struct ccs_modesel_head
{
u_char	_r1;
u_char	medium;
u_char 	_r2;
u_char	block_desc_length;
u_char	density;
u_char	number_blocks_hi;
u_char	number_blocks_med;
u_char	number_blocks_lo;
u_char	_r3;
u_char	block_length_hi;
u_short	block_length;
};
struct ccs_err_recovery
{
u_char	_r1 : 2;
u_char	page_code : 6;
u_char	page_length;
u_char	awre	: 1;
u_char	arre	: 1;
u_char	tb	: 1;
u_char 	rc	: 1;
u_char	eec	: 1;
u_char	per	: 1;
u_char	dte	: 1;
u_char	dcr	: 1;
u_char	retry_count;
u_char	correction_span;
u_char	head_offset_count;
u_char	strobe_offset_count;
u_char	recovery_time_limit;
};
struct ccs_disco_reco
{
u_char	_r1	: 2;
u_char	page_code : 6;
u_char	page_length;
u_char	buffer_full_ratio;
u_char	buffer_empty_ratio;
u_short	bus_inactivity_limit;
u_short	disconnect_time_limit;
u_short	connect_time_limit;
u_short	_r2;
};
struct ccs_geometry
{
u_char	_r1	: 2;
u_char	page_code : 6;
u_char	page_length;
u_char	cyl_ub;
u_char	cyl_mb;
u_char	cyl_lb;
u_char	heads;
u_char	precomp_cyl_ub;
u_char	precomp_cyl_mb;
u_char	precomp_cyl_lb;
u_char	current_cyl_ub;
u_char	current_cyl_mb;
u_char	current_cyl_lb;
u_short	step_rate;
u_char	landing_cyl_ub;
u_char	landing_cyl_mb;
u_char	landing_cyl_lb;
u_char  _r2;
u_char	_r3;
u_char	_r4;
};
struct ccs_cache
{
u_char	_r1	: 2;
u_char	page_code : 6;
u_char	page_length;
u_char	mode;
u_char	threshold;
u_char	max_prefetch;
u_char	max_multiplier;
u_char	min_prefetch;
u_char	min_multiplier;
u_char	_r2[8];
};
#endif