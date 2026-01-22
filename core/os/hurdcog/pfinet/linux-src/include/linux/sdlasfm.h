#ifndef _SDLASFM_H
#define _SDLASFM_H
#define SFM_VERSION 2
#define SFM_SIGNATURE "SFM - Sangoma SDLA Firmware Module"
#define SFM_IMAGE_SIZE 0x8000
#define SFM_DESCR_LEN 256
#define SFM_MAX_SDLA 16
#define SDLA_S502A 5020
#define SDLA_S502E 5021
#define SDLA_S503 5030
#define SDLA_S508 5080
#define SDLA_S507 5070
#define SDLA_S509 5090
#define SDLA_S514 5140
#define S514_CPU_A 'A'
#define S514_CPU_B 'B'
#define SFID_CALIB502 200
#define SFID_STRM502 1200
#define SFID_STRM508 1800
#define SFID_BSC502 2200
#define SFID_SDLC502 3200
#define SFID_HDLC502 4200
#define SFID_HDLC508 4800
#define SFID_X25_502 5200
#define SFID_X25_508 5800
#define SFID_FR502 6200
#define SFID_FR508 6800
#define SFID_PPP502 7200
#define SFID_PPP508 7800
#define SFID_PPP514 7140
#define SFID_CHDLC508 8800
#define SFID_CHDLC514 8140
typedef struct sfm_info
{
unsigned short codeid;
unsigned short version;
unsigned short adapter[SFM_MAX_SDLA];
unsigned long memsize;
unsigned short reserved[2];
unsigned short startoffs;
unsigned short winoffs;
unsigned short codeoffs;
unsigned short codesize;
unsigned short dataoffs;
unsigned short datasize;
} sfm_info_t;
typedef struct sfm
{
char signature[80];
unsigned short version;
unsigned short checksum;
unsigned short reserved[6];
char descr[SFM_DESCR_LEN];
sfm_info_t info;
unsigned char image[1];
} sfm_t;
#endif