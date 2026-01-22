#ifndef LAME_VERSION_H
#define LAME_VERSION_H
# include <stdio.h>
# define LAME_URL              "http:
# define LAME_MAJOR_VERSION      3
# define LAME_MINOR_VERSION     88
# define LAME_ALPHA_VERSION      0
# define LAME_BETA_VERSION       1
# define PSY_MAJOR_VERSION       0
# define PSY_MINOR_VERSION      85
# define PSY_ALPHA_VERSION       0
# define PSY_BETA_VERSION        0
# define MP3X_MAJOR_VERSION      0
# define MP3X_MINOR_VERSION     82
# define MP3X_ALPHA_VERSION      0
# define MP3X_BETA_VERSION       0
const char*  get_lame_version       ( void );
const char*  get_lame_short_version ( void );
const char*  get_psy_version        ( void );
const char*  get_mp3x_version       ( void );
const char*  get_lame_url           ( void );
void         get_lame_version_numerical ( lame_version_t *const lvp );
#endif