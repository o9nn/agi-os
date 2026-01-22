#ifndef __DC_PARAM_H__
#define __DC_PARAM_H__
#ifdef __cplusplus
extern "C" {
#endif
typedef struct _dc_param dc_param_t;
struct _dc_param
{
char*           packed;
};
#define DC_PARAM_FILE              'f'
#define DC_PARAM_WIDTH             'w'
#define DC_PARAM_HEIGHT            'h'
#define DC_PARAM_DURATION          'd'
#define DC_PARAM_MIMETYPE          'm'
#define DC_PARAM_GUARANTEE_E2EE    'c'
#define DC_PARAM_ERRONEOUS_E2EE    'e'
#define DC_PARAM_FORCE_PLAINTEXT   'u'
#define DC_PARAM_WANTS_MDN         'r'
#define DC_PARAM_FORWARDED         'a'
#define DC_PARAM_CMD               'S'
#define DC_PARAM_CMD_ARG           'E'
#define DC_PARAM_CMD_ARG2          'F'
#define DC_PARAM_CMD_ARG3          'G'
#define DC_PARAM_CMD_ARG4          'H'
#define DC_PARAM_ERROR             'L'
#define DC_PARAM_PREP_FORWARDS     'P'
#define DC_PARAM_SET_LATITUDE      'l'
#define DC_PARAM_SET_LONGITUDE     'n'
#define DC_PARAM_SERVER_FOLDER     'Z'
#define DC_PARAM_SERVER_UID        'z'
#define DC_PARAM_ALSO_MOVE         'M'
#define DC_PARAM_RECIPIENTS        'R'
#define DC_PARAM_UNPROMOTED        'U'
#define DC_PARAM_PROFILE_IMAGE     'i'
#define DC_PARAM_SELFTALK          'K'
#define DC_FP_ADD_AUTOCRYPT_HEADER 1
#define DC_FP_NO_AUTOCRYPT_HEADER  2
int             dc_param_exists         (dc_param_t*, int key);
char*           dc_param_get            (const dc_param_t*, int key, const char* def);
int32_t         dc_param_get_int        (const dc_param_t*, int key, int32_t def);
double          dc_param_get_float      (const dc_param_t*, int key, double def);
void            dc_param_set            (dc_param_t*, int key, const char* value);
void            dc_param_set_int        (dc_param_t*, int key, int32_t value);
void            dc_param_set_float      (dc_param_t*, int key, double value);
dc_param_t*     dc_param_new            ();
void            dc_param_empty          (dc_param_t*);
void            dc_param_unref          (dc_param_t*);
void            dc_param_set_packed     (dc_param_t*, const char*);
void            dc_param_set_urlencoded (dc_param_t*, const char*);
#ifdef __cplusplus
}
#endif
#endif