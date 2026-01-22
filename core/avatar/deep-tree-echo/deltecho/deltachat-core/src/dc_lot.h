#ifndef __DC_LOT_H__
#define __DC_LOT_H__
#ifdef __cplusplus
extern "C" {
#endif
struct _dc_lot
{
uint32_t        magic;
int             text1_meaning;
char*           text1;
char*           text2;
time_t          timestamp;
int             state;
uint32_t        id;
char*           fingerprint;
char*           invitenumber;
char*           auth;
};
#define DC_SUMMARY_CHARACTERS 160
void            dc_lot_fill      (dc_lot_t*, const dc_msg_t*, const dc_chat_t*, const dc_contact_t*, dc_context_t*);
#ifdef __cplusplus
}
#endif
#endif