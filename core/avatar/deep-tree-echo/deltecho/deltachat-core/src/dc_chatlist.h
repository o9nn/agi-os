#ifndef __DC_CHATLIST_H__
#define __DC_CHATLIST_H__
#ifdef __cplusplus
extern "C" {
#endif
struct _dc_chatlist
{
uint32_t        magic;
dc_context_t*   context;
#define         DC_CHATLIST_IDS_PER_RESULT 2
size_t          cnt;
dc_array_t*     chatNlastmsg_ids;
};
int             dc_get_archived_cnt        (dc_context_t*);
#ifdef __cplusplus
}
#endif
#endif