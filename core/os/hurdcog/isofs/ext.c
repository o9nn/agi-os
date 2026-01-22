#include "isofs.h"
struct susp_field susp_extension[] =
{
{ 'C', 'E', 1, process_su_ce },
{ 'P', 'D', 1, process_su_pd },
{ 'S', 'P', 1, process_su_sp },
{ 'E', 'R', 1, process_su_er },
{ 'S', 'T', 1, process_su_st },
{ 0, 0, 0, 0 },
};
struct susp_field rr_extension[] =
{
{ 'P', 'X', 1, process_rr_px },
{ 'P', 'N', 1, process_rr_pn },
{ 'S', 'L', 1, process_rr_sl },
{ 'N', 'M', 1, process_rr_nm },
{ 'C', 'L', 1, process_rr_cl },
{ 'P', 'L', 1, process_rr_pl },
{ 'R', 'E', 1, process_rr_re },
{ 'T', 'F', 1, process_rr_tf },
{ 'S', 'F', 1, process_rr_sf },
{ 0, 0, 0, 0 },
};
struct susp_ext extensions[] =
{
{ "RRIP_1991A", 1,
"THE ROCK RIDGE INTERCHANGE PROTOCOL PROVIDES SUPPORT FOR POSIX FILE SYSTEM SEMANTICS",
"ROCK RIDGE SPECIFICATION VERSION 1 REVISION 1.10 JULY 13 1993",
rr_extensions
},
{ 0, 0, 0, 0, susp_extensions },
{ 0, 0, 0, 0, 0 },
}