#include "iso9660.h"
struct rrip_lookup
{
mode_t mode;
nlink_t nlink;
uid_t uid;
gid_t gid;
dev_t rdev;
char *target;
char *name;
off_t newloc;
off_t parloc;
int tfflags;
struct timespec atime, mtime, ctime;
struct dirrect *realdirent;
off_t realfilestart;
uid_t author;
size_t translen;
char *trans;
mode_t allmode;
long flags;
int valid;
};
#define VALID_PX 0x0001
#define VALID_PN 0x0002
#define VALID_SL 0x0004
#define VALID_NM 0x0008
#define VALID_CL 0x0010
#define VALID_PL 0x0020
#define VALID_TF 0x0040
#define VALID_RE 0x0080
#define VALID_AU 0x0100
#define VALID_TR 0x0200
#define VALID_MD 0x0400
#define VALID_FL 0x0800
struct su_header
{
char sig[2];
unsigned char len;
char version;
};
struct su_ce
{
unsigned char continuation[8];
unsigned char offset[8];
unsigned char size[8];
};
struct su_sp
{
unsigned char check[2];
u_char skip;
};
#define SU_SP_CHECK_0 0xbe
#define SU_SP_CHECK_1 0xef
struct su_er
{
u_char len_id;
u_char len_des;
u_char len_src;
u_char ext_ver;
char more[0];
};
#define ROCK_VERS 1
#define ROCK_ID "RRIP_1991A"
#define ROCK_DES \
"THE ROCK RIDGE INTERCHANGE PROTOCOL PROVIDES SUPPORT FOR POSIX FILE SYSTEM SEMANTICS"
#define ROCK_SRC \
"ROCK RIDGE SPECIFICATION VERSION 1 REVISION 1.10 JULY 13 1993"
struct rr_px
{
unsigned char mode[8];
unsigned char nlink[8];
unsigned char uid[8];
unsigned char gid[8];
};
struct rr_pn
{
unsigned char high[8];
unsigned char low[8];
};
struct rr_sl
{
u_char flags;
char data[0];
};
struct rr_sl_comp
{
u_char flags;
u_char len;
char name[0];
};
struct rr_nm
{
u_char flags;
char name[0];
};
#define NAME_CONTINUE 0x01
#define NAME_DOT 0x02
#define NAME_DOTDOT 0x04
#define NAME_ROOT 0x08
#define NAME_VOLROOT 0x10
#define NAME_HOST 0x20
struct rr_cl
{
unsigned char loc[8];
};
struct rr_pl
{
unsigned char loc[8];
};
struct rr_tf
{
u_char flags;
char data[0];
};
#define TF_CREATION 0x01
#define TF_MODIFY 0x02
#define TF_ACCESS 0x04
#define TF_ATTRIBUTES 0x08
#define TF_BACKUP 0x10
#define TF_EXPIRATION 0x20
#define TF_EFFECTIVE 0x40
#define TF_LONG_FORM 0x80
struct rr_sf
{
char size[8];
};
#define GNUEXT_VERS 1
#define GNUEXT_ID "GNUEXT_1997"
#define GNUEXT_DES \
"The GNU Extensions provide support for special GNU filesystem features"
#define GNUEXT_SRC \
"GNU Hurd source release 0.3 or later"
struct gn_au
{
unsigned char author[8];
};
struct gn_tr
{
u_char len;
char data[0];
};
struct gn_md
{
unsigned char mode[8];
};
struct gn_fl
{
unsigned char flags[8];
};
int rrip_match_lookup (struct dirrect *, const char *,
size_t, struct rrip_lookup *);
void rrip_lookup (struct dirrect *, struct rrip_lookup *, int);
void rrip_initialize (struct dirrect *);
void release_rrip (struct rrip_lookup *);