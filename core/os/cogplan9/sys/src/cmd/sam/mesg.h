#define VERSION 2
#define TBLOCKSIZE 512
#define DATASIZE (UTFmax*TBLOCKSIZE+30)
#define SNARFSIZE 32000
typedef enum Tmesg
{
Tversion,
Tstartcmdfile,
Tcheck,
Trequest,
Torigin,
Tstartfile,
Tworkfile,
Ttype,
Tcut,
Tpaste,
Tsnarf,
Tstartnewfile,
Twrite,
Tclose,
Tlook,
Tsearch,
Tsend,
Tdclick,
Tstartsnarf,
Tsetsnarf,
Tack,
Texit,
Tplumb,
TMAX,
}Tmesg;
typedef enum Hmesg
{
Hversion,
Hbindname,
Hcurrent,
Hnewname,
Hmovname,
Hgrow,
Hcheck0,
Hcheck,
Hunlock,
Hdata,
Horigin,
Hunlockfile,
Hsetdot,
Hgrowdata,
Hmoveto,
Hclean,
Hdirty,
Hcut,
Hsetpat,
Hdelname,
Hclose,
Hsetsnarf,
Hsnarflen,
Hack,
Hexit,
Hplumb,
HMAX,
}Hmesg;
typedef struct Header{
uchar type;
uchar count0;
uchar count1;
uchar data[1];
}Header;