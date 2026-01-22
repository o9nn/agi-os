enum {
MaxScsi		= 4,
NTarget		= 16,
Maxnets		= 8,
};
enum {
STblank		=-6,
STnomem		=-5,
STtimeout	=-4,
STownid		=-3,
STharderr	=-2,
STinit		=-1,
STok		= 0,
STcheck		= 0x02,
STcondmet	= 0x04,
STbusy		= 0x08,
STintok		= 0x10,
STintcondmet	= 0x14,
STresconf	= 0x18,
STterminated	= 0x22,
STqfull		= 0x28,
};
typedef struct Target {
Scsi	*sc;
int	ctlrno;
int	targetno;
uchar*	inquiry;
uchar*	sense;
QLock;
char	id[NAMELEN];
int	ok;
} Target;