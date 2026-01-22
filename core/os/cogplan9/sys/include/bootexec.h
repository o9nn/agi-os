struct coffsect
{
char	name[8];
ulong	phys;
ulong	virt;
ulong	size;
ulong	fptr;
ulong	fptrreloc;
ulong	fptrlineno;
ulong	nrelocnlineno;
ulong	flags;
};
struct mipsexec
{
short	mmagic;
short	nscns;
long	timdat;
long	symptr;
long	nsyms;
short	opthdr;
short	pcszs;
short	amagic;
short	vstamp;
long	tsize;
long	dsize;
long	bsize;
long	mentry;
long	text_start;
long	data_start;
long	bss_start;
long	gprmask;
union{
long	cprmask[4];
long	pcsize;
};
long	gp_value;
};
struct mips4kexec
{
struct mipsexec	h;
struct coffsect	itexts;
struct coffsect idatas;
struct coffsect ibsss;
};
struct sparcexec
{
short	sjunk;
short	smagic;
ulong	stext;
ulong	sdata;
ulong	sbss;
ulong	ssyms;
ulong	sentry;
ulong	strsize;
ulong	sdrsize;
};
struct nextexec
{
struct	nexthdr{
ulong	nmagic;
ulong	ncputype;
ulong	ncpusubtype;
ulong	nfiletype;
ulong	ncmds;
ulong	nsizeofcmds;
ulong	nflags;
};
struct nextcmd{
ulong	cmd;
ulong	cmdsize;
uchar	segname[16];
ulong	vmaddr;
ulong	vmsize;
ulong	fileoff;
ulong	filesize;
ulong	maxprot;
ulong	initprot;
ulong	nsects;
ulong	flags;
}textc;
struct nextsect{
char	sectname[16];
char	segname[16];
ulong	addr;
ulong	size;
ulong	offset;
ulong	align;
ulong	reloff;
ulong	nreloc;
ulong	flags;
ulong	reserved1;
ulong	reserved2;
}texts;
struct nextcmd	datac;
struct nextsect	datas;
struct nextsect	bsss;
struct nextsym{
ulong	cmd;
ulong	cmdsize;
ulong	symoff;
ulong	nsyms;
ulong	spoff;
ulong	pcoff;
}symc;
};
struct i386exec
{
struct	i386coff{
ulong	isectmagic;
ulong	itime;
ulong	isyms;
ulong	insyms;
ulong	iflags;
};
struct	i386hdr{
ulong	imagic;
ulong	itextsize;
ulong	idatasize;
ulong	ibsssize;
ulong	ientry;
ulong	itextstart;
ulong	idatastart;
};
struct coffsect	itexts;
struct coffsect idatas;
struct coffsect ibsss;
struct coffsect icomments;
};