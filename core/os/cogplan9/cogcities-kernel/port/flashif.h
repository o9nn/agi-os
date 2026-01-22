typedef struct Flash Flash;
typedef struct Flashchip Flashchip;
typedef struct Flashpart Flashpart;
typedef struct Flashregion Flashregion;
enum {
Maxflashpart = 8
};
struct Flashpart {
char*	name;
ulong	start;
ulong	end;
};
enum {
Maxflashregion = 4
};
struct Flashregion {
int	n;
ulong	start;
ulong	end;
ulong	erasesize;
ulong	eraseshift;
ulong	pagesize;
ulong	pageshift;
ulong	spares;
};
struct Flashchip {
int	nr;
Flashregion regions[Maxflashregion];
uchar	id;
ushort	devid;
int	width;
int	maxwb;
ulong	devsize;
int	alg;
int	protect;
};
struct Flash {
QLock;
Flash*	next;
char*	type;
void*	addr;
ulong	size;
int	xip;
int	(*reset)(Flash*);
int	(*eraseall)(Flash*);
int	(*erasezone)(Flash*, Flashregion*, ulong);
int	(*read)(Flash*, ulong, void*, long);
int	(*write)(Flash*, ulong, void*, long);
int	(*suspend)(Flash*);
int	(*resume)(Flash*);
int	(*attach)(Flash*);
int	nr;
Flashregion regions[Maxflashregion];
uchar	id;
ushort	devid;
int	width;
int	interleave;
int	bshift;
ulong	cmask;
int	maxwb;
ulong	devsize;
int	alg;
void*	data;
Flashpart part[Maxflashpart];
int	protect;
char*	sort;
};
void	addflashcard(char*, int (*)(Flash*));
int	archflashreset(int, Flash*);
void	archflashwp(Flash*, int);
int	flashget(Flash*, ulong);
void	flashput(Flash*, ulong, int);
void archnand_init(Flash*);
void archnand_claim(Flash*, int claim);
void archnand_setCLEandALE(Flash*, int cle, int ale);
void archnand_write(Flash*, void *buf, int len);
void archnand_read(Flash*, void *buf, int len);