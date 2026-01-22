typedef struct Flash Flash;
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
Maxflashregion = 8
};
struct Flashregion {
int	n;
ulong	start;
ulong	end;
ulong	erasesize;
};
struct Flash {
QLock;
Flash*	next;
char*	name;
void*	addr;
ulong	size;
int	(*reset)(Flash*);
int	(*eraseall)(Flash*);
int	(*erasezone)(Flash*, int);
int	(*write)(Flash*, ulong, void*, long);
int	(*suspend)(Flash*);
int	(*resume)(Flash*);
int	nr;
Flashregion	regions[Maxflashregion];
uchar	id;
uchar	devid;
int	width;
int	erasesize;
void*	data;
ulong	unusable;
Flashpart	part[Maxflashpart];
int	protect;
};
void	addflashcard(char*, int (*)(Flash*));
int	archflashreset(char*, void**, long*);
void	archflashwp(int);