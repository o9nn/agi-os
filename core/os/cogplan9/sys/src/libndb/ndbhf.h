struct Ndbhf
{
Ndbhf	*next;
int	fd;
ulong	dbmtime;
int	hlen;
char	attr[Ndbalen];
uchar	buf[256];
long	off;
int	len;
};
char*		_ndbparsetuple(char*, Ndbtuple**);
Ndbtuple*	_ndbparseline(char*);
#define ISWHITE(x) ((x) == ' ' || (x) == '\t' || (x) == '\r')
#define EATWHITE(x) while(ISWHITE(*(x)))(x)++
extern Ndbtuple *_ndbtfree;
void	_ndbcacheflush(Ndb *db);
int	_ndbcachesearch(Ndb *db, Ndbs *s, char *attr, char *val, Ndbtuple **t);
Ndbtuple* _ndbcacheadd(Ndb *db, Ndbs *s, char *attr, char *val, Ndbtuple *t);