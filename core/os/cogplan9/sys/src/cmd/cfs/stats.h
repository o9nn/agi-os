struct Cfsmsg {
ulong	n;
vlong	t;
vlong	s;
};
struct Cfsstat {
struct Cfsmsg cm[128];
struct Cfsmsg sm[128];
ulong ndirread;
ulong ndelegateread;
ulong ninsert;
ulong ndelete;
ulong nupdate;
uvlong bytesread;
uvlong byteswritten;
uvlong bytesfromserver;
uvlong bytesfromdirs;
uvlong bytesfromcache;
uvlong bytestocache;
};
extern struct Cfsstat cfsstat, cfsprev;
extern int statson;