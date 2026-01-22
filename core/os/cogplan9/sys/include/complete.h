#pragma	lib	"libcomplete.a"
#pragma src "/sys/src/libcomplete"
typedef struct Completion Completion;
struct Completion{
uchar advance;
uchar complete;
char *string;
int nmatch;
int nfile;
char **filename;
};
Completion* complete(char *dir, char *s);
void freecompletion(Completion*);