typedef enum Kind
{
aNone,
aName,
aText,
aData,
} Kind;
typedef struct	Prog	Prog;
struct Prog
{
Kind	kind;
char	type;
char	sym;
char	*id;
uint	sig;
};
#define UNKNOWN	'?'
void		_offset(int, vlong);