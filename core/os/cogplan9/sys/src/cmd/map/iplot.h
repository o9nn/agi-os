#define openpl() print("o\n")
#define closepl() print("cl\n")
#define erase() print("e\n")
#define point(_x,_y) print("poi %d %d\n", _x,_y)
#define range(_x,_y,_X,_Y) print("ra %d %d %d %d\n", _x,_y,_X,_Y)
#define text(_s) {if(*(_s) == ' ')print("t \"%s\"\n",_s); else print("t %s\n", _s); }
#define vec(_x,_y) print("v %d %d\n", _x,_y)
#define move(_x, _y) print("m %d %d\n", _x, _y)
#define SOLID "solid"
#define DOTTED "dotted"
#define DASHED "dashed"
#define DOTDASH "dotdash"
#define pen(_s) print("pe %s\n", _s)
#define BLACK "z"
#define RED "r"
#define YELLOW "y"
#define GREEN "g"
#define BLUE "b"
#define CYAN "c"
#define MAGENTA "m"
#define WHITE "w"
#define colorcode(_s) ((strcmp(_s,"black")==0)?BLACK:_s)
#define colorx(_s) print("co %s\n", _s);
#define comment(s,f)