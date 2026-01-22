enum {
Empty = 0,
Background,
Wall,
Cargo,
Goal,
GoalCargo,
Glenda,
Up,
Down,
Left,
Right,
};
enum {
GLeft = 0,
GRight = 1,
};
enum {
MazeX = 20,
MazeY = 18,
BoardX = 49,
BoardY = 49,
SizeX = MazeX*BoardX+10,
SizeY = MazeY*BoardY+10,
Maxlevels = 200,
};
typedef struct Step {
uint dir;
uint count;
} Step;
typedef struct Route {
uint nstep;
Step *step;
Point dest;
} Route;
typedef struct Walk {
uint nroute;
Route **route;
uint beyond;
} Walk;
typedef struct Visited {
uint board[MazeX][MazeY];
} Visited;
typedef struct Animation {
Route* route;
Step *step;
int count;
} Animation;
typedef struct {
Point glenda;
Point max;
uint index;
uint done;
uint board[MazeX][MazeY];
} Level;
Level level;
Level levels[Maxlevels];
int numlevels;
Image *img;
Image *text;
Image *win;
Image *goal;
Image *cargo;
Image *goalcargo;
Image *wall;
Image *empty;
Image *gleft;
Image *gright;
Image *glenda;
Image *bg;
void drawscreen(void);
void drawlevel(void);
void drawwin(void);
void drawglenda(void);
void drawboard(Point);
void resize(Point);
Point boardsize(Point);
int loadlevels(char *);
void move(int);
int validpush(Point, Step*, Point*);
int isvalid(Point, Route*, int (*)(Point, Step*, Point*));
void freeroute(Route*);
Route* extend(Route*, int, int, Point);
Route* findroute(Point, Point);
void initanimation(Animation*);
void setupanimation(Animation*, Route*);
int onestep(Animation*);
void stopanimation(Animation*);
char *genlevels(int);
Image *eallocimage(Rectangle, int, uint);