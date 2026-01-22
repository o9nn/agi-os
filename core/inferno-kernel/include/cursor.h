typedef struct Drawcursor Drawcursor;
struct Drawcursor
{
int	hotx;
int	hoty;
int	minx;
int	miny;
int	maxx;
int	maxy;
uchar*	data;
};
void	drawcursor(Drawcursor*);