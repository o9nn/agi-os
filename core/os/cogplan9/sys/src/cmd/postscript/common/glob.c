#include <stdio.h>
#include "gen.h"
char	**argv;
int	argc;
int	x_stat = 0;
int	debug = OFF;
int	ignore = OFF;
long	lineno = 0;
long	position = 0;
char	*prog_name = "";
char	*temp_file = NULL;
char	*fontencoding = NULL;
int	dobbox = FALSE;
double	pageheight = PAGEHEIGHT;
double	pagewidth = PAGEWIDTH;
int	reading = UTFENCODING;
int	writing = WRITING;