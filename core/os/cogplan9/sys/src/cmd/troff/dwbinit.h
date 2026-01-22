typedef struct {
char	**address;
char	*value;
int	length;
} dwbinit;
extern void	DWBinit(char *, dwbinit *);
extern char*	DWBhome(void);
extern void	DWBprefix(char *, char *, int);