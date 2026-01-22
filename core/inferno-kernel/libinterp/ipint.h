typedef struct IPint IPint;
#pragma incomplete IPint
void*	newIPint(mpint*);
mpint*	checkIPint(void*);
void	freeIPint(Heap*, int);
void	ipintsmodinit(void);
extern	Type*	TIPint;