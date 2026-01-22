#ifndef DLISTS_H
#define DLISTS_H
#define DEF_GENERIC_INSERT(CHANGE,PREFIX,NAME,TYPE,NEXT,PREV) \
static inline void PREFIX##NAME(TYPE ** anchor, TYPE * elem)\
{\
TYPE * oldfirst = *anchor;\
if(!oldfirst) {\
elem->NEXT = elem->PREV = *anchor = elem;\
} else {\
elem->PREV = oldfirst->PREV;\
elem->NEXT = oldfirst;\
oldfirst->PREV->NEXT = elem;\
oldfirst->PREV = elem;\
if(CHANGE)\
*anchor = elem;\
}\
}
#define DEF_INSERT(NAME,TYPE,NEXT,PREV) \
DEF_GENERIC_INSERT(1,insert_,NAME,TYPE,NEXT,PREV)
#define DEF_APPEND(NAME,TYPE,NEXT,PREV) \
DEF_GENERIC_INSERT(0,append_,NAME,TYPE,NEXT,PREV)
#define DEF_INSERT_MIDDLE(NAME,TYPE) \
static inline void insert_middle_##NAME(TYPE ** anchor, TYPE * oldelem, TYPE * elem)\
{\
int status = (oldelem == *anchor);\
insert_##NAME(&oldelem, elem);\
if(status)\
*anchor = oldelem;\
}
#define DEF_REMOVE(NAME,TYPE,NEXT,PREV) \
static inline void remove_##NAME(TYPE ** anchor, TYPE * elem)\
{\
TYPE * next = elem->NEXT;\
if(next == elem) {\
*anchor = NULL;\
} else {\
TYPE * prev = elem->PREV;\
prev->NEXT = next;\
next->PREV = prev;\
elem->NEXT = elem->PREV = NULL;\
if(*anchor == elem)\
*anchor = next;\
}\
}
#define DEF_LIN_INSERT(NAME,TYPE,NEXT,PPREV) \
static inline void insert_##NAME(TYPE ** anchor, TYPE * elem)\
{\
TYPE * first;\
if((elem->NEXT = first = *anchor))\
first->PPREV = &elem->NEXT;\
*anchor = elem;\
elem->PPREV = anchor;\
}
#define DEF_LIN_REMOVE(NAME,TYPE,NEXT,PPREV) \
static inline void remove_##NAME(TYPE ** anchor, TYPE * elem)\
{\
TYPE * pprev;\
if((pprev = elem->PPREV)) {\
TYPE * next;\
if((next = elem->NEXT))\
next->PPREV = pprev;\
*pprev = next;\
elem->PPREV = elem->NEXT = NULL; \
}\
}
#endif