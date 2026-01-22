#define POSTBEGIN "statusdict /waittimeout 0 put\n"
#define NOTCONNECTED 0
#define START 1
#define SEND 2
#define DONE 3
#define READ 1
#define WRITE 2
#define READWRITE 3
#define BUSY 0
#define WAITING 1
#define PRINTING 2
#define IDLE 3
#define ENDOFJOB 4
#define PRINTERERROR 5
#define ERROR 6
#define FLUSHING 7
#define INITIALIZING 8
#define DISCONNECT 9
#define UNKNOWN 10
#define NOSTATUS 11
#define WRITEPROCESS 12
#define INTERACTIVE 13
typedef struct {
char *state;
int val;
} Status;
#define STATUS \
\
{ \
"busy", BUSY, \
"waiting", WAITING, \
"printing", PRINTING, \
"idle", IDLE, \
"endofjob", ENDOFJOB, \
"printererror", PRINTERERROR, \
"error", ERROR, \
"flushing", FLUSHING, \
"initializing", INITIALIZING, \
NULL, UNKNOWN \
}
#define BAUDRATE B9600
typedef struct {
char *rate;
short val;
} Baud;
#define BAUDTABLE \
\
{ \
"9600", B9600, \
"B9600", B9600, \
"19200", EXTA, \
"19.2", EXTA, \
"B19200", EXTA, \
"EXTA", EXTA, \
"1200", B1200, \
"B1200", B1200, \
"2400", B2400, \
"B2400", B2400, \
"B4800", B4800, \
"4800", B4800, \
"38400", EXTB, \
"38.4", EXTB, \
"B38400", EXTB, \
"EXTB", EXTB, \
NULL, B9600 \
}
#define BLOCKSIZE 2048
#define MESGSIZE 512
char *find();
char *malloc();
char *strtok();