#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#ifdef WIN32
#include <winsock.h>
#include <SQL.h>
#include <SQLEXT.h>
#else
#include <unistd.h>
#include <sql.h>
#include <sqlext.h>
#define max(a, b) ((a) > (b) ? (a) : (b))
#define strnicmp strncasecmp
#endif
#define MAXCOLS 100
#define BUFSIZE 8192
#define REQ_HEADER_SIZE 18
#define RES_HEADER_SIZE 22
#define OFFSET_LENGTH 2
#define OFFSET_STREAM 14
#define OFFSET_REQ_DATA 18
#define OFFSET_RETURN 18
#define OFFSET_RES_DATA 22
#define CONN_ALLOC_FAIL 1
#define STREAM_ALLOC_FAIL 2
#define STREAM_BAD_ID 3
#define LAST_ERROR_NO 4
typedef int STATUS;
#define OK 0
#define WARN -1
#define ERR -2
typedef struct {
int state;
#define SQLC_FREE 0
#define SQLC_INUSE 1
int connid;
int refcount;
UCHAR user[48];
UCHAR passwd[48];
UCHAR dbname[48];
UCHAR errmsg[256];
HDBC hdbc;
} SQLConn;
typedef struct {
int state;
#define SQLS_FREE 0
#define SQLS_INUSE 1
int streamid;
int connid;
HSTMT hstmt;
UCHAR errmsg[256];
UCHAR colname[MAXCOLS][32];
SWORD coltype[MAXCOLS];
SWORD colnamelen;
SWORD nullable;
UDWORD collen[MAXCOLS];
SWORD scale;
SDWORD outlen[MAXCOLS];
UCHAR *data[MAXCOLS];
SWORD nresultcols;
SDWORD rowcount;
SWORD rownum;
RETCODE rc;
UCHAR *setdata[MAXCOLS];
SDWORD setdatalen[MAXCOLS];
} SQLStream;
typedef struct {
HENV henv;
int maxconn;
int numconn;
SQLConn **scarray;
int maxstream;
int numstream;
SQLStream **ssarray;
} SQLEnv;
typedef struct {
char mtype;
char version;
int nbytes;
int sstream;
int retcode;
int bytesNotRead;
char *data;
} DBMSG, *DBMSGP;
int getCommand (DBMSGP msgp, UCHAR *buf, int bufsiz);
void sendResponse (char type, int lendata, int sstream, int retcode, char *data);
void sendError (char *errmsg, int sstream);
void print_err (SQLEnv *sqle, int connid, int streamid, UCHAR * buf, int bufsiz);
UDWORD display_size (SWORD coltype, UDWORD collen, UCHAR *colname);
STATUS newSqlEnv (SQLEnv **sqle);
STATUS freeSqlEnv (SQLEnv **sqle);
STATUS newSqlConn (SQLEnv *sqle, char *info, int *connid);
STATUS mapSqlConn (SQLEnv *sqle, int connid, SQLConn **sqlc);
STATUS freeSqlConn (SQLEnv *sqle, int connid);
STATUS newSqlStream (SQLEnv *sqle, int connid, int *streamid);
STATUS mapSqlStream (SQLEnv *sqle, int streamid, SQLStream **sqls);
STATUS freeSqlStream (SQLEnv *sqle, int streamid);
STATUS parseConnInfo (SQLConn *sqlc, char *info);
char *iError[] = {
"INFDB: DB connection allocation failed",
"INFDB: couldn't allocate SQL stream",
"INFDB: bad SQL stream identifier"
};
int
main(int argc, char *argv[])
{
int i;
int notdone = 1;
int infErrno;
DBMSG msg;
SQLEnv *sqle = NULL;
SQLStream *sqls;
char buf[BUFSIZE];
char outbuf[BUFSIZE];
char errmsg[256];
STATUS rc;
#ifdef WIN32
_setmode(0, _O_BINARY);
_setmode(1, _O_BINARY);
#endif
rc = newSqlEnv(&sqle);
if ( rc != OK ) {
sendError("INFDB: Failed to allocate SQL environment.", -1);
return -1;
}
while ( notdone ) {
int bytesRead;
bytesRead = 0;
if ( (bytesRead = getCommand(&msg, buf, sizeof(buf))) <= 0 ) {
continue;
}
msg.retcode = 0;
infErrno = 0;
switch ( msg.mtype ) {
case 'I':
{
int connid;
rc = newSqlConn(sqle, msg.data, &connid);
if ( rc != OK ) {
infErrno = CONN_ALLOC_FAIL;
break;
}
sprintf(outbuf, "%d", connid);
sendResponse('i', strlen(outbuf), msg.sstream, 0, outbuf);
break;
}
case 'O':
{
int connid, streamid;
connid = atoi(msg.data);
rc = newSqlStream(sqle, connid, &streamid);
if (rc != OK) {
infErrno = STREAM_ALLOC_FAIL;
break;
}
sprintf(outbuf, "%d", streamid);
sendResponse('o', strlen(outbuf), msg.sstream, 0, outbuf);
break;
}
case 'K':
rc = freeSqlStream(sqle, msg.sstream);
sendResponse('k', 0, msg.sstream, 0, "");
break;
case 'C':
rc = mapSqlStream(sqle, msg.sstream, &sqls);
if ( rc != OK ) {
infErrno = STREAM_BAD_ID;
break;
}
sprintf(outbuf, "%d", sqls->nresultcols);
sendResponse('c', strlen(outbuf), msg.sstream, 0, outbuf);
break;
case 'N':
rc = mapSqlStream(sqle, msg.sstream, &sqls);
if ( rc != OK ) {
infErrno = STREAM_BAD_ID;
break;
}
sqls->errmsg[0] = '\0';
rc = SQLFetch(sqls->hstmt);
if ( rc == SQL_SUCCESS || rc == SQL_SUCCESS_WITH_INFO ) {
sqls->rownum++;
for ( i = 0; i < sqls->nresultcols; i++ ) {
rc = SQLGetData(sqls->hstmt, (UWORD)(i+1),
(sqls->coltype[i] == SQL_LONGVARBINARY ||
sqls->coltype[i] == SQL_LONGVARCHAR) ? SQL_C_DEFAULT :
SQL_C_CHAR,
sqls->data[i], sqls->collen[i], &sqls->outlen[i]);
if ( rc == SQL_SUCCESS_WITH_INFO &&
(UDWORD) sqls->outlen[i] > sqls->collen[i] ) {
UCHAR *tmp;
tmp = (UCHAR *) realloc(sqls->data[i], sqls->outlen[i]+1);
if ( tmp != NULL ) {
SDWORD dummy;
sqls->data[i] = tmp;
rc = SQLGetData(sqls->hstmt, (UWORD)(i+1), SQL_C_DEFAULT,
&tmp[sqls->collen[i]], sqls->outlen[i], &dummy);
sqls->collen[i] = sqls->outlen[i];
sqls->data[i][sqls->outlen[i]] = 0;
}
}
else if ( rc != SQL_SUCCESS ) {
sprintf(sqls->errmsg, "Problem retrieving data from data base, col %d", i+1);
msg.retcode = 2;
}
}
}
else if ( rc == SQL_NO_DATA_FOUND ) {
sqls->rownum = 0;
msg.retcode = 1;
}
else {
sqls->rownum = -1;
sprintf(sqls->errmsg, "Error occurred in fetching data");
}
if ( sqls->rownum < 0 ) {
sendError(errmsg, msg.sstream);
}
else {
sprintf(outbuf, "%d", sqls->rownum);
sendResponse('n', strlen(outbuf), msg.sstream, msg.retcode, outbuf);
}
break;
case 'H':
rc = mapSqlStream(sqle, msg.sstream, &sqls);
if ( rc != OK ) {
infErrno = STREAM_BAD_ID;
break;
}
sendError(sqls->errmsg, msg.sstream);
sqls->errmsg[0] = 0;
break;
case 'P':
rc = mapSqlStream(sqle, msg.sstream, &sqls);
if ( rc != OK ) {
infErrno = STREAM_BAD_ID;
break;
}
sqls->errmsg[0] = 0;
if ( (i = atoi(msg.data) + 1) < 1 || i >= MAXCOLS ) {
sendError("Illegal param number", msg.sstream);
}
else {
int len;
char *p = msg.data + 4;
len = msg.nbytes - 4;
if ( len < 0 ) {
sendError("Write phase error II", msg.sstream);
break;
}
if ( sqls->setdata[i] != NULL ) {
free(sqls->setdata[i]);
}
sqls->setdata[i] = (char *) malloc(len + 1);
if ( sqls->setdata[i] == NULL ) {
sendError("Allocation error in server", msg.sstream);
break;
}
sqls->setdatalen[i] = len++;
bytesRead = &buf[bytesRead] - p;
memcpy(sqls->setdata[i], p, bytesRead);
len -= bytesRead;
while ( len > 0 ) {
int n;
if ( (n = read(0, sqls->setdata[i] + bytesRead, len)) <= 0 ) {
break;
}
bytesRead += n;
len -= n;
}
if ( len > 0 ) {
sendError("Couldn't read all of parameter", msg.sstream);
break;
}
rc = SQLBindParameter(sqls->hstmt, (UWORD)i, SQL_PARAM_INPUT,
SQL_C_BINARY, SQL_LONGVARBINARY,
sqls->setdatalen[i], 0, (PTR) i, 0, sqls->setdatalen);
if ( rc != SQL_SUCCESS ) {
sendError("BindParameter failed: maybe not supported", msg.sstream);
break;
}
sqls->setdatalen[0] = SQL_LEN_DATA_AT_EXEC(0);
sprintf(outbuf, "%d", bytesRead - 1);
sendResponse('p', strlen(outbuf), msg.sstream, 0, outbuf);
}
break;
case 'R':
rc = mapSqlStream(sqle, msg.sstream, &sqls);
if ( rc != OK ) {
infErrno = STREAM_BAD_ID;
break;
}
if ( (i = atoi(msg.data)) < 0 || i >= sqls->nresultcols || sqls->rownum <= 0 ) {
sendError(sqls->rownum <= 0 ? "No current row" : "Illegal column number", msg.sstream);
}
else if ( sqls->outlen[i] == SQL_NULL_DATA || sqls->outlen[i] == SQL_NO_TOTAL ) {
sendResponse('r', 0, msg.sstream, 0, "");
}
else {
if ( sqls->coltype[i] == SQL_VARCHAR ) {
sqls->outlen[i] = strlen(sqls->data[i]);
}
sendResponse('r', sqls->outlen[i], msg.sstream, 0, sqls->data[i]);
}
break;
case 'T':
rc = mapSqlStream(sqle, msg.sstream, &sqls);
if ( rc != OK ) {
infErrno = STREAM_BAD_ID;
break;
}
if ( (i = atoi(msg.data)) < 0 || i >= sqls->nresultcols ) {
sendError("Illegal column number", msg.sstream);
}
else {
sendResponse('t', strlen(sqls->colname[i]), msg.sstream, 0, sqls->colname[i]);
}
break;
case 'W':
rc = mapSqlStream(sqle, msg.sstream, &sqls);
if ( rc != OK ) {
infErrno = STREAM_BAD_ID;
break;
}
if ( sqls->hstmt ) {
SQLFreeStmt(sqls->hstmt, SQL_CLOSE);
SQLFreeStmt(sqls->hstmt, SQL_UNBIND);
}
if ( strnicmp(msg.data, "commit", 6) == 0 ) {
SQLConn *sqlc;
rc = mapSqlConn(sqle, sqls->connid, &sqlc);
rc = SQLTransact(SQL_NULL_HENV, sqlc->hdbc, SQL_COMMIT);
}
else if ( strnicmp(msg.data, "rollback", 8) == 0 ) {
SQLConn *sqlc;
rc = mapSqlConn(sqle, sqls->connid, &sqlc);
rc = SQLTransact(SQL_NULL_HENV, sqlc->hdbc, SQL_ROLLBACK);
}
else if ( strnicmp(msg.data, "tables", 6) == 0 ) {
rc = SQLTables(sqls->hstmt, NULL, 0, NULL, 0, NULL, 0, NULL, 0);
}
else if ( strnicmp(msg.data, "columns", 7) == 0 ) {
UCHAR *tbl;
for ( tbl = msg.data+8; *tbl == ' ' || *tbl == '\t'; tbl++ ) { }
rc = SQLColumns(sqls->hstmt, NULL, 0, NULL, 0, tbl, SQL_NTS, NULL, 0);
}
else {
rc = SQLExecDirect(sqls->hstmt, msg.data, SQL_NTS);
}
outbuf[0] = '\0';
while ( rc == SQL_NEED_DATA ) {
PTR pToken;
rc = SQLParamData(sqls->hstmt, &pToken);
#define pnum (int)pToken
if ( rc == SQL_NEED_DATA ) {
int retcode;
if ( sqls->setdata[pnum] == NULL || sqls->setdatalen[pnum] <= 0 ) {
sprintf(outbuf, "Parameter %d not set\n", pnum);
break;
}
for ( i = 0; i < sqls->setdatalen[pnum]; ) {
int togo;
togo = 1024;
if ( sqls->setdatalen[pnum] - i < 1024 ) {
togo = sqls->setdatalen[pnum] - i;
}
retcode = SQLPutData(sqls->hstmt,
sqls->setdata[pnum] + i, togo);
i += togo;
if ( retcode != SQL_SUCCESS ) {
print_err(sqle, -1, msg.sstream, &outbuf[strlen(outbuf)], sizeof (outbuf) - strlen(outbuf));
break;
}
}
if ( retcode != SQL_SUCCESS ) {
break;
}
}
}
if ( rc != SQL_SUCCESS ) {
strcat(outbuf, "Command execution failed\n");
switch ( rc ) {
case SQL_SUCCESS_WITH_INFO:
strcat(outbuf, ": SQL_SUCCESS_WITH_INFO");
print_err(sqle, -1, msg.sstream, &outbuf[strlen(outbuf)], sizeof (outbuf) - strlen(outbuf));
break;
case SQL_ERROR:
strcat(outbuf, ": SQL_ERROR");
print_err(sqle, -1, msg.sstream, &outbuf[strlen(outbuf)], sizeof (outbuf) - strlen(outbuf));
break;
case SQL_NEED_DATA:
strcat(outbuf, ": SQL_NEED_DATA");
break;
case SQL_STILL_EXECUTING:
strcat(outbuf, ": SQL_STILL_EXECUTING");
break;
case SQL_INVALID_HANDLE:
strcat(outbuf, ": SQL_INVALID_HANDLE");
break;
}
sendError(outbuf, msg.sstream);
break;
}
SQLNumResultCols(sqls->hstmt, &sqls->nresultcols);
if ( sqls->nresultcols == 0 ) {
SQLRowCount(sqls->hstmt, &sqls->rowcount);
}
else {
for ( i = 0; i < sqls->nresultcols; i++ ) {
int newlen;
SQLDescribeCol(sqls->hstmt, (UWORD) (i+1), sqls->colname[i],
(SWORD)sizeof(sqls->colname[i]),
&sqls->colnamelen, &sqls->coltype[i], &sqls->collen[i],
&sqls->scale, &sqls->nullable);
sqls->colname[i][sqls->colnamelen] = 0;
if ( (newlen = display_size(sqls->coltype[i], sqls->collen[i],
sqls->colname[i])) != 0 ) {
sqls->collen[i] = newlen;
}
if ( sqls->collen[i] == 0 ) {
sqls->collen[i] = BUFSIZE;
}
sqls->data[i] = (UCHAR *) malloc(sqls->collen[i] + 1);
}
sqls->rownum = 0;
}
sendResponse('w', 0, msg.sstream, 0, "");
break;
case 'X':
notdone = 0;
break;
default:
sprintf(sqls->errmsg, "Unknown command: %c", msg.mtype);
sendError(sqls->errmsg, msg.sstream);
sqls->errmsg[0] = '\0';
break;
}
if ( infErrno > 0 && infErrno < LAST_ERROR_NO ) {
sendError(iError[infErrno - 1], msg.sstream);
}
}
rc = freeSqlEnv(&sqle);
return 0;
}
int
getCommand(DBMSGP msgp, UCHAR *buf, int bufsiz)
{
int bytesRead = 0;
int rc = 0;
msgp->mtype = '\0';
while ( bufsiz > 0 && (rc = read(0, &buf[bytesRead], bufsiz)) > 0 ) {
bytesRead += rc;
bufsiz -= rc;
msgp->bytesNotRead -= rc;
if ( msgp->mtype == '\0' && bytesRead >= REQ_HEADER_SIZE ) {
if ( (msgp->version = buf[1]) != '1' ) {
char *wrong_version = "Message has wrong version number";
sendResponse('h', strlen(wrong_version), 0, 0, wrong_version);
return -1;
}
msgp->mtype = buf[0];
msgp->nbytes = atoi(buf+OFFSET_LENGTH);
msgp->sstream = atoi(buf+OFFSET_STREAM);
msgp->data = buf+OFFSET_REQ_DATA;
msgp->bytesNotRead = REQ_HEADER_SIZE + msgp->nbytes + 1 - bytesRead;
if ( bufsiz > msgp->bytesNotRead ) {
bufsiz = msgp->bytesNotRead;
}
}
}
if ( rc < 0 ) {
return rc;
}
if ( msgp->bytesNotRead == 0 ) {
msgp->data[msgp->nbytes] = 0;
}
return bytesRead;
}
void
sendResponse(char type, int lendata, int sstream, int retcode, char *data)
{
char hdr[RES_HEADER_SIZE+2];
sprintf(hdr, "%c1%11d %3d %3d ", type, lendata, sstream, retcode);
write(1, hdr, RES_HEADER_SIZE);
write(1, data, lendata);
write(1, "\n", 1);
}
void
sendError(char *errmsg, int sstream)
{
sendResponse('h', strlen(errmsg), sstream, 0, errmsg);
}
void
print_err(SQLEnv *sqle, int connid, int streamid, UCHAR * buf, int bufsiz)
{
RETCODE rc;
UCHAR stateString[40];
SDWORD native;
SWORD msglen;
SQLConn *sqlc;
SQLStream *sqls;
HENV *henv;
HDBC *hdbc;
HSTMT *hstmt;
henv = sqle->henv;
rc = mapSqlConn(sqle, connid, &sqlc);
hdbc = rc == OK ? sqlc->hdbc : SQL_NULL_HDBC;
rc = mapSqlStream(sqle, streamid, &sqls);
hstmt = rc == OK ? sqls->hstmt : SQL_NULL_HSTMT;
rc = SQLError(henv, hdbc, hstmt, stateString, &native, buf, (SWORD) bufsiz, &msglen);
}
#define MAX_NUM_PRECISION 15
#define MAX_NUM_STRING_SIZE (MAX_NUM_PRECISION + 5)
UDWORD
display_size(SWORD coltype, UDWORD collen, UCHAR *colname)
{
switch (coltype) {
case SQL_CHAR:
case SQL_VARCHAR:
return max(collen, strlen(colname));
case SQL_SMALLINT:
case SQL_TINYINT:
case SQL_BIT:
return max(6, strlen(colname));
case SQL_INTEGER:
return max(11, strlen(colname));
case SQL_BIGINT:
return max(30, strlen(colname));
case SQL_DATE:
case SQL_TIME:
case SQL_TIMESTAMP:
return max(50, strlen(colname));
case SQL_DECIMAL:
case SQL_NUMERIC:
case SQL_REAL:
case SQL_FLOAT:
case SQL_DOUBLE:
return(max(MAX_NUM_STRING_SIZE, strlen(colname)));
case SQL_LONGVARBINARY:
case SQL_LONGVARCHAR:
return BUFSIZE;
default:
return 0;
}
}
STATUS
newSqlEnv(SQLEnv **sqle)
{
SQLEnv *newenv;
STATUS rc;
newenv = (SQLEnv *) calloc(1, sizeof(SQLEnv));
if (newenv == NULL) {
return ERR;
}
rc = SQLAllocEnv(&newenv->henv);
if ( rc != SQL_SUCCESS) {
free (newenv);
return ERR;
}
*sqle = newenv;
return OK;
}
STATUS
freeSqlEnv(SQLEnv **sqle)
{
int i;
STATUS rc;
for (i = 0; i < (*sqle)->maxstream; i++) {
rc = freeSqlStream(*sqle, i);
}
SQLFreeEnv((*sqle)->henv);
return OK;
}
STATUS
mapSqlConn(SQLEnv *sqle, int connid, SQLConn **sqlc)
{
if ( connid >= 0 && connid < sqle->maxconn ) {
*sqlc = sqle->scarray[connid];
if ( (*sqlc)->state == SQLC_INUSE )
return OK;
}
return ERR;
}
STATUS
newSqlConn(SQLEnv *sqle, char *info, int *connid)
{
SQLConn **newarray, *sqlc;
int newid = -1, i;
STATUS rc;
*connid = -1;
for ( i = 0; i < sqle->maxconn; i++ ) {
sqlc = sqle->scarray[i];
if ( sqlc != NULL && sqlc->state == SQLC_FREE ) {
newid = i;
break;
}
}
if ( newid == -1 ) {
newid = sqle->maxconn++;
newarray = (SQLConn **) realloc((char *) sqle->scarray,
sqle->maxconn * sizeof(SQLConn*));
if ( newarray == NULL ) {
return ERR;
}
sqle->scarray = newarray;
sqlc = (SQLConn *) calloc(1, sizeof(SQLConn));
if ( sqlc == NULL ) {
return ERR;
}
sqle->scarray[newid] = sqlc;
}
rc = SQLAllocConnect(sqle->henv, &sqlc->hdbc);
if (rc == SQL_ERROR) {
return ERR;
}
sqlc->refcount = 0;
sqlc->state = SQLC_INUSE;
rc = parseConnInfo(sqlc, info);
if ( rc != OK ) {
return ERR;
}
rc = SQLConnect(sqlc->hdbc, sqlc->dbname, SQL_NTS, sqlc->user, SQL_NTS,
sqlc->passwd, SQL_NTS);
if ( rc != SQL_SUCCESS && rc != SQL_SUCCESS_WITH_INFO ) {
sprintf(sqlc->errmsg, "Connect failed: user = %s, passwd = %s, dbname = %s",
sqlc->user, sqlc->passwd, sqlc->dbname);
SQLDisconnect(sqlc->hdbc);
SQLFreeConnect(sqlc->hdbc);
return ERR;
}
*connid = newid;
rc = SQLSetConnectOption(sqlc->hdbc, SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF);
if ( rc != SQL_SUCCESS ) {
return WARN;
}
return OK;
}
STATUS
freeSqlConn(SQLEnv *sqle, int connid)
{
SQLConn *sqlc;
STATUS rc;
rc = mapSqlConn(sqle, connid, &sqlc);
if ( rc != OK ) {
return WARN;
}
SQLDisconnect(sqlc->hdbc);
SQLFreeConnect(sqlc->hdbc);
sqlc->state = SQLC_FREE;
return OK;
}
STATUS
mapSqlStream(SQLEnv *sqle, int streamid, SQLStream **sqls)
{
if ( streamid >= 0 && streamid < sqle->maxstream ) {
*sqls = sqle->ssarray[streamid];
if ( (*sqls)->state == SQLS_INUSE )
return OK;
}
return ERR;
}
STATUS
newSqlStream(SQLEnv *sqle, int connid, int *streamid)
{
HSTMT hstmt;
SQLConn *sqlc;
SQLStream **newarray, *sqls;
int newid = -1, i;
STATUS rc;
rc = mapSqlConn(sqle, connid, &sqlc);
if (rc != OK) {
return ERR;
}
for ( i = 0; i < sqle->maxstream; i++ ) {
sqls = sqle->ssarray[i];
if ( sqls != NULL && sqls->state == SQLS_FREE ) {
newid = i;
break;
}
}
if ( newid == -1 ) {
newid = sqle->maxstream++;
newarray = (SQLStream **) realloc((char *) sqle->ssarray,
sqle->maxstream * sizeof(SQLStream*));
if ( newarray == NULL ) {
return ERR;
}
sqle->ssarray = newarray;
sqls = (SQLStream *) calloc(1, sizeof(SQLStream));
if ( sqls == NULL ) {
return ERR;
}
sqle->ssarray[newid] = sqls;
}
sqls->connid = connid;
sqlc->refcount++;
rc = SQLAllocStmt(sqlc->hdbc, &hstmt);
if (rc == SQL_ERROR) {
return ERR;
}
sqls->hstmt = hstmt;
sqls->state = SQLS_INUSE;
*streamid = newid;
return OK;
}
STATUS
freeSqlStream(SQLEnv *sqle, int streamid)
{
SQLConn *sqlc;
SQLStream *sqls;
STATUS rc;
rc = mapSqlStream(sqle, streamid, &sqls);
if ( rc != OK ) {
return WARN;
}
sqls->state = SQLS_FREE;
rc = SQLFreeStmt(sqls->hstmt, SQL_DROP);
rc = mapSqlConn(sqle, sqls->connid, &sqlc);
if ( rc != OK ) {
return WARN;
}
if ( --sqlc->refcount == 0 )
{
rc = freeSqlConn(sqle, sqls->connid);
if (rc != OK) {
return WARN;
}
}
return OK;
}
STATUS
parseConnInfo(SQLConn *sqlc, char *info)
{
UCHAR *temp;
temp = strtok(info, "/\n");
if ( temp == NULL ) {
return ERR;
}
strncpy(sqlc->user, temp, 48);
temp = strtok(NULL, "/\n");
if ( temp == NULL ) {
return ERR;
}
strncpy(sqlc->passwd, temp, 48);
temp = strtok(NULL, "/\n");
if ( temp == NULL ) {
return ERR;
}
strncpy(sqlc->dbname, temp, 48);
return OK;
}