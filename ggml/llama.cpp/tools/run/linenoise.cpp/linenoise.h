#ifndef __LINENOISE_H
#define __LINENOISE_H
#ifdef __cplusplus
extern "C" {
#endif
#include <stddef.h>
#include <stdlib.h>
extern const char * linenoiseEditMore;
struct linenoiseState {
int in_completion;
size_t completion_idx;
int ifd;
int ofd;
char * buf;
size_t buflen;
const char * prompt;
size_t plen;
size_t pos;
size_t oldcolpos;
size_t len;
size_t cols;
size_t oldrows;
int history_index;
};
struct linenoiseCompletions {
size_t len = 0;
char ** cvec = nullptr;
bool to_free = true;
~linenoiseCompletions() {
if (!to_free) {
return;
}
for (size_t i = 0; i < len; ++i) {
free(cvec[i]);
}
free(cvec);
}
};
int linenoiseEditStart(struct linenoiseState * l, int stdin_fd, int stdout_fd, char * buf, size_t buflen,
const char * prompt);
const char * linenoiseEditFeed(struct linenoiseState * l);
void linenoiseEditStop(struct linenoiseState * l);
void linenoiseHide(struct linenoiseState * l);
void linenoiseShow(struct linenoiseState * l);
const char * linenoise(const char * prompt);
void linenoiseFree(void * ptr);
typedef void(linenoiseCompletionCallback)(const char *, linenoiseCompletions *);
typedef const char *(linenoiseHintsCallback) (const char *, int * color, int * bold);
typedef void(linenoiseFreeHintsCallback)(const char *);
void linenoiseSetCompletionCallback(linenoiseCompletionCallback *);
void linenoiseSetHintsCallback(linenoiseHintsCallback *);
void linenoiseSetFreeHintsCallback(linenoiseFreeHintsCallback *);
void linenoiseAddCompletion(linenoiseCompletions *, const char *);
int linenoiseHistoryAdd(const char * line);
int linenoiseHistorySetMaxLen(int len);
int linenoiseHistorySave(const char * filename);
int linenoiseHistoryLoad(const char * filename);
void linenoiseClearScreen(void);
void linenoiseSetMultiLine(int ml);
void linenoisePrintKeyCodes(void);
void linenoiseMaskModeEnable(void);
void linenoiseMaskModeDisable(void);
typedef size_t(linenoisePrevCharLen)(const char * buf, size_t buf_len, size_t pos, size_t * col_len);
typedef size_t(linenoiseNextCharLen)(const char * buf, size_t buf_len, size_t pos, size_t * col_len);
typedef size_t(linenoiseReadCode)(int fd, char * buf, size_t buf_len, int * c);
void linenoiseSetEncodingFunctions(linenoisePrevCharLen * prevCharLenFunc, linenoiseNextCharLen * nextCharLenFunc,
linenoiseReadCode * readCodeFunc);
#ifdef __cplusplus
}
#endif
#endif