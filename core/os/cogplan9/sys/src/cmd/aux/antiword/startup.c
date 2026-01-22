#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "DeskLib:Error.h"
#include "DeskLib:Event.h"
#include "DeskLib:SWI.h"
#include "antiword.h"
#if !defined(TaskManager_EnumerateTasks)
#define TaskManager_EnumerateTasks	0x042681
#endif
static BOOL
bIsMatch(const char *szStr1, const char *szStr2)
{
const char	*pcTmp1, *pcTmp2;
for (pcTmp1 = szStr1, pcTmp2 = szStr2;
*pcTmp1 != '\0';
pcTmp1++, pcTmp2++) {
if (toupper(*pcTmp1) != toupper(*pcTmp2)) {
return FALSE;
}
}
return *pcTmp2 == '\0';
}
static task_handle
tGetTaskHandle(const char *szTaskname)
{
const char	*pcTmp;
int	iReg0, iIndex;
int	aiBuffer[4];
char	szTmp[21];
iReg0 = 0;
do {
Error_CheckFatal(SWI(3, 1, TaskManager_EnumerateTasks | XOS_Bit,
iReg0, aiBuffer, sizeof(aiBuffer), &iReg0));
for (iIndex = 0, pcTmp = (const char *)aiBuffer[1];
iIndex < elementsof(szTmp);
iIndex++, pcTmp++) {
if (iscntrl(*pcTmp)) {
szTmp[iIndex] = '\0';
break;
}
szTmp[iIndex] = *pcTmp;
}
szTmp[elementsof(szTmp) - 1] = '\0';
if (bIsMatch(szTmp, szTaskname)) {
return (task_handle)aiBuffer[0];
}
} while (iReg0 >= 0);
return 0;
}
int
main(int argc, char **argv)
{
message_block	tMsg;
task_handle	tTaskHandle;
size_t	tArgLen;
int	aiMessages[] = {0};
char	szCommand[512];
Event_Initialise3("StartUp", 310, aiMessages);
if (argc > 1) {
tArgLen = strlen(argv[1]);
} else {
tArgLen = 0;
}
if (tArgLen >= sizeof(tMsg.data.dataload.filename)) {
werr(1, "Input filename too long");
return EXIT_FAILURE;
}
tTaskHandle = tGetTaskHandle("antiword");
if (tTaskHandle == 0) {
strcpy(szCommand, "chain:<Antiword$Dir>.!Antiword");
if (argc > 1) {
strcat(szCommand, " ");
strcat(szCommand, argv[1]);
}
#if defined(DEBUG)
strcat(szCommand, " ");
strcat(szCommand, "2><Antiword$Dir>.Debug");
#endif
system(szCommand);
return EXIT_FAILURE;
}
if (argc > 1) {
memset(&tMsg, 0, sizeof(tMsg));
tMsg.header.size = ROUND4(offsetof(message_block, data) +
offsetof(message_dataload, filename) +
1 + tArgLen);
tMsg.header.yourref = 0;
tMsg.header.action = message_DATALOAD;
tMsg.data.dataload.window = window_ICONBAR;
tMsg.data.dataload.icon = -1;
tMsg.data.dataload.size = 0;
tMsg.data.dataload.filetype = FILETYPE_MSWORD;
strcpy(tMsg.data.dataload.filename, argv[1]);
Error_CheckFatal(Wimp_SendMessage(event_SEND,
&tMsg, tTaskHandle, 0));
return EXIT_SUCCESS;
} else {
werr(1, "Antiword is already running");
return EXIT_FAILURE;
}
}