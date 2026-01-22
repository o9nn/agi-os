#ifndef dwinst_INCLUDED
#  define dwinst_INCLUDED
#ifndef MAXSTR
#ifdef MAX_PATH
#define MAXSTR MAX_PATH
#else
#define MAXSTR 256
#endif
#endif
class CInstall
{
public:
CInstall();
virtual ~CInstall();
void SetMessageFunction(void(*fn)(const char *));
void AddMessage(const char *message);
const char *GetMainDir();
const char *GetUninstallName();
BOOL GetPrograms(BOOL bUseCommon, char *buf, int buflen);
BOOL Init(const char *szSourceDir, const char *szFileList);
BOOL InstallFiles(BOOL bNoCopy, BOOL *pbQuit);
BOOL InstallFile(char *filename, BOOL bNoCopy);
BOOL MakeDir(const char *dirname);
FILE * MakeTemp(char *name);
BOOL SetAllUsers(BOOL bUseCommon);
void SetTargetDir(const char *szTargetDir);
void SetTargetGroup(const char *szTargetGroup);
BOOL StartMenuBegin();
BOOL StartMenuEnd();
BOOL StartMenuAdd(const char *szDescription, const char *szProgram, const char *szArguments);
BOOL UpdateRegistryBegin();
BOOL UpdateRegistryKey(const char *product, const char *version);
BOOL UpdateRegistryValue(const char *product, const char *version, const char *name, const char *value);
BOOL UpdateRegistryEnd();
BOOL WriteUninstall(const char *prog, BOOL bNoCopy);
BOOL MakeLog(void);
void CleanUp(void);
void AppendFileNew(const char *filename);
private:
BOOL m_bNoCopy;
BOOL m_bUseCommon;
BOOL m_bQuit;
char m_szSourceDir[MAXSTR];
char m_szFileList[MAXSTR];
char m_szTargetDir[MAXSTR];
char m_szTargetGroup[MAXSTR];
char m_szPrograms[MAXSTR];
char m_szUninstallName[MAXSTR];
char m_szMainDir[MAXSTR];
char m_szLogDir[MAXSTR];
char m_szFileNew[MAXSTR];
char m_szRegistryNew[MAXSTR];
char m_szRegistryOld[MAXSTR];
char m_szShellNew[MAXSTR];
char m_szShellOld[MAXSTR];
FILE * m_fLogNew;
FILE * m_fLogOld;
BOOL SetRegistryValue(HKEY hkey, const char *value_name, const char *value);
BOOL CreateShellLink(LPCSTR description, LPCSTR program, LPCSTR arguments, LPCSTR icon = NULL, int nIconIndex = 0);
void CopyFileContents(FILE *df, FILE *sf);
void ResetReadonly(const char *filename);
void(*AddMessageFn)(const char *);
};
#endif