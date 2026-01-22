#include <stdio.h>
#include <stdlib.h>
#include "error.h"
#include "lexxer.h"
#include "global.h"
#include "write.h"
extern int yyparse();
static FILE *myfopen(const char *name, const char *mode);
static void myfclose(FILE *file, const char *name);
static string_t RoutineListFileName;
static void
parseArgs(int argc, char **argv)
{
while (--argc > 0)
if ((++argv)[0][0] == '-')
{
switch (argv[0][1])
{
case 'n':
DefaultFiles = false;
break;
case 'q':
BeQuiet = true;
break;
case 'Q':
BeQuiet = false;
break;
case 'v':
BeVerbose = true;
break;
case 'V':
BeVerbose = false;
break;
case 'r':
break;
case 'R':
fatal("the option `-R' cannot be used anymore, use the rpc "
"calls (`-r', default) instead.");
break;
case 'l':
if (streql(argv[0], "-list"))
{
--argc; ++argv;
if (argc == 0)
fatal("missing name for -list option");
RoutineListFileName = strmake(argv[0]);
}
break;
case 's':
if (streql(argv[0], "-server"))
{
--argc; ++argv;
if (argc == 0)
fatal("missing name for -server option");
ServerFileName = strmake(argv[0]);
}
else if (streql(argv[0], "-sheader"))
{
--argc; ++argv;
if (argc == 0)
fatal ("missing name for -sheader option");
ServerHeaderFileName = strmake(argv[0]);
}
else if (streql(argv[0], "-subrprefix"))
{
--argc; ++argv;
if (argc == 0)
fatal ("missing string for -subrprefix option");
SubrPrefix = strmake(argv[0]);
}
else
GenSymTab = true;
break;
case 'S':
GenSymTab = false;
break;
case 'i':
if (streql(argv[0], "-iheader"))
{
--argc; ++argv;
if (argc == 0)
fatal("missing name for -iheader option");
InternalHeaderFileName = strmake(argv[0]);
}
else
{
--argc; ++argv;
if (argc == 0)
fatal("missing prefix for -i option");
UserFilePrefix = strmake(argv[0]);
}
break;
case 'u':
if (streql(argv[0], "-user"))
{
--argc; ++argv;
if (argc == 0)
fatal("missing name for -user option");
UserFileName = strmake(argv[0]);
}
else
fatal("unknown flag: '%s'", argv[0]);
break;
case 'h':
if (streql(argv[0], "-header"))
{
--argc; ++argv;
if (argc == 0)
fatal("missing name for -header option");
UserHeaderFileName = strmake(argv[0]);
}
else
fatal("unknown flag: '%s'", argv[0]);
break;
case 'p':
if (streql(argv[0], "-prefix"))
{
if (--argc == 0)
fatal ("missing string for -prefix option");
RoutinePrefix = strmake(*++argv);
}
break;
default:
fatal("unknown flag: '%s'", argv[0]);
}
}
else
fatal("bad argument: '%s'", *argv);
}
int
main(int argc, char **argv)
{
FILE *uheader, *server, *user;
FILE *iheader, *sheader;
set_program_name("mig");
parseArgs(argc, argv);
init_global();
LookNormal();
(void) yyparse();
if (errors > 0)
exit(1);
more_global();
uheader = myfopen(UserHeaderFileName, "w");
if (!UserFilePrefix)
user = myfopen(UserFileName, "w");
else
user = NULL;
server = myfopen(ServerFileName, "w");
if (ServerHeaderFileName)
sheader = myfopen(ServerHeaderFileName, "w");
else
sheader = NULL;
if (IsKernelServer)
iheader = myfopen(InternalHeaderFileName, "w");
else
iheader = NULL;
if (BeVerbose)
{
printf("Writing %s ... ", UserHeaderFileName);
fflush(stdout);
}
WriteUserHeader(uheader, StatementList);
myfclose(uheader, UserHeaderFileName);
if (ServerHeaderFileName)
{
if (BeVerbose)
{
printf ("done.\nWriting %s ...", ServerHeaderFileName);
fflush (stdout);
}
WriteServerHeader(sheader, StatementList);
myfclose(sheader, ServerHeaderFileName);
}
if (IsKernelServer)
{
if (BeVerbose)
{
printf("done.\nWriting %s ... ", InternalHeaderFileName);
fflush(stdout);
}
WriteInternalHeader(iheader, StatementList);
myfclose(iheader, InternalHeaderFileName);
}
if (UserFilePrefix)
{
if (BeVerbose)
{
printf("done.\nWriting individual user files ... ");
fflush(stdout);
}
WriteUserIndividual(StatementList);
}
else
{
if (BeVerbose)
{
printf("done.\nWriting %s ... ", UserFileName);
fflush(stdout);
}
WriteUser(user, StatementList);
myfclose(user, UserFileName);
}
if (BeVerbose)
{
printf("done.\nWriting %s ... ", ServerFileName);
fflush(stdout);
}
WriteServer(server, StatementList);
myfclose(server, ServerFileName);
if (RoutineListFileName != strNULL)
{
FILE *listfile = myfopen (RoutineListFileName, "w");
WriteRoutineList (listfile, StatementList);
myfclose (listfile, RoutineListFileName);
}
if (BeVerbose)
printf("done.\n");
return 0;
}
static FILE *
myfopen(const char *name, const char *mode)
{
const char *realname;
FILE *file;
extern int errno;
if (name == strNULL)
realname = "/dev/null";
else
realname = name;
file = fopen(realname, mode);
if (file == NULL)
fatal("fopen(%s): %s", realname, unix_error_string(errno));
return file;
}
static void
myfclose(FILE *file, const char *name)
{
if (ferror(file) || fclose(file))
fatal("fclose(%s): %s", name, unix_error_string(errno));
}