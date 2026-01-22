#include "common.h"
char *REMFROMRE =
"^>?From[ \t]+((\".*\")?[^\" \t]+?(\".*\")?[^\" \t]+?)[ \t]+(.+)[ \t]+remote[ \t]+from[ \t]+(.*)\n$";
int REMSENDERMATCH = 1;
int REMDATEMATCH = 4;
int REMSYSMATCH = 5;
char *FROMRE =
"^>?From[ \t]+((\".*\")?[^\" \t]+?(\".*\")?[^\" \t]+?)[ \t]+(.+)\n$";
int SENDERMATCH = 1;
int DATEMATCH = 4;
int
print_header(Biobuf *fp, char *sender, char *date)
{
return Bprint(fp, "From %s %s\n", sender, date);
}
int
print_remote_header(Biobuf *fp, char *sender, char *date, char *system)
{
return Bprint(fp, "From %s %s remote from %s\n", sender, date, system);
}
int
parse_header(char *line, String *sender, String *date)
{
if (!IS_HEADER(line))
return -1;
line += sizeof("From ") - 1;
s_restart(sender);
while(*line==' '||*line=='\t')
line++;
if(*line == '"'){
s_putc(sender, *line++);
while(*line && *line != '"')
s_putc(sender, *line++);
s_putc(sender, *line++);
} else {
while(*line && *line != ' ' && *line != '\t')
s_putc(sender, *line++);
}
s_terminate(sender);
s_restart(date);
while(*line==' '||*line=='\t')
line++;
while(*line)
s_putc(date, *line++);
s_terminate(date);
return 0;
}