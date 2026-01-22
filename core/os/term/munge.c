#include "term.h"
#include <termios.h>
#include <unistd.h>
#include <signal.h>
#include <ctype.h>
#include <string.h>
int echo_qsize;
int echo_pstart;
int output_psize;
static inline void
poutput (int c)
{
if (termflags & FLUSH_OUTPUT)
return;
if ((c >= ' ') && (c < '\177'))
output_psize++;
else if (c == '\r')
output_psize = 0;
else if (c == '\t')
{
output_psize++;
while (output_psize % 8)
output_psize++;
}
else if (c == '\b')
output_psize--;
enqueue (&outputq, c);
}
void
output_character (int c)
{
int oflag = termstate.c_oflag;
if (oflag & OPOST)
{
if ((oflag & ONLCR) && c == '\n')
{
poutput ('\r');
poutput ('\n');
}
else if (!external_processing && (oflag & OXTABS) && c == '\t')
{
poutput (' ');
while (output_psize % 8)
poutput (' ');
}
else if ((oflag & ONOEOT) && c == CHAR_EOT)
;
else if ((oflag & OLCASE) && isalpha (c))
{
if (isupper (c))
poutput ('\\');
else
c = toupper (c);
poutput (c);
}
else
poutput (c);
}
else
poutput (c);
}
void
write_character (int c)
{
output_character (c);
echo_qsize = 0;
echo_pstart = output_psize;
}
int
output_width (int c, int loc)
{
int oflag = termstate.c_oflag;
if (oflag & OPOST)
{
if ((oflag & OLCASE) && isalpha (c) && isupper (c))
return 2;
}
if (c == '\t')
{
int n = loc + 1;
while (n % 8)
n++;
return n - loc;
}
if ((c >= ' ') && (c < '\177'))
return 1;
return 0;
}
struct queue *rawq;
char const char_parity[] =
{
1, 0, 0, 1, 0, 1, 1, 0,
0, 1, 1, 0, 1, 0, 0, 1,
0, 1, 1, 0, 1, 0, 0, 1,
1, 0, 0, 1, 0, 1, 1, 0,
0, 1, 1, 0, 1, 0, 0, 1,
1, 0, 0, 1, 0, 1, 1, 0,
1, 0, 0, 1, 0, 1, 1, 0,
0, 1, 1, 0, 1, 0, 0, 1,
0, 1, 1, 0, 1, 0, 0, 1,
1, 0, 0, 1, 0, 1, 1, 0,
1, 0, 0, 1, 0, 1, 1, 0,
0, 1, 1, 0, 1, 0, 0, 1,
1, 0, 0, 1, 0, 1, 1, 0,
0, 1, 1, 0, 1, 0, 0, 1,
0, 1, 1, 0, 1, 0, 0, 1,
1, 0, 0, 1, 0, 1, 1, 0,
};
#define checkevenpar(c) (((c)&0x80) \
? !char_parity[(c)&0x7f] \
: char_parity[(c)&0x7f])
#define checkoddpar(c) (((c)&0x80) \
? char_parity[(c)&0x7f] \
: !char_parity[(c)&0x7f])
static inline int
echo_p (char c, int quoted)
{
if (external_processing)
return 0;
return ((termstate.c_lflag & ECHO)
|| (c == '\n' && (termstate.c_lflag & ECHONL) && !quoted));
}
static inline int
echo_double (char c, int quoted)
{
return (iscntrl (c) && (termstate.c_lflag & ECHOCTL)
&& !((c == '\n' || c == '\t') && !quoted));
}
static inline void
write_erase_sequence (void)
{
poutput ('\b');
poutput (' ');
poutput ('\b');
}
static void
echo_char (char c, int hderase, int quoted)
{
echo_qsize++;
if (echo_p (c, quoted))
{
if (!hderase && (termflags & INSIDE_HDERASE))
{
write_character ('/');
termflags &= ~INSIDE_HDERASE;
}
if (hderase && !(termflags & INSIDE_HDERASE))
{
output_character ('\\');
termflags |= INSIDE_HDERASE;
}
if (echo_double (c, quoted))
{
output_character ('^');
output_character (c ^ CTRL_BIT);
}
else
output_character (c);
}
}
static inline void
reprint_line (void)
{
short *cp;
if (termstate.c_cc[VREPRINT] != _POSIX_VDISABLE
&& termstate.c_cc[VREPRINT] != (unsigned char) -1)
echo_char (termstate.c_cc[VREPRINT], 0, 0);
else
echo_char (CHAR_DC2, 0, 0);
echo_char ('\n', 0, 0);
echo_qsize = 0;
echo_pstart = output_psize;
for (cp = rawq->cs; cp != rawq->ce; cp++)
echo_char (unquote_char (*cp), 0, char_quoted_p (*cp));
}
static void
erase_1 (char erase_char)
{
int quoted;
char c;
quoted_char cq;
if (qsize (rawq) == 0)
return;
cq = queue_erase (rawq);
c = unquote_char (cq);
quoted = char_quoted_p (cq);
if (!echo_p (c, quoted))
return;
if (echo_qsize--)
{
if (termstate.c_lflag & ECHOPRT)
echo_char (c, 1, quoted);
else if (!(termstate.c_lflag & ECHOE) && erase_char)
echo_char (erase_char, 0, 0);
else
{
int nerase;
if (echo_double (c, quoted))
nerase = 2;
else if (c == '\t')
{
quoted_char *cp;
int loc = echo_pstart;
for (cp = rawq->ce - echo_qsize; cp != rawq->ce; cp++)
loc += (echo_double (unquote_char (*cp), char_quoted_p (*cp))
? 2
: output_width (*cp, loc));
nerase = output_psize - loc;
}
else
nerase = output_width (c, output_psize);
while (nerase--)
write_erase_sequence ();
}
if (echo_qsize == 0)
assert_backtrace (echo_pstart == output_psize);
}
else
reprint_line ();
}
int
input_character (int c)
{
int lflag = termstate.c_lflag;
int iflag = termstate.c_iflag;
int cflag = termstate.c_cflag;
cc_t *cc = termstate.c_cc;
struct queue **qp = (lflag & ICANON) ? &rawq : &inputq;
int flush = 0;
if ((iflag & INPCK)
&& ((cflag & PARODD) ? checkoddpar (c) : checkevenpar (c)))
{
if (iflag & IGNPAR)
goto alldone;
else if (iflag & PARMRK)
{
enqueue_quote (qp, CHAR_USER_QUOTE);
enqueue_quote (qp, '\0');
enqueue_quote (qp, c);
goto alldone;
}
else
c = 0;
}
if ((iflag & IXOFF)
&& !qavail (*qp)
&& (cc[VSTOP] != _POSIX_VDISABLE)
&& (cc[VSTOP] != (unsigned char) -1))
{
poutput (cc[VSTOP]);
termflags |= SENT_VSTOP;
}
if (!(iflag & ISTRIP) && (iflag & PARMRK) && (c == CHAR_USER_QUOTE))
enqueue_quote (qp, CHAR_USER_QUOTE);
if (iflag & ISTRIP)
c &= 0x7f;
if (!external_processing && (termflags & LAST_LNEXT))
{
enqueue_quote (qp, c);
echo_char (c, 0, 1);
termflags &= ~LAST_LNEXT;
goto alldone;
}
if (!external_processing && (iflag & ILCASE) && isalpha(c))
{
if (termflags & LAST_SLASH)
erase_1 (0);
else
c = isupper(c) ? tolower (c) : c;
}
if (!external_processing && (lflag & IEXTEN))
{
if (CCEQ (cc[VLNEXT], c))
{
if (lflag & ECHO)
{
poutput ('^');
poutput ('\b');
}
termflags |= LAST_LNEXT;
goto alldone;
}
if (CCEQ (cc[VDISCARD], c))
{
if (termflags & FLUSH_OUTPUT)
termflags &= ~FLUSH_OUTPUT;
else
{
drop_output ();
poutput (cc[VDISCARD]);
termflags |= FLUSH_OUTPUT;
}
goto alldone;
}
}
if (!external_processing && (lflag & ISIG))
{
if (CCEQ (cc[VINTR], c) || CCEQ (cc[VQUIT], c))
{
if (!(lflag & NOFLSH))
{
drop_output ();
clear_queue (inputq);
clear_queue (rawq);
flush = 1;
}
echo_char (c, 0, 0);
echo_qsize = 0;
echo_pstart = output_psize;
send_signal (CCEQ (cc[VINTR], c) ? SIGINT : SIGQUIT);
goto alldone;
}
if (CCEQ (cc[VSUSP], c))
{
if (!(lflag & NOFLSH))
{
flush = 1;
clear_queue (inputq);
clear_queue (rawq);
}
echo_char (c, 0, 0);
echo_qsize = 0;
echo_pstart = output_psize;
send_signal (SIGTSTP);
goto alldone;
}
}
if (!external_processing && (iflag & IXON))
{
if (CCEQ (cc[VSTOP], c))
{
if (CCEQ(cc[VSTART], c) && (termflags & USER_OUTPUT_SUSP))
goto alldone;
termflags |= USER_OUTPUT_SUSP;
(*bottom->suspend_physical_output) ();
return flush;
}
if (CCEQ (cc[VSTART], c))
goto alldone;
}
if (!external_processing)
{
if (c == '\r')
{
if (iflag & ICRNL)
c = '\n';
else if (iflag & IGNCR)
goto alldone;
}
else if ((c == '\n') && (iflag & INLCR))
c = '\r';
}
if (!external_processing && (lflag & ICANON))
{
if (CCEQ (cc[VERASE], c))
{
if (qsize(rawq))
erase_1 (c);
if (!(termflags & LAST_SLASH)
|| !(lflag & IEXTEN))
goto alldone;
}
if (CCEQ (cc[VKILL], c))
{
if (!(termflags & LAST_SLASH)
|| !(lflag & IEXTEN))
{
if ((lflag & ECHOKE) && !(lflag & ECHOPRT)
&& (echo_qsize == qsize (rawq)))
{
while (output_psize > echo_pstart)
write_erase_sequence ();
}
else
{
echo_char (c, 0, 0);
if ((lflag & ECHOK) || (lflag & ECHOKE))
echo_char ('\n', 0, 0);
}
clear_queue (rawq);
echo_qsize = 0;
echo_pstart = output_psize;
termflags &= ~(LAST_SLASH|LAST_LNEXT|INSIDE_HDERASE);
goto alldone;
}
else
erase_1 (0);
}
if (CCEQ (cc[VWERASE], c))
{
if (!(lflag & (ECHOPRT|ECHOE)))
echo_char (cc[VWERASE], 0, 1);
while (qsize (rawq) && isblank (unquote_char (rawq->ce[-1])))
erase_1 (0);
if (lflag & ALTWERASE)
while (qsize (rawq) && !isblank (unquote_char (rawq->ce[-1])))
erase_1 (0);
else
while (qsize (rawq) && !isblank (unquote_char (rawq->ce[-1]))
&& (isalnum (unquote_char (rawq->ce[-1]))
|| (unquote_char (rawq->ce[-1]) != '_')))
erase_1 (0);
goto alldone;
}
if (CCEQ (cc[VREPRINT], c) && (lflag & IEXTEN))
{
reprint_line ();
goto alldone;
}
if (CCEQ (cc[VSTATUS], c) && (lflag & ISIG) && (lflag & IEXTEN))
{
send_signal (SIGINFO);
goto alldone;
}
}
if (!qavail (*qp))
{
if (iflag & IMAXBEL)
poutput ('\a');
else
{
drop_output ();
clear_queue (inputq);
clear_queue (rawq);
echo_pstart = 0;
echo_qsize = 0;
flush = 1;
}
goto alldone;
}
echo_char (c, 0, 0);
if (CCEQ (cc[VEOF], c) && (lflag & ECHO))
{
int n;
n = echo_double (c, 0) ? 2 : output_width (c, output_psize);
while (n--)
poutput ('\b');
}
enqueue (qp, c);
if (lflag & ICANON)
{
if (CCEQ (cc[VEOL], c)
|| CCEQ (cc[VEOL2], c)
|| CCEQ (cc[VEOF], c)
|| c == '\n')
while (qsize (rawq))
enqueue (&inputq, dequeue (rawq));
}
alldone:
if ((iflag & IXANY) || (CCEQ (cc[VSTART], c)))
termflags &= ~USER_OUTPUT_SUSP;
(*bottom->start_output) ();
return flush;
}
void
input_break (void)
{
struct queue **qp = termstate.c_lflag & ICANON ? &rawq : &inputq;
if (termstate.c_iflag & IGNBRK)
return;
if (termstate.c_iflag & BRKINT)
{
drop_output ();
send_signal (SIGINT);
return;
}
if (termstate.c_iflag & PARMRK)
{
enqueue_quote (qp, CHAR_USER_QUOTE);
enqueue_quote (qp, '\0');
}
enqueue_quote (qp, '\0');
}
void
input_framing_error (int c)
{
struct queue **qp = termstate.c_lflag & ICANON ? &rawq : &inputq;
if (termstate.c_iflag & IGNPAR)
return;
if (termstate.c_iflag & PARMRK)
{
enqueue_quote (qp, CHAR_USER_QUOTE);
enqueue_quote (qp, '\0');
enqueue_quote (qp, c);
}
else
enqueue_quote (qp, '\0');
}
void
copy_rawq (void)
{
while (qsize (rawq))
enqueue (&inputq, dequeue (rawq));
}
void
rescan_inputq (void)
{
short *buf;
int i, n;
n = qsize (inputq);
buf = alloca (n * sizeof (quoted_char));
memcpy (buf, inputq->cs, n * sizeof (quoted_char));
clear_queue (inputq);
for (i = 0; i < n; i++)
input_character (unquote_char (buf[i]));
}
error_t
drop_output (void)
{
error_t err = (*bottom->abandon_physical_output) ();
if (!err)
clear_queue (outputq);
return err;
}
error_t
drain_output (void)
{
int cancel = 0;
while ((qsize (outputq) || (*bottom->pending_output_size) ())
&& (!(termflags & NO_CARRIER) || (termstate.c_cflag & CLOCAL))
&& !cancel)
cancel = pthread_hurd_cond_wait_np (outputq->wait, &global_lock);
return cancel ? EINTR : 0;
}
struct queue *
create_queue (int size, int lowat, int hiwat)
{
struct queue *q;
q = malloc (sizeof (struct queue) + size * sizeof (quoted_char));
assert_backtrace (q);
q->susp = 0;
q->lowat = lowat;
q->hiwat = hiwat;
q->cs = q->ce = q->array;
q->arraylen = size;
q->wait = malloc (sizeof (pthread_cond_t));
assert_backtrace (q->wait);
pthread_cond_init (q->wait, NULL);
return q;
}
struct queue *
reallocate_queue (struct queue *q)
{
int len;
struct queue *newq;
len = qsize (q);
if (len < q->arraylen / 2)
{
memmove (q->array, q->cs, len * sizeof (quoted_char));
q->cs = q->array;
q->ce = q->cs + len;
}
else
{
newq = malloc (sizeof (struct queue)
+ q->arraylen * 2 * sizeof (quoted_char));
newq->susp = q->susp;
newq->lowat = q->lowat;
newq->hiwat = q->hiwat;
newq->cs = newq->array;
newq->ce = newq->array + len;
newq->arraylen = q->arraylen * 2;
newq->wait = q->wait;
memmove (newq->array, q->cs, len * sizeof (quoted_char));
free (q);
q = newq;
}
return q;
}