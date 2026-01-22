#include <errno.h>
#include <sys/io.h>
#include <string.h>
#include <argp.h>
#include "driver.h"
#include "timer.h"
static struct timer_list generic_speaker_timer;
static struct bell_ops generic_speaker_ops;
#define SPEAKER 0x61
#define SPEAKER_TMR2 0x01
#define SPEAKER_DATA 0x02
#define PIT_COUNTER_0 0x40
#define PIT_COUNTER_1 0x41
#define PIT_COUNTER_2 0x42
#define PIT_FREQUENCY 0x1234dd
#define PIT_CTRL 0x43
#define PIT_CTRL_SELECT_MASK 0xc0
#define PIT_CTRL_SELECT_0 0x00
#define PIT_CTRL_SELECT_1 0x40
#define PIT_CTRL_SELECT_2 0x80
#define PIT_CTRL_READLOAD_MASK 0x30
#define PIT_CTRL_COUNTER_LATCH 0x00
#define PIT_CTRL_READLOAD_LSB 0x10
#define PIT_CTRL_READLOAD_MSB 0x20
#define PIT_CTRL_READLOAD_WORD 0x30
#define PIT_CTRL_MODE_MASK 0x0e
#define PIT_CTRL_INTR_ON_TERM 0x00
#define PIT_CTRL_PROGR_ONE_SHOT 0x02
#define PIT_CTRL_RATE_GEN 0x04
#define PIT_CTRL_SQUAREWAVE_GEN 0x06
#define PIT_CTRL_SOFTSTROBE 0x08
#define PIT_CTRL_HARDSTROBE 0x0a
#define PIT_CTRL_COUNT_MASK 0x01
#define PIT_CTRL_COUNT_BINARY 0x00
#define PIT_CTRL_COUNT_BCD 0x01
#define T_TEMP_SCALE 1.0594630943592952645
#define T_DOWN_ONE_HALF(x) ((short) (x / T_TEMP_SCALE))
#define T_UP_ONE_HALF(x) ((short) (x * T_TEMP_SCALE))
#define T_DOWN_ONE_OCTAVE(x) ((short) (x / 2))
#define T_UP_ONE_OCTAVE(x) ((short) (x * 2))
#define T_REST ((short) 0)
#define T_FINE ((short) (-1))
#define T_b_3 T_UP_ONE_OCTAVE (T_b_2)
#define T_b_F_3 T_UP_ONE_OCTAVE (T_b_F_2)
#define T_a_S_3 T_b_F_3
#define T_a_3 T_UP_ONE_OCTAVE (T_a_2)
#define T_a_F_3 T_UP_ONE_OCTAVE (T_a_F_2)
#define T_g_S_3 T_a_F_3
#define T_g_3 T_UP_ONE_OCTAVE (T_g_2)
#define T_g_F_3 T_f_S_3
#define T_f_S_3 T_UP_ONE_OCTAVE (T_f_S_2)
#define T_f_3 T_UP_ONE_OCTAVE (T_f_2)
#define T_e_3 T_UP_ONE_OCTAVE (T_e_2)
#define T_e_F_3 T_UP_ONE_OCTAVE (T_e_F_2)
#define T_d_S_3 T_e_F_3
#define T_d_3 T_UP_ONE_OCTAVE (T_d_2)
#define T_d_F_3 T_c_S_3
#define T_c_S_3 T_UP_ONE_OCTAVE (T_c_S_2)
#define T_c_3 T_UP_ONE_OCTAVE (T_c_2)
#define T_b_2 T_UP_ONE_OCTAVE (T_b_1)
#define T_b_F_2 T_UP_ONE_OCTAVE (T_b_F_1)
#define T_a_S_2 T_b_F_2
#define T_a_2 T_UP_ONE_OCTAVE (T_a_1)
#define T_a_F_2 T_UP_ONE_OCTAVE (T_a_F_1)
#define T_g_S_2 T_a_F_2
#define T_g_2 T_UP_ONE_OCTAVE (T_g_1)
#define T_g_F_2 T_f_S_2
#define T_f_S_2 T_UP_ONE_OCTAVE (T_f_S_1)
#define T_f_2 T_UP_ONE_OCTAVE (T_f_1)
#define T_e_2 T_UP_ONE_OCTAVE (T_e_1)
#define T_e_F_2 T_UP_ONE_OCTAVE (T_e_F_1)
#define T_d_S_2 T_e_F_2
#define T_d_2 T_UP_ONE_OCTAVE (T_d_1)
#define T_d_F_2 T_c_S_2
#define T_c_S_2 T_UP_ONE_OCTAVE (T_c_S_1)
#define T_c_2 T_UP_ONE_OCTAVE (T_c_1)
#define T_b_1 T_UP_ONE_HALF (T_b_F_1)
#define T_b_F_1 T_UP_ONE_HALF (T_a_1)
#define T_a_S_1 T_b_F_1
#define T_a_1 ((short) (440))
#define T_a_F_1 T_DOWN_ONE_HALF (T_a_1)
#define T_g_S_1 T_a_F_1
#define T_g_1 T_DOWN_ONE_HALF (T_a_F_1)
#define T_g_F_1 T_f_S_1
#define T_f_S_1 T_DOWN_ONE_HALF (T_g_1)
#define T_f_1 T_DOWN_ONE_HALF (T_f_S_1)
#define T_e_1 T_DOWN_ONE_HALF (T_f_1)
#define T_e_F_1 T_DOWN_ONE_HALF (T_e_1)
#define T_d_S_1 T_e_F_1
#define T_d_1 T_DOWN_ONE_HALF (T_e_F_1)
#define T_d_F_1 T_c_S_1
#define T_c_S_1 T_DOWN_ONE_HALF (T_d_1)
#define T_c_1 T_DOWN_ONE_HALF (T_c_S_1)
#define T_b T_DOWN_ONE_OCTAVE (T_b_1)
#define T_b_F T_DOWN_ONE_OCTAVE (T_b_F_1)
#define T_a_S T_b_F
#define T_a T_DOWN_ONE_OCTAVE (T_a_1)
#define T_a_F T_DOWN_ONE_OCTAVE (T_a_F_1)
#define T_g_S T_a_F
#define T_g T_DOWN_ONE_OCTAVE (T_g_1)
#define T_g_F T_f_S
#define T_f_S T_DOWN_ONE_OCTAVE (T_f_S_1)
#define T_f T_DOWN_ONE_OCTAVE (T_f_1)
#define T_e T_DOWN_ONE_OCTAVE (T_e_1)
#define T_e_F T_DOWN_ONE_OCTAVE (T_e_F_1)
#define T_d_S T_e_F
#define T_d T_DOWN_ONE_OCTAVE (T_d_1)
#define T_d_F T_c_S
#define T_c_S T_DOWN_ONE_OCTAVE (T_c_S_1)
#define T_c T_DOWN_ONE_OCTAVE (T_c_1)
struct note
{
short pitch;
short duration;
};
struct melody
{
char *name;
int measure;
struct note *next;
struct note note[];
};
#define BELL_CLASSIC "classic"
#define BELL_LINUX "linux"
#define BELL_ALARM "alarm"
#define BELL_CMAJOR "cmajor"
static struct melody beep1 =
{ BELL_CLASSIC, 160, NULL, {
{ T_a_1, 4 }, { T_FINE, 0 }
} };
static struct melody beep2 =
{ BELL_LINUX, 60, NULL, {
{ 750, 1 }, { T_FINE, 0 }
} };
static struct melody beep3 =
{ BELL_ALARM, 160, NULL, {
{ T_f_2, 2 }, { T_b_1, 4 }, { T_FINE, 0 }
} };
static struct melody beep4 =
{ BELL_CMAJOR, 160, NULL, {
{ T_c_2, 2 }, { T_e_2, 2 }, { T_g_2, 2 },
{ T_FINE, 0 }
} };
static struct melody *beep[] = { &beep1, &beep2, &beep3, &beep4 };
static int active_beep;
#if QUAERENDO_INVENIETIS
struct melody tune1 =
{ "FSF Song", 160, NULL, {
{ T_d_2, 16 }, { T_c_2, 8 }, { T_b_1, 16 }, { T_a_1, 16 },
{ T_b_1, 16 }, { T_c_2, 8 }, { T_b_1, 8 }, { T_a_1, 8 }, { T_g_1, 16 },
{ T_g_1, 24 }, { T_a_1, 24 }, { T_b_1, 8 },
{ T_c_2, 24 }, { T_b_1, 14 }, { T_REST, 2 }, { T_b_1, 8 }, { T_d_2, 8 },
{ T_a_1, 22 }, { T_REST, 2 }, { T_a_1, 32 },
{ T_c_2, 16 }, { T_d_2, 8 }, { T_c_2, 16 }, { T_b_1, 24 },
{ T_d_2, 16 }, { T_c_2, 8 }, { T_b_1, 16 }, { T_a_1, 16 },
{ T_b_1, 16 }, { T_c_2, 8 }, { T_b_1, 8 }, { T_a_1, 8 }, { T_g_1, 16 },
{ T_g_1, 24 }, { T_a_1, 24 }, { T_b_1, 8 },
{ T_c_2, 24 }, { T_b_1, 14 }, { T_REST, 2 }, { T_b_1, 8 }, { T_d_2, 8 },
{ T_a_1, 22 }, { T_REST, 2 }, { T_a_1, 30 }, { T_REST, 2 },
{ T_a_1, 56 }, { T_FINE, 0 }
} };
struct melody tune2 =
{ "I Feel Pretty", 160, NULL, {
{ T_c_1, 8 }, { T_e_1, 8 },
{ T_f_1, 4 }, { T_a_1, 20 },
{ T_REST, 8 }, { T_c_1, 8 }, { T_e_1, 8 },
{ T_f_1, 4 }, { T_a_1, 20 },
{ T_REST, 8 }, { T_c_1, 8 }, { T_e_1, 8 },
{ T_f_1, 4 }, { T_a_1, 12 }, { T_e_1, 8 },
{ T_f_1, 4 }, { T_a_1, 12 }, { T_f_1, 8 },
{ T_c_2, 32 },
{ T_b_F_1,8 }, { T_a_1, 8 },
{ T_g_1, 4 }, { T_f_1, 20 },
{ T_REST, 8 }, { T_g_1, 8 }, { T_a_1, 8 },
{ T_b_F_1,12}, { T_a_1, 4 }, { T_g_1, 4 }, { T_f_1, 4 },
{ T_e_1, 16 }, { T_d_1, 3 }, { T_e_1, 2 }, { T_d_1, 3 },
{ T_c_1, 56 }, { T_FINE, 0 }
} };
struct melody tune3 =
{ "Summertime", 120, NULL, {
{ T_b_1, 8 }, { T_g_1, 8 },
{ T_b_1, 36 }, { T_REST, 4 }, { T_a_1, 6 }, { T_g_1, 2 },
{ T_a_1, 6 }, { T_b_1, 2 }, { T_g_1, 8 },
{ T_e_1, 16 }, { T_b, 24 }, { T_REST, 8 },
{ T_b_1, 8 }, { T_g_1, 8 },
{ T_a_1, 3 }, { T_REST, 1 }, { T_a_1, 28 },
{ T_REST, 8 }, { T_g_1, 6 }, { T_e_1, 2 },
{ T_g_1, 6 }, { T_e_1, 2 }, { T_g_1, 8 },
{ T_f_S_1,48}, { T_REST, 4 }, { T_b_1, 8 }, { T_g_1, 4 },
{ T_b_1, 3 }, { T_REST, 1 }, { T_b_1, 7 }, { T_REST, 1 }, { T_b_1, 20 },
{ T_REST, 8 }, { T_a_1, 6 }, { T_g_1, 2 },
{ T_a_1, 6 }, { T_b_1, 2 }, { T_g_1, 8 },
{ T_e_1, 16 }, { T_b, 32 }, { T_REST, 8 },
{ T_b, 8 }, { T_d_1, 8 }, { T_b, 4 },
{ T_d_1, 4 }, { T_e_1, 8 }, { T_g_1, 8 },
{ T_b_1, 2 }, { T_b_1 + (T_b_1 - T_a_1) / 3, 1 },
{ T_b_1 + 2 * (T_b_1 - T_a_1) / 3, 1 },
{ T_a_1, 12 }, { T_g_1, 15 }, { T_REST, 1 },
{ T_g_1, 4 }, { T_e_1, 68 },
{ T_FINE, 0 }
} };
struct melody tune4 =
{ "Indiana Jones Theme", 250, NULL, {
{ T_e_1, 4 }, { T_REST, 8 }, { T_f_1, 4 },
{ T_g_1, 4 }, { T_REST, 4 }, { T_c_2, 24 },
{ T_REST, 16 }, { T_d_1, 4 }, { T_REST, 8 }, { T_e_1, 4 },
{ T_f_1, 32 },
{ T_REST, 16 }, { T_g_1, 4 }, { T_REST, 8 }, { T_a_1, 4 },
{ T_b_1, 4 }, { T_REST, 4 }, { T_f_2, 24 },
{ T_REST, 16 }, { T_a_1, 4 }, { T_REST, 8 }, { T_b_1, 4 },
{ T_c_2, 8 }, { T_REST, 8 }, { T_d_2, 12 }, { T_REST, 4 },
{ T_e_2, 8 }, { T_REST, 8 },
{ T_e_1, 4 }, { T_REST, 8 }, { T_f_1, 4 },
{ T_g_1, 4 }, { T_REST, 4 }, { T_c_2, 24 },
{ T_REST, 16 }, { T_d_2, 4 }, { T_REST, 8 }, { T_e_2, 4 },
{ T_f_2, 32 },
{ T_REST, 16 }, { T_g_1, 4 }, { T_REST, 8 }, { T_g_1, 4 },
{ T_e_2, 16 }, { T_d_2, 4 }, { T_REST, 8 }, { T_g_1, 4 },
{ T_e_2, 16 }, { T_d_2, 4 }, { T_REST, 8 }, { T_g_1, 4 },
{ T_f_2, 16 }, { T_e_2, 4 }, { T_REST, 8 }, { T_d_2, 4 },
{ T_c_2, 32 }, { T_FINE, 0 }
} };
struct melody *tune[] = { &tune1, &tune2, &tune3, &tune4 };
#endif
static void
beep_off (void)
{
unsigned char status;
status = inb (SPEAKER) & ~(SPEAKER_TMR2 | SPEAKER_DATA);
outb (status, SPEAKER);
}
static void
beep_on (short pitch)
{
unsigned char status;
unsigned int counter;
if (pitch < 20)
pitch = 20;
else if (pitch > 20000)
pitch = 20000;
counter = PIT_FREQUENCY / pitch;
outb (PIT_CTRL_SELECT_2 | PIT_CTRL_READLOAD_WORD
| PIT_CTRL_SQUAREWAVE_GEN | PIT_CTRL_COUNT_BINARY, PIT_CTRL);
outb (counter & 0xff, PIT_COUNTER_2);
outb ((counter >> 8) & 0xff, PIT_COUNTER_2);
status = inb (SPEAKER) | SPEAKER_TMR2 | SPEAKER_DATA;
outb (status, SPEAKER);
}
static int
next_note (void *handle)
{
struct melody *melody = handle;
switch (melody->next->pitch)
{
case T_FINE:
beep_off ();
return 0;
case T_REST:
beep_off ();
break;
default:
beep_on (melody->next->pitch);
break;
}
generic_speaker_timer.expires
+= (60 * HZ * melody->next->duration / (8 * melody->measure));
melody->next++;
return 1;
}
static const char doc[] = "Generic speaker driver";
static const struct argp_option options[] =
{
{"bell-style", 'b', "BELL", 0, "Use one of the bells: "
BELL_CLASSIC ", " BELL_LINUX ", " BELL_ALARM
" or " BELL_CMAJOR},
{ 0 }
};
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
int *pos = (int *) state->input;
switch (key)
{
case 'b':
{
unsigned int i;
int found = 0;
for (i = 0; i < sizeof (beep) / sizeof (*beep); i++)
{
if (! strcasecmp (beep[i]->name, arg))
{
found = 1;
break;
}
}
if (! found)
argp_usage (state);
active_beep = i;
break;
}
default:
return ARGP_ERR_UNKNOWN;
}
*pos = state->next;
return 0;
}
static struct argp argp = {options, parse_opt, 0, doc};
static error_t
generic_speaker_init (void **handle, int no_exit,
int argc, char *argv[], int *next)
{
error_t err;
int pos = 1;
err = argp_parse (&argp, argc, argv, ARGP_IN_ORDER | ARGP_NO_EXIT
| ARGP_SILENT, 0 , &pos);
*next += pos - 1;
if (err && err != EINVAL)
return err;
timer_clear (&generic_speaker_timer);
generic_speaker_timer.fnc = &next_note;
return 0;
}
static error_t
generic_speaker_start (void *handle)
{
error_t err;
if (ioperm (SPEAKER, 1, 1) < 0)
return errno;
if (ioperm (PIT_COUNTER_2, PIT_CTRL - PIT_COUNTER_2 + 1, 1) < 0)
return errno;
beep_off ();
err = driver_add_bell (&generic_speaker_ops, NULL);
return err;
}
static error_t
generic_speaker_fini (void *handle, int force)
{
driver_remove_bell (&generic_speaker_ops, NULL);
if (timer_remove (&generic_speaker_timer))
beep_off ();
return 0;
}
static error_t
generic_speaker_beep (void *handle)
{
if (timer_remove (&generic_speaker_timer))
beep_off ();
generic_speaker_timer.fnc_data = beep[active_beep];
beep[active_beep]->next = &beep[active_beep]->note[1];
beep_on (beep[active_beep]->note[0].pitch);
generic_speaker_timer.expires = fetch_jiffies ()
+ (60 * HZ * beep[active_beep]->note[0].duration
/ (8 * beep[active_beep]->measure));
timer_add (&generic_speaker_timer);
return 0;
}
#if QUAERENDO_INVENIETIS
static void
generic_speaker_play_melody (void *handle, unsigned int key)
{
if (key < 0 || key >= sizeof (tune) / sizeof (tune[0]))
return;
if (timer_remove (&generic_speaker_timer))
beep_off ();
generic_speaker_timer.fnc_data = tune[key];
tune[key]->next = &tune[key]->note[1];
beep_on (tune[key]->note[0].pitch);
generic_speaker_timer.expires = fetch_jiffies ()
+ (60 * HZ * tune[key]->note[0].duration / (8 * tune[key]->measure));
timer_add (&generic_speaker_timer);
return;
}
#endif
struct driver_ops driver_generic_speaker_ops =
{
generic_speaker_init,
generic_speaker_start,
generic_speaker_fini
};
static struct bell_ops generic_speaker_ops =
{
generic_speaker_beep,
#if QUAERENDO_INVENIETIS
generic_speaker_play_melody
#else
NULL
#endif
};