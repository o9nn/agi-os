#include <errno.h>
#include <malloc.h>
#include <sys/mman.h>
#include "cons.h"
error_t
cons_lookup (cons_t cons, int id, int create, vcons_list_t *r_vcons_entry)
{
vcons_list_t previous_vcons_entry = 0;
vcons_list_t vcons_entry;
if (!id && !create)
return EINVAL;
if (id)
{
if (cons->vcons_list && cons->vcons_list->id <= id)
{
previous_vcons_entry = cons->vcons_list;
while (previous_vcons_entry->next
&& previous_vcons_entry->next->id <= id)
previous_vcons_entry = previous_vcons_entry->next;
if (previous_vcons_entry->id == id)
{
*r_vcons_entry = previous_vcons_entry;
return 0;
}
}
else if (!create)
return ESRCH;
}
else
{
id = 1;
if (cons->vcons_list && cons->vcons_list->id == 1)
{
previous_vcons_entry = cons->vcons_list;
while (previous_vcons_entry && previous_vcons_entry->id == id)
{
id++;
previous_vcons_entry = previous_vcons_entry->next;
}
}
}
vcons_entry = calloc (1, sizeof (struct vcons_list));
if (!vcons_entry)
return ENOMEM;
vcons_entry->id = id;
vcons_entry->vcons = NULL;
if (previous_vcons_entry)
{
vcons_entry->prev = previous_vcons_entry;
if (previous_vcons_entry->next)
{
previous_vcons_entry->next->prev = vcons_entry;
vcons_entry->next = previous_vcons_entry->next;
}
else
cons->vcons_last = vcons_entry;
previous_vcons_entry->next = vcons_entry;
}
else
{
if (cons->vcons_list)
{
cons->vcons_list->prev = vcons_entry;
vcons_entry->next = cons->vcons_list;
}
else
cons->vcons_last = vcons_entry;
cons->vcons_list = vcons_entry;
}
cons_vcons_add (cons, vcons_entry);
*r_vcons_entry = vcons_entry;
return 0;
}