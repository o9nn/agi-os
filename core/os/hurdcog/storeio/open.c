#include <hurd.h>
#include <stdio.h>
#include "open.h"
#include "dev.h"
error_t
open_create (struct dev *dev, struct open **open)
{
*open = malloc (sizeof (struct open));
if (*open == NULL)
return ENOMEM;
(*open)->dev = dev;
(*open)->offs = 0;
pthread_mutex_init (&(*open)->lock, NULL);
return 0;
}
void
open_free (struct open *open)
{
free (open);
}
error_t
open_write (struct open *open, off_t offs, const void *buf, size_t len,
vm_size_t *amount)
{
error_t err;
if (offs < 0)
{
pthread_mutex_lock (&open->lock);
err = dev_write (open->dev, open->offs, buf, len, amount);
if (! err)
open->offs += *amount;
pthread_mutex_unlock (&open->lock);
}
else
err = dev_write (open->dev, offs, buf, len, amount);
return err;
}
error_t
open_read (struct open *open, off_t offs, vm_size_t amount,
void **buf, vm_size_t *len)
{
error_t err;
if (offs < 0)
{
pthread_mutex_lock (&open->lock);
err = dev_read (open->dev, open->offs, amount, buf, len);
if (! err)
open->offs += *len;
pthread_mutex_unlock (&open->lock);
}
else
err = dev_read (open->dev, offs, amount, buf, len);
return err;
}
error_t
open_seek (struct open *open, off_t offs, int whence, off_t *new_offs)
{
error_t err = 0;
pthread_mutex_lock (&open->lock);
switch (whence)
{
case SEEK_CUR:
offs += open->offs;
goto check;
case SEEK_END:
offs += open->dev->store->size;
case SEEK_SET:
check:
if (offs >= 0)
{
*new_offs = open->offs = offs;
break;
}
default:
err = EINVAL;
}
pthread_mutex_unlock (&open->lock);
return err;
}