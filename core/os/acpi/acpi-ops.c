#include <acpi_S.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/io.h>
#include <idvec.h>
#include <acpi/acpi_init.h>
#include "acpifs.h"
#define WRONG_IRQ 9
static error_t
check_permissions (struct protid *master, int flags)
{
struct node *node;
struct acpifs_dirent *e;
node = master->po->np;
e = node->nn->ln;
return entry_check_perms (master->user, e, flags);
}
kern_return_t
S_acpi_sleep (struct protid *master,
int sleep_state)
{
error_t err;
if (!master)
return EOPNOTSUPP;
if (!master->user)
return EOPNOTSUPP;
if (!idvec_contains (master->user->uids, 0))
return EOPNOTSUPP;
acpi_enter_sleep(sleep_state);
return err;
}
kern_return_t
S_acpi_get_pci_irq (struct protid *master,
int bus,
int dev,
int func,
int *irq)
{
error_t err;
int ret;
if (!master)
return EOPNOTSUPP;
err = check_permissions (master, O_READ);
if (err)
return err;
ret = acpi_get_irq_number(bus, dev, func);
if (ret == WRONG_IRQ)
return EOPNOTSUPP;
if (ret < 0)
return EIO;
*irq = ret;
return 0;
}