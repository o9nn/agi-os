#ifndef	_CHIPS_BUSSES_H_
#define	_CHIPS_BUSSES_H_
#include <mach/boolean.h>
#include <mach/machine/vm_types.h>
struct bus_ctlr {
struct bus_driver  *driver;
char		   *name;
int		    unit;
void		  (*intr)(int);
vm_offset_t	    address;
int		    am;
vm_offset_t	    phys_address;
char		    adaptor;
char		    alive;
char		    flags;
vm_offset_t	    sysdep;
natural_t	    sysdep1;
};
struct bus_device {
struct bus_driver  *driver;
char		   *name;
int		    unit;
void		  (*intr)(int);
vm_offset_t	    address;
int		    am;
vm_offset_t	    phys_address;
char		    adaptor;
char		    alive;
char		    ctlr;
char		    slave;
int		    flags;
struct bus_ctlr    *mi;
struct bus_device  *next;
vm_offset_t	    sysdep;
natural_t	    sysdep1;
};
#define BUS_INTR_B4_PROBE  0x01
#define BUS_INTR_DISABLED  0x02
#define	BUS_CTLR	   0x04
#define BUS_XCLU	   0x80
struct bus_driver {
int	(*probe)(
vm_offset_t	address,
struct bus_ctlr *);
int	(*slave)(
struct bus_device *,
vm_offset_t);
void	(*attach)(
struct bus_device *);
int	(*dgo)(struct bus_device *);
vm_offset_t *addr;
char	*dname;
struct	bus_device **dinfo;
char	*mname;
struct	bus_ctlr **minfo;
int	flags;
};
#ifdef	KERNEL
extern struct bus_ctlr		bus_master_init[];
extern struct bus_device	bus_device_init[];
extern boolean_t configure_bus_master(const char *, vm_offset_t, vm_offset_t,
int, const char * );
extern boolean_t configure_bus_device(const char *, vm_offset_t, vm_offset_t,
int, const char * );
#endif
#endif