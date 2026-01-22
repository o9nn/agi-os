#ifndef	_MACH_SYS_REBOOT_H_
#define	_MACH_SYS_REBOOT_H_
#define	RB_AUTOBOOT	0
#define	RB_ASKNAME	0x01
#define	RB_SINGLE	0x02
#define	RB_KDB		0x04
#define	RB_HALT		0x08
#define	RB_INITNAME	0x10
#define	RB_DFLTROOT	0x20
#define	RB_NOBOOTRC	0x20
#define RB_ALTBOOT	0x40
#define	RB_UNIPROC	0x80
#define	RB_SHIFT	8
#define	RB_DEBUGGER	0x1000
#define BSD_RB_NOSYNC   0x04
#define BSD_RB_KDB      0x40
#define BSD_RB_RDONLY   0x80
#define BSD_RB_DUMP     0x100
#define BSD_RB_MINIROOT 0x200
#define BSD_RB_CONFIG   0x400
#define	B_ADAPTORSHIFT		24
#define	B_ADAPTORMASK		0x0f
#define	B_ADAPTOR(val)		(((val) >> B_ADAPTORSHIFT) & B_ADAPTORMASK)
#define B_CONTROLLERSHIFT	20
#define B_CONTROLLERMASK	0xf
#define	B_CONTROLLER(val)	(((val)>>B_CONTROLLERSHIFT) & B_CONTROLLERMASK)
#define B_UNITSHIFT		16
#define B_UNITMASK		0xf
#define	B_UNIT(val)		(((val) >> B_UNITSHIFT) & B_UNITMASK)
#define B_PARTITIONSHIFT	8
#define B_PARTITIONMASK		0xff
#define	B_PARTITION(val)	(((val) >> B_PARTITIONSHIFT) & B_PARTITIONMASK)
#define	B_TYPESHIFT		0
#define	B_TYPEMASK		0xff
#define	B_TYPE(val)		(((val) >> B_TYPESHIFT) & B_TYPEMASK)
#define	B_MAGICMASK	((u_int)0xf0000000U)
#define	B_DEVMAGIC	((u_int)0xa0000000U)
#define MAKEBOOTDEV(type, adaptor, controller, unit, partition) \
(((type) << B_TYPESHIFT) | ((adaptor) << B_ADAPTORSHIFT) | \
((controller) << B_CONTROLLERSHIFT) | ((unit) << B_UNITSHIFT) | \
((partition) << B_PARTITIONSHIFT) | B_DEVMAGIC)
#endif