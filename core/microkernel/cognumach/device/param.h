#ifndef	_DEVICE_PARAM_H_
#define	_DEVICE_PARAM_H_
#define	DEV_BSIZE	512
#define	btodb(byte_offset)	((byte_offset) >> 9)
#define	dbtob(block_number)	((block_number) << 9)
#endif