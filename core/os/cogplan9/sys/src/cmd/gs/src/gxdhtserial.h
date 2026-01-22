#ifndef gxdhtserial_INCLUDED
# define gxdhtserial_INCLUDED
#ifndef gs_memory_DEFINED
# define gs_memory_DEFINED
typedef struct gs_memory_s gs_memory_t;
#endif
#ifndef gx_device_DEFINED
# define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
#ifndef gx_device_halftone_DEFINED
# define gx_device_halftone_DEFINED
typedef struct gx_device_halftone_s gx_device_halftone;
#endif
#ifndef gs_imager_state_DEFINED
# define gs_imager_state_DEFINED
typedef struct gs_imager_state_s gs_imager_state;
#endif
extern int gx_ht_write( const gx_device_halftone * pdht,
const gx_device * dev,
byte * data,
uint * psize );
extern int gx_ht_read_and_install( gs_imager_state * pis,
const gx_device * dev,
const byte * data,
uint size,
gs_memory_t * mem );
#endif