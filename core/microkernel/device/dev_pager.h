#ifndef _DEVICE_DEV_PAGER_H_
#define _DEVICE_DEV_PAGER_H_
phys_addr_t device_map_page(void *dsp, vm_offset_t offset);
boolean_t device_pager_data_request_done(io_req_t ior);
#endif