#ifndef __XEN_PUBLIC_IO_TPMIF_H__
#define __XEN_PUBLIC_IO_TPMIF_H__
#include "../grant_table.h"
struct tpmif_tx_request {
unsigned long addr;
grant_ref_t ref;
uint16_t unused;
uint16_t size;
};
typedef struct tpmif_tx_request tpmif_tx_request_t;
typedef uint32_t TPMIF_RING_IDX;
#define TPMIF_TX_RING_SIZE 1
struct tpmif_ring {
struct tpmif_tx_request req;
};
typedef struct tpmif_ring tpmif_ring_t;
struct tpmif_tx_interface {
struct tpmif_ring ring[TPMIF_TX_RING_SIZE];
};
typedef struct tpmif_tx_interface tpmif_tx_interface_t;
#endif