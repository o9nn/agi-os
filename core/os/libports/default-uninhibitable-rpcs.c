#include "ports.h"
static struct ports_msg_id_range
interrupt_operation_ids = { 33000, 33001, 0 };
struct ports_msg_id_range *
ports_default_uninhibitable_rpcs = &interrupt_operation_ids;