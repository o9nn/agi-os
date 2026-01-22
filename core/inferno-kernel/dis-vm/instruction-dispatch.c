#include <stdio.h>
#include <stdint.h>
int dis_dispatch_instruction(uint8_t opcode, const uint8_t *operands) {
printf("Dispatching instruction: 0x%02x\n", opcode);
return 0;
}