#ifndef TENSOR_DIS_H
#define TENSOR_DIS_H

#include "lib9.h"

// Dis VM tensor operation structures
typedef struct DisTensorOp DisTensorOp;
typedef struct DisTensorContext DisTensorContext;
typedef struct DisAgentContext DisAgentContext;

struct DisTensorOp {
    int opcode;
    int dtype;
    int dims[4];
    void *data;
    int data_size;
    DisTensorOp *next;
};

struct DisTensorContext {
    int node_id;
    char *namespace_name;
    void *tensor_cache;
    DisTensorContext *next;
};

struct DisAgentContext {
    int agent_id;
    char *agent_name;
    void *cognitive_state;
    DisAgentContext *next;
};

// Function declarations
int execute_dis_tensor_op(DisTensorOp *op, DisTensorContext *ctx);
DisTensorContext* create_dis_tensor_context(int node_id, char *namespace_name);
DisAgentContext* create_dis_agent_context(int agent_id, char *agent_name);
DisTensorOp* create_dis_tensor_op(int opcode, int dtype, int *dims, void *data, int data_size);
void free_dis_tensor_context(DisTensorContext *ctx);
void free_dis_agent_context(DisAgentContext *ctx);
void free_dis_tensor_op(DisTensorOp *op);

// Dis VM tensor operation codes
#define DTENSOR_ADD 0x100
#define DTENSOR_MUL 0x101
#define DTENSOR_MATMUL 0x102
#define DTENSOR_CONV2D 0x103
#define DTENSOR_MAXPOOL 0x104
#define DTENSOR_SOFTMAX 0x105
#define DTENSOR_RELU 0x106
#define DTENSOR_SIGMOID 0x107
#define DTENSOR_LAYERNORM 0x108
#define DTENSOR_EMBEDDING 0x109
#define DTENSOR_ATTENTION 0x10A
#define DTENSOR_GRU 0x10B
#define DTENSOR_LSTM 0x10C
#define DTENSOR_DISTRIBUTED_SYNC 0x10D
#define DTENSOR_GRAMMAR_PARSE 0x10E
#define DTENSOR_COGNITIVE_UPDATE 0x10F
#define DTENSOR_AGENT_COMMUNICATE 0x110
#define DTENSOR_NAMESPACE_SYNC 0x111

#endif // TENSOR_DIS_H