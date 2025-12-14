#ifndef TENSOR_OPS_H
#define TENSOR_OPS_H

#include "lib9.h"

// Tensor operation structures
typedef struct TensorOp TensorOp;
typedef struct DistributedTensor DistributedTensor;

struct TensorOp {
    int opcode;
    int dtype;
    int dims[4];
    void *data;
    TensorOp *next;
};

struct DistributedTensor {
    int id;
    int node_id;
    int dims[4];
    void *data;
    DistributedTensor *next;
};

// Function declarations
int execute_tensor_op(TensorOp *op, void *ctx);
DistributedTensor* create_distributed_tensor(int id, int node_id, int *dims);
void free_distributed_tensor(DistributedTensor *dt);

// Tensor operation codes
#define TOP_ADD 1
#define TOP_MUL 2
#define TOP_MATMUL 3
#define TOP_CONV2D 4
#define TOP_MAXPOOL 5
#define TOP_SOFTMAX 6
#define TOP_RELU 7
#define TOP_SIGMOID 8
#define TOP_LAYERNORM 9
#define TOP_EMBEDDING 10
#define TOP_ATTENTION 11
#define TOP_GRU 12
#define TOP_LSTM 13
#define TOP_DISTRIBUTED_SYNC 14
#define TOP_GRAMMAR_PARSE 15
#define TOP_COGNITIVE_UPDATE 16

#endif // TENSOR_OPS_H