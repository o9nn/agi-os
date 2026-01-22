#ifndef _9P_BATCH_H_
#define _9P_BATCH_H_
#include <stdint.h>
#include <stdbool.h>
#define TBATCH      200
#define RBATCH      201
#define TQUERY      202
#define RQUERY      203
#define MAX_BATCH_OPS   1000
typedef enum {
BATCH_OP_READ = 0,
BATCH_OP_WRITE,
BATCH_OP_WALK,
BATCH_OP_STAT,
BATCH_OP_CREATE,
BATCH_OP_REMOVE,
BATCH_OP_QUERY,
BATCH_OP_INFER,
BATCH_OP_ATTEND,
} BatchOpType;
typedef enum {
BATCH_STATUS_OK = 0,
BATCH_STATUS_ERROR,
BATCH_STATUS_PARTIAL,
BATCH_STATUS_PENDING,
} BatchStatus;
typedef struct {
uint32_t op_id;
uint8_t op_type;
uint32_t fid;
uint64_t offset;
uint32_t count;
uint16_t nwname;
char** wname;
uint8_t* data;
} BatchOperation;
typedef struct {
uint32_t op_id;
uint8_t status;
uint32_t count;
uint8_t* data;
char* error;
} BatchResult;
typedef struct {
uint8_t type;
uint16_t tag;
uint32_t batch_id;
uint32_t op_count;
BatchOperation* ops;
} Tbatch;
typedef struct {
uint8_t type;
uint16_t tag;
uint32_t batch_id;
uint32_t result_count;
BatchResult* results;
} Rbatch;
typedef enum {
QUERY_PRED_EQ = 0,
QUERY_PRED_NE,
QUERY_PRED_GT,
QUERY_PRED_LT,
QUERY_PRED_GE,
QUERY_PRED_LE,
QUERY_PRED_MATCH,
QUERY_PRED_AND,
QUERY_PRED_OR,
QUERY_PRED_NOT,
} QueryPredicate;
typedef struct QueryCondition {
uint8_t predicate;
char* field;
char* value;
struct QueryCondition* left;
struct QueryCondition* right;
} QueryCondition;
typedef struct {
uint8_t type;
uint16_t tag;
uint32_t query_id;
char* path;
QueryCondition* condition;
uint32_t limit;
uint32_t offset;
} Tquery;
typedef struct {
uint8_t type;
uint16_t tag;
uint32_t query_id;
uint32_t total_count;
uint32_t result_count;
char** paths;
uint8_t** data;
uint32_t* sizes;
} Rquery;
Tbatch* tbatch_create(uint16_t tag, uint32_t batch_id);
int tbatch_add_op(Tbatch* batch, BatchOperation* op);
int tbatch_serialize(Tbatch* batch, uint8_t* buf, uint32_t bufsize);
Tbatch* tbatch_deserialize(uint8_t* buf, uint32_t bufsize);
void tbatch_free(Tbatch* batch);
Rbatch* rbatch_create(uint16_t tag, uint32_t batch_id);
int rbatch_add_result(Rbatch* batch, BatchResult* result);
int rbatch_serialize(Rbatch* batch, uint8_t* buf, uint32_t bufsize);
Rbatch* rbatch_deserialize(uint8_t* buf, uint32_t bufsize);
void rbatch_free(Rbatch* batch);
Tquery* tquery_create(uint16_t tag, uint32_t query_id, const char* path);
int tquery_set_condition(Tquery* query, QueryCondition* condition);
int tquery_serialize(Tquery* query, uint8_t* buf, uint32_t bufsize);
Tquery* tquery_deserialize(uint8_t* buf, uint32_t bufsize);
void tquery_free(Tquery* query);
Rquery* rquery_create(uint16_t tag, uint32_t query_id);
int rquery_add_result(Rquery* query, const char* path, uint8_t* data, uint32_t size);
int rquery_serialize(Rquery* query, uint8_t* buf, uint32_t bufsize);
Rquery* rquery_deserialize(uint8_t* buf, uint32_t bufsize);
void rquery_free(Rquery* query);
QueryCondition* query_condition_create(QueryPredicate pred, const char* field, const char* value);
QueryCondition* query_condition_and(QueryCondition* left, QueryCondition* right);
QueryCondition* query_condition_or(QueryCondition* left, QueryCondition* right);
QueryCondition* query_condition_not(QueryCondition* condition);
void query_condition_free(QueryCondition* condition);
#endif