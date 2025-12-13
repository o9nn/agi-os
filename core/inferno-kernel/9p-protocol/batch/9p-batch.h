/**
 * 9P Protocol Batch Extensions for AGI-OS
 * 
 * This header defines extended 9P protocol messages for batch operations,
 * enabling message coalescing for high-performance cognitive operations.
 * 
 * Key insight: Message coalescing is critical for performance.
 * Batch operations can achieve >10x speedup over individual operations.
 */

#ifndef _9P_BATCH_H_
#define _9P_BATCH_H_

#include <stdint.h>
#include <stdbool.h>

/* Extended 9P message types for batch operations */
#define TBATCH      200  /* Batch request */
#define RBATCH      201  /* Batch response */
#define TQUERY      202  /* Batch query request */
#define RQUERY      203  /* Batch query response */

/* Maximum batch size */
#define MAX_BATCH_OPS   1000

/* Batch operation types */
typedef enum {
    BATCH_OP_READ = 0,
    BATCH_OP_WRITE,
    BATCH_OP_WALK,
    BATCH_OP_STAT,
    BATCH_OP_CREATE,
    BATCH_OP_REMOVE,
    BATCH_OP_QUERY,     /* AtomSpace query */
    BATCH_OP_INFER,     /* PLN inference */
    BATCH_OP_ATTEND,    /* ECAN attention update */
} BatchOpType;

/* Batch operation status */
typedef enum {
    BATCH_STATUS_OK = 0,
    BATCH_STATUS_ERROR,
    BATCH_STATUS_PARTIAL,
    BATCH_STATUS_PENDING,
} BatchStatus;

/* Single operation in a batch */
typedef struct {
    uint32_t op_id;         /* Operation identifier */
    uint8_t op_type;        /* BatchOpType */
    uint32_t fid;           /* File identifier */
    uint64_t offset;        /* For read/write operations */
    uint32_t count;         /* Data size */
    uint16_t nwname;        /* Number of path elements (for walk) */
    char** wname;           /* Path elements (for walk) */
    uint8_t* data;          /* Operation data */
} BatchOperation;

/* Result of a single operation */
typedef struct {
    uint32_t op_id;         /* Matches request op_id */
    uint8_t status;         /* BatchStatus */
    uint32_t count;         /* Result data size */
    uint8_t* data;          /* Result data */
    char* error;            /* Error message (if status == ERROR) */
} BatchResult;

/* Tbatch: Batch request message */
typedef struct {
    uint8_t type;           /* TBATCH */
    uint16_t tag;           /* Message tag */
    uint32_t batch_id;      /* Batch identifier */
    uint32_t op_count;      /* Number of operations */
    BatchOperation* ops;    /* Array of operations */
} Tbatch;

/* Rbatch: Batch response message */
typedef struct {
    uint8_t type;           /* RBATCH */
    uint16_t tag;           /* Message tag */
    uint32_t batch_id;      /* Matches request batch_id */
    uint32_t result_count;  /* Number of results */
    BatchResult* results;   /* Array of results */
} Rbatch;

/* Query predicate for batch queries */
typedef enum {
    QUERY_PRED_EQ = 0,      /* Equal */
    QUERY_PRED_NE,          /* Not equal */
    QUERY_PRED_GT,          /* Greater than */
    QUERY_PRED_LT,          /* Less than */
    QUERY_PRED_GE,          /* Greater or equal */
    QUERY_PRED_LE,          /* Less or equal */
    QUERY_PRED_MATCH,       /* Pattern match */
    QUERY_PRED_AND,         /* Logical AND */
    QUERY_PRED_OR,          /* Logical OR */
    QUERY_PRED_NOT,         /* Logical NOT */
} QueryPredicate;

/* Query condition */
typedef struct QueryCondition {
    uint8_t predicate;          /* QueryPredicate */
    char* field;                /* Field name */
    char* value;                /* Comparison value */
    struct QueryCondition* left;    /* For AND/OR/NOT */
    struct QueryCondition* right;   /* For AND/OR */
} QueryCondition;

/* Tquery: Batch query request */
typedef struct {
    uint8_t type;           /* TQUERY */
    uint16_t tag;           /* Message tag */
    uint32_t query_id;      /* Query identifier */
    char* path;             /* Base path for query */
    QueryCondition* condition;  /* Query condition tree */
    uint32_t limit;         /* Maximum results (0 = unlimited) */
    uint32_t offset;        /* Result offset (for pagination) */
} Tquery;

/* Rquery: Batch query response */
typedef struct {
    uint8_t type;           /* RQUERY */
    uint16_t tag;           /* Message tag */
    uint32_t query_id;      /* Matches request query_id */
    uint32_t total_count;   /* Total matching results */
    uint32_t result_count;  /* Results in this response */
    char** paths;           /* Array of matching paths */
    uint8_t** data;         /* Array of result data */
    uint32_t* sizes;        /* Array of data sizes */
} Rquery;

/* Function prototypes */

/**
 * Create a new batch request
 */
Tbatch* tbatch_create(uint16_t tag, uint32_t batch_id);

/**
 * Add operation to batch
 */
int tbatch_add_op(Tbatch* batch, BatchOperation* op);

/**
 * Serialize batch request to wire format
 */
int tbatch_serialize(Tbatch* batch, uint8_t* buf, uint32_t bufsize);

/**
 * Deserialize batch request from wire format
 */
Tbatch* tbatch_deserialize(uint8_t* buf, uint32_t bufsize);

/**
 * Free batch request
 */
void tbatch_free(Tbatch* batch);

/**
 * Create a new batch response
 */
Rbatch* rbatch_create(uint16_t tag, uint32_t batch_id);

/**
 * Add result to batch response
 */
int rbatch_add_result(Rbatch* batch, BatchResult* result);

/**
 * Serialize batch response to wire format
 */
int rbatch_serialize(Rbatch* batch, uint8_t* buf, uint32_t bufsize);

/**
 * Deserialize batch response from wire format
 */
Rbatch* rbatch_deserialize(uint8_t* buf, uint32_t bufsize);

/**
 * Free batch response
 */
void rbatch_free(Rbatch* batch);

/**
 * Create a new query request
 */
Tquery* tquery_create(uint16_t tag, uint32_t query_id, const char* path);

/**
 * Set query condition
 */
int tquery_set_condition(Tquery* query, QueryCondition* condition);

/**
 * Serialize query request to wire format
 */
int tquery_serialize(Tquery* query, uint8_t* buf, uint32_t bufsize);

/**
 * Deserialize query request from wire format
 */
Tquery* tquery_deserialize(uint8_t* buf, uint32_t bufsize);

/**
 * Free query request
 */
void tquery_free(Tquery* query);

/**
 * Create a new query response
 */
Rquery* rquery_create(uint16_t tag, uint32_t query_id);

/**
 * Add result to query response
 */
int rquery_add_result(Rquery* query, const char* path, uint8_t* data, uint32_t size);

/**
 * Serialize query response to wire format
 */
int rquery_serialize(Rquery* query, uint8_t* buf, uint32_t bufsize);

/**
 * Deserialize query response from wire format
 */
Rquery* rquery_deserialize(uint8_t* buf, uint32_t bufsize);

/**
 * Free query response
 */
void rquery_free(Rquery* query);

/**
 * Create query condition
 */
QueryCondition* query_condition_create(QueryPredicate pred, const char* field, const char* value);

/**
 * Create AND condition
 */
QueryCondition* query_condition_and(QueryCondition* left, QueryCondition* right);

/**
 * Create OR condition
 */
QueryCondition* query_condition_or(QueryCondition* left, QueryCondition* right);

/**
 * Create NOT condition
 */
QueryCondition* query_condition_not(QueryCondition* condition);

/**
 * Free query condition
 */
void query_condition_free(QueryCondition* condition);

#endif /* _9P_BATCH_H_ */
