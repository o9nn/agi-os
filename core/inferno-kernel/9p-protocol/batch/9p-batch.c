/**
 * 9P Protocol Batch Extensions Implementation
 * 
 * This implements the batch operation protocol for high-performance
 * message coalescing in AGI-OS.
 */

#include "9p-batch.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Helper: Allocate and zero memory */
static void* zalloc(size_t size) {
    void* ptr = malloc(size);
    if (ptr) {
        memset(ptr, 0, size);
    }
    return ptr;
}

/* Helper: Write uint8 to buffer */
static int write_u8(uint8_t** buf, uint8_t val) {
    **buf = val;
    (*buf)++;
    return 1;
}

/* Helper: Write uint16 to buffer */
static int write_u16(uint8_t** buf, uint16_t val) {
    (*buf)[0] = val & 0xFF;
    (*buf)[1] = (val >> 8) & 0xFF;
    *buf += 2;
    return 2;
}

/* Helper: Write uint32 to buffer */
static int write_u32(uint8_t** buf, uint32_t val) {
    (*buf)[0] = val & 0xFF;
    (*buf)[1] = (val >> 8) & 0xFF;
    (*buf)[2] = (val >> 16) & 0xFF;
    (*buf)[3] = (val >> 24) & 0xFF;
    *buf += 4;
    return 4;
}

/* Helper: Write string to buffer */
static int write_string(uint8_t** buf, const char* str) {
    uint16_t len = str ? strlen(str) : 0;
    write_u16(buf, len);
    if (len > 0) {
        memcpy(*buf, str, len);
        *buf += len;
    }
    return 2 + len;
}

/* Helper: Read uint8 from buffer */
static uint8_t read_u8(uint8_t** buf) {
    uint8_t val = **buf;
    (*buf)++;
    return val;
}

/* Helper: Read uint16 from buffer */
static uint16_t read_u16(uint8_t** buf) {
    uint16_t val = (*buf)[0] | ((*buf)[1] << 8);
    *buf += 2;
    return val;
}

/* Helper: Read uint32 from buffer */
static uint32_t read_u32(uint8_t** buf) {
    uint32_t val = (*buf)[0] | ((*buf)[1] << 8) | 
                   ((*buf)[2] << 16) | ((*buf)[3] << 24);
    *buf += 4;
    return val;
}

/* Helper: Read string from buffer */
static char* read_string(uint8_t** buf) {
    uint16_t len = read_u16(buf);
    if (len == 0) return NULL;
    
    char* str = malloc(len + 1);
    if (!str) return NULL;
    
    memcpy(str, *buf, len);
    str[len] = '\0';
    *buf += len;
    return str;
}

/* Create a new batch request */
Tbatch* tbatch_create(uint16_t tag, uint32_t batch_id) {
    Tbatch* batch = zalloc(sizeof(Tbatch));
    if (!batch) return NULL;
    
    batch->type = TBATCH;
    batch->tag = tag;
    batch->batch_id = batch_id;
    batch->op_count = 0;
    batch->ops = NULL;
    
    return batch;
}

/* Add operation to batch */
int tbatch_add_op(Tbatch* batch, BatchOperation* op) {
    if (!batch || !op) return -1;
    if (batch->op_count >= MAX_BATCH_OPS) return -1;
    
    /* Reallocate operations array */
    BatchOperation* new_ops = realloc(batch->ops, 
        sizeof(BatchOperation) * (batch->op_count + 1));
    if (!new_ops) return -1;
    
    batch->ops = new_ops;
    batch->ops[batch->op_count] = *op;
    batch->op_count++;
    
    return 0;
}

/* Serialize batch request to wire format */
int tbatch_serialize(Tbatch* batch, uint8_t* buf, uint32_t bufsize) {
    if (!batch || !buf) return -1;
    
    uint8_t* start = buf;
    uint8_t* end = buf + bufsize;
    
    /* Write header */
    if (buf + 11 > end) return -1;
    write_u8(&buf, batch->type);
    write_u16(&buf, batch->tag);
    write_u32(&buf, batch->batch_id);
    write_u32(&buf, batch->op_count);
    
    /* Write operations */
    for (uint32_t i = 0; i < batch->op_count; i++) {
        BatchOperation* op = &batch->ops[i];
        
        if (buf + 14 > end) return -1;
        write_u32(&buf, op->op_id);
        write_u8(&buf, op->op_type);
        write_u32(&buf, op->fid);
        write_u32(&buf, op->count);
        
        /* Write operation-specific data */
        if (op->op_type == BATCH_OP_WALK) {
            if (buf + 2 > end) return -1;
            write_u16(&buf, op->nwname);
            
            for (uint16_t j = 0; j < op->nwname; j++) {
                write_string(&buf, op->wname[j]);
            }
        } else if (op->op_type == BATCH_OP_READ || op->op_type == BATCH_OP_WRITE) {
            if (buf + 8 > end) return -1;
            write_u32(&buf, (uint32_t)(op->offset & 0xFFFFFFFF));
            write_u32(&buf, (uint32_t)(op->offset >> 32));
        }
        
        /* Write data */
        if (op->count > 0 && op->data) {
            if (buf + op->count > end) return -1;
            memcpy(buf, op->data, op->count);
            buf += op->count;
        }
    }
    
    return buf - start;
}

/* Deserialize batch request from wire format */
Tbatch* tbatch_deserialize(uint8_t* buf, uint32_t bufsize) {
    if (!buf || bufsize < 11) return NULL;
    
    uint8_t* start = buf;
    
    /* Read header */
    uint8_t type = read_u8(&buf);
    if (type != TBATCH) return NULL;
    
    uint16_t tag = read_u16(&buf);
    uint32_t batch_id = read_u32(&buf);
    uint32_t op_count = read_u32(&buf);
    
    if (op_count > MAX_BATCH_OPS) return NULL;
    
    Tbatch* batch = tbatch_create(tag, batch_id);
    if (!batch) return NULL;
    
    /* Read operations */
    for (uint32_t i = 0; i < op_count; i++) {
        if ((buf - start) + 14 > bufsize) {
            tbatch_free(batch);
            return NULL;
        }
        
        BatchOperation op = {0};
        op.op_id = read_u32(&buf);
        op.op_type = read_u8(&buf);
        op.fid = read_u32(&buf);
        op.count = read_u32(&buf);
        
        /* Read operation-specific data */
        if (op.op_type == BATCH_OP_WALK) {
            op.nwname = read_u16(&buf);
            op.wname = malloc(sizeof(char*) * op.nwname);
            if (!op.wname) {
                tbatch_free(batch);
                return NULL;
            }
            
            for (uint16_t j = 0; j < op.nwname; j++) {
                op.wname[j] = read_string(&buf);
            }
        } else if (op.op_type == BATCH_OP_READ || op.op_type == BATCH_OP_WRITE) {
            uint32_t offset_lo = read_u32(&buf);
            uint32_t offset_hi = read_u32(&buf);
            op.offset = ((uint64_t)offset_hi << 32) | offset_lo;
        }
        
        /* Read data */
        if (op.count > 0) {
            if ((buf - start) + op.count > bufsize) {
                tbatch_free(batch);
                return NULL;
            }
            
            op.data = malloc(op.count);
            if (!op.data) {
                tbatch_free(batch);
                return NULL;
            }
            memcpy(op.data, buf, op.count);
            buf += op.count;
        }
        
        tbatch_add_op(batch, &op);
    }
    
    return batch;
}

/* Free batch request */
void tbatch_free(Tbatch* batch) {
    if (!batch) return;
    
    if (batch->ops) {
        for (uint32_t i = 0; i < batch->op_count; i++) {
            if (batch->ops[i].data) {
                free(batch->ops[i].data);
            }
            if (batch->ops[i].wname) {
                for (uint16_t j = 0; j < batch->ops[i].nwname; j++) {
                    free(batch->ops[i].wname[j]);
                }
                free(batch->ops[i].wname);
            }
        }
        free(batch->ops);
    }
    
    free(batch);
}

/* Create a new batch response */
Rbatch* rbatch_create(uint16_t tag, uint32_t batch_id) {
    Rbatch* batch = zalloc(sizeof(Rbatch));
    if (!batch) return NULL;
    
    batch->type = RBATCH;
    batch->tag = tag;
    batch->batch_id = batch_id;
    batch->result_count = 0;
    batch->results = NULL;
    
    return batch;
}

/* Add result to batch response */
int rbatch_add_result(Rbatch* batch, BatchResult* result) {
    if (!batch || !result) return -1;
    
    /* Reallocate results array */
    BatchResult* new_results = realloc(batch->results, 
        sizeof(BatchResult) * (batch->result_count + 1));
    if (!new_results) return -1;
    
    batch->results = new_results;
    batch->results[batch->result_count] = *result;
    batch->result_count++;
    
    return 0;
}

/* Serialize batch response to wire format */
int rbatch_serialize(Rbatch* batch, uint8_t* buf, uint32_t bufsize) {
    if (!batch || !buf) return -1;
    
    uint8_t* start = buf;
    uint8_t* end = buf + bufsize;
    
    /* Write header */
    if (buf + 11 > end) return -1;
    write_u8(&buf, batch->type);
    write_u16(&buf, batch->tag);
    write_u32(&buf, batch->batch_id);
    write_u32(&buf, batch->result_count);
    
    /* Write results */
    for (uint32_t i = 0; i < batch->result_count; i++) {
        BatchResult* result = &batch->results[i];
        
        if (buf + 9 > end) return -1;
        write_u32(&buf, result->op_id);
        write_u8(&buf, result->status);
        write_u32(&buf, result->count);
        
        /* Write data */
        if (result->count > 0 && result->data) {
            if (buf + result->count > end) return -1;
            memcpy(buf, result->data, result->count);
            buf += result->count;
        }
        
        /* Write error message if present */
        write_string(&buf, result->error);
    }
    
    return buf - start;
}

/* Deserialize batch response from wire format */
Rbatch* rbatch_deserialize(uint8_t* buf, uint32_t bufsize) {
    if (!buf || bufsize < 11) return NULL;
    
    uint8_t* start = buf;
    
    /* Read header */
    uint8_t type = read_u8(&buf);
    if (type != RBATCH) return NULL;
    
    uint16_t tag = read_u16(&buf);
    uint32_t batch_id = read_u32(&buf);
    uint32_t result_count = read_u32(&buf);
    
    Rbatch* batch = rbatch_create(tag, batch_id);
    if (!batch) return NULL;
    
    /* Read results */
    for (uint32_t i = 0; i < result_count; i++) {
        if ((buf - start) + 9 > bufsize) {
            rbatch_free(batch);
            return NULL;
        }
        
        BatchResult result = {0};
        result.op_id = read_u32(&buf);
        result.status = read_u8(&buf);
        result.count = read_u32(&buf);
        
        /* Read data */
        if (result.count > 0) {
            if ((buf - start) + result.count > bufsize) {
                rbatch_free(batch);
                return NULL;
            }
            
            result.data = malloc(result.count);
            if (!result.data) {
                rbatch_free(batch);
                return NULL;
            }
            memcpy(result.data, buf, result.count);
            buf += result.count;
        }
        
        /* Read error message */
        result.error = read_string(&buf);
        
        rbatch_add_result(batch, &result);
    }
    
    return batch;
}

/* Free batch response */
void rbatch_free(Rbatch* batch) {
    if (!batch) return;
    
    if (batch->results) {
        for (uint32_t i = 0; i < batch->result_count; i++) {
            if (batch->results[i].data) {
                free(batch->results[i].data);
            }
            if (batch->results[i].error) {
                free(batch->results[i].error);
            }
        }
        free(batch->results);
    }
    
    free(batch);
}

/* Create query condition */
QueryCondition* query_condition_create(QueryPredicate pred, const char* field, const char* value) {
    QueryCondition* cond = zalloc(sizeof(QueryCondition));
    if (!cond) return NULL;
    
    cond->predicate = pred;
    cond->field = field ? strdup(field) : NULL;
    cond->value = value ? strdup(value) : NULL;
    cond->left = NULL;
    cond->right = NULL;
    
    return cond;
}

/* Create AND condition */
QueryCondition* query_condition_and(QueryCondition* left, QueryCondition* right) {
    QueryCondition* cond = zalloc(sizeof(QueryCondition));
    if (!cond) return NULL;
    
    cond->predicate = QUERY_PRED_AND;
    cond->left = left;
    cond->right = right;
    
    return cond;
}

/* Create OR condition */
QueryCondition* query_condition_or(QueryCondition* left, QueryCondition* right) {
    QueryCondition* cond = zalloc(sizeof(QueryCondition));
    if (!cond) return NULL;
    
    cond->predicate = QUERY_PRED_OR;
    cond->left = left;
    cond->right = right;
    
    return cond;
}

/* Create NOT condition */
QueryCondition* query_condition_not(QueryCondition* condition) {
    QueryCondition* cond = zalloc(sizeof(QueryCondition));
    if (!cond) return NULL;
    
    cond->predicate = QUERY_PRED_NOT;
    cond->left = condition;
    
    return cond;
}

/* Free query condition */
void query_condition_free(QueryCondition* condition) {
    if (!condition) return;
    
    if (condition->field) free(condition->field);
    if (condition->value) free(condition->value);
    if (condition->left) query_condition_free(condition->left);
    if (condition->right) query_condition_free(condition->right);
    
    free(condition);
}

/* Stub implementations for query functions (to be completed) */
Tquery* tquery_create(uint16_t tag, uint32_t query_id, const char* path) {
    Tquery* query = zalloc(sizeof(Tquery));
    if (!query) return NULL;
    
    query->type = TQUERY;
    query->tag = tag;
    query->query_id = query_id;
    query->path = path ? strdup(path) : NULL;
    query->condition = NULL;
    query->limit = 0;
    query->offset = 0;
    
    return query;
}

int tquery_set_condition(Tquery* query, QueryCondition* condition) {
    if (!query) return -1;
    query->condition = condition;
    return 0;
}

void tquery_free(Tquery* query) {
    if (!query) return;
    if (query->path) free(query->path);
    if (query->condition) query_condition_free(query->condition);
    free(query);
}

Rquery* rquery_create(uint16_t tag, uint32_t query_id) {
    Rquery* query = zalloc(sizeof(Rquery));
    if (!query) return NULL;
    
    query->type = RQUERY;
    query->tag = tag;
    query->query_id = query_id;
    query->total_count = 0;
    query->result_count = 0;
    query->paths = NULL;
    query->data = NULL;
    query->sizes = NULL;
    
    return query;
}

int rquery_add_result(Rquery* query, const char* path, uint8_t* data, uint32_t size) {
    if (!query) return -1;
    
    /* Reallocate arrays */
    char** new_paths = realloc(query->paths, sizeof(char*) * (query->result_count + 1));
    uint8_t** new_data = realloc(query->data, sizeof(uint8_t*) * (query->result_count + 1));
    uint32_t* new_sizes = realloc(query->sizes, sizeof(uint32_t) * (query->result_count + 1));
    
    if (!new_paths || !new_data || !new_sizes) return -1;
    
    query->paths = new_paths;
    query->data = new_data;
    query->sizes = new_sizes;
    
    query->paths[query->result_count] = path ? strdup(path) : NULL;
    query->data[query->result_count] = data;
    query->sizes[query->result_count] = size;
    query->result_count++;
    
    return 0;
}

void rquery_free(Rquery* query) {
    if (!query) return;
    
    if (query->paths) {
        for (uint32_t i = 0; i < query->result_count; i++) {
            if (query->paths[i]) free(query->paths[i]);
        }
        free(query->paths);
    }
    
    if (query->data) {
        for (uint32_t i = 0; i < query->result_count; i++) {
            if (query->data[i]) free(query->data[i]);
        }
        free(query->data);
    }
    
    if (query->sizes) free(query->sizes);
    
    free(query);
}

/* Stub implementations for serialization (to be completed) */
int tquery_serialize(Tquery* query, uint8_t* buf, uint32_t bufsize) {
    /* TODO: Implement query serialization */
    return -1;
}

Tquery* tquery_deserialize(uint8_t* buf, uint32_t bufsize) {
    /* TODO: Implement query deserialization */
    return NULL;
}

int rquery_serialize(Rquery* query, uint8_t* buf, uint32_t bufsize) {
    /* TODO: Implement query response serialization */
    return -1;
}

Rquery* rquery_deserialize(uint8_t* buf, uint32_t bufsize) {
    /* TODO: Implement query response deserialization */
    return NULL;
}
