#ifndef UNIFIED_COG_H
#define UNIFIED_COG_H
#include <stdint.h>
#include <stdbool.h>
#ifdef __cplusplus
extern "C" {
#endif
typedef uint64_t CogHandle;
typedef struct {
double strength;
double confidence;
} CogTruthValue;
typedef struct {
int16_t sti;
int16_t lti;
int16_t vlti;
} CogAttentionValue;
typedef enum {
COG_NODE = 1,
COG_LINK = 2,
COG_CONCEPT_NODE = 3,
COG_PREDICATE_NODE = 4,
COG_INHERITANCE_LINK = 5,
COG_SIMILARITY_LINK = 6,
COG_IMPLICATION_LINK = 7,
COG_EVALUATION_LINK = 8,
COG_EXECUTION_LINK = 9,
} CogAtomType;
typedef struct {
CogHandle handle;
CogAtomType type;
char* name;
CogHandle* outgoing;
size_t outgoing_count;
CogTruthValue tv;
CogAttentionValue av;
} CogAtom;
CogHandle cog_atom_create(
CogAtomType type,
const char* name,
const CogHandle* outgoing,
size_t outgoing_count,
CogTruthValue tv
);
bool cog_atom_read(CogHandle handle, CogAtom* atom);
bool cog_atom_update_tv(CogHandle handle, CogTruthValue tv);
bool cog_atom_update_av(CogHandle handle, CogAttentionValue av);
bool cog_atom_delete(CogHandle handle);
size_t cog_query_pattern(
const char* pattern,
CogHandle* results,
size_t max_results
);
size_t cog_query_distributed(
const char* query,
CogHandle* results,
size_t max_results
);
size_t cog_reason_pln(
CogHandle target,
int max_steps,
CogHandle* results,
size_t max_results
);
size_t cog_reason_forward(
const CogHandle* premises,
size_t premise_count,
CogHandle* results,
size_t max_results
);
size_t cog_reason_backward(
CogHandle goal,
CogHandle* results,
size_t max_results
);
bool cog_attention_allocate(CogHandle handle, int16_t sti_delta);
bool cog_attention_spread(CogHandle handle, int16_t amount);
size_t cog_attention_get_top(CogHandle* results, size_t max_results);
size_t cog_learn_mine(
double min_support,
CogHandle* results,
size_t max_results
);
bool cog_distributed_sync(void);
size_t cog_distributed_status(char* status_buffer, size_t buffer_size);
bool cog_init(const char* config_path);
void cog_shutdown(void);
bool cog_connect_9p(const char* mount_point);
#ifdef __cplusplus
}
#endif
#endif