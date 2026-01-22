#ifndef ATOMSPACE_BRIDGE_H
#define ATOMSPACE_BRIDGE_H
#include "cognitive_kernels.h"
#include "tensor_signatures.h"
#include <stdint.h>
#include <stdbool.h>
struct atomspace_handle {
void* atomspace_ptr;
uint64_t handle_id;
bool is_valid;
};
typedef enum {
ATOM_NODE,
ATOM_LINK,
ATOM_CONCEPT_NODE,
ATOM_PREDICATE_NODE,
ATOM_EVALUATION_LINK,
ATOM_INHERITANCE_LINK,
ATOM_SIMILARITY_LINK,
ATOM_COUNT
} atom_type_t;
typedef struct {
float strength;
float confidence;
} truth_value_t;
typedef struct {
uint64_t atom_id;
atom_type_t type;
char* name;
truth_value_t truth_value;
struct atomspace_handle* atomspace_ref;
} atom_t;
int atomspace_bridge_init(void);
void atomspace_bridge_shutdown(void);
struct atomspace_handle* atomspace_create_handle(void);
void atomspace_destroy_handle(struct atomspace_handle* handle);
int atomspace_integrate_result(cognitive_result_t result);
int atomspace_tensor_to_atoms(
cognitive_tensor_t* tensor,
struct atomspace_handle* handle,
atom_type_t default_type
);
cognitive_tensor_t* atomspace_atoms_to_tensor(
struct atomspace_handle* handle,
cognitive_tensor_shape_t shape
);
cognitive_result_t atomspace_pattern_query(
struct atomspace_handle* handle,
cognitive_tensor_t* pattern,
cognitive_kernel_config_t config
);
atom_t* atomspace_add_atom(
struct atomspace_handle* handle,
atom_type_t type,
const char* name,
truth_value_t tv
);
atom_t* atomspace_get_atom(
struct atomspace_handle* handle,
uint64_t atom_id
);
int atomspace_remove_atom(
struct atomspace_handle* handle,
uint64_t atom_id
);
atom_t* atomspace_create_link(
struct atomspace_handle* handle,
atom_type_t link_type,
atom_t** outgoing,
size_t outgoing_count,
truth_value_t tv
);
cognitive_result_t atomspace_pln_inference(
struct atomspace_handle* handle,
cognitive_tensor_t* premises,
cognitive_kernel_config_t config
);
int atomspace_update_attention(
struct atomspace_handle* handle,
cognitive_tensor_t* attention_tensor
);
cognitive_tensor_t* atomspace_get_attention(
struct atomspace_handle* handle,
cognitive_tensor_shape_t shape
);
cognitive_result_t atomspace_pattern_mining(
struct atomspace_handle* handle,
cognitive_tensor_t* data,
uint32_t min_support,
cognitive_kernel_config_t config
);
void atomspace_free_atom(atom_t* atom);
#endif