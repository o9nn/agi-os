#ifndef iplugin_INCLUDED
#define iplugin_INCLUDED
#ifndef i_ctx_t_DEFINED
#define i_ctx_t_DEFINED
typedef struct gs_context_state_s i_ctx_t;
#endif
#ifndef gs_memory_DEFINED
#define gs_memory_DEFINED
typedef struct gs_memory_s gs_memory_t;
#endif
typedef struct i_plugin_holder_s i_plugin_holder;
typedef struct i_plugin_instance_s i_plugin_instance;
typedef struct i_plugin_descriptor_s i_plugin_descriptor;
typedef struct i_plugin_client_memory_s i_plugin_client_memory;
struct i_plugin_descriptor_s {
const char *type;
const char *subtype;
void (*finit)(i_plugin_instance *instance, i_plugin_client_memory *mem);
};
struct i_plugin_instance_s {
const i_plugin_descriptor *d;
};
struct i_plugin_holder_s {
i_plugin_holder *next;
i_plugin_instance *I;
};
struct i_plugin_client_memory_s {
void *client_data;
void *(*alloc)(i_plugin_client_memory *mem, unsigned int size, const char *id);
void (*free)(i_plugin_client_memory *mem, void *data, const char *cname);
};
#define plugin_instantiation_proc(proc)\
int proc(i_ctx_t *, i_plugin_client_memory *client_mem, i_plugin_instance **instance)
#define extern_i_plugin_table()\
typedef plugin_instantiation_proc((*i_plugin_instantiation_proc));\
extern const i_plugin_instantiation_proc i_plugin_table[]
void i_plugin_make_memory(i_plugin_client_memory *mem, gs_memory_t *mem_raw);
int i_plugin_init(i_ctx_t *);
void i_plugin_finit(gs_memory_t *mem, i_plugin_holder *list);
i_plugin_instance *i_plugin_find(i_ctx_t *i_ctx_p, const char *type, const char *subtype);
i_plugin_holder * i_plugin_get_list(i_ctx_t *i_ctx_p);
#endif