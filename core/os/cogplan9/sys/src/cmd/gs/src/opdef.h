#ifndef opdef_INCLUDED
# define opdef_INCLUDED
typedef struct {
const char *oname;
op_proc_t proc;
} op_def;
#define op_def_begin_dict(dname) {dname, 0}
#define op_def_begin_filter() op_def_begin_dict("filterdict")
#define op_def_begin_level2() op_def_begin_dict("level2dict")
#define op_def_begin_ll3() op_def_begin_dict("ll3dict")
#define op_def_is_begin_dict(def) ((def)->proc == 0)
#define op_def_end(iproc) {0, iproc}
#define OP_DEFS_LOG2_MAX_SIZE 4
#define OP_DEFS_MAX_SIZE (1 << OP_DEFS_LOG2_MAX_SIZE)
extern const op_def *const op_defs_all[];
#define op_def_is_internal(def) ((def)->oname[1] == '%')
ushort op_find_index(const ref *);
#define op_index(opref)\
(r_size(opref) == 0 ? op_find_index(opref) : r_size(opref))
#define op_index_is_operator(index) ((index) < op_def_count)
extern const uint op_def_count;
#define op_index_def(index)\
(&op_defs_all[(index) >> OP_DEFS_LOG2_MAX_SIZE]\
[(index) & (OP_DEFS_MAX_SIZE - 1)])
#define op_num_args(opref) (op_index_def(op_index(opref))->oname[0] - '0')
#define op_index_proc(index) (op_index_def(index)->proc)
typedef struct op_array_table_s {
ref table;
ushort *nx_table;
uint count;
uint base_index;
uint attrs;
ref *root_p;
} op_array_table;
extern op_array_table
op_array_table_global, op_array_table_local;
#define op_index_op_array_table(index)\
((index) < op_array_table_local.base_index ?\
&op_array_table_global : &op_array_table_local)
void op_index_ref(uint, ref *);
#endif