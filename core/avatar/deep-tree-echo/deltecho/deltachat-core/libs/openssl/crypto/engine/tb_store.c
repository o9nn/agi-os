#include "eng_int.h"
static ENGINE_TABLE *store_table = NULL;
static const int dummy_nid = 1;
void ENGINE_unregister_STORE(ENGINE *e)
{
engine_table_unregister(&store_table, e);
}
static void engine_unregister_all_STORE(void)
{
engine_table_cleanup(&store_table);
}
int ENGINE_register_STORE(ENGINE *e)
{
if (e->store_meth)
return engine_table_register(&store_table,
engine_unregister_all_STORE, e,
&dummy_nid, 1, 0);
return 1;
}
void ENGINE_register_all_STORE()
{
ENGINE *e;
for (e = ENGINE_get_first(); e; e = ENGINE_get_next(e))
ENGINE_register_STORE(e);
}
#if 0
int ENGINE_set_default_STORE(ENGINE *e)
{
if (e->store_meth)
return engine_table_register(&store_table,
engine_unregister_all_STORE, e,
&dummy_nid, 1, 1);
return 1;
}
#endif
#if 0
ENGINE *ENGINE_get_default_STORE(void)
{
return engine_table_select(&store_table, dummy_nid);
}
#endif
const STORE_METHOD *ENGINE_get_STORE(const ENGINE *e)
{
return e->store_meth;
}
int ENGINE_set_STORE(ENGINE *e, const STORE_METHOD *store_meth)
{
e->store_meth = store_meth;
return 1;
}