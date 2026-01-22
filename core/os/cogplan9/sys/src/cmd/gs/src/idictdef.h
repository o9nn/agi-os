#ifndef idictdef_INCLUDED
# define idictdef_INCLUDED
#define dict_is_packed(dct) r_has_type(&(dct)->keys, t_shortarray)
#define packed_key_empty (pt_tag(pt_integer) + 0)
#define packed_key_deleted (pt_tag(pt_integer) + 1)
#define packed_key_impossible pt_tag(pt_full_ref)
#define packed_name_key(nidx)\
((nidx) <= packed_name_max_index ? pt_tag(pt_literal_name) + (nidx) :\
packed_key_impossible)
#define d_maxlength(dct) ((uint)((dct)->maxlength.value.intval))
#define d_set_maxlength(dct,siz) ((dct)->maxlength.value.intval = (siz))
#define nslots(dct) r_size(&(dct)->values)
#define npairs(dct) (nslots(dct) - 1)
#define d_length(dct) ((uint)((dct)->count.value.intval))
#define packed_search_value_pointer (pdict->values.value.refs + (kp - kbot))
#define packed_search_body(found1,found2,del,miss)\
{ if_debug2('D', "[D]probe 0x%lx: 0x%x\n", (ulong)kp, *kp);\
if ( *kp == kpack )\
{ found1;\
found2;\
}\
else if ( !r_packed_is_name(kp) )\
{ \
if ( *kp == packed_key_empty ) miss;\
if ( kp == kbot ) break; \
else { del; }\
}\
}
#define packed_search_1(found1,found2,del,miss)\
const ref_packed *kbot = pdict->keys.value.packed;\
register const ref_packed *kp;\
for ( kp = kbot + dict_hash_mod(hash, size) + 1; ; kp-- )\
packed_search_body(found1,found2,del,miss)
#define packed_search_2(found1,found2,del,miss)\
for ( kp += size; ; kp-- )\
packed_search_body(found1,found2,del,miss)
#endif