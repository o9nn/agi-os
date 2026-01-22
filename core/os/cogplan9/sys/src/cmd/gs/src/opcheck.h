#ifndef opcheck_INCLUDED
#  define opcheck_INCLUDED
#define check_type_only(rf,typ)\
BEGIN if ( !r_has_type(&rf,typ) ) return_error(e_typecheck); END
#define check_stype_only(rf,styp)\
BEGIN if ( !r_has_stype(&rf,imemory,styp) ) return_error(e_typecheck); END
#define check_array_else(rf,errstat)\
BEGIN if ( !r_has_type(&rf, t_array) ) errstat; END
#define check_array_only(rf)\
check_array_else(rf, return_error(e_typecheck))
int check_proc_failed(const ref *);
#define check_proc(rf)\
BEGIN if ( !r_is_proc(&rf) ) return_error(check_proc_failed(&rf)); END
#define check_proc_only(rf) check_proc(rf)
#define check_access(rf,acc1)\
BEGIN if ( !r_has_attr(&rf,acc1) ) return_error(e_invalidaccess); END
#define check_read(rf) check_access(rf,a_read)
#define check_write(rf) check_access(rf,a_write)
#define check_execute(rf) check_access(rf,a_execute)
#define check_type_access_only(rf,typ,acc1)\
BEGIN\
if ( !r_has_type_attrs(&rf,typ,acc1) )\
return_error((!r_has_type(&rf,typ) ? e_typecheck : e_invalidaccess));\
END
#define check_read_type_only(rf,typ)\
check_type_access_only(rf,typ,a_read)
#define check_write_type_only(rf,typ)\
check_type_access_only(rf,typ,a_write)
#define check_int_leu(orf, u)\
BEGIN\
check_type(orf, t_integer);\
if ( (ulong)(orf).value.intval > (u) ) return_error(e_rangecheck);\
END
#define check_int_leu_only(rf, u)\
BEGIN\
check_type_only(rf, t_integer);\
if ( (ulong)(rf).value.intval > (u) ) return_error(e_rangecheck);\
END
#define check_int_ltu(orf, u)\
BEGIN\
check_type(orf, t_integer);\
if ( (ulong)(orf).value.intval >= (u) ) return_error(e_rangecheck);\
END
#endif