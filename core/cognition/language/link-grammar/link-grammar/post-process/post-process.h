#ifndef _POSTPROCESS_H_
#define _POSTPROCESS_H_
#include "api-types.h"
#include "link-includes.h"
typedef struct PP_data_s PP_data;
Postprocessor * post_process_new(pp_knowledge *);
void post_process_free(Postprocessor *);
void post_process_lkgs(Sentence, Parse_Options);
void     do_post_process(Postprocessor *, Linkage, bool);
void     post_process_free_data(PP_data * ppd);
bool     post_process_match(const char *, const char *);
void compute_domain_names(Linkage);
void linkage_free_pp_domains(Linkage);
#endif