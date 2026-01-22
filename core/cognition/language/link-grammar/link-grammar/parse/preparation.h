#ifndef _PREPARATION_H
#define _PREPARATION_H
#include "link-includes.h"
void prepare_to_parse(Sentence, Parse_Options);
bool set_connector_hash(Sentence);
void gword_record_in_connector(Sentence);
#endif