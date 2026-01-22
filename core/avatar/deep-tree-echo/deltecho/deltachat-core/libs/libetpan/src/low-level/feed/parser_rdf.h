#ifndef PARSER_RDF_H
#define PARSER_RDF_H
void newsfeed_parser_rdf_start(void * data, const char * el, const char ** attr);
void newsfeed_parser_rdf_end(void * data, const char * el);
enum {
FEED_LOC_RDF_NONE,
FEED_LOC_RDF_CHANNEL,
FEED_LOC_RDF_ITEM
};
#endif