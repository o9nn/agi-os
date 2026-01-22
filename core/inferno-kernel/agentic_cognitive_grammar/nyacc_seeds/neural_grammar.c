#include "lib9.h"
#include "bio.h"
#include "isa.h"
#include "mathi.h"
#include "neural_grammar.h"
typedef struct GrammarRule GrammarRule;
typedef struct NeuralParser NeuralParser;
typedef struct GrammarSeed GrammarSeed;
typedef struct DistributedGrammar DistributedGrammar;
struct GrammarRule {
char *name;
char *pattern;
float *neural_weights;
int weight_size;
GrammarRule *next;
};
struct NeuralParser {
int id;
char *name;
GrammarRule *rules;
float *embedding_weights;
int embedding_dim;
NeuralParser *next;
};
struct GrammarSeed {
int id;
char *seed_pattern;
float *neural_seed;
int seed_size;
GrammarSeed *next;
};
struct DistributedGrammar {
int node_id;
NeuralParser *parsers;
GrammarSeed *seeds;
DistributedGrammar *next;
};
static int
parse_grammar_rule(char *input, GrammarRule *rule, float *output)
{
int len = strlen(input);
int embed_dim = rule->weight_size / len;
for(int i = 0; i < len; i++) {
for(int j = 0; j < embed_dim; j++) {
output[i * embed_dim + j] = 0;
for(int k = 0; k < len; k++) {
output[i * embed_dim + j] +=
rule->neural_weights[k * embed_dim + j] * input[k];
}
}
}
return 0;
}
static int
generate_grammar_seed(char *pattern, float *seed, int seed_size)
{
int len = strlen(pattern);
for(int i = 0; i < seed_size; i++) {
seed[i] = 0;
for(int j = 0; j < len; j++) {
seed[i] += pattern[j] * (i + j + 1) / (float)(len + seed_size);
}
}
return 0;
}
static int
distributed_grammar_sync(DistributedGrammar *dg, int target_node)
{
return 0;
}
NeuralParser*
create_neural_parser(int id, char *name, int embedding_dim)
{
NeuralParser *np = malloc(sizeof(NeuralParser));
if(np == nil)
return nil;
np->id = id;
np->name = strdup(name);
np->rules = nil;
np->embedding_dim = embedding_dim;
np->embedding_weights = malloc(embedding_dim * embedding_dim * sizeof(float));
np->next = nil;
return np;
}
int
add_grammar_rule(NeuralParser *np, char *name, char *pattern, float *weights, int weight_size)
{
GrammarRule *rule = malloc(sizeof(GrammarRule));
if(rule == nil)
return -1;
rule->name = strdup(name);
rule->pattern = strdup(pattern);
rule->weight_size = weight_size;
rule->neural_weights = malloc(weight_size * sizeof(float));
rule->next = np->rules;
for(int i = 0; i < weight_size; i++)
rule->neural_weights[i] = weights[i];
np->rules = rule;
return 0;
}
int
parse_with_neural_grammar(NeuralParser *np, char *input, float *output)
{
GrammarRule *rule;
float *temp_output;
int max_output_size = strlen(input) * np->embedding_dim;
temp_output = malloc(max_output_size * sizeof(float));
if(temp_output == nil)
return -1;
for(rule = np->rules; rule != nil; rule = rule->next) {
if(parse_grammar_rule(input, rule, temp_output) == 0) {
for(int i = 0; i < max_output_size; i++)
output[i] = temp_output[i];
free(temp_output);
return 0;
}
}
free(temp_output);
return -1;
}
GrammarSeed*
create_grammar_seed(int id, char *pattern, int seed_size)
{
GrammarSeed *seed = malloc(sizeof(GrammarSeed));
if(seed == nil)
return nil;
seed->id = id;
seed->seed_pattern = strdup(pattern);
seed->seed_size = seed_size;
seed->neural_seed = malloc(seed_size * sizeof(float));
seed->next = nil;
generate_grammar_seed(pattern, seed->neural_seed, seed_size);
return seed;
}
DistributedGrammar*
create_distributed_grammar(int node_id)
{
DistributedGrammar *dg = malloc(sizeof(DistributedGrammar));
if(dg == nil)
return nil;
dg->node_id = node_id;
dg->parsers = nil;
dg->seeds = nil;
dg->next = nil;
return dg;
}
int
add_parser_to_distributed_grammar(DistributedGrammar *dg, NeuralParser *np)
{
np->next = dg->parsers;
dg->parsers = np;
return 0;
}
int
add_seed_to_distributed_grammar(DistributedGrammar *dg, GrammarSeed *seed)
{
seed->next = dg->seeds;
dg->seeds = seed;
return 0;
}
void
free_neural_parser(NeuralParser *np)
{
GrammarRule *rule, *next_rule;
if(np) {
for(rule = np->rules; rule != nil; rule = next_rule) {
next_rule = rule->next;
free(rule->name);
free(rule->pattern);
free(rule->neural_weights);
free(rule);
}
free(np->name);
free(np->embedding_weights);
free(np);
}
}
void
free_grammar_seed(GrammarSeed *seed)
{
if(seed) {
free(seed->seed_pattern);
free(seed->neural_seed);
free(seed);
}
}
void
free_distributed_grammar(DistributedGrammar *dg)
{
NeuralParser *np, *next_np;
GrammarSeed *seed, *next_seed;
if(dg) {
for(np = dg->parsers; np != nil; np = next_np) {
next_np = np->next;
free_neural_parser(np);
}
for(seed = dg->seeds; seed != nil; seed = next_seed) {
next_seed = seed->next;
free_grammar_seed(seed);
}
free(dg);
}
}