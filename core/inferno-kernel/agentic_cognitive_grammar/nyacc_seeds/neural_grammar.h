#ifndef NEURAL_GRAMMAR_H
#define NEURAL_GRAMMAR_H

#include "lib9.h"

// Neural grammar structures
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

// Function declarations
NeuralParser* create_neural_parser(int id, char *name, int embedding_dim);
int add_grammar_rule(NeuralParser *np, char *name, char *pattern, float *weights, int weight_size);
int parse_with_neural_grammar(NeuralParser *np, char *input, float *output);
GrammarSeed* create_grammar_seed(int id, char *pattern, int seed_size);
DistributedGrammar* create_distributed_grammar(int node_id);
int add_parser_to_distributed_grammar(DistributedGrammar *dg, NeuralParser *np);
int add_seed_to_distributed_grammar(DistributedGrammar *dg, GrammarSeed *seed);
void free_neural_parser(NeuralParser *np);
void free_grammar_seed(GrammarSeed *seed);
void free_distributed_grammar(DistributedGrammar *dg);

#endif // NEURAL_GRAMMAR_H