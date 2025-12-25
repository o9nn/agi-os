/**
 * @file cognitive_demo.c
 * @brief Demonstration of AGI-OS Cognitive Synergy Bridge
 * 
 * This example demonstrates how to use the Cognitive Synergy Bridge
 * to integrate multiple reasoning systems for AGI applications.
 * 
 * @copyright AGI-OS Project
 * @license GPL-3.0
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cognitive_synergy_bridge.h"
#include "integrations/nars_integration.h"
#include "integrations/ggml_integration.h"
#include "integrations/metta_integration.h"

/* Callback for NARS output */
void nars_output_handler(cog_context_t* ctx,
                          const nars_output_t* output,
                          void* user_data) {
    (void)ctx;
    (void)user_data;
    
    printf("[NARS Output] Type=%d, Term=%s, TV=<%0.2f, %0.2f>\n",
           output->type,
           output->term ? output->term : "NULL",
           output->strength,
           output->confidence);
}

/* Demonstrate basic AtomSpace operations */
void demo_atomspace(cog_context_t* ctx) {
    printf("\n=== AtomSpace Demo ===\n\n");
    
    /* Create concepts */
    cog_atom_t cat = cog_create_concept(ctx, "Cat");
    cog_atom_t animal = cog_create_concept(ctx, "Animal");
    cog_atom_t mammal = cog_create_concept(ctx, "Mammal");
    
    printf("Created concepts: Cat, Animal, Mammal\n");
    
    /* Create inheritance links */
    cog_atom_t cat_mammal[] = {cat, mammal};
    cog_atom_t mammal_animal[] = {mammal, animal};
    
    cog_atom_t link1 = cog_create_link(ctx, COG_LINK_INHERITANCE, cat_mammal, 2);
    cog_atom_t link2 = cog_create_link(ctx, COG_LINK_INHERITANCE, mammal_animal, 2);
    
    printf("Created inheritance links:\n");
    printf("  Cat -> Mammal\n");
    printf("  Mammal -> Animal\n");
    
    /* Set truth values */
    cog_truth_value_t tv_high = {0.95f, 0.9f};
    cog_set_truth_value(ctx, link1, &tv_high);
    cog_set_truth_value(ctx, link2, &tv_high);
    
    printf("Set truth values: <0.95, 0.9>\n");
    
    /* Run reasoning to derive Cat -> Animal */
    printf("\nRunning reasoning cycles...\n");
    int derivations = cog_reason(ctx, 100);
    printf("Completed %d derivations\n", derivations);
    
    (void)link1;
    (void)link2;
}

/* Demonstrate NARS integration */
void demo_nars(cog_context_t* ctx) {
    printf("\n=== NARS Integration Demo ===\n\n");
    
    /* Initialize NARS */
    nars_config_t config = NARS_CONFIG_DEFAULT;
    config.decision_threshold = 60;
    
    if (nars_init(ctx, &config) != 0) {
        printf("Failed to initialize NARS\n");
        return;
    }
    
    /* Register output callback */
    nars_register_output_callback(ctx, nars_output_handler, NULL);
    
    /* Add beliefs */
    printf("Adding beliefs to NARS...\n");
    nars_add_belief(ctx, "<cat --> animal>", 0.9f, 0.9f);
    nars_add_belief(ctx, "<cat --> [furry]>", 0.85f, 0.9f);
    nars_add_belief(ctx, "<[furry] --> soft>", 0.8f, 0.85f);
    
    /* Ask a question */
    printf("\nAsking: Is cat soft?\n");
    nars_ask(ctx, "<cat --> soft>");
    
    /* Run reasoning cycles */
    printf("Running NARS cycles...\n");
    nars_run(ctx, 1000, 5000);
    
    /* Get statistics */
    nars_stats_t stats;
    nars_get_stats(ctx, &stats);
    printf("\nNARS Statistics:\n");
    printf("  Total cycles: %lu\n", stats.total_cycles);
    printf("  Total inputs: %lu\n", stats.total_inputs);
    printf("  Total derivations: %lu\n", stats.total_derivations);
    
    /* Sync with AtomSpace */
    printf("\nSyncing NARS beliefs with AtomSpace...\n");
    int synced = nars_sync_atomspace(ctx, 1);
    printf("Synced %d items\n", synced);
    
    nars_shutdown(ctx);
}

/* Demonstrate GGML tensor operations */
void demo_ggml(cog_context_t* ctx) {
    printf("\n=== GGML Tensor Demo ===\n\n");
    
    /* Initialize GGML */
    ggml_config_t config = GGML_CONFIG_DEFAULT;
    config.mem_size = 64 * 1024 * 1024;  /* 64MB */
    config.n_threads = 4;
    
    if (ggml_cog_init(ctx, &config) != 0) {
        printf("Failed to initialize GGML\n");
        return;
    }
    
    /* Create tensors */
    printf("Creating tensors...\n");
    int64_t dims_a[] = {4, 3};
    int64_t dims_b[] = {3, 2};
    
    ggml_tensor_handle_t a = ggml_cog_tensor_new(ctx, GGML_TYPE_F32, dims_a, 2);
    ggml_tensor_handle_t b = ggml_cog_tensor_new(ctx, GGML_TYPE_F32, dims_b, 2);
    
    printf("  Tensor A: 4x3\n");
    printf("  Tensor B: 3x2\n");
    
    /* Initialize with data */
    float* data_a = (float*)ggml_cog_tensor_data(ctx, a);
    float* data_b = (float*)ggml_cog_tensor_data(ctx, b);
    
    for (int i = 0; i < 12; i++) data_a[i] = (float)(i + 1) / 12.0f;
    for (int i = 0; i < 6; i++) data_b[i] = (float)(i + 1) / 6.0f;
    
    /* Create computation graph */
    ggml_graph_handle_t graph = ggml_cog_graph_new(ctx);
    
    /* Matrix multiplication */
    printf("\nComputing matrix multiplication...\n");
    ggml_tensor_handle_t c = ggml_cog_matmul(ctx, graph, a, b);
    
    /* Apply activation */
    printf("Applying GELU activation...\n");
    ggml_tensor_handle_t activated = ggml_cog_gelu(ctx, graph, c);
    
    /* Execute graph */
    ggml_cog_graph_compute(ctx, graph);
    
    /* Print result */
    int64_t result_dims[4];
    int ndims = ggml_cog_tensor_shape(ctx, activated, result_dims);
    printf("\nResult shape: %ldx%ld (ndims=%d)\n", 
           result_dims[0], result_dims[1], ndims);
    
    /* Get statistics */
    ggml_stats_t stats;
    ggml_cog_get_stats(ctx, &stats);
    printf("\nGGML Statistics:\n");
    printf("  Memory used: %zu bytes\n", stats.memory_used);
    printf("  Tensors created: %lu\n", stats.tensors_created);
    printf("  Graphs computed: %lu\n", stats.graphs_computed);
    
    /* Cleanup */
    ggml_cog_graph_free(ctx, graph);
    ggml_cog_tensor_free(ctx, a);
    ggml_cog_tensor_free(ctx, b);
    ggml_cog_tensor_free(ctx, c);
    ggml_cog_tensor_free(ctx, activated);
    
    ggml_cog_shutdown(ctx);
}

/* Demonstrate MeTTa integration */
void demo_metta(cog_context_t* ctx) {
    printf("\n=== MeTTa Integration Demo ===\n\n");
    
    /* Initialize MeTTa */
    metta_config_t config = METTA_CONFIG_DEFAULT;
    config.enable_stdlib = true;
    
    if (metta_init(ctx, &config) != 0) {
        printf("Failed to initialize MeTTa\n");
        return;
    }
    
    printf("MeTTa version: %s\n", metta_version());
    
    /* Get default space */
    metta_space_t space = metta_space_default(ctx);
    printf("Using default space\n");
    
    /* Create atoms */
    printf("\nCreating MeTTa atoms...\n");
    metta_atom_t sym_cat = metta_atom_sym(ctx, "Cat");
    metta_atom_t sym_animal = metta_atom_sym(ctx, "Animal");
    metta_atom_t var_x = metta_atom_var(ctx, "x");
    
    /* Add to space */
    metta_space_add(ctx, space, sym_cat);
    metta_space_add(ctx, space, sym_animal);
    
    /* Create runner */
    metta_runner_t runner = metta_runner_new(ctx, space);
    
    /* Execute MeTTa code */
    printf("\nExecuting MeTTa code...\n");
    metta_atom_t results[10];
    int result_count = metta_run(ctx, runner, "(+ 1 2)", results, 10);
    printf("Execution returned %d results\n", result_count);
    
    /* Load PLN module */
    printf("\nLoading PLN module...\n");
    metta_load_pln(ctx, space);
    
    /* Sync with AtomSpace */
    printf("Syncing MeTTa space with AtomSpace...\n");
    int synced = metta_sync_atomspace(ctx, space, 0);
    printf("Synced %d atoms\n", synced);
    
    /* Get statistics */
    metta_stats_t stats;
    metta_get_stats(ctx, &stats);
    printf("\nMeTTa Statistics:\n");
    printf("  Atoms created: %lu\n", stats.atoms_created);
    printf("  Atoms in space: %lu\n", stats.atoms_in_space);
    printf("  Evaluations: %lu\n", stats.evaluations);
    
    /* Cleanup */
    metta_runner_free(ctx, runner);
    metta_shutdown(ctx);
    
    (void)var_x;
}

/* Demonstrate cognitive synergy - all systems working together */
void demo_cognitive_synergy(cog_context_t* ctx) {
    printf("\n=== Cognitive Synergy Demo ===\n\n");
    printf("This demonstrates all systems working together:\n");
    printf("  1. AtomSpace for knowledge representation\n");
    printf("  2. NARS for non-axiomatic reasoning\n");
    printf("  3. GGML for neural processing\n");
    printf("  4. MeTTa for meta-programming\n\n");
    
    /* Initialize all subsystems */
    nars_init(ctx, NULL);
    ggml_cog_init(ctx, NULL);
    metta_init(ctx, NULL);
    
    /* Create shared knowledge in AtomSpace */
    cog_atom_t perception = cog_create_concept(ctx, "Perception");
    cog_atom_t action = cog_create_concept(ctx, "Action");
    cog_atom_t goal = cog_create_concept(ctx, "Goal");
    
    /* Feed to NARS for reasoning */
    nars_add_belief(ctx, "<perception --> input>", 0.9f, 0.9f);
    nars_add_goal(ctx, "<goal --> achieved>", 0.8f, 0.8f);
    
    /* Create embedding table for atoms */
    ggml_tensor_handle_t embed_table = ggml_cog_create_embedding_table(ctx, 1000, 64);
    
    /* Run cognitive cycle */
    printf("Running integrated cognitive cycle...\n");
    for (int i = 0; i < 10; i++) {
        /* NARS reasoning step */
        nars_cycle(ctx, 10);
        
        /* AtomSpace reasoning */
        cog_reason(ctx, 10);
        
        printf("  Cycle %d complete\n", i + 1);
    }
    
    printf("\nCognitive synergy demonstration complete!\n");
    
    /* Cleanup */
    ggml_cog_tensor_free(ctx, embed_table);
    metta_shutdown(ctx);
    ggml_cog_shutdown(ctx);
    nars_shutdown(ctx);
    
    (void)perception;
    (void)action;
    (void)goal;
}

int main(int argc, char** argv) {
    (void)argc;
    (void)argv;
    
    printf("AGI-OS Cognitive Synergy Bridge Demo\n");
    printf("====================================\n");
    
    /* Create cognitive context */
    cog_config_t config = COG_CONFIG_DEFAULT;
    config.max_atoms = 100000;
    config.enable_logging = true;
    
    cog_context_t* ctx = cog_context_create(&config);
    if (!ctx) {
        fprintf(stderr, "Failed to create cognitive context\n");
        return 1;
    }
    
    /* Run demonstrations */
    demo_atomspace(ctx);
    demo_nars(ctx);
    demo_ggml(ctx);
    demo_metta(ctx);
    demo_cognitive_synergy(ctx);
    
    /* Cleanup */
    cog_context_destroy(ctx);
    
    printf("\n====================================\n");
    printf("Demo complete!\n");
    
    return 0;
}
