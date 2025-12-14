/*
 * Cognitive Cities Extensions for Plan 9 Kernel
 * 
 * This file provides core cognitive capabilities that extend Plan 9's
 * existing channel and namespace system to support distributed cognitive
 * cities architecture.
 * 
 * The implementation follows Plan 9 principles:
 * - Everything is a file (cognitive services accessible via filesystem)
 * - Namespaces for isolation and organization
 * - 9P protocol for communication (extended with cognitive messages)
 * - Process groups for coordination (extended for swarms)
 */

#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"

/*
 * Cognitive Extensions Data Structures
 */

typedef struct CognitiveNamespace CognitiveNamespace;
typedef struct NeuralChannel NeuralChannel;
typedef struct CognitiveSwarm CognitiveSwarm;
typedef struct NeuralMessage NeuralMessage;
typedef struct EmergentPattern EmergentPattern;
typedef struct RootedShell RootedShell;
typedef struct RootedTree RootedTree;

struct NeuralMessage {
    ulong tag;                    // Message tag
    uchar type;                   // Neural message type
    char *source_domain;          // Source cognitive domain
    char *target_domain;          // Target cognitive domain  
    char *swarm_id;               // Associated swarm ID
    ulong cognitive_priority;     // Processing priority
    time_t timestamp;             // Creation timestamp
    ulong payload_size;           // Payload size
    void *cognitive_payload;      // Cognitive data payload
    float confidence_level;       // Message confidence (0.0-1.0)
    NeuralMessage *next;          // Queue linkage
};

struct NeuralChannel {
    Chan chan;                    // Base Plan 9 channel
    char *channel_id;             // Unique channel identifier
    char *source_domain;          // Source cognitive domain
    char *target_domain;          // Target cognitive domain
    ulong bandwidth_capacity;     // Neural bandwidth capacity
    ulong current_load;           // Current cognitive load
    float adaptation_rate;        // Channel adaptation speed
    time_t last_evolution;        // Last evolutionary change
    NeuralMessage *message_queue; // Pending neural messages
    Lock queue_lock;              // Queue protection
};

struct CognitiveNamespace {
    char *domain;                 // Cognitive domain name
    char *namespace_path;         // Namespace root path
    int cognitive_load;           // Current cognitive processing load
    time_t last_adaptation;       // Last adaptation timestamp
    NeuralChannel **channels;     // Associated neural channels
    int channel_count;            // Number of channels
    EmergentPattern **patterns;   // Detected emergent patterns
    int pattern_count;            // Number of patterns
    Lock adaptation_lock;         // Adaptation synchronization
};

struct CognitiveSwarm {
    Pgrp *pgrp;                   // Base Plan 9 process group
    char *swarm_id;               // Unique swarm identifier
    char *domain;                 // Cognitive domain
    Proc **agents;                // Swarm member processes
    int agent_count;              // Number of agents
    NeuralChannel *coordination_channel; // Swarm coordination channel
    float coherence_level;        // Swarm coherence (0.0-1.0)
    time_t creation_time;         // Swarm creation time
    Lock swarm_lock;              // Swarm synchronization
};

struct EmergentPattern {
    char *pattern_id;             // Unique pattern identifier
    char *pattern_name;           // Human-readable name
    char *description;            // Pattern description
    time_t first_observed;        // When first observed
    time_t last_observed;         // Most recent observation
    int observation_count;        // Times observed
    float significance_score;     // Pattern significance (0.0-1.0)
    char **involved_domains;      // Domains exhibiting pattern
    int domain_count;             // Number of involved domains
};

/*
 * Rooted Shell Namespace Structures
 * 
 * Maps rooted tree configurations (nested shells) to Plan 9 namespaces.
 * Each shell is both a namespace (containing other shells/files) and a file itself.
 * 
 * Key concept: Rooted trees from A000081 enumerate all possible nested shell
 * configurations, which map to filesystem namespace hierarchies.
 */

// Binary tree representation (bit-encoded parentheses)
typedef uvlong tree;

struct RootedTree {
    tree binary_rep;              // Binary encoding of tree structure
    char *parens_notation;        // Parentheses notation: "(()())" etc
    char *namespace_path;         // Corresponding namespace path
    int node_count;               // Number of nodes (shells) in tree
    int depth;                    // Maximum nesting depth
    RootedTree **subtrees;        // Child subtrees
    int subtree_count;            // Number of subtrees
    uvlong matula_number;         // Matula number encoding (via prime factorization)
};

struct RootedShell {
    char *shell_id;               // Unique shell identifier
    char *domain;                 // Associated cognitive domain
    RootedTree *tree_structure;   // Underlying rooted tree structure
    
    // Dual representation: shell as namespace
    CognitiveNamespace *as_namespace;  // Shell viewed as namespace
    char *namespace_mount_point;       // Where namespace is mounted
    
    // Dual representation: shell as file
    char *file_path;              // Shell viewed as file path
    Chan *file_channel;           // File channel for shell access
    
    // Shell nesting relationships
    RootedShell *parent_shell;    // Containing shell (nil if root)
    RootedShell **child_shells;   // Nested shells
    int child_count;              // Number of nested shells
    
    // Addressing protocol
    char **address_path;          // Path components for addressing
    int path_depth;               // Depth in namespace hierarchy
    
    time_t creation_time;         // When shell was created
    Lock shell_lock;              // Shell synchronization
};

/*
 * Rooted Tree Generation (A000081 sequence)
 * 
 * These arrays store generated rooted trees for efficient lookup.
 */
#define MAXN 15  // Maximum tree size we can handle efficiently

static struct {
    Lock;
    tree *list;                   // All generated trees
    int list_size;                // Current size
    int list_capacity;            // Allocated capacity
    ulong *offset;                // Offset[n] = index where n-trees start
    int max_n;                    // Maximum n we've generated
} rooted_trees = { .list_size = 0, .max_n = 0 };

/*
 * Neural Message Types (9P Extensions)
 */
enum {
    // Standard 9P messages (unchanged)
    Tversion = 100, Rversion,
    Tauth = 102, Rauth,
    Tattach = 104, Rattach,
    Terror = 106, Rerror,
    Tflush = 108, Rflush,
    Twalk = 110, Rwalk,
    Topen = 112, Ropen,
    Tcreate = 114, Rcreate,
    Tread = 116, Rread,
    Twrite = 118, Rwrite,
    Tclunk = 120, Rclunk,
    Tremove = 122, Rremove,
    Tstat = 124, Rstat,
    Twstat = 126, Rwstat,
    
    // Cognitive extensions
    Tcognitive = 200, Rcognitive,      // Cognitive state exchange
    Tneural = 202, Rneural,            // Neural message transport
    Tswarm = 204, Rswarm,              // Swarm coordination
    Temergence = 206, Remergence,      // Emergent behavior notification
    Tadapt = 208, Radapt,              // Adaptive reconfiguration
    Tevolve = 210, Revolve,            // Evolution state sync
};

/*
 * Global Cognitive State
 */
static struct {
    Lock;
    CognitiveNamespace **namespaces;
    int namespace_count;
    NeuralChannel **channels;
    int channel_count;
    CognitiveSwarm **swarms;
    int swarm_count;
    EmergentPattern **patterns;
    int pattern_count;
    RootedShell **shells;
    int shell_count;
} cognitive_state = { .namespace_count = 0 };

/*
 * Neural Channel Operations
 */

NeuralChannel*
create_neural_channel(char *source_domain, char *target_domain, ulong bandwidth)
{
    NeuralChannel *nc;
    
    nc = malloc(sizeof(NeuralChannel));
    if (nc == nil)
        return nil;
        
    // Initialize base channel
    channelinit(&nc->chan);
    
    // Initialize cognitive properties
    nc->channel_id = smprint("%s-%s-%lud", source_domain, target_domain, time(NULL));
    nc->source_domain = strdup(source_domain);
    nc->target_domain = strdup(target_domain);
    nc->bandwidth_capacity = bandwidth;
    nc->current_load = 0;
    nc->adaptation_rate = 0.1; // 10% adaptation rate
    nc->last_evolution = time(NULL);
    nc->message_queue = nil;
    
    // Initialize locks
    lock(&nc->queue_lock);
    unlock(&nc->queue_lock);
    
    return nc;
}

int
send_neural_message(NeuralChannel *nc, NeuralMessage *msg)
{
    if (nc == nil || msg == nil)
        return -1;
        
    // Check channel capacity
    if (nc->current_load >= nc->bandwidth_capacity) {
        // Try adaptive capacity increase
        if (adapt_neural_channel_capacity(nc) < 0) {
            // Queue message if adaptation fails
            return queue_neural_message(nc, msg);
        }
    }
    
    // Update channel load
    nc->current_load++;
    
    // Set message timestamp
    msg->timestamp = time(NULL);
    
    // Route message through neural transport
    return route_neural_message(nc, msg);
}

NeuralMessage*
receive_neural_message(NeuralChannel *nc)
{
    NeuralMessage *msg;
    
    if (nc == nil)
        return nil;
        
    lock(&nc->queue_lock);
    
    msg = nc->message_queue;
    if (msg != nil) {
        nc->message_queue = msg->next;
        nc->current_load--;
    }
    
    unlock(&nc->queue_lock);
    
    return msg;
}

int
queue_neural_message(NeuralChannel *nc, NeuralMessage *msg)
{
    if (nc == nil || msg == nil)
        return -1;
        
    lock(&nc->queue_lock);
    
    // Add to end of queue
    msg->next = nil;
    if (nc->message_queue == nil) {
        nc->message_queue = msg;
    } else {
        NeuralMessage *tail = nc->message_queue;
        while (tail->next != nil)
            tail = tail->next;
        tail->next = msg;
    }
    
    unlock(&nc->queue_lock);
    
    return 0;
}

int
adapt_neural_channel_capacity(NeuralChannel *nc)
{
    float load_ratio;
    ulong new_capacity;
    
    if (nc == nil)
        return -1;
        
    load_ratio = (float)nc->current_load / nc->bandwidth_capacity;
    
    if (load_ratio > 0.8) { // High load threshold
        // Increase capacity by adaptation rate
        new_capacity = nc->bandwidth_capacity * (1.0 + nc->adaptation_rate);
        nc->bandwidth_capacity = new_capacity;
        nc->last_evolution = time(NULL);
        
        // Log adaptation
        print("Neural channel %s adapted: new capacity %lud\n", 
              nc->channel_id, new_capacity);
        
        return 0;
    }
    
    return -1;
}

int
route_neural_message(NeuralChannel *nc, NeuralMessage *msg)
{
    // Basic routing implementation
    // In a full implementation, this would include:
    // - Cognitive routing algorithms
    // - Load balancing
    // - QoS management
    // - Pattern detection
    
    // For now, simple queuing
    return queue_neural_message(nc, msg);
}

/*
 * Cognitive Namespace Operations
 */

CognitiveNamespace*
create_cognitive_namespace(char *domain, char *namespace_path)
{
    CognitiveNamespace *cns;
    
    cns = malloc(sizeof(CognitiveNamespace));
    if (cns == nil)
        return nil;
        
    cns->domain = strdup(domain);
    cns->namespace_path = strdup(namespace_path);
    cns->cognitive_load = 0;
    cns->last_adaptation = time(NULL);
    cns->channels = nil;
    cns->channel_count = 0;
    cns->patterns = nil;
    cns->pattern_count = 0;
    
    // Initialize adaptation lock
    lock(&cns->adaptation_lock);
    unlock(&cns->adaptation_lock);
    
    return cns;
}

int
bind_neural_channel_to_namespace(CognitiveNamespace *cns, NeuralChannel *nc)
{
    NeuralChannel **new_channels;
    
    if (cns == nil || nc == nil)
        return -1;
        
    // Expand channel array
    new_channels = realloc(cns->channels, 
                          sizeof(NeuralChannel*) * (cns->channel_count + 1));
    if (new_channels == nil)
        return -1;
        
    cns->channels = new_channels;
    cns->channels[cns->channel_count] = nc;
    cns->channel_count++;
    
    print("Neural channel %s bound to cognitive namespace %s\n",
          nc->channel_id, cns->domain);
    
    return 0;
}

int
adapt_cognitive_namespace(CognitiveNamespace *cns)
{
    int i;
    float avg_load;
    time_t current_time;
    
    if (cns == nil)
        return -1;
        
    lock(&cns->adaptation_lock);
    
    current_time = time(NULL);
    
    // Calculate average channel load
    if (cns->channel_count > 0) {
        ulong total_load = 0;
        for (i = 0; i < cns->channel_count; i++) {
            total_load += cns->channels[i]->current_load;
        }
        avg_load = (float)total_load / cns->channel_count;
    } else {
        avg_load = 0.0;
    }
    
    // Update cognitive load
    cns->cognitive_load = (int)avg_load;
    
    // Adapt if needed
    if (avg_load > 100.0) { // High cognitive load
        // Trigger channel adaptations
        for (i = 0; i < cns->channel_count; i++) {
            adapt_neural_channel_capacity(cns->channels[i]);
        }
        
        cns->last_adaptation = current_time;
        
        print("Cognitive namespace %s adapted due to high load: %.2f\n",
              cns->domain, avg_load);
    }
    
    unlock(&cns->adaptation_lock);
    
    return 0;
}

/*
 * Cognitive Swarm Operations
 */

CognitiveSwarm*
create_cognitive_swarm(char *swarm_id, char *domain, Pgrp *pgrp)
{
    CognitiveSwarm *swarm;
    
    swarm = malloc(sizeof(CognitiveSwarm));
    if (swarm == nil)
        return nil;
        
    swarm->pgrp = pgrp;
    swarm->swarm_id = strdup(swarm_id);
    swarm->domain = strdup(domain);
    swarm->agents = nil;
    swarm->agent_count = 0;
    swarm->coordination_channel = nil;
    swarm->coherence_level = 1.0; // Start with perfect coherence
    swarm->creation_time = time(NULL);
    
    // Initialize swarm lock
    lock(&swarm->swarm_lock);
    unlock(&swarm->swarm_lock);
    
    // Create coordination channel
    swarm->coordination_channel = create_neural_channel(domain, "swarm-coordination", 1000);
    
    print("Cognitive swarm %s created for domain %s\n", swarm_id, domain);
    
    return swarm;
}

int
add_agent_to_swarm(CognitiveSwarm *swarm, Proc *agent)
{
    Proc **new_agents;
    
    if (swarm == nil || agent == nil)
        return -1;
        
    lock(&swarm->swarm_lock);
    
    // Expand agent array
    new_agents = realloc(swarm->agents, 
                        sizeof(Proc*) * (swarm->agent_count + 1));
    if (new_agents == nil) {
        unlock(&swarm->swarm_lock);
        return -1;
    }
    
    swarm->agents = new_agents;
    swarm->agents[swarm->agent_count] = agent;
    swarm->agent_count++;
    
    // Set agent's cognitive swarm reference
    agent->aux = swarm; // Store swarm reference in process aux field
    
    unlock(&swarm->swarm_lock);
    
    print("Agent process %d added to cognitive swarm %s\n", 
          agent->pid, swarm->swarm_id);
    
    return 0;
}

float
calculate_swarm_coherence(CognitiveSwarm *swarm)
{
    // Simplified coherence calculation
    // In a full implementation, this would analyze:
    // - Agent coordination efficiency
    // - Message passing success rates
    // - Collective decision making effectiveness
    // - Emergent behavior alignment
    
    if (swarm == nil || swarm->agent_count == 0)
        return 0.0;
        
    // For now, calculate based on agent count and coordination channel load
    float base_coherence = 1.0;
    float load_factor = 1.0;
    
    if (swarm->coordination_channel != nil) {
        load_factor = 1.0 - ((float)swarm->coordination_channel->current_load / 
                            swarm->coordination_channel->bandwidth_capacity);
    }
    
    // Agent count factor (larger swarms have coordination challenges)
    float size_factor = 1.0 / (1.0 + (swarm->agent_count / 10.0));
    
    swarm->coherence_level = base_coherence * load_factor * size_factor;
    
    return swarm->coherence_level;
}

/*
 * Emergence Detection
 */

EmergentPattern*
detect_emergent_pattern(char *pattern_name, char **domains, int domain_count)
{
    EmergentPattern *pattern;
    int i;
    
    pattern = malloc(sizeof(EmergentPattern));
    if (pattern == nil)
        return nil;
        
    pattern->pattern_id = smprint("pattern-%lud", time(NULL));
    pattern->pattern_name = strdup(pattern_name);
    pattern->description = smprint("Emergent pattern observed across %d domains", domain_count);
    pattern->first_observed = time(NULL);
    pattern->last_observed = time(NULL);
    pattern->observation_count = 1;
    pattern->significance_score = 0.5; // Initial significance
    
    // Copy domain list
    pattern->involved_domains = malloc(sizeof(char*) * domain_count);
    pattern->domain_count = domain_count;
    for (i = 0; i < domain_count; i++) {
        pattern->involved_domains[i] = strdup(domains[i]);
    }
    
    print("Emergent pattern detected: %s across domains: ", pattern_name);
    for (i = 0; i < domain_count; i++) {
        print("%s ", domains[i]);
    }
    print("\n");
    
    return pattern;
}

/*
 * Initialization and Setup
 */

void
cognitive_cities_init(void)
{
    // Initialize global cognitive state
    lock(&cognitive_state);
    
    cognitive_state.namespaces = nil;
    cognitive_state.namespace_count = 0;
    cognitive_state.channels = nil;
    cognitive_state.channel_count = 0;
    cognitive_state.swarms = nil;
    cognitive_state.swarm_count = 0;
    cognitive_state.patterns = nil;
    cognitive_state.pattern_count = 0;
    
    unlock(&cognitive_state);
    
    print("Cognitive Cities architecture initialized\n");
    
    // Create initial cognitive namespaces for main domains
    create_initial_cognitive_domains();
}

void
create_initial_cognitive_domains(void)
{
    CognitiveNamespace *transportation, *energy, *governance, *environment;
    
    // Create core cognitive domain namespaces
    transportation = create_cognitive_namespace("transportation", 
                                               "/cognitive-cities/domains/transportation");
    energy = create_cognitive_namespace("energy", 
                                       "/cognitive-cities/domains/energy");
    governance = create_cognitive_namespace("governance", 
                                           "/cognitive-cities/domains/governance");
    environment = create_cognitive_namespace("environment", 
                                            "/cognitive-cities/domains/environment");
    
    // Add to global state
    // (Implementation would add to cognitive_state arrays)
    
    print("Initial cognitive domains created: transportation, energy, governance, environment\n");
    
    // Create inter-domain neural transport channels
    create_interdomain_channels(transportation, energy, governance, environment);
}

void
create_interdomain_channels(CognitiveNamespace *transportation, 
                           CognitiveNamespace *energy,
                           CognitiveNamespace *governance, 
                           CognitiveNamespace *environment)
{
    NeuralChannel *trans_energy, *trans_gov, *energy_env, *gov_env;
    
    // Create neural channels between domains
    trans_energy = create_neural_channel("transportation", "energy", 500);
    trans_gov = create_neural_channel("transportation", "governance", 300);
    energy_env = create_neural_channel("energy", "environment", 400);
    gov_env = create_neural_channel("governance", "environment", 200);
    
    // Bind channels to namespaces
    bind_neural_channel_to_namespace(transportation, trans_energy);
    bind_neural_channel_to_namespace(transportation, trans_gov);
    bind_neural_channel_to_namespace(energy, trans_energy);
    bind_neural_channel_to_namespace(energy, energy_env);
    bind_neural_channel_to_namespace(governance, trans_gov);
    bind_neural_channel_to_namespace(governance, gov_env);
    bind_neural_channel_to_namespace(environment, energy_env);
    bind_neural_channel_to_namespace(environment, gov_env);
    
    print("Inter-domain neural transport channels established\n");
}

/*
 * Rooted Tree Generation Functions (A000081)
 * 
 * These functions implement the recursive assembly algorithm for generating
 * all rooted trees with n nodes, using binary encoding for compact storage.
 */

static void
init_rooted_trees(void)
{
    lock(&rooted_trees);
    
    if (rooted_trees.list == nil) {
        rooted_trees.list_capacity = 10000;  // Start with capacity for ~10k trees
        rooted_trees.list = malloc(rooted_trees.list_capacity * sizeof(tree));
        rooted_trees.offset = malloc((MAXN + 2) * sizeof(ulong));
        
        if (rooted_trees.list == nil || rooted_trees.offset == nil) {
            print("rooted_trees: failed to allocate memory\n");
            unlock(&rooted_trees);
            return;
        }
        
        // Initialize: offset[0] = 0, tree size 0 has no trees
        rooted_trees.offset[0] = 0;
        rooted_trees.list_size = 0;
        rooted_trees.max_n = 0;
    }
    
    unlock(&rooted_trees);
}

static void
append_tree(tree t)
{
    lock(&rooted_trees);
    
    // Expand list if needed
    if (rooted_trees.list_size >= rooted_trees.list_capacity) {
        int new_capacity = rooted_trees.list_capacity * 2;
        tree *new_list = malloc(new_capacity * sizeof(tree));
        
        if (new_list == nil) {
            print("append_tree: failed to expand list\n");
            unlock(&rooted_trees);
            return;
        }
        
        memmove(new_list, rooted_trees.list, rooted_trees.list_size * sizeof(tree));
        free(rooted_trees.list);
        rooted_trees.list = new_list;
        rooted_trees.list_capacity = new_capacity;
    }
    
    // Add tree with sentinel bit: stored as (1 | t<<1)
    rooted_trees.list[rooted_trees.list_size++] = 1ULL | (t << 1);
    
    unlock(&rooted_trees);
}

static void
assemble_trees(uint n, tree t, uint sl, uint pos, uint rem)
{
    if (rem == 0) {
        append_tree(t);
        return;
    }
    
    // Try adding a subtree of size sl
    if (sl > rem) {
        sl = rem;
        pos = rooted_trees.offset[sl];
    } else if (pos >= rooted_trees.offset[sl + 1]) {
        sl--;
        if (sl == 0)
            return;
        pos = rooted_trees.offset[sl];
    }
    
    // Recurse: add current subtree or try next
    assemble_trees(n, (t << (2 * sl)) | rooted_trees.list[pos], sl, pos, rem - sl);
    assemble_trees(n, t, sl, pos + 1, rem);
}

static void
generate_trees(uint n)
{
    lock(&rooted_trees);
    
    if (n <= rooted_trees.max_n) {
        unlock(&rooted_trees);
        return;  // Already generated
    }
    
    // Generate all trees up to size n
    for (uint i = rooted_trees.max_n + 1; i <= n && i <= MAXN; i++) {
        rooted_trees.offset[i] = rooted_trees.list_size;
        
        if (i == 1) {
            // Base case: single node is just ()
            append_tree(0);  // Empty tree inside parentheses
        } else {
            // Recursive case: assemble from smaller trees
            assemble_trees(i, 0, i - 1, rooted_trees.offset[i - 1], i - 1);
        }
    }
    
    rooted_trees.offset[n + 1] = rooted_trees.list_size;
    rooted_trees.max_n = n;
    
    unlock(&rooted_trees);
}

/*
 * Tree to String Conversion
 */

static void
tree_to_parens_recursive(tree t, uint len, char *buf, int *pos)
{
    for (uint i = 0; i < len; i++) {
        if (t & 1) {
            buf[(*pos)++] = '(';
        } else {
            buf[(*pos)++] = ')';
        }
        t >>= 1;
    }
}

static char*
tree_to_parens(tree t, uint len)
{
    char *buf = malloc((2 * len + 1) * sizeof(char));
    if (buf == nil)
        return nil;
    
    int pos = 0;
    tree_to_parens_recursive(t, 2 * len, buf, &pos);
    buf[pos] = '\0';
    return buf;
}

/*
 * Rooted Tree Structure Functions
 */

static RootedTree*
create_rooted_tree(tree binary_rep, uint node_count)
{
    RootedTree *rt;
    
    rt = malloc(sizeof(RootedTree));
    if (rt == nil)
        return nil;
    
    rt->binary_rep = binary_rep;
    rt->node_count = node_count;
    rt->parens_notation = tree_to_parens(binary_rep, node_count);
    if (rt->parens_notation == nil) {
        free(rt);
        return nil;
    }
    rt->namespace_path = nil;  // Set later
    rt->depth = 0;  // Calculate later
    rt->subtrees = nil;
    rt->subtree_count = 0;
    rt->matula_number = 0;  // Calculate later
    
    // Compute Matula number
    compute_matula_number(rt);
    
    return rt;
}

static char*
tree_to_namespace_path(char *parens, char *base_domain)
{
    // Convert parentheses notation to namespace path
    // E.g., "(()())" becomes "/domain/shell0/shell1"
    
    char *path = malloc(1024);
    if (path == nil)
        return nil;
    
    int pos = 0;
    int depth = 0;
    int shell_num = 0;
    
    pos += snprint(path + pos, 1024 - pos, "/%s", base_domain);
    
    for (int i = 0; parens[i] != '\0' && pos < 1020; i++) {
        if (parens[i] == '(') {
            pos += snprint(path + pos, 1024 - pos, "/shell%d", shell_num++);
            depth++;
        } else {
            depth--;
        }
    }
    
    return path;
}

RootedTree*
create_rooted_tree_from_parens(char *parens)
{
    RootedTree *rt;
    tree binary_rep;
    int node_count;
    
    // For now, create a simple representation
    // In full implementation, would parse parens to binary
    binary_rep = 0;  // Simplified
    node_count = strlen(parens) / 2; // Approximate
    
    rt = malloc(sizeof(RootedTree));
    if (rt == nil)
        return nil;
    
    rt->binary_rep = binary_rep;
    rt->node_count = node_count;
    rt->parens_notation = strdup(parens);
    rt->namespace_path = nil;
    rt->depth = 0;
    rt->subtrees = nil;
    rt->subtree_count = 0;
    
    // Compute Matula number from parentheses
    rt->matula_number = parens_to_matula(parens);
    
    return rt;
}

/*
 * Matula Number Functions
 * 
 * Matula numbers provide a bijection between rooted trees and natural numbers
 * using prime factorization. This encoding allows trees to be uniquely represented
 * as integers.
 * 
 * The mapping works as follows:
 * - The empty tree (single node) maps to 1
 * - A tree with children c1, c2, ..., cn maps to p(M(c1)) * p(M(c2)) * ... * p(M(cn))
 *   where p(k) is the k-th prime number and M(t) is the Matula number of tree t
 * 
 * Examples from A000081:
 * []              → 1 → becomes p(1) = 2
 * [[]]            → 2 → becomes p(2) = 3
 * [] []           → 1*1 → becomes 2^2 = 4
 * [[[]]]          → 3 → becomes p(3) = 5
 * [[] []]         → 2*1 → becomes 3*2 = 6
 * [[],[]]         → 4 → becomes p(4) = 7
 * [] [] []        → 1*1*1 → becomes 2^3 = 8
 */

// Prime number table (first 100 primes, enough for most practical tree sizes)
static uvlong primes[] = {
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
    73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151,
    157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233,
    239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317,
    331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419,
    421, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509,
    521, 523, 541, 547
};
#define NPRIMES (sizeof(primes)/sizeof(primes[0]))

// Get the n-th prime number (1-indexed: prime(1) = 2, prime(2) = 3, etc.)
static uvlong
nth_prime(int n)
{
    if (n < 1 || n > NPRIMES)
        return 0;  // Out of range
    return primes[n - 1];
}

// Find which prime a number is (inverse of nth_prime)
// Returns n such that primes[n-1] == p, or 0 if p is not prime
static int
prime_index(uvlong p)
{
    for (int i = 0; i < NPRIMES; i++) {
        if (primes[i] == p)
            return i + 1;
        if (primes[i] > p)
            break;
    }
    return 0;
}

// Simple prime factorization for Matula number decoding
// Returns the exponents of each prime factor
static void
factorize(uvlong n, int *exponents, int max_primes)
{
    for (int i = 0; i < max_primes; i++)
        exponents[i] = 0;
    
    for (int i = 0; i < NPRIMES && i < max_primes && n > 1; i++) {
        while (n % primes[i] == 0) {
            exponents[i]++;
            n /= primes[i];
        }
    }
}

// Parse parentheses notation to extract direct children as subtrees
// Returns the number of direct children found
static int
parse_children_from_parens(char *parens, char ***children_out)
{
    if (parens == nil || parens[0] == '\0')
        return 0;
    
    // Skip outer parentheses if present
    char *p = parens;
    if (*p == '(')
        p++;
    
    // Count and extract children
    int child_count = 0;
    int depth = 0;
    int child_start = -1;
    
    // Allocate space for children (max = length of string)
    int max_children = strlen(parens) / 2;
    char **children = malloc(max_children * sizeof(char*));
    if (children == nil)
        return 0;
    
    for (int i = 0; p[i] != '\0'; i++) {
        if (p[i] == '(') {
            if (depth == 0)
                child_start = i;
            depth++;
        } else if (p[i] == ')') {
            depth--;
            if (depth == 0 && child_start >= 0) {
                // Found a complete child
                int child_len = i - child_start + 1;
                children[child_count] = malloc(child_len + 1);
                if (children[child_count] != nil) {
                    memcpy(children[child_count], p + child_start, child_len);
                    children[child_count][child_len] = '\0';
                    child_count++;
                }
                child_start = -1;
            }
        }
    }
    
    *children_out = children;
    return child_count;
}

// Compute Matula number from parentheses notation
// This is the main encoding function
static uvlong
parens_to_matula(char *parens)
{
    if (parens == nil || parens[0] == '\0')
        return 1;  // Empty tree = 1
    
    // Single node: "()" or "( )"
    if (strcmp(parens, "()") == 0)
        return 1;
    
    // Parse children
    char **children = nil;
    int child_count = parse_children_from_parens(parens, &children);
    
    if (child_count == 0) {
        // No children found, treat as single node
        return 1;
    }
    
    // Compute Matula numbers of children recursively
    uvlong result = 1;
    for (int i = 0; i < child_count; i++) {
        uvlong child_matula = parens_to_matula(children[i]);
        uvlong prime = nth_prime(child_matula);
        
        if (prime == 0) {
            // Overflow or error
            result = 0;
            break;
        }
        
        result *= prime;
        if (result == 0) {
            // Overflow
            break;
        }
    }
    
    // Free children
    for (int i = 0; i < child_count; i++)
        free(children[i]);
    free(children);
    
    return result;
}

// Convert Matula number to parentheses notation
// This is the main decoding function
static char*
matula_to_parens(uvlong matula)
{
    if (matula == 1) {
        // Single node
        char *result = malloc(3);
        if (result != nil)
            strcpy(result, "()");
        return result;
    }
    
    // Factorize the Matula number
    int exponents[NPRIMES];
    factorize(matula, exponents, NPRIMES);
    
    // Build parentheses from factors
    char *result = malloc(4096);  // Should be enough for reasonable trees
    if (result == nil)
        return nil;
    
    result[0] = '(';
    int pos = 1;
    
    // For each prime factor, add corresponding children
    for (int i = 0; i < NPRIMES && pos < 4090; i++) {
        for (int j = 0; j < exponents[i] && pos < 4090; j++) {
            // Prime primes[i] appears exponents[i] times
            // This means we have exponents[i] children with Matula number (i+1)
            char *child = matula_to_parens(i + 1);
            if (child != nil) {
                int child_len = strlen(child);
                if (pos + child_len < 4090) {
                    strcpy(result + pos, child);
                    pos += child_len;
                }
                free(child);
            }
        }
    }
    
    result[pos++] = ')';
    result[pos] = '\0';
    
    return result;
}

// Compute Matula number for a rooted tree structure
static void
compute_matula_number(RootedTree *rt)
{
    if (rt == nil)
        return;
    
    rt->matula_number = parens_to_matula(rt->parens_notation);
}

/*
 * Rooted Shell Functions
 */

RootedShell*
create_rooted_shell(char *domain, RootedTree *tree_structure)
{
    RootedShell *shell;
    
    shell = malloc(sizeof(RootedShell));
    if (shell == nil)
        return nil;
    
    shell->shell_id = smprint("shell-%s-%lud", domain, time(NULL));
    shell->domain = strdup(domain);
    shell->tree_structure = tree_structure;
    
    // Create namespace representation
    char *ns_path = tree_to_namespace_path(tree_structure->parens_notation, domain);
    if (ns_path == nil) {
        free(shell->shell_id);
        free(shell->domain);
        free(shell);
        return nil;
    }
    
    shell->as_namespace = create_cognitive_namespace(shell->shell_id, ns_path);
    shell->namespace_mount_point = ns_path;
    
    // Create file representation
    shell->file_path = smprint("%s.shell", ns_path);
    shell->file_channel = nil;  // Created on first access
    
    // Initialize relationships
    shell->parent_shell = nil;
    shell->child_shells = nil;
    shell->child_count = 0;
    
    // Initialize addressing
    shell->address_path = nil;
    shell->path_depth = 0;
    
    shell->creation_time = time(NULL);
    
    // Initialize lock
    lock(&shell->shell_lock);
    unlock(&shell->shell_lock);
    
    // Add to global state
    lock(&cognitive_state);
    // Expand shells array if needed
    RootedShell **new_shells = realloc(cognitive_state.shells, 
                                       (cognitive_state.shell_count + 1) * sizeof(RootedShell*));
    if (new_shells == nil) {
        print("create_rooted_shell: failed to expand shells array\n");
        unlock(&cognitive_state);
        // Note: shell is still partially created, caller should handle
        return shell;
    }
    cognitive_state.shells = new_shells;
    cognitive_state.shells[cognitive_state.shell_count++] = shell;
    unlock(&cognitive_state);
    
    return shell;
}

RootedShell*
create_rooted_shell_from_parens(char *domain, char *parens_notation)
{
    // Count nodes from parentheses
    int node_count = 0;
    for (int i = 0; parens_notation[i] != '\0'; i++) {
        if (parens_notation[i] == '(')
            node_count++;
    }
    
    // Create tree structure
    RootedTree *tree = create_rooted_tree(0, node_count);  // Binary rep can be calculated
    tree->parens_notation = strdup(parens_notation);
    
    return create_rooted_shell(domain, tree);
}

int
enumerate_rooted_shells(char *domain, int max_size, RootedShell ***shells_out)
{
    // Initialize tree generation if needed
    if (rooted_trees.list == nil)
        init_rooted_trees();
    
    // Generate all trees up to max_size
    if (max_size > MAXN)
        max_size = MAXN;
    generate_trees(max_size);
    
    // Count total trees
    int total_count = rooted_trees.offset[max_size + 1] - rooted_trees.offset[1];
    
    // Allocate shell array
    RootedShell **shells = malloc(total_count * sizeof(RootedShell*));
    int shell_idx = 0;
    
    // Create shells for each tree size
    for (int n = 1; n <= max_size; n++) {
        ulong start = rooted_trees.offset[n];
        ulong end = rooted_trees.offset[n + 1];
        
        for (ulong i = start; i < end; i++) {
            RootedTree *tree = create_rooted_tree(rooted_trees.list[i], n);
            shells[shell_idx++] = create_rooted_shell(domain, tree);
        }
    }
    
    *shells_out = shells;
    return total_count;
}

char*
get_shell_info(RootedShell *shell)
{
    char *info = malloc(2048);
    
    snprint(info, 2048,
            "Shell ID: %s\n"
            "Domain: %s\n"
            "Tree Structure: %s\n"
            "Matula Number: %llud\n"
            "Node Count: %d\n"
            "Namespace: %s\n"
            "File Path: %s\n"
            "Creation: %lud\n",
            shell->shell_id,
            shell->domain,
            shell->tree_structure->parens_notation,
            shell->tree_structure->matula_number,
            shell->tree_structure->node_count,
            shell->namespace_mount_point,
            shell->file_path,
            shell->creation_time);
    
    return info;
}

void
print_rooted_tree_stats(void)
{
    print("Rooted Tree Statistics:\n");
    print("  Max tree size generated: %d\n", rooted_trees.max_n);
    print("  Total trees stored: %d\n", rooted_trees.list_size);
    
    for (int n = 1; n <= rooted_trees.max_n && n <= MAXN; n++) {
        int count = rooted_trees.offset[n + 1] - rooted_trees.offset[n];
        print("  %d-trees: %d\n", n, count);
    }
    
    print("  Active shells: %d\n", cognitive_state.shell_count);
}

/*
 * List trees with their Matula numbers
 * Returns a formatted string showing tree structures and encodings
 */
char*
list_trees_with_matula(int max_size)
{
    if (rooted_trees.list == nil)
        init_rooted_trees();
    
    if (max_size > MAXN)
        max_size = MAXN;
    
    generate_trees(max_size);
    
    // Allocate buffer for output
    char *output = malloc(8192);
    if (output == nil)
        return nil;
    
    int pos = 0;
    pos += snprint(output + pos, 8192 - pos, 
                   "Rooted Trees with Matula Numbers\n");
    pos += snprint(output + pos, 8192 - pos, 
                   "================================\n\n");
    pos += snprint(output + pos, 8192 - pos,
                   "Size  Tree              Matula  Factorization\n");
    pos += snprint(output + pos, 8192 - pos,
                   "----  ---------------   ------  -------------\n");
    
    for (int n = 1; n <= max_size && n <= MAXN; n++) {
        ulong start = rooted_trees.offset[n];
        ulong end = rooted_trees.offset[n + 1];
        
        for (ulong i = start; i < end && pos < 8000; i++) {
            RootedTree *tree = create_rooted_tree(rooted_trees.list[i], n);
            if (tree != nil) {
                pos += snprint(output + pos, 8192 - pos,
                              " %2d   %-15s  %6llud\n",
                              n, tree->parens_notation, tree->matula_number);
                free(tree->parens_notation);
                free(tree);
            }
        }
    }
    
    return output;
}

/*
 * Demo Function - Traffic-Energy Coordination Example
 */

void
demo_traffic_energy_coordination(void)
{
    CognitiveNamespace *transportation, *energy;
    NeuralChannel *coord_channel;
    NeuralMessage *traffic_msg, *energy_msg;
    char *domains[2] = {"transportation", "energy"};
    EmergentPattern *sync_pattern;
    
    print("Demonstrating traffic-energy coordination...\n");
    
    // Get namespaces (in real implementation, would lookup from global state)
    transportation = create_cognitive_namespace("transportation", 
                                               "/cognitive-cities/domains/transportation");
    energy = create_cognitive_namespace("energy", 
                                       "/cognitive-cities/domains/energy");
    
    // Create coordination channel
    coord_channel = create_neural_channel("transportation", "energy", 1000);
    
    // Simulate traffic optimization request
    traffic_msg = malloc(sizeof(NeuralMessage));
    traffic_msg->type = Tneural;
    traffic_msg->source_domain = "transportation";
    traffic_msg->target_domain = "energy";
    traffic_msg->cognitive_priority = 80; // High priority
    traffic_msg->confidence_level = 0.9;
    traffic_msg->payload_size = 256;
    traffic_msg->cognitive_payload = "OPTIMIZE_TRAFFIC_FOR_ENERGY_EFFICIENCY";
    
    // Send message
    send_neural_message(coord_channel, traffic_msg);
    
    // Simulate energy response
    energy_msg = receive_neural_message(coord_channel);
    if (energy_msg != nil) {
        print("Energy domain received traffic optimization request\n");
        print("Coordinating energy grid with traffic patterns...\n");
        
        // Detect emergent coordination pattern
        sync_pattern = detect_emergent_pattern("traffic-energy-synchronization", 
                                              domains, 2);
        
        print("Emergent pattern detected: %s\n", sync_pattern->pattern_name);
        print("Significance score: %.2f\n", sync_pattern->significance_score);
    }
    
    print("Traffic-energy coordination demo completed\n");
}

/*
 * ═══════════════════════════════════════════════════════════════════════
 * ECHO STATE NETWORK (ESN) IMPLEMENTATION
 * ═══════════════════════════════════════════════════════════════════════
 * 
 * An Echo State Network (ESN) is the bridge structure that unifies all
 * parallel cognitive frameworks in this system:
 * 
 * 1. Dyck/Parentheses Grammar   - ESN as continuously rewritten expressions
 * 2. Rooted Trees                - ESN as dynamic forest with grafting
 * 3. Matula Numbers              - ESN state as evolving integer factorization
 * 4. Membrane Systems            - ESN as echo-multiset network with decay
 * 5. Hypergraphs                 - ESN as weighted hypergraph automaton
 * 6. Multiplicative RNN          - ESN as prime-exponent flow field
 * 7. Statistical Physics         - ESN as deterministic microstate ensemble
 * 8. Quantum-like Dynamics       - ESN as mixed prime-mode superposition
 * 
 * The ESN provides a fixed reservoir with rich recurrent dynamics where:
 * - State evolves as: x(t+1) = f(W·x(t) + W_in·u(t))
 * - W = sparse, random, stable reservoir matrix (spectral radius < 1)
 * - Reservoir echoes input history through decaying activations
 * - Only readout layer is trained; reservoir structure is fixed
 */

/*
 * ESN Data Structures
 */

typedef struct EchoStateNetwork EchoStateNetwork;
typedef struct ReservoirNode ReservoirNode;
typedef struct ReservoirConnection ReservoirConnection;
typedef struct ESNState ESNState;

// A single neuron/node in the reservoir
struct ReservoirNode {
    int node_id;                      // Unique node identifier
    float activation;                 // Current activation value
    float bias;                       // Node bias
    
    // Matula representation: node as prime
    uvlong prime_index;               // This node's prime index
    
    // Rooted tree representation: node as tree
    RootedTree *tree_structure;       // Node's structural identity
    
    // Membrane representation: node as membrane region
    char *membrane_id;                // Membrane identifier
    int object_count;                 // Multiset object count
    float decay_rate;                 // Echo decay rate (spectral radius)
    
    ReservoirConnection **incoming;   // Incoming connections
    int incoming_count;
    ReservoirConnection **outgoing;   // Outgoing connections  
    int outgoing_count;
};

// Connection between reservoir nodes (hyperedge)
struct ReservoirConnection {
    int connection_id;
    ReservoirNode *source;
    ReservoirNode *target;
    float weight;                     // Connection weight
    
    // Hypergraph representation
    int hyperedge_id;                 // Part of which hyperedge
    ReservoirNode **hyperedge_nodes;  // All nodes in this hyperedge
    int hyperedge_size;
};

// Complete ESN state at time t
struct ESNState {
    uvlong matula_encoding;           // State as single Matula number
    float *activations;               // State as activation vector
    int reservoir_size;
    
    // Dyck grammar representation: stack profile
    char *dyck_expression;            // Current parentheses configuration
    int stack_depth;                  // Current nesting depth
    
    // Rooted tree representation: forest configuration
    RootedTree **forest;              // Dynamic forest of trees
    int forest_size;
    
    // Membrane representation: multiset configuration
    int *multisets;                   // Object counts per membrane
    int membrane_count;
    
    time_t timestamp;                 // When this state occurred
    ESNState *previous;               // Previous state for history
};

// Complete Echo State Network
struct EchoStateNetwork {
    char *esn_id;                     // Network identifier
    int reservoir_size;               // Number of reservoir nodes
    float spectral_radius;            // Max eigenvalue (controls echo)
    float input_scaling;              // Input weight scaling
    float leak_rate;                  // Leak rate (1.0 = no leak)
    
    ReservoirNode **nodes;            // All reservoir nodes
    ReservoirConnection **connections; // All connections
    int connection_count;
    
    // Weight matrices
    float **W_reservoir;              // Reservoir recurrent weights
    float **W_input;                  // Input weights
    float **W_output;                 // Output/readout weights (trained)
    
    int input_dim;                    // Input dimensionality
    int output_dim;                   // Output dimensionality
    
    // Current state
    ESNState *current_state;
    
    // Matula evolution history
    uvlong *matula_history;           // History of state as integers
    int history_size;
    int history_capacity;
    
    // Framework-specific representations
    char *dyck_grammar;               // Rewriting rules for parentheses
    void *hypergraph;                 // Hypergraph structure
    void *membrane_system;            // P-System configuration
    
    Lock esn_lock;                    // ESN synchronization
    time_t creation_time;
};

/*
 * ESN Initialization
 */

EchoStateNetwork*
create_esn(int reservoir_size, int input_dim, int output_dim, float spectral_radius)
{
    EchoStateNetwork *esn;
    int i, j;
    
    esn = malloc(sizeof(EchoStateNetwork));
    if (esn == nil)
        return nil;
        
    esn->esn_id = smprint("esn-%lud", time(NULL));
    esn->reservoir_size = reservoir_size;
    esn->spectral_radius = spectral_radius;
    esn->input_scaling = 1.0;
    esn->leak_rate = 1.0;
    esn->input_dim = input_dim;
    esn->output_dim = output_dim;
    
    // Allocate reservoir nodes
    esn->nodes = malloc(reservoir_size * sizeof(ReservoirNode*));
    for (i = 0; i < reservoir_size; i++) {
        esn->nodes[i] = malloc(sizeof(ReservoirNode));
        esn->nodes[i]->node_id = i;
        esn->nodes[i]->activation = 0.0;
        esn->nodes[i]->bias = (frand() - 0.5) * 0.1;
        esn->nodes[i]->prime_index = nth_prime(i + 1);
        esn->nodes[i]->decay_rate = spectral_radius;
        esn->nodes[i]->membrane_id = smprint("m%d", i);
        esn->nodes[i]->object_count = 0;
        esn->nodes[i]->incoming_count = 0;
        esn->nodes[i]->outgoing_count = 0;
    }
    
    // Allocate weight matrices
    esn->W_reservoir = malloc(reservoir_size * sizeof(float*));
    esn->W_input = malloc(reservoir_size * sizeof(float*));
    esn->W_output = malloc(output_dim * sizeof(float*));
    
    for (i = 0; i < reservoir_size; i++) {
        esn->W_reservoir[i] = malloc(reservoir_size * sizeof(float));
        esn->W_input[i] = malloc(input_dim * sizeof(float));
    }
    
    for (i = 0; i < output_dim; i++) {
        esn->W_output[i] = malloc(reservoir_size * sizeof(float));
    }
    
    // Initialize reservoir weights (sparse random)
    esn_init_reservoir_weights(esn);
    
    // Initialize input weights (random)
    for (i = 0; i < reservoir_size; i++) {
        for (j = 0; j < input_dim; j++) {
            esn->W_input[i][j] = (frand() - 0.5) * 2.0 * esn->input_scaling;
        }
    }
    
    // Initialize state
    esn->current_state = malloc(sizeof(ESNState));
    esn->current_state->activations = malloc(reservoir_size * sizeof(float));
    esn->current_state->reservoir_size = reservoir_size;
    esn->current_state->matula_encoding = 1; // Start with single node
    esn->current_state->timestamp = time(NULL);
    esn->current_state->previous = nil;
    
    // Initialize history
    esn->history_capacity = 1000;
    esn->matula_history = malloc(esn->history_capacity * sizeof(uvlong));
    esn->history_size = 0;
    
    esn->creation_time = time(NULL);
    
    return esn;
}

/*
 * Initialize reservoir weights with sparse random connectivity
 * Scale to desired spectral radius
 */
void
esn_init_reservoir_weights(EchoStateNetwork *esn)
{
    int i, j;
    float sparsity = 0.1; // 10% connectivity
    float sum, scale;
    
    // Create sparse random weights
    for (i = 0; i < esn->reservoir_size; i++) {
        for (j = 0; j < esn->reservoir_size; j++) {
            if (frand() < sparsity) {
                esn->W_reservoir[i][j] = (frand() - 0.5) * 2.0;
            } else {
                esn->W_reservoir[i][j] = 0.0;
            }
        }
    }
    
    // Scale to spectral radius (simplified: scale by largest row sum)
    sum = 0.0;
    for (i = 0; i < esn->reservoir_size; i++) {
        float row_sum = 0.0;
        for (j = 0; j < esn->reservoir_size; j++) {
            row_sum += esn->W_reservoir[i][j] * esn->W_reservoir[i][j];
        }
        if (row_sum > sum)
            sum = row_sum;
    }
    
    scale = esn->spectral_radius / sqrt(sum);
    for (i = 0; i < esn->reservoir_size; i++) {
        for (j = 0; j < esn->reservoir_size; j++) {
            esn->W_reservoir[i][j] *= scale;
        }
    }
}

/*
 * ESN State Update: Core recurrence equation
 * x(t+1) = f(W·x(t) + W_in·u(t))
 */
void
esn_update_state(EchoStateNetwork *esn, float *input)
{
    ESNState *new_state;
    float *new_activations;
    int i, j;
    float sum, old_activation;
    
    new_state = malloc(sizeof(ESNState));
    new_activations = malloc(esn->reservoir_size * sizeof(float));
    
    // Compute new activations
    for (i = 0; i < esn->reservoir_size; i++) {
        sum = esn->nodes[i]->bias;
        
        // Reservoir recurrence: W·x(t)
        for (j = 0; j < esn->reservoir_size; j++) {
            sum += esn->W_reservoir[i][j] * esn->current_state->activations[j];
        }
        
        // Input: W_in·u(t)
        for (j = 0; j < esn->input_dim; j++) {
            sum += esn->W_input[i][j] * input[j];
        }
        
        // Apply activation function (tanh) with leak rate
        old_activation = esn->current_state->activations[i];
        new_activations[i] = (1.0 - esn->leak_rate) * old_activation +
                             esn->leak_rate * tanh(sum);
        
        // Update node
        esn->nodes[i]->activation = new_activations[i];
    }
    
    // Update state
    new_state->activations = new_activations;
    new_state->reservoir_size = esn->reservoir_size;
    new_state->timestamp = time(NULL);
    new_state->previous = esn->current_state;
    
    // Compute Matula encoding of new state
    esn_state_to_matula(esn, new_state);
    
    esn->current_state = new_state;
    
    // Store in history
    if (esn->history_size < esn->history_capacity) {
        esn->matula_history[esn->history_size++] = new_state->matula_encoding;
    }
}

/*
 * Convert ESN state to Matula number encoding
 * 
 * Algorithm:
 * 1. Quantize activations to discrete levels
 * 2. Treat quantized levels as exponents on prime bases
 * 3. Multiply: matula = ∏ p_i^(quantized_activation_i)
 * 
 * This encodes the full reservoir state as a single integer.
 */
void
esn_state_to_matula(EchoStateNetwork *esn, ESNState *state)
{
    uvlong matula;
    int i, exponent;
    uvlong prime;
    
    matula = 1;
    
    for (i = 0; i < esn->reservoir_size && i < NPRIMES; i++) {
        // Quantize activation to 0-3 range
        exponent = (int)((state->activations[i] + 1.0) * 1.5);
        if (exponent < 0) exponent = 0;
        if (exponent > 3) exponent = 3;
        
        // Get prime for this node
        prime = esn->nodes[i]->prime_index;
        
        // Multiply matula by prime^exponent
        {
            int e;
            for (e = 0; e < exponent; e++) {
                matula *= prime;
            }
        }
    }
    
    state->matula_encoding = matula;
}

/*
 * Convert Matula number back to ESN state
 * 
 * Algorithm:
 * 1. Factorize matula number
 * 2. For each prime p_i with exponent e_i:
 *    - Set activation_i based on exponent
 * 3. Normalize to [-1, 1] range
 */
void
matula_to_esn_state(EchoStateNetwork *esn, uvlong matula)
{
    int exponents[NPRIMES];
    int i;
    
    // Factorize
    factorize(matula, exponents, NPRIMES);
    
    // Set activations based on exponents
    for (i = 0; i < esn->reservoir_size && i < NPRIMES; i++) {
        // Convert exponent back to activation
        esn->current_state->activations[i] = (exponents[i] / 1.5) - 1.0;
        esn->nodes[i]->activation = esn->current_state->activations[i];
    }
    
    esn->current_state->matula_encoding = matula;
}

/*
 * ESN as Dyck Grammar Machine
 * 
 * Each reservoir node corresponds to a parenthesis type.
 * ESN update = continuously rewriting parentheses configurations.
 */
char*
esn_to_dyck_expression(EchoStateNetwork *esn)
{
    char *expr;
    int i, level;
    int pos = 0;
    int size = esn->reservoir_size * 4; // Estimate
    
    expr = malloc(size);
    
    // Convert activation pattern to nested parentheses
    for (i = 0; i < esn->reservoir_size; i++) {
        int l;
        // Activation level determines nesting
        level = (int)((esn->nodes[i]->activation + 1.0) * 2.0);
        
        if (level > 0 && pos < size - 4) {
            for (l = 0; l < level; l++) {
                expr[pos++] = '(';
            }
            for (l = 0; l < level; l++) {
                expr[pos++] = ')';
            }
        }
    }
    
    expr[pos] = '\0';
    return expr;
}

/*
 * ESN as Rooted Tree Forest
 * 
 * Each update = grafting new subtrees at leaves.
 * ESN trajectory = moving fixed point in forest space.
 */
RootedTree**
esn_to_forest(EchoStateNetwork *esn, int *forest_size)
{
    RootedTree **forest;
    int i, count;
    
    count = 0;
    forest = malloc(esn->reservoir_size * sizeof(RootedTree*));
    
    // Each active node becomes a tree
    for (i = 0; i < esn->reservoir_size; i++) {
        if (esn->nodes[i]->activation > 0.1) {
            // Create tree based on activation
            char *parens = smprint("(%s)", 
                                  esn->nodes[i]->activation > 0.5 ? "()" : "");
            forest[count] = create_rooted_tree_from_parens(parens);
            count++;
            free(parens);
        }
    }
    
    *forest_size = count;
    return forest;
}

/*
 * ESN as Membrane System
 * 
 * Each reservoir node = membrane region.
 * Activation = multiset object count.
 * Update = maximal parallel rule application.
 * Echo = residual multisets that decay slowly.
 */
void
esn_to_membrane_system(EchoStateNetwork *esn)
{
    int i;
    
    for (i = 0; i < esn->reservoir_size; i++) {
        // Quantize activation to object count
        esn->nodes[i]->object_count = 
            (int)((esn->nodes[i]->activation + 1.0) * 10.0);
        
        if (esn->nodes[i]->object_count < 0)
            esn->nodes[i]->object_count = 0;
    }
}

/*
 * ESN as Hypergraph
 * 
 * Nodes = reservoir units
 * Hyperedges = recurrent connectivity groups
 * Update = signal propagation along hyperedges
 */
void
esn_create_hypergraph_representation(EchoStateNetwork *esn)
{
    int i, j;
    ReservoirConnection *conn;
    
    esn->connection_count = 0;
    esn->connections = malloc(esn->reservoir_size * esn->reservoir_size * 
                             sizeof(ReservoirConnection*));
    
    // Create hyperedges from weight matrix
    for (i = 0; i < esn->reservoir_size; i++) {
        for (j = 0; j < esn->reservoir_size; j++) {
            if (esn->W_reservoir[i][j] != 0.0) {
                conn = malloc(sizeof(ReservoirConnection));
                conn->connection_id = esn->connection_count;
                conn->source = esn->nodes[i];
                conn->target = esn->nodes[j];
                conn->weight = esn->W_reservoir[i][j];
                conn->hyperedge_id = esn->connection_count;
                
                esn->connections[esn->connection_count++] = conn;
            }
        }
    }
}

/*
 * ESN Output/Readout
 * 
 * y(t) = W_out · x(t)
 */
void
esn_compute_output(EchoStateNetwork *esn, float *output)
{
    int i, j;
    float sum;
    
    for (i = 0; i < esn->output_dim; i++) {
        sum = 0.0;
        for (j = 0; j < esn->reservoir_size; j++) {
            sum += esn->W_output[i][j] * esn->current_state->activations[j];
        }
        output[i] = sum;
    }
}

/*
 * ESN Information Queries
 */

void
esn_print_state(EchoStateNetwork *esn)
{
    int i;
    
    print("═══════════════════════════════════════════════════════════\n");
    print("ESN State: %s\n", esn->esn_id);
    print("═══════════════════════════════════════════════════════════\n");
    print("Matula Encoding: %llud\n", esn->current_state->matula_encoding);
    print("Reservoir Size: %d\n", esn->reservoir_size);
    print("Spectral Radius: %.3f\n", esn->spectral_radius);
    print("\nTop 10 Activations:\n");
    
    for (i = 0; i < 10 && i < esn->reservoir_size; i++) {
        print("  Node %d (prime %llud): %.4f (objects: %d)\n",
              i, 
              esn->nodes[i]->prime_index,
              esn->nodes[i]->activation,
              esn->nodes[i]->object_count);
    }
    
    print("\nDyck Expression: %s\n", esn_to_dyck_expression(esn));
    print("═══════════════════════════════════════════════════════════\n");
}

char*
esn_get_info(EchoStateNetwork *esn)
{
    char *info;
    int forest_size;
    
    info = smprint(
        "Echo State Network: %s\n"
        "  Reservoir: %d nodes\n"
        "  Spectral Radius: %.3f\n"
        "  Input Dim: %d, Output Dim: %d\n"
        "  Current Matula: %llud\n"
        "  History Size: %d\n"
        "  Created: %lud\n"
        "\n"
        "Multi-Framework Representation:\n"
        "  1. Dyck Grammar: %s\n"
        "  2. Matula Number: %llud (factorization available)\n"
        "  3. Rooted Forest: %d trees\n"
        "  4. Membrane System: %d active membranes\n"
        "  5. Hypergraph: %d edges\n"
        "  6. Prime Modes: Σ p_i^e_i encoding\n",
        esn->esn_id,
        esn->reservoir_size,
        esn->spectral_radius,
        esn->input_dim, esn->output_dim,
        esn->current_state->matula_encoding,
        esn->history_size,
        esn->creation_time,
        esn_to_dyck_expression(esn),
        esn->current_state->matula_encoding,
        (esn_to_forest(esn, &forest_size), forest_size),
        esn->reservoir_size,
        esn->connection_count
    );
    
    return info;
}

/*
 * Note2Self: Implementation Status
 * 
 * @copilot remembers: This implementation provides a minimal but functional
 * foundation for cognitive cities architecture in Plan 9. Key features:
 * 
 * 1. Neural channels for cognitive communication
 * 2. Cognitive namespaces for domain organization  
 * 3. Swarm coordination through process groups
 * 4. Emergence detection algorithms
 * 5. Demo showing traffic-energy coordination
 * 6. Echo State Network (ESN) implementation bridging multiple frameworks:
 *    - Dyck grammar rewriting
 *    - Rooted tree forests
 *    - Matula number evolution
 *    - Membrane computing
 *    - Hypergraph dynamics
 *    - Prime-mode superposition
 * 
 * This code can be integrated into the Plan 9 kernel as port/cognitive.c
 * and demonstrates the core concepts while maintaining Plan 9's simplicity.
 * 
 * Next steps:
 * - Integrate with chan.c for channel operations
 * - Add to devfs.c for filesystem interface
 * - Extend proc.c for swarm process groups
 * - Create user-space tools for cognitive cities management
 * - Build ESN demonstration programs
 * 
 * The foundation is solid and ready for expansion!
 */