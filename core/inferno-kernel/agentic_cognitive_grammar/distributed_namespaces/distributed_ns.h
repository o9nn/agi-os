#ifndef DISTRIBUTED_NS_H
#define DISTRIBUTED_NS_H

#include "lib9.h"

// Distributed namespace structures
typedef struct DistributedNamespace DistributedNamespace;
typedef struct AgentNode AgentNode;
typedef struct ResourceMapping ResourceMapping;
typedef struct NamespaceProtocol NamespaceProtocol;

struct DistributedNamespace {
    int id;
    char *name;
    AgentNode *nodes;
    ResourceMapping *resources;
    NamespaceProtocol *protocols;
    DistributedNamespace *next;
};

struct AgentNode {
    int id;
    char *address;
    int port;
    char *capabilities;
    AgentNode *next;
};

struct ResourceMapping {
    char *resource_name;
    int node_id;
    char *resource_type;
    void *resource_data;
    ResourceMapping *next;
};

struct NamespaceProtocol {
    char *name;
    int (*handler)(void*, void*);
    NamespaceProtocol *next;
};

// Function declarations
DistributedNamespace* create_distributed_namespace(int id, char *name);
int add_agent_node(DistributedNamespace *ns, int id, char *address, int port, char *capabilities);
ResourceMapping* find_resource_in_namespace(DistributedNamespace *ns, char *resource_name);
int register_namespace_protocol(DistributedNamespace *ns, char *name, int (*handler)(void*, void*));
int execute_namespace_protocol(DistributedNamespace *ns, char *protocol_name, void *arg1, void *arg2);
int join_distributed_namespace(DistributedNamespace *ns, char *node_address, int node_port);
int sync_namespace_state(DistributedNamespace *ns);
void free_distributed_namespace(DistributedNamespace *ns);

#endif // DISTRIBUTED_NS_H