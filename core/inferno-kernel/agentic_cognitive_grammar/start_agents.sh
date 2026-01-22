#!/bin/sh
echo "Starting Distributed Agentic Cognitive Grammar System..."
export AGENTIC_COGNITIVE_GRAMMAR_HOME=$(pwd)
export AGENTIC_NODE_ID=${AGENTIC_NODE_ID:-1}
export AGENTIC_NAMESPACE=${AGENTIC_NAMESPACE:-"cognitive_grammar_ns"}
export AGENTIC_PORT=${AGENTIC_PORT:-8080}
if [ ! -f "/dis/agentic_limbo.dis" ]; then
echo "Building agentic cognitive grammar components..."
cd agentic_cognitive_grammar
mk install
cd ..
fi
echo "Starting distributed namespace: $AGENTIC_NAMESPACE"
echo "Node ID: $AGENTIC_NODE_ID"
echo "Port: $AGENTIC_PORT"
cat > /tmp/agentic_ns.conf << EOF
namespace: $AGENTIC_NAMESPACE
node_id: $AGENTIC_NODE_ID
port: $AGENTIC_PORT
capabilities: tensor_ops,neural_grammar,distributed_namespace
EOF
echo "Starting agentic cognitive grammar agents..."
/dis/agentic_limbo.dis /tmp/agentic_ns.conf &
echo "Starting tensor operation daemon..."
/dis/tensor_ops.dis &
echo "Starting neural grammar parser..."
/dis/neural_grammar.dis &
echo "Starting distributed namespace daemon..."
/dis/distributed_ns.dis &
sleep 2
echo "Checking component status..."
ps aux | grep -E "(agentic_limbo|tensor_ops|neural_grammar|distributed_ns)" | grep -v grep
echo "Distributed Agentic Cognitive Grammar System started successfully!"
echo "Components running:"
echo "  - Agentic Limbo System"
echo "  - GGML Tensor Kernels"
echo "  - NYACC Neural Grammar"
echo "  - Distributed Namespaces"
echo "  - Dis VM Extensions"
echo "Starting system monitoring..."
/dis/monitor_agentic.dis &
echo "System ready for distributed cognitive processing!"