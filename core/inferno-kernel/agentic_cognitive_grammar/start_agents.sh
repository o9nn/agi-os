#!/bin/sh

# Startup script for distributed agentic cognitive grammar system

echo "Starting Distributed Agentic Cognitive Grammar System..."

# Set environment variables
export AGENTIC_COGNITIVE_GRAMMAR_HOME=$(pwd)
export AGENTIC_NODE_ID=${AGENTIC_NODE_ID:-1}
export AGENTIC_NAMESPACE=${AGENTIC_NAMESPACE:-"cognitive_grammar_ns"}
export AGENTIC_PORT=${AGENTIC_PORT:-8080}

# Check if Inferno is available
if [ ! -f "/dis/agentic_limbo.dis" ]; then
    echo "Building agentic cognitive grammar components..."
    cd agentic_cognitive_grammar
    mk install
    cd ..
fi

# Start distributed namespace
echo "Starting distributed namespace: $AGENTIC_NAMESPACE"
echo "Node ID: $AGENTIC_NODE_ID"
echo "Port: $AGENTIC_PORT"

# Create namespace configuration
cat > /tmp/agentic_ns.conf << EOF
namespace: $AGENTIC_NAMESPACE
node_id: $AGENTIC_NODE_ID
port: $AGENTIC_PORT
capabilities: tensor_ops,neural_grammar,distributed_namespace
EOF

# Start agentic cognitive grammar system
echo "Starting agentic cognitive grammar agents..."

# Start main agentic system
/dis/agentic_limbo.dis /tmp/agentic_ns.conf &

# Start tensor operation daemon
echo "Starting tensor operation daemon..."
/dis/tensor_ops.dis &

# Start neural grammar parser
echo "Starting neural grammar parser..."
/dis/neural_grammar.dis &

# Start distributed namespace daemon
echo "Starting distributed namespace daemon..."
/dis/distributed_ns.dis &

# Wait for all components to start
sleep 2

# Check if all components are running
echo "Checking component status..."
ps aux | grep -E "(agentic_limbo|tensor_ops|neural_grammar|distributed_ns)" | grep -v grep

echo "Distributed Agentic Cognitive Grammar System started successfully!"
echo "Components running:"
echo "  - Agentic Limbo System"
echo "  - GGML Tensor Kernels"
echo "  - NYACC Neural Grammar"
echo "  - Distributed Namespaces"
echo "  - Dis VM Extensions"

# Start monitoring
echo "Starting system monitoring..."
/dis/monitor_agentic.dis &

echo "System ready for distributed cognitive processing!"