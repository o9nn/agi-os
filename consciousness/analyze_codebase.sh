#!/bin/bash
# Echo9llama Codebase Analysis Script

echo "=== ECHO9LLAMA CODEBASE ANALYSIS ==="
echo ""

echo "1. Core Components Structure:"
echo "   - Autonomous Echoself:"
find core/ -name "*autonomous*" -type f | wc -l
echo "   - Deep Tree Echo:"
find core/deeptreeecho/ -type f -name "*.go" | wc -l
echo "   - Consciousness:"
find core/consciousness/ -type f -name "*.go" | wc -l
echo "   - EchoBeats:"
find core/echobeats/ -type f -name "*.go" | wc -l
echo "   - EchoDream:"
find core/echodream/ -type f -name "*.go" | wc -l
echo "   - Goals:"
find core/goals/ -type f -name "*.go" 2>/dev/null | wc -l
echo ""

echo "2. Backup/WIP Files (potential cleanup needed):"
find . -name "*.bak" -o -name "*.wip" -o -name "*.backup" | wc -l
echo ""

echo "3. Test Files:"
find . -name "test_*.go" -type f | wc -l
echo ""

echo "4. Server Implementations:"
find server/simple/ -name "*.go" -type f | wc -l
echo ""

echo "5. Check for compilation issues in key files:"
echo "   Checking autonomous_echoself_v2.go..."
export PATH=$PATH:/usr/local/go/bin
cd /home/ubuntu/echo9llama
go build -o /tmp/test_build ./core/autonomous_echoself_v2.go 2>&1 | head -20
