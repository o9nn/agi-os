#!/bin/bash
set -e
echo "GNU Mach DTrace Framework Demonstration"
echo "======================================="
echo
if [ ! -f "dtrace-analyze.c" ]; then
echo "Error: Please run this script from the tools/ directory"
exit 1
fi
echo "1. Building analysis tools..."
make clean && make all
echo "   ✓ Analysis tools built successfully"
echo
echo "2. Running analysis tool demonstration..."
echo "   This shows how probe data would be analyzed:"
echo
./dtrace-analyze -v -n 10
echo
echo "3. Running visualization tool demonstration..."
echo "   This shows timeline and frequency analysis:"
echo
./dtrace-visualize.py --demo --all
echo
echo "4. Testing different visualization options..."
echo "   Timeline with custom width:"
./dtrace-visualize.py --demo --timeline -w 50
echo
echo "   Performance metrics only:"
./dtrace-visualize.py --demo --performance
echo
echo "5. Framework capabilities summary:"
echo "   ✓ Dynamic probe registration and management"
echo "   ✓ High-resolution timestamp collection"
echo "   ✓ Thread-safe operation with IRQ-safe locks"
echo "   ✓ Circular buffer with overflow protection"
echo "   ✓ Performance metrics tracking"
echo "   ✓ Multiple probe types (function, syscall, IPC, VM, thread)"
echo "   ✓ Zero overhead when disabled"
echo "   ✓ Analysis and visualization tools"
echo
echo "6. Current instrumentation points:"
echo "   - thread_invoke() in kern/sched_prim.c (thread switching)"
echo "   - mach_msg_trap() in ipc/mach_msg.c (IPC messages)"
echo "   - vm_fault() in vm/vm_fault.c (memory faults)"
echo
echo "7. Usage examples:"
echo "
echo "   ./dtrace-analyze"
echo
echo "
echo "   ./dtrace-analyze -v -n 500"
echo
echo "
echo "   ./dtrace-visualize.py --demo --all"
echo
echo "
echo "   ./dtrace-visualize.py --demo --timeline -w 100"
echo
echo "Demonstration complete!"
echo
echo "Note: This demonstration uses sample data. In a real kernel environment,"
echo "the tools would interface with the actual DTrace framework to read"
echo "live probe data from the kernel."
echo
echo "For more information, see docs/DTRACE_README.md"