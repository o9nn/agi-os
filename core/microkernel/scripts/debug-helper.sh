#!/bin/bash
set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
find_testable_kernel() {
    local kernel_paths=(
        "${PROJECT_ROOT}/gnumach"
        "${PROJECT_ROOT}/build-i686/gnumach"  
        "${PROJECT_ROOT}/build-i686-debug/gnumach"
        "${PROJECT_ROOT}/build-x86_64/gnumach"
    )
    for kernel in "${kernel_paths[@]}"; do
        if [[ -f "$kernel" ]]; then
            local arch_info=$(file "$kernel" | grep -o "ELF [0-9]*-bit" || echo "unknown")
            if [[ "$arch_info" == *"32-bit"* ]]; then
                echo "$kernel"
                return 0
            fi
        fi
    done
    return 1
}
create_minimal_test_setup() {
    local test_kernel="${PROJECT_ROOT}/test-gnumach-debug"
    cat > /tmp/minimal-kernel.c << 'EOF'
/* Minimal multiboot kernel for GDB testing */
struct multiboot_header {
    unsigned int magic;
    unsigned int flags; 
    unsigned int checksum;
};
__attribute__((section(".multiboot")))
struct multiboot_header mb_header = {
    MULTIBOOT_MAGIC,
    MULTIBOOT_FLAGS,
    -(MULTIBOOT_MAGIC + MULTIBOOT_FLAGS)
};
void kernel_main(void) {
    /* Simple loop that can be debugged with GDB */
    volatile int counter = 0;
    for (int i = 0; i < 10; i++) {
        counter += i;
    }
    /* Infinite loop to keep kernel running */
    while (1) {
        __asm__ volatile ("hlt");
    }
}
void _start(void) {
    kernel_main();
}
EOF
    cat > /tmp/kernel.ld << 'EOF'
ENTRY(_start)
SECTIONS {
    . = 1M;
    .multiboot ALIGN(4K) : {
        *(.multiboot)
    }
    .text ALIGN(4K) : {
        *(.text)
    }
    .data ALIGN(4K) : {
        *(.data)
    }
    .bss ALIGN(4K) : {
        *(.bss)
    }
}
EOF
    if gcc -m32 -ffreestanding -nostdlib -g -O1 -T /tmp/kernel.ld \
        -o "$test_kernel" /tmp/minimal-kernel.c 2>/dev/null; then
        echo "$test_kernel"
        return 0
    else
        return 1
    fi
}
main() {
    echo "=== GNU Mach Debugging Helper ==="
    echo "Finding suitable kernel for debugging tests..."
    local kernel
    if kernel=$(find_testable_kernel); then
        echo "Found suitable kernel: $kernel"
    else
        echo "No suitable 32-bit kernel found."
        echo "Creating minimal test setup for debugging..."
        if ! kernel=$(create_minimal_test_setup); then
            echo "Error: Could not create test setup"
            echo ""
            echo "To build a proper GNU Mach kernel for debugging:"
            echo "1. Install dependencies:"
            echo "   sudo apt install gcc-multilib build-essential autoconf automake"
            echo "2. Build MIG if not available:"
            echo "   cd mig && autoreconf -i && ./configure && make && sudo make install && cd .."
            echo "3. Configure and build i686 kernel:"
            echo "   mkdir -p build-i686-debug"
            echo "   cd build-i686-debug"
            echo "   ../configure --host=i686-gnu CC='gcc -m32' LD='ld -melf_i386' --enable-kdb"
            echo "   make"
            exit 1
        fi
    fi
    echo ""
    echo "=== Testing Debugging Setup ==="
    echo "Kernel: $kernel"
    echo "Testing QEMU+GDB setup script..."
    "${PROJECT_ROOT}/scripts/setup-qemu-gdb.sh" -k "$kernel" -n &
    local qemu_pid=$!
    sleep 3
    kill $qemu_pid 2>/dev/null || true
    wait $qemu_pid 2>/dev/null || true
    echo ""
    echo "=== Debugging Setup Ready ==="
    echo "To start debugging:"
    echo "1. Terminal 1: ./scripts/setup-qemu-gdb.sh -k '$kernel'"
    echo "2. Terminal 2: gdb -x debug.gdb"
    echo ""
    echo "For building a full GNU Mach kernel, see the documentation in:"
    echo "- docs/debugging-guide.md"
    echo "- .github/copilot-instructions.md"
}
main "$@"