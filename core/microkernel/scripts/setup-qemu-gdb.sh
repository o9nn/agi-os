#!/bin/bash
set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
GNUMACH_BINARY="${PROJECT_ROOT}/gnumach"
GDB_PORT=${GDB_PORT:-1234}
QEMU_ARGS=${QEMU_ARGS:-}
usage() {
    cat << EOF
Usage: $0 [OPTIONS]
Sets up QEMU with GDB debugging for GNU Mach kernel development.
OPTIONS:
    -k, --kernel PATH       Path to GNU Mach kernel binary (default: ./gnumach)
    -p, --port PORT         GDB server port (default: 1234)
    -a, --args ARGS         Additional QEMU arguments
    -m, --memory SIZE       Memory size for VM (default: 128M)
    -n, --no-gdb           Start QEMU without waiting for GDB
    -h, --help             Show this help message
EXAMPLES:
    $0
    $0 -k /path/to/gnumach -p 1235
    $0 -m 256M -a "-enable-kvm"
    $0 -n
DEBUGGING WORKFLOW:
    1. Run this script in one terminal
    2. In another terminal, run:
       gdb gnumach
       (gdb) target remote localhost:1234
       (gdb) continue
EOF
}
MEMORY="128M"
WAIT_GDB=true
while [[ $
    case $1 in
        -k|--kernel)
            GNUMACH_BINARY="$2"
            shift 2
            ;;
        -p|--port)
            GDB_PORT="$2"
            shift 2
            ;;
        -a|--args)
            QEMU_ARGS="$2"
            shift 2
            ;;
        -m|--memory)
            MEMORY="$2"
            shift 2
            ;;
        -n|--no-gdb)
            WAIT_GDB=false
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage >&2
            exit 1
            ;;
    esac
done
if [[ ! -f "$GNUMACH_BINARY" ]]; then
    echo "Error: GNU Mach kernel not found at: $GNUMACH_BINARY" >&2
    echo "Please build the kernel first or specify correct path with -k" >&2
    exit 1
fi
KERNEL_ARCH=$(file "$GNUMACH_BINARY" | grep -o "ELF [0-9]*-bit" | awk '{print $2}' | sed 's/-bit//')
if [[ "$KERNEL_ARCH" == "64" ]]; then
    QEMU_SYSTEM="qemu-system-x86_64"
    echo "Detected 64-bit kernel, using qemu-system-x86_64"
else
    QEMU_SYSTEM="qemu-system-i386"
    echo "Detected 32-bit kernel, using qemu-system-i386"
fi
if ! command -v "$QEMU_SYSTEM" &> /dev/null; then
    echo "Error: $QEMU_SYSTEM not found" >&2
    echo "Please install QEMU system emulation for your kernel architecture" >&2
    if [[ "$KERNEL_ARCH" == "64" ]]; then
        echo "  sudo apt-get install qemu-system-x86"
    else
        echo "  sudo apt-get install qemu-system-x86"
    fi
    exit 1
fi
echo "=== GNU Mach QEMU+GDB Debugging Setup ==="
echo "Kernel: $GNUMACH_BINARY"
echo "GDB Port: $GDB_PORT"
echo "Memory: $MEMORY"
echo "Wait for GDB: $WAIT_GDB"
echo
GDB_SCRIPT="${PROJECT_ROOT}/debug.gdb"
cat > "$GDB_SCRIPT" << EOF
file $GNUMACH_BINARY
target remote localhost:$GDB_PORT
set disassembly-flavor intel
set print pretty on
define debug-info
    info registers
    x/10i \$pc
end
echo \\n=== GNU Mach debugging session started ===\\n
echo Use 'debug-info' to show registers and disassembly\\n
echo Use 'continue' to start the kernel\\n
echo Use 'break function_name' to set breakpoints\\n
EOF
echo "Created GDB script: $GDB_SCRIPT"
QEMU_CMD="$QEMU_SYSTEM"
QEMU_CMD="$QEMU_CMD -m $MEMORY"
QEMU_CMD="$QEMU_CMD -kernel $GNUMACH_BINARY"
QEMU_CMD="$QEMU_CMD -nographic"
if [[ "$WAIT_GDB" == "true" ]]; then
    QEMU_CMD="$QEMU_CMD -s -S"
    echo "QEMU will wait for GDB connection on port $GDB_PORT"
    echo "To connect with GDB, run in another terminal:"
    echo "  gdb -x $GDB_SCRIPT"
    echo "  or manually:"
    echo "  gdb $GNUMACH_BINARY"
    echo "  (gdb) target remote localhost:$GDB_PORT"
    echo "  (gdb) continue"
else
    echo "Starting QEMU without GDB (testing mode)"
fi
if [[ -n "$QEMU_ARGS" ]]; then
    QEMU_CMD="$QEMU_CMD $QEMU_ARGS"
fi
echo
echo "Starting QEMU with command:"
echo "$QEMU_CMD"
echo
exec $QEMU_CMD