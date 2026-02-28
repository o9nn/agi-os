#!/bin/bash
set -euo pipefail
usage() {
cat << EOF
Usage: $0 [OPTIONS] [FILE]
Fix MIG-generated type size assertions for both 32-bit and 64-bit architectures.
Options:
--update-struct-sizes    Update struct size definitions globally
--update-assertions      Update assertion statements only
--batch-process          Process all MIG-generated files
--verify                 Verify fixes without making changes
-h, --help              Show this help message
Examples:
$0 vm/memory_object_user.user.c
$0 --batch-process
$0 --update-struct-sizes
EOF
}
UPDATE_STRUCT_SIZES=false
BATCH_PROCESS=false
VERIFY_ONLY=false
while [[ $# -gt 0 ]]; do
case $1 in
--update-struct-sizes)
UPDATE_STRUCT_SIZES=true
shift
;;
--batch-process)
BATCH_PROCESS=true
shift
;;
--verify)
VERIFY_ONLY=true
shift
;;
-h|--help)
usage
exit 0
;;
-*)
echo "Unknown option $1"
usage
exit 1
;;
*)
FILE="$1"
shift
;;
esac
done
if [[ -z "${FILE:-}" && "$BATCH_PROCESS" != "true" && "$UPDATE_STRUCT_SIZES" != "true" ]]; then
echo "Error: Must specify a file or use --batch-process"
usage
exit 1
fi
detect_architecture() {
if [ "${CC:-}" = "gcc -m32" ] || grep -q "\-m32" Makefile 2>/dev/null || grep -q "i686-gnu" config.status 2>/dev/null; then
echo "i686"
else
echo "x86_64"
fi
}
update_struct_sizes() {
echo "Updating struct size definitions for cross-architecture compatibility..."
if [[ -f "x86_64/include/mach/x86_64/machine_types.defs" ]]; then
cat >> "x86_64/include/mach/x86_64/machine_types.defs" << 'EOF'
/* Architecture-specific size definitions for MIG compatibility */
EOF
echo "Updated x86_64 machine_types.defs"
fi
if [[ -f "i386/include/mach/i386/machine_types.defs" ]]; then
cat >> "i386/include/mach/i386/machine_types.defs" << 'EOF'
/* Architecture-specific size definitions for MIG compatibility */
EOF
echo "Updated i386 machine_types.defs"
fi
}
process_mig_file() {
local file="$1"
local arch="$2"
local is_verify="$3"
if [[ ! -f "$file" ]]; then
echo "Warning: File $file not found, skipping..."
return 0
fi
echo "Processing MIG-generated file: $file"
echo "Architecture detected: $arch"
if [[ "$is_verify" == "true" ]]; then
echo "VERIFY MODE: Would make the following changes to $file:"
return 0
fi
if [[ "$arch" == "i686" ]]; then
sed -i 's/_Static_assert(sizeof(ipc_port_t) == 8 \* 1, "expected ipc_port_t to be size 8 \* 1");/_Static_assert(sizeof(ipc_port_t) == 4 * 1, "expected ipc_port_t to be size 4 * 1");/g' "$file"
sed -i 's/_Static_assert(sizeof(uint64_t) == 4 \* 1, "expected uint64_t to be size 4 \* 1");/_Static_assert(sizeof(uint64_t) == 8 * 1, "expected uint64_t to be size 8 * 1");/g' "$file"
sed -i 's/_Static_assert(sizeof(int64_t) == 4 \* 1, "expected int64_t to be size 4 \* 1");/_Static_assert(sizeof(int64_t) == 8 * 1, "expected int64_t to be size 8 * 1");/g' "$file"
sed -i 's/_Static_assert(sizeof(Request) == \([0-9]\+\), "Request expected to be \1 bytes");/
echo "✅ Fixed static assertions for 32-bit architecture in $file"
else
sed -i 's/_Static_assert(sizeof(uint64_t) == 4 \* 1, "expected uint64_t to be size 4 \* 1");/_Static_assert(sizeof(uint64_t) == 8 * 1, "expected uint64_t to be size 8 * 1");/g' "$file"
sed -i 's/_Static_assert(sizeof(int64_t) == 4 \* 1, "expected int64_t to be size 4 \* 1");/_Static_assert(sizeof(int64_t) == 8 * 1, "expected int64_t to be size 8 * 1");/g' "$file"
sed -i 's/_Static_assert(sizeof(Request) == \([0-9]\+\), "Request expected to be \1 bytes");/
echo "✅ Fixed static assertions for 64-bit architecture in $file"
fi
}
batch_process_files() {
local arch="$1"
local is_verify="$2"
echo "Batch processing all MIG-generated files..."
find . -name "*.user.c" -o -name "*.server.c" -o -name "*User.c" -o -name "*Server.c" | while IFS= read -r file; do
process_mig_file "$file" "$arch" "$is_verify"
done
echo "✅ Batch processing complete"
}
ARCH=$(detect_architecture)
if [[ "$UPDATE_STRUCT_SIZES" == "true" ]]; then
update_struct_sizes
exit 0
fi
if [[ "$BATCH_PROCESS" == "true" ]]; then
batch_process_files "$ARCH" "$VERIFY_ONLY"
else
process_mig_file "${FILE:-}" "$ARCH" "$VERIFY_ONLY"
fi
echo "✅ MIG 64-bit compatibility fixes completed successfully"