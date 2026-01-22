#ifndef CONFIGS_H
#define CONFIGS_H
#define PIPELINE_LEVEL_GMEM 2
#define PIPELINE_LEVEL_SMEM 2
#define WARP_SIZE 32
#define REG_BIT_WIDTH 32
#define MMA_8 8
#define MMA_16 16
#define THREAD_OPT_ACCESS_BIT_WIDTH_128 128
#define BIT_WIDTH_PER_HALF 16
#define REG_PER_THREAD_C_TENSOR_16_16 8
#define PADDING_BYTES_16 16
#define PADDING_SHARED_MEM_FOR_B_8 \
8
#define PADDING_SHARED_MEM_FOR_C_4 \
4
#define WARP_ROW_MMA_TENSORS 4
#define WARP_M (WARP_ROW_MMA_TENSORS * MMA_16)
#define WARP_K_MMA_TENSORS 4
#define WARP_K (WARP_K_MMA_TENSORS * MMA_16)
template <int BLOCK_ROW_WARPS_, int BLOCK_COL_WARPS_, int WARP_COL_MMA_TENSORS_>
struct TilingConfig {
static constexpr int BLOCK_ROW_WARPS = BLOCK_ROW_WARPS_;
static constexpr int BLOCK_COL_WARPS = BLOCK_COL_WARPS_;
static constexpr int WARP_COL_MMA_TENSORS = WARP_COL_MMA_TENSORS_;
static constexpr int WARP_N = WARP_COL_MMA_TENSORS * MMA_8;
static constexpr int TILE_M = WARP_M * BLOCK_ROW_WARPS;
static constexpr int TILE_N = MMA_8 * WARP_COL_MMA_TENSORS * BLOCK_COL_WARPS;
static constexpr int TILE_K = WARP_K;
static constexpr int BLOCK_WARPS = BLOCK_ROW_WARPS * BLOCK_COL_WARPS;
static constexpr int BLOCK_THREADS = BLOCK_WARPS * WARP_SIZE;
static constexpr int SMEM_SIZE_B_TILE =
TILE_N * (TILE_K + PADDING_BYTES_16) * 2 *
PIPELINE_LEVEL_GMEM;
static constexpr int SMEM_SIZE_C_TILE =
TILE_N * (TILE_M + PADDING_BYTES_16) * 4;
};
#endif