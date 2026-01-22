#pragma once
#include "decoder_xqa_impl_precompiled.h"
#include "xqa_params.h"
#include "decoder_xqa_impl_common.h"
class DecoderXQARunner {
public:
DecoderXQARunner(const XQADataType data_type, int num_heads, int num_kv_heads,
int head_size, bool multi_block_mode);
~DecoderXQARunner();
bool shouldUse(XQAParams const& xqaParams);
size_t getWorkspaceSize(int max_num_tokens);
void prepare(XQAParams const& xqa_params) { this->prepareForRun(xqa_params); }
void dispatch(XQAParams const& xqa_params,
KVCacheListParams const& kv_cache_buffer,
cudaStream_t const& stream) {
this->run(xqa_params, kv_cache_buffer, stream);
}
class Resource;
static Resource* getResourceGlobal();
private:
void prepareForRun(XQAParams const& xqa_params);
void run(XQAParams const& xqa_params,
KVCacheListParams const& kv_cache_buffer,
cudaStream_t const& stream);
static constexpr int kMaxBeamWidth = 4;
XQADataType mDataType;
int mNumHeads;
int mNumKVHeads;
int mHeadSize;
bool mMultiBlockMode;
int mMultiProcessorCount;
std::unique_ptr<DecoderXQAImpl> mPrecompiledImpl;
DecoderXQAImpl* getImplFromXQAParams(XQAParams const& params);
friend DecoderXQAImplPrecompiled;
};