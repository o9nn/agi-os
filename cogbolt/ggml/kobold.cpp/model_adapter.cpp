#include <cassert>
#include <cstring>
#include <fstream>
#include <regex>
#include <iostream>
#include <iterator>
#include <queue>
#include <string>
#include <math.h>
#include <vector>
#include "model_adapter.h"
#include "ggml.h"
#include "ggml-cpu.h"
#include "gguf.h"
#include <chrono>
#include <filesystem>
static auto bench_timer = std::chrono::high_resolution_clock().now();
void timer_start()
{
bench_timer = std::chrono::high_resolution_clock().now();
}
double timer_check()
{
auto endtime = std::chrono::high_resolution_clock().now();
auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(endtime - bench_timer);
double time_taken = duration.count()/1000.0;
return time_taken;
}
void print_vec(std::vector<std::string> &embd)
{
std::cout << "[";
bool first = true;
for (auto i : embd)
{
if (!first)
{
std::cout << ',';
}
first = false;
std::cout << i;
}
std::cout << "]\n";
}
void print_tok_vec(std::vector<int> &embd)
{
std::cout << "[";
bool first = true;
for (auto i : embd)
{
if (!first)
{
std::cout << ',';
}
first = false;
std::cout << i;
}
std::cout << "]\n";
}
void print_tok_vec(std::vector<float> &embd)
{
std::cout << "[";
bool first = true;
int n = 0;
for (auto i : embd)
{
if (!first)
{
std::cout << ',';
}
first = false;
std::cout << i;
if(++n>20)
{
break;
}
}
std::cout << "]\n";
}
FileFormat check_file_format(const std::string & fname, FileFormatExtraMeta * fileformatmeta)
{
std::vector<char> f_buf(1024*1024);
#ifdef _WIN32
std::filesystem::path fpath = std::filesystem::u8path(fname);
#else
std::filesystem::path fpath = std::filesystem::path(fname);
#endif
auto fin = std::ifstream(fpath, std::ios::binary);
fin.rdbuf()->pubsetbuf(f_buf.data(), f_buf.size());
if (!fin) {
fprintf(stderr, "%s: failed to open '%s'\n", __func__, fname.c_str());
return FileFormat::BADFORMAT;
}
FileFormat fileformat = FileFormat::BADFORMAT;
uint32_t magic;
fin.read((char *) &magic, sizeof(magic));
if (magic == 0x67676d6c) {
fileformat = FileFormat::GGML;
int32_t vocabsiz = 0;
fin.read((char *) &vocabsiz, sizeof(int32_t));
if(vocabsiz==4096 || vocabsiz==7168)
{
fileformat = FileFormat::MPT_1;
}
else if(vocabsiz==50400)
{
fileformat = FileFormat::GPTJ_1;
uint32_t temp;
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
const int32_t qntvr = temp / 1000;
temp %= 1000;
if (qntvr != 0)
{
if (qntvr == 1)
{
fileformat = FileFormat::GPTJ_4;
}
else
{
fileformat = FileFormat::GPTJ_5;
}
}
else if (temp != 0 && temp != 1)
{
fileformat = FileFormat::GPTJ_3;
}
}
else if(vocabsiz==50257 || (vocabsiz>=49152&&vocabsiz<=49157))
{
fileformat = FileFormat::GPT2_1;
uint32_t temp, v1,v2,v3;
fin.read((char *)&v1, sizeof(temp));
fin.read((char *)&v2, sizeof(temp));
fin.read((char *)&v3, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
if(vocabsiz==49152 && v1==4096 && v2==2560 && v3==32 && temp==32)
{
fileformat = FileFormat::NEOX_6;
}
else
{
fin.read((char *)&temp, sizeof(temp));
const int32_t qntvr = temp / 1000;
temp %= 1000;
if (qntvr != 0)
{
if (qntvr == 1)
{
fileformat = FileFormat::GPT2_3;
}
else
{
fileformat = FileFormat::GPT2_4;
}
}
else if (temp != 0 && temp != 1)
{
fileformat = FileFormat::GPT2_2;
}
}
}
else if(vocabsiz < 31998 || vocabsiz > 33000)
{
fileformat = FileFormat::NEOX_6;
uint32_t temp,temp2;
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
if(temp!=0 && temp!=1){
fileformat = FileFormat::NEOX_2;
}
else
{
fin.read((char *)&temp2, sizeof(temp2));
bool isNewFtype = (temp2>=1000 && temp2<=9000 && temp2%1000<20);
if(!isNewFtype)
{
fileformat = FileFormat::NEOX_2;
if((temp==0||temp==1)&&(temp2==0||temp2==1))
{
fileformat = temp==0?FileFormat::NEOX_7:FileFormat::NEOX_6;
}
}
else
{
const int32_t qntvr = temp2 / 1000;
if(qntvr==1)
{
fileformat = (temp==0?FileFormat::NEOX_5:FileFormat::NEOX_4);
}
else
{
fileformat = (temp==0?FileFormat::NEOX_7:FileFormat::NEOX_6);
}
}
}
}
}
else if(magic == 0x67676d66)
{
fileformat = FileFormat::GGHF;
uint32_t temp;
fin.read((char *)&temp, sizeof(temp));
if(temp==100)
{
fileformat = FileFormat::RWKV_1;
}
else if(temp==101)
{
fileformat = FileFormat::RWKV_2;
}
}
else if(magic == 0x67676a74)
{
fileformat = FileFormat::GGJT_3;
uint32_t ver, temp, ftype;
fin.read((char *)&ver, sizeof(ver));
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&temp, sizeof(temp));
fin.read((char *)&ftype, sizeof(ftype));
if(ver==1)
{
fileformat = FileFormat::GGJT;
}
else if(ver==2)
{
fileformat = FileFormat::GGJT_2;
}
}
else if(magic == 0x46554747)
{
fin.close();
fileformat = FileFormat::GGUF_GENERIC;
struct gguf_init_params ggufparams;
ggufparams.no_alloc = true;
ggufparams.ctx = NULL;
auto ctx = gguf_init_from_file(fname.c_str(), ggufparams);
auto keyidx = gguf_find_key(ctx, "general.architecture");
std::string modelarch = "";
if (keyidx != -1) { modelarch = gguf_get_val_str(ctx, keyidx); }
printf("\nThe reported GGUF Arch is: %s\n",(modelarch==""?"unknown":modelarch.c_str()));
if(modelarch!="" && fileformatmeta!=nullptr)
{
int n_tensors = gguf_get_n_tensors(ctx);
float freq_base_train = 0;
std::string fkey = modelarch+".context_length";
int keyidx = gguf_find_key(ctx, fkey.c_str());
if (keyidx != -1) {
fileformatmeta->n_ctx_train = gguf_get_val_u32(ctx, keyidx);
}
fkey = modelarch+".expert_count";
keyidx = gguf_find_key(ctx, fkey.c_str());
if (keyidx != -1) {
fileformatmeta->n_expert_count = gguf_get_val_u32(ctx, keyidx);
}
fkey = modelarch+".rope.freq_base";
keyidx = gguf_find_key(ctx, fkey.c_str());
if (keyidx != -1) {
freq_base_train = gguf_get_val_f32(ctx, keyidx);
}
fkey = "tokenizer.ggml.add_bos_token";
keyidx = gguf_find_key(ctx, fkey.c_str());
if (keyidx != -1) {
bool result = gguf_get_val_bool(ctx, keyidx);
if(result==false)
{
fileformatmeta->explicitly_no_bos = true;
}
}
int filever = gguf_get_version(ctx);
fileformatmeta->fileversion = filever;
fileformatmeta->model_architecture = GGUFArch::ARCH_DEFAULT;
fileformatmeta->model_architecture_str = modelarch;
if(modelarch=="phi2")
{
fileformatmeta->model_architecture = GGUFArch::ARCH_PHI;
}
else if(modelarch=="falcon")
{
fileformatmeta->model_architecture = GGUFArch::ARCH_FALCON;
}
else if(modelarch=="mamba")
{
fileformatmeta->model_architecture = GGUFArch::ARCH_MAMBA;
}
else if(modelarch=="jamba")
{
fileformatmeta->model_architecture = GGUFArch::ARCH_JAMBA;
}
else if(modelarch=="llama" && freq_base_train==10000.0f && (n_tensors==435 || n_tensors==611))
{
fileformatmeta->model_architecture = GGUFArch::ARCH_SOLAR;
}
else if(modelarch=="qwen2")
{
fileformatmeta->model_architecture = GGUFArch::ARCH_QWEN2;
}
else if(modelarch=="qwen2vl")
{
fileformatmeta->model_architecture = GGUFArch::ARCH_QWEN2VL;
}
else if(modelarch=="gemma3")
{
fileformatmeta->model_architecture = GGUFArch::ARCH_GEMMA3;
}
else if(modelarch=="gemma3n")
{
fileformatmeta->model_architecture = GGUFArch::ARCH_GEMMA3N;
}
else if(modelarch=="rwkv6" || modelarch=="rwkv7")
{
fileformatmeta->model_architecture = GGUFArch::ARCH_RWKV;
}
else if(modelarch=="glm4" || modelarch=="glm4moe")
{
fileformatmeta->model_architecture = GGUFArch::ARCH_GLM4;
}
else if(modelarch=="gpt-oss")
{
fileformatmeta->model_architecture = GGUFArch::ARCH_GPTOSS;
}
printf("Arch Category: %d\n",fileformatmeta->model_architecture);
}
gguf_free(ctx);
}
if(fin.is_open())
{
fin.close();
}
return fileformat;
}
bool ArrStartWith(const std::vector<int> targetArray, const std::vector<int> searchSeq)
{
int ss = searchSeq.size();
if(targetArray.size()<ss)
{
return false;
}
for(int i=0;i<ss;++i)
{
if(targetArray[i]!=searchSeq[i])
{
return false;
}
}
return true;
}
int ArrFindIndexOf(const std::vector<int> targetArray, const std::vector<int> searchSeq)
{
int ss = searchSeq.size();
int tas = targetArray.size();
if(tas<ss)
{
return -1;
}
for(int i=0;i<tas;++i)
{
int srch = 0;
bool fail = false;
for(int srch=0;srch<ss;++srch)
{
if ((i + srch) >= tas || targetArray[i + srch] != searchSeq[srch])
{
fail = true;
break;
}
}
if(!fail)
{
return i;
}
}
return -1;
}
std::vector<int> LongestCommonSubseq(const std::vector<int> x, const std::vector<int> y)
{
int m = x.size(), n = y.size();
std::vector<std::vector<int>> LCSuff(m+1, std::vector<int>(n+1));
for (int j = 0; j <= n; j++)
LCSuff[0][j] = 0;
for (int i = 0; i <= m; i++)
LCSuff[i][0] = 0;
for (int i = 1; i <= m; i++)
{
for (int j = 1; j <= n; j++)
{
if (x[i - 1] == y[j - 1])
LCSuff[i][j] = LCSuff[i - 1][j - 1] + 1;
else
LCSuff[i][j] = 0;
}
}
std::vector<int> longest;
for (int i = 1; i <= m; i++)
{
for (int j = 1; j <= n; j++)
{
if (LCSuff[i][j] > longest.size())
{
auto off1 = ((i - LCSuff[i][j] + 1) - 1);
auto off2 = off1 + LCSuff[i][j];
longest.clear();
longest = std::vector<int>(x.begin() + off1, x.begin() + off2);
}
}
}
return longest;
}
void ContextFastForward(std::vector<int> &current_context_tokens, std::vector<int> &embd_inp,
int &n_past, std::vector<int> &last_n_tokens, const int nctx, std::vector<int> &smartcontext,
bool useSmartContext, const bool requireFullSubset)
{
const int SCCtxLenThreshold = nctx * 0.8;
const int SCInpLenThreshold = nctx * 0.6;
const int SCPastLenThreshold = nctx * 0.5;
const float SCTruncationRatio = 0.5;
const int SCTokThreshold = 32 + (nctx*0.05);
int embd_inp_len = embd_inp.size();
int cur_ctx_len = current_context_tokens.size();
bool fastforwardok = true;
for (int i = 0; i < cur_ctx_len; ++i)
{
if (current_context_tokens[i] == embd_inp[i])
{
n_past += 1;
last_n_tokens.push_back(current_context_tokens[i]);
}
else
{
if(requireFullSubset)
{
last_n_tokens.erase(last_n_tokens.end() - n_past, last_n_tokens.end());
n_past = 0;
fastforwardok = false;
}
break;
}
if (requireFullSubset)
{
if (i >= embd_inp_len)
{
last_n_tokens.erase(last_n_tokens.end() - n_past, last_n_tokens.end());
n_past = 0;
fastforwardok = false;
break;
}
}
else
{
if ((i + 2) >= embd_inp_len)
{
break;
}
if ((i + 2) >= cur_ctx_len)
{
break;
}
}
}
if(fastforwardok)
{
last_n_tokens.erase(last_n_tokens.begin(), last_n_tokens.begin() + n_past);
embd_inp.erase(embd_inp.begin(), embd_inp.begin() + n_past);
embd_inp_len = embd_inp.size();
}
if (fastforwardok && useSmartContext && smartcontext.size() > 0 && embd_inp_len >= SCInpLenThreshold)
{
auto shared = LongestCommonSubseq(smartcontext, embd_inp);
if (shared.size() > SCTokThreshold && ArrStartWith(smartcontext, shared))
{
int found = ArrFindIndexOf(embd_inp,shared);
if(found>=0)
{
auto trimmed = std::vector<int>(embd_inp.begin() + found, embd_inp.end());
embd_inp = trimmed;
embd_inp_len = embd_inp.size();
printf("\n[Reusing Smart Context: %d allowance remaining]", found);
int old_n_past = n_past;
int offset_fix = old_n_past;
if (current_context_tokens[n_past] != embd_inp[0])
{
offset_fix = 0;
}
for (int i = n_past; i < current_context_tokens.size(); ++i)
{
if (current_context_tokens[i] == embd_inp[i-offset_fix])
{
n_past += 1;
last_n_tokens.push_back(current_context_tokens[i]);
}
else
{
break;
}
if ((i + 2 - offset_fix) >= embd_inp_len)
{
break;
}
}
last_n_tokens.erase(last_n_tokens.begin(), last_n_tokens.begin() + (n_past-old_n_past));
embd_inp.erase(embd_inp.begin(), embd_inp.begin() + (n_past-old_n_past));
}else{
smartcontext.clear();
}
}
else
{
smartcontext.clear();
}
}
else
{
smartcontext.clear();
}
if(fastforwardok && useSmartContext
&& smartcontext.size()==0 && current_context_tokens.size() >= SCCtxLenThreshold
&& embd_inp_len >= SCInpLenThreshold
&& current_context_tokens.size() - n_past > SCPastLenThreshold)
{
int shiftamt = embd_inp.size() * SCTruncationRatio;
smartcontext = std::vector<int>(embd_inp.begin() + shiftamt, embd_inp.end());
printf("\n[New Smart Context Triggered! Buffered Token Allowance: %d]",shiftamt);
embd_inp = smartcontext;
}
}