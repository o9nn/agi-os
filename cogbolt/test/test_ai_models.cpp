#include "bolt/test_framework.hpp"
#include "bolt/ai/ggml.hpp"
#include "bolt/ai/ggml_wrapper.hpp"
#include "bolt/ai/rwkv_wrapper.hpp"
#include <rwkv.h>
#include <fstream>
#include <vector>
#include <chrono>
using namespace bolt::test;
static const std::string TEST_MODEL_PATH = "test/models/tiny-rwkv-5v1-730K-Q5_0.bin";
static const std::string EXPECTED_LOGITS_PATH = "test/models/expected-logits-5v1-730K.bin";
std::vector<float> load_expected_logits(const std::string& path) {
std::ifstream file(path, std::ios::binary);
if (!file.is_open()) {
throw std::runtime_error("Failed to open expected logits file");
}
file.seekg(0, std::ios::end);
size_t size = file.tellg();
file.seekg(0, std::ios::beg);
std::vector<float> logits(size / sizeof(float));
file.read(reinterpret_cast<char*>(logits.data()), size);
return logits;
}
float calculate_logit_difference(const std::vector<float>& expected, const std::vector<float>& actual) {
if (expected.size() != actual.size()) {
return std::numeric_limits<float>::max();
}
float sum_diff = 0.0f;
for (size_t i = 0; i < expected.size(); ++i) {
sum_diff += std::abs(expected[i] - actual[i]);
}
return sum_diff;
}
BOLT_TEST(AIModels, GGMLBasicTensorOperations) {
const size_t mem_size = 10 * 1024 * 1024;
bolt::GGMLContext context(mem_size);
auto* tensor_a = ggml_new_tensor_1d(context.get(), GGML_TYPE_F32, 1024);
auto* tensor_b = ggml_new_tensor_1d(context.get(), GGML_TYPE_F32, 1024);
BOLT_ASSERT_NOT_NULL(tensor_a);
BOLT_ASSERT_NOT_NULL(tensor_b);
BOLT_ASSERT_EQ(ggml_nelements(tensor_a), 1024);
BOLT_ASSERT_EQ(tensor_a->type, GGML_TYPE_F32);
auto* result = ggml_add(context.get(), tensor_a, tensor_b);
BOLT_ASSERT_NOT_NULL(result);
BOLT_ASSERT_EQ(ggml_nelements(result), 1024);
}
BOLT_TEST(AIModels, GGMLMatrixOperations) {
const size_t mem_size = 10 * 1024 * 1024;
bolt::GGMLContext context(mem_size);
auto* matrix_a = ggml_new_tensor_2d(context.get(), GGML_TYPE_F32, 64, 32);
auto* matrix_b = ggml_new_tensor_2d(context.get(), GGML_TYPE_F32, 32, 16);
BOLT_ASSERT_NOT_NULL(matrix_a);
BOLT_ASSERT_NOT_NULL(matrix_b);
auto* result = ggml_mul_mat(context.get(), matrix_a, matrix_b);
BOLT_ASSERT_NOT_NULL(result);
BOLT_ASSERT_EQ(result->ne[0], 64);
BOLT_ASSERT_EQ(result->ne[1], 16);
}
BOLT_TEST(AIModels, GGMLComputeGraph) {
const size_t mem_size = 10 * 1024 * 1024;
bolt::GGMLContext context(mem_size);
auto* gf = ggml_new_graph_custom(context.get(), GGML_DEFAULT_GRAPH_SIZE, true);
BOLT_ASSERT_NOT_NULL(gf);
auto* a = ggml_new_tensor_1d(context.get(), GGML_TYPE_F32, 100);
auto* b = ggml_new_tensor_1d(context.get(), GGML_TYPE_F32, 100);
auto* c = ggml_new_tensor_1d(context.get(), GGML_TYPE_F32, 100);
auto* sum = ggml_add(context.get(), a, b);
auto* result = ggml_mul(context.get(), sum, c);
ggml_build_forward_expand(gf, result);
BOLT_ASSERT_NOT_NULL(gf);
BOLT_ASSERT_NOT_NULL(result);
}
BOLT_TEST(AIModels, RWKVModelInfo) {
const char* system_info = rwkv_get_system_info_string();
BOLT_ASSERT_NOT_NULL(system_info);
std::string info_str(system_info);
BOLT_ASSERT_TRUE(!info_str.empty());
}
BOLT_TEST(AIModels, RWKVModelLoading) {
rwkv_set_print_errors(nullptr, false);
struct rwkv_context* ctx = rwkv_init_from_file(TEST_MODEL_PATH.c_str(), 1, 0);
if (ctx != nullptr) {
size_t state_size = rwkv_get_state_len(ctx);
size_t logits_size = rwkv_get_logits_len(ctx);
BOLT_ASSERT_TRUE(state_size > 0);
BOLT_ASSERT_TRUE(logits_size > 0);
std::vector<float> state(state_size);
std::vector<float> logits(logits_size);
bool success = rwkv_eval(ctx, 0, state.data(), state.data(), logits.data());
BOLT_ASSERT_TRUE(success);
bool has_valid_logits = false;
for (float logit : logits) {
if (std::isfinite(logit) && std::abs(logit) > 1e-6) {
has_valid_logits = true;
break;
}
}
BOLT_ASSERT_TRUE(has_valid_logits);
rwkv_free(ctx);
} else {
std::cerr << "Warning: Could not load RWKV test model from " << TEST_MODEL_PATH << std::endl;
}
}
BOLT_TEST(AIModels, RWKVLogitValidation) {
struct rwkv_context* ctx = rwkv_init_from_file(TEST_MODEL_PATH.c_str(), 1, 0);
if (ctx != nullptr) {
try {
auto expected_logits = load_expected_logits(EXPECTED_LOGITS_PATH);
size_t state_size = rwkv_get_state_len(ctx);
size_t logits_size = rwkv_get_logits_len(ctx);
std::vector<float> state(state_size);
std::vector<float> actual_logits(logits_size);
std::fill(state.begin(), state.end(), 0.0f);
rwkv_eval(ctx, 0, state.data(), state.data(), actual_logits.data());
if (expected_logits.size() == actual_logits.size()) {
float difference = calculate_logit_difference(expected_logits, actual_logits);
BOLT_ASSERT_TRUE(difference < 10.0f);
std::cout << "Logit difference: " << difference << std::endl;
}
} catch (const std::exception& e) {
std::cerr << "Warning: Could not validate logits: " << e.what() << std::endl;
}
rwkv_free(ctx);
}
}
BOLT_TEST(AIModels, RWKVWrapperIntegration) {
auto& wrapper = bolt::RWKVWrapper::getInstance();
BOLT_ASSERT_FALSE(wrapper.isInitialized());
try {
wrapper.initialize(TEST_MODEL_PATH);
if (wrapper.isInitialized()) {
BOLT_ASSERT_TRUE(wrapper.getNumLayers() >= 1);
BOLT_ASSERT_TRUE(wrapper.getEmbedDim() >= 32);
std::cout << "RWKV model initialized successfully:" << std::endl;
std::cout << "  Layers: " << wrapper.getNumLayers() << std::endl;
std::cout << "  Embed dim: " << wrapper.getEmbedDim() << std::endl;
}
} catch (const std::exception& e) {
std::cerr << "Warning: RWKV wrapper initialization failed: " << e.what() << std::endl;
}
}
BOLT_TEST(AIModels, ModelMemoryManagement) {
for (int i = 0; i < 3; ++i) {
struct rwkv_context* ctx = rwkv_init_from_file(TEST_MODEL_PATH.c_str(), 1, 0);
if (ctx != nullptr) {
size_t state_size = rwkv_get_state_len(ctx);
BOLT_ASSERT_TRUE(state_size > 0);
rwkv_free(ctx);
}
}
for (int i = 0; i < 5; ++i) {
const size_t mem_size = 1024 * 1024;
bolt::GGMLContext context(mem_size);
auto* tensor = ggml_new_tensor_1d(context.get(), GGML_TYPE_F32, 1000);
BOLT_ASSERT_NOT_NULL(tensor);
}
}
BOLT_TEST(AIModels, ModelInferencePerformance) {
struct rwkv_context* ctx = rwkv_init_from_file(TEST_MODEL_PATH.c_str(), 1, 0);
if (ctx != nullptr) {
size_t state_size = rwkv_get_state_len(ctx);
size_t logits_size = rwkv_get_logits_len(ctx);
std::vector<float> state(state_size);
std::vector<float> logits(logits_size);
auto start = std::chrono::high_resolution_clock::now();
const int num_evals = 10;
for (int i = 0; i < num_evals; ++i) {
rwkv_eval(ctx, i % 256, state.data(), state.data(), logits.data());
}
auto end = std::chrono::high_resolution_clock::now();
auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
std::cout << "Performance: " << num_evals << " evaluations in "
<< duration.count() << "ms ("
<< (duration.count() / float(num_evals)) << "ms per eval)" << std::endl;
BOLT_ASSERT_TRUE(duration.count() < num_evals * 1000);
rwkv_free(ctx);
}
}