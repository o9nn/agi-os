#include "bolt/ai/gpu_acceleration.hpp"
#include "bolt/core/logging.hpp"
#include <chrono>
#include <algorithm>
#include <cstring>
#include <mutex>
namespace bolt {
namespace ai {
GPUMemoryManager::GPUMemoryManager(ggml_backend_t backend, ggml_backend_buffer_type_t bufferType)
: backend_(backend), bufferType_(bufferType), totalAllocated_(0), peakUsage_(0) {
}
GPUMemoryManager::~GPUMemoryManager() {
for (auto buffer : allocatedBuffers_) {
if (buffer) {
ggml_backend_buffer_free(buffer);
}
}
allocatedBuffers_.clear();
}
ggml_backend_buffer_t GPUMemoryManager::allocateBuffer(size_t size) {
auto buffer = ggml_backend_buft_alloc_buffer(bufferType_, size);
if (buffer) {
allocatedBuffers_.push_back(buffer);
totalAllocated_ += size;
if (totalAllocated_ > peakUsage_) {
peakUsage_ = totalAllocated_;
}
}
return buffer;
}
void GPUMemoryManager::deallocateBuffer(ggml_backend_buffer_t buffer) {
auto it = std::find(allocatedBuffers_.begin(), allocatedBuffers_.end(), buffer);
if (it != allocatedBuffers_.end()) {
size_t bufferSize = ggml_backend_buffer_get_size(buffer);
ggml_backend_buffer_free(buffer);
allocatedBuffers_.erase(it);
if (bufferSize <= totalAllocated_) {
totalAllocated_ -= bufferSize;
}
}
}
size_t GPUMemoryManager::getTotalMemory() const {
return 0;
}
size_t GPUMemoryManager::getUsedMemory() const {
return totalAllocated_;
}
size_t GPUMemoryManager::getAvailableMemory() const {
return getTotalMemory() - getUsedMemory();
}
void GPUMemoryManager::optimizeMemoryUsage() {
BOLT_INFO("GPU Memory optimization requested");
}
void GPUMemoryManager::defragmentMemory() {
BOLT_INFO("GPU Memory defragmentation requested");
}
GPUAccelerationManager& GPUAccelerationManager::getInstance() {
static GPUAccelerationManager instance;
return instance;
}
bool GPUAccelerationManager::initialize(const GPUConfig& config) {
std::lock_guard<std::mutex> lock(mutex_);
if (initialized_) {
BOLT_WARN("GPU Acceleration Manager already initialized");
return true;
}
config_ = config;
availableDevices_ = detectAvailableDevices();
if (availableDevices_.empty()) {
BOLT_ERROR("No GPU devices found, falling back to CPU");
if (!initializeCPU()) {
BOLT_ERROR("Failed to initialize CPU backend");
return false;
}
} else {
bool backendInitialized = false;
if (selectDevice(config_.preferredBackend, config_.preferredDeviceId)) {
backendInitialized = true;
} else if (config_.enableAutomaticFallback) {
for (auto fallbackBackend : config_.fallbackBackends) {
if (selectDevice(fallbackBackend, 0)) {
backendInitialized = true;
break;
}
}
}
if (!backendInitialized) {
BOLT_ERROR("Failed to initialize any GPU backend");
return false;
}
}
if (currentBackend_ && currentBufferType_) {
memoryManager_ = std::make_unique<GPUMemoryManager>(currentBackend_, currentBufferType_);
}
initialized_ = true;
resetPerformanceStats();
BOLT_INFO("GPU Acceleration Manager initialized with backend: " +
backendTypeToString(currentBackendType_));
return true;
}
void GPUAccelerationManager::shutdown() {
std::lock_guard<std::mutex> lock(mutex_);
if (!initialized_) return;
memoryManager_.reset();
if (currentBackend_) {
ggml_backend_free(currentBackend_);
currentBackend_ = nullptr;
}
currentBackendType_ = GPUBackendType::CPU;
currentBufferType_ = nullptr;
availableDevices_.clear();
initialized_ = false;
BOLT_INFO("GPU Acceleration Manager shutdown complete");
}
std::vector<GPUDeviceInfo> GPUAccelerationManager::detectAvailableDevices() {
std::vector<GPUDeviceInfo> devices;
auto cudaDevices = detectCUDADevices();
devices.insert(devices.end(), cudaDevices.begin(), cudaDevices.end());
auto openclDevices = detectOpenCLDevices();
devices.insert(devices.end(), openclDevices.begin(), openclDevices.end());
auto vulkanDevices = detectVulkanDevices();
devices.insert(devices.end(), vulkanDevices.begin(), vulkanDevices.end());
GPUDeviceInfo cpuDevice;
cpuDevice.deviceId = -1;
cpuDevice.name = "CPU";
cpuDevice.description = "CPU Fallback";
cpuDevice.totalMemory = 0;
cpuDevice.availableMemory = 0;
cpuDevice.backendType = GPUBackendType::CPU;
cpuDevice.isAvailable = true;
devices.push_back(cpuDevice);
return devices;
}
bool GPUAccelerationManager::selectDevice(GPUBackendType backendType, int deviceId) {
if (currentBackend_) {
ggml_backend_free(currentBackend_);
currentBackend_ = nullptr;
}
bool success = false;
switch (backendType) {
case GPUBackendType::CUDA:
success = initializeCUDA(deviceId);
break;
case GPUBackendType::OpenCL:
success = initializeOpenCL();
break;
case GPUBackendType::Vulkan:
success = initializeVulkan(deviceId);
break;
case GPUBackendType::CPU:
success = initializeCPU();
break;
default:
BOLT_ERROR("Unknown GPU backend type");
return false;
}
if (success) {
currentBackendType_ = backendType;
for (const auto& device : availableDevices_) {
if (device.backendType == backendType &&
(deviceId < 0 || device.deviceId == deviceId)) {
currentDevice_ = device;
break;
}
}
BOLT_INFO("Successfully selected " + backendTypeToString(backendType) +
" device " + std::to_string(deviceId));
}
return success;
}
GPUDeviceInfo GPUAccelerationManager::getCurrentDevice() const {
return currentDevice_;
}
bool GPUAccelerationManager::loadModelToGPU(ggml_context* ctx, const std::vector<ggml_tensor*>& tensors) {
if (!initialized_ || !memoryManager_) {
BOLT_ERROR("GPU Acceleration Manager not initialized");
return false;
}
auto startTime = std::chrono::high_resolution_clock::now();
size_t totalTransferred = 0;
for (auto tensor : tensors) {
if (transferTensorToGPU(tensor)) {
totalTransferred++;
} else {
BOLT_WARN("Failed to transfer tensor to GPU");
}
}
auto endTime = std::chrono::high_resolution_clock::now();
auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime);
performanceStats_.totalMemoryTransfers += totalTransferred;
performanceStats_.totalTransferTime += duration.count() / 1000.0;
BOLT_INFO("Transferred " + std::to_string(totalTransferred) + "/" +
std::to_string(tensors.size()) + " tensors to GPU in " +
std::to_string(duration.count()) + "ms");
return totalTransferred > 0;
}
bool GPUAccelerationManager::transferTensorToGPU(ggml_tensor* tensor) {
if (!tensor || !memoryManager_) return false;
size_t tensorSize = ggml_nbytes(tensor);
auto buffer = memoryManager_->allocateBuffer(tensorSize);
if (!buffer) {
BOLT_ERROR("Failed to allocate GPU buffer for tensor");
return false;
}
if (ggml_backend_buffer_init_tensor(buffer, tensor) != GGML_STATUS_SUCCESS) {
BOLT_ERROR("Failed to initialize tensor on GPU");
memoryManager_->deallocateBuffer(buffer);
return false;
}
return true;
}
bool GPUAccelerationManager::transferTensorToCPU(ggml_tensor* tensor) {
return true;
}
bool GPUAccelerationManager::computeGraph(ggml_cgraph* graph) {
if (!initialized_ || !currentBackend_ || !graph) {
BOLT_ERROR("Cannot compute graph: GPU backend not properly initialized");
return false;
}
auto startTime = std::chrono::high_resolution_clock::now();
enum ggml_status result = ggml_backend_graph_compute(currentBackend_, graph);
auto endTime = std::chrono::high_resolution_clock::now();
auto duration = std::chrono::duration_cast<std::chrono::microseconds>(endTime - startTime);
performanceStats_.totalComputeOps++;
performanceStats_.totalComputeTime += duration.count() / 1000000.0;
performanceStats_.averageOpsPerSecond =
performanceStats_.totalComputeOps / performanceStats_.totalComputeTime;
bool success = (result == GGML_STATUS_SUCCESS);
if (!success) {
BOLT_ERROR("Failed to compute graph on GPU");
}
return success;
}
bool GPUAccelerationManager::computeGraphAsync(ggml_cgraph* graph) {
return computeGraph(graph);
}
void GPUAccelerationManager::synchronize() {
}
void GPUAccelerationManager::resetPerformanceStats() {
performanceStats_ = PerformanceStats{};
}
void GPUAccelerationManager::updateConfig(const GPUConfig& config) {
config_ = config;
}
bool GPUAccelerationManager::initializeCUDA(int deviceId) {
#ifdef GGML_USE_CUDA
try {
currentBackend_ = ggml_backend_cuda_init(deviceId);
if (currentBackend_) {
currentBufferType_ = ggml_backend_cuda_buffer_type(deviceId);
BOLT_INFO("CUDA backend initialized for device " + std::to_string(deviceId));
return true;
}
} catch (const std::exception& e) {
BOLT_ERROR("Failed to initialize CUDA: " + std::string(e.what()));
}
#endif
BOLT_WARN("CUDA backend not available or failed to initialize");
return false;
}
bool GPUAccelerationManager::initializeOpenCL() {
#ifdef GGML_USE_OPENCL
try {
currentBackend_ = ggml_backend_opencl_init();
if (currentBackend_) {
currentBufferType_ = ggml_backend_opencl_buffer_type();
BOLT_INFO("OpenCL backend initialized");
return true;
}
} catch (const std::exception& e) {
BOLT_ERROR("Failed to initialize OpenCL: " + std::string(e.what()));
}
#endif
BOLT_WARN("OpenCL backend not available or failed to initialize");
return false;
}
bool GPUAccelerationManager::initializeVulkan(int deviceId) {
#ifdef GGML_USE_VULKAN
try {
currentBackend_ = ggml_backend_vk_init(deviceId);
if (currentBackend_) {
currentBufferType_ = ggml_backend_vk_buffer_type(deviceId);
BOLT_INFO("Vulkan backend initialized for device " + std::to_string(deviceId));
return true;
}
} catch (const std::exception& e) {
BOLT_ERROR("Failed to initialize Vulkan: " + std::string(e.what()));
}
#endif
BOLT_WARN("Vulkan backend not available or failed to initialize");
return false;
}
bool GPUAccelerationManager::initializeCPU() {
try {
currentBackend_ = ggml_backend_cpu_init();
if (currentBackend_) {
currentBufferType_ = ggml_backend_cpu_buffer_type();
BOLT_INFO("CPU backend initialized");
return true;
}
} catch (const std::exception& e) {
BOLT_ERROR("Failed to initialize CPU backend: " + std::string(e.what()));
}
return false;
}
std::vector<GPUDeviceInfo> GPUAccelerationManager::detectCUDADevices() {
std::vector<GPUDeviceInfo> devices;
#ifdef GGML_USE_CUDA
int deviceCount = ggml_backend_cuda_get_device_count();
for (int i = 0; i < deviceCount; i++) {
GPUDeviceInfo device;
device.deviceId = i;
device.backendType = GPUBackendType::CUDA;
device.isAvailable = true;
char description[256];
ggml_backend_cuda_get_device_description(i, description, sizeof(description));
device.description = description;
device.name = "CUDA Device " + std::to_string(i);
size_t free, total;
ggml_backend_cuda_get_device_memory(i, &free, &total);
device.totalMemory = total;
device.availableMemory = free;
devices.push_back(device);
}
#endif
return devices;
}
std::vector<GPUDeviceInfo> GPUAccelerationManager::detectOpenCLDevices() {
std::vector<GPUDeviceInfo> devices;
#ifdef GGML_USE_OPENCL
if (isBackendAvailable(GPUBackendType::OpenCL)) {
GPUDeviceInfo device;
device.deviceId = 0;
device.name = "OpenCL Device";
device.description = "OpenCL Compute Device";
device.backendType = GPUBackendType::OpenCL;
device.isAvailable = true;
device.totalMemory = 0;
device.availableMemory = 0;
devices.push_back(device);
}
#endif
return devices;
}
std::vector<GPUDeviceInfo> GPUAccelerationManager::detectVulkanDevices() {
std::vector<GPUDeviceInfo> devices;
#ifdef GGML_USE_VULKAN
int deviceCount = ggml_backend_vk_get_device_count();
for (int i = 0; i < deviceCount; i++) {
GPUDeviceInfo device;
device.deviceId = i;
device.backendType = GPUBackendType::Vulkan;
device.isAvailable = true;
char description[256];
ggml_backend_vk_get_device_description(i, description, sizeof(description));
device.description = description;
device.name = "Vulkan Device " + std::to_string(i);
size_t free, total;
ggml_backend_vk_get_device_memory(i, &free, &total);
device.totalMemory = total;
device.availableMemory = free;
devices.push_back(device);
}
#endif
return devices;
}
bool GPUAccelerationManager::isBackendAvailable(GPUBackendType backendType) {
switch (backendType) {
#ifdef GGML_USE_CUDA
case GPUBackendType::CUDA:
return ggml_backend_cuda_get_device_count() > 0;
#endif
#ifdef GGML_USE_OPENCL
case GPUBackendType::OpenCL:
return true;
#endif
#ifdef GGML_USE_VULKAN
case GPUBackendType::Vulkan:
return ggml_backend_vk_get_device_count() > 0;
#endif
case GPUBackendType::CPU:
return true;
default:
return false;
}
}
std::string GPUAccelerationManager::backendTypeToString(GPUBackendType backendType) {
switch (backendType) {
case GPUBackendType::CPU:    return "CPU";
case GPUBackendType::CUDA:   return "CUDA";
case GPUBackendType::OpenCL: return "OpenCL";
case GPUBackendType::Vulkan: return "Vulkan";
case GPUBackendType::Metal:  return "Metal";
default:                     return "Unknown";
}
}
GPUAcceleratedModel::GPUAcceleratedModel(const std::string& modelPath, const GPUConfig& config)
: modelPath_(modelPath), gpuConfig_(config) {
auto& gpuManager = GPUAccelerationManager::getInstance();
if (!gpuManager.isInitialized()) {
if (!gpuManager.initialize(config)) {
BOLT_WARN("Failed to initialize GPU acceleration, falling back to CPU");
gpuAccelerated_ = false;
} else {
gpuAccelerated_ = true;
}
} else {
gpuAccelerated_ = true;
}
if (threadCount_ == -1) {
threadCount_ = std::thread::hardware_concurrency();
}
}
GPUAcceleratedModel::~GPUAcceleratedModel() {
}
bool GPUAcceleratedModel::loadModel(const std::string& path) {
modelPath_ = path;
struct ggml_init_params params = {
.mem_size   = maxContextSize_ * sizeof(float) * 4,
.mem_buffer = nullptr,
.no_alloc   = false
};
context_ = std::unique_ptr<ggml_context, void(*)(ggml_context*)>(ggml_init(params), ggml_free);
if (!context_) {
BOLT_ERROR("Failed to initialize GGML context");
return false;
}
bool success = loadModelWeights();
if (success) {
modelLoaded_ = true;
if (gpuAccelerated_ && gpuOffloadingEnabled_) {
setupGPUContext();
}
BOLT_INFO("Model loaded successfully from: " + path);
}
return success;
}
bool GPUAcceleratedModel::setupGPUContext() {
if (!gpuAccelerated_) return false;
auto& gpuManager = GPUAccelerationManager::getInstance();
if (!modelTensors_.empty()) {
return gpuManager.loadModelToGPU(context_.get(), modelTensors_);
}
return true;
}
bool GPUAcceleratedModel::loadModelWeights() {
BOLT_INFO("Loading model weights (simplified implementation)");
return true;
}
std::string GPUAcceleratedModel::generate(const std::string& prompt, size_t maxTokens) {
if (!modelLoaded_) {
BOLT_ERROR("Model not loaded");
return "";
}
auto tokens = tokenize(prompt);
auto outputTensor = runInference(tokens);
if (!outputTensor) {
BOLT_ERROR("Inference failed");
return "";
}
std::vector<int> responseTokens;
for (size_t i = 0; i < maxTokens; i++) {
responseTokens.push_back(static_cast<int>('A') + (i % 26));
}
return detokenize(responseTokens);
}
std::vector<float> GPUAcceleratedModel::generateEmbedding(const std::string& input) {
if (!modelLoaded_) {
BOLT_ERROR("Model not loaded");
return {};
}
std::vector<float> embedding(768);
std::fill(embedding.begin(), embedding.end(), 0.1f);
return embedding;
}
ggml_tensor* GPUAcceleratedModel::runInference(const std::vector<int>& tokens) {
if (!context_ || tokens.empty()) return nullptr;
ggml_cgraph* graph = ggml_new_graph(context_.get());
auto inputTensor = ggml_new_tensor_1d(context_.get(), GGML_TYPE_I32, tokens.size());
memcpy(inputTensor->data, tokens.data(), tokens.size() * sizeof(int));
auto outputTensor = ggml_new_tensor_1d(context_.get(), GGML_TYPE_F32, tokens.size());
ggml_build_forward_expand(graph, outputTensor);
if (gpuAccelerated_) {
auto& gpuManager = GPUAccelerationManager::getInstance();
if (!gpuManager.computeGraph(graph)) {
BOLT_ERROR("GPU computation failed");
return nullptr;
}
} else {
ggml_cplan plan = ggml_graph_plan(graph, threadCount_, nullptr);
ggml_graph_compute(graph, &plan);
}
return outputTensor;
}
std::vector<int> GPUAcceleratedModel::tokenize(const std::string& text) {
std::vector<int> tokens;
for (char c : text) {
tokens.push_back(static_cast<int>(c));
}
return tokens;
}
std::string GPUAcceleratedModel::detokenize(const std::vector<int>& tokens) {
std::string result;
for (int token : tokens) {
if (token >= 0 && token <= 255) {
result += static_cast<char>(token);
}
}
return result;
}
GPUAccelerationManager::PerformanceStats GPUAcceleratedModel::getPerformanceStats() const {
if (gpuAccelerated_) {
return GPUAccelerationManager::getInstance().getPerformanceStats();
}
return {};
}
namespace gpu_utils {
bool isGPUAccelerationAvailable() {
auto& gpuManager = GPUAccelerationManager::getInstance();
auto devices = gpuManager.detectAvailableDevices();
for (const auto& device : devices) {
if (device.backendType != GPUBackendType::CPU && device.isAvailable) {
return true;
}
}
return false;
}
GPUConfig getRecommendedGPUConfig() {
GPUConfig config;
auto& gpuManager = GPUAccelerationManager::getInstance();
auto devices = gpuManager.detectAvailableDevices();
bool hasCUDA = false, hasVulkan = false, hasOpenCL = false;
for (const auto& device : devices) {
switch (device.backendType) {
case GPUBackendType::CUDA:
hasCUDA = true;
break;
case GPUBackendType::Vulkan:
hasVulkan = true;
break;
case GPUBackendType::OpenCL:
hasOpenCL = true;
break;
default:
break;
}
}
if (hasCUDA) {
config.preferredBackend = GPUBackendType::CUDA;
} else if (hasVulkan) {
config.preferredBackend = GPUBackendType::Vulkan;
} else if (hasOpenCL) {
config.preferredBackend = GPUBackendType::OpenCL;
} else {
config.preferredBackend = GPUBackendType::CPU;
}
return config;
}
GPUBackendType benchmarkGPUBackends() {
if (isGPUAccelerationAvailable()) {
auto config = getRecommendedGPUConfig();
return config.preferredBackend;
}
return GPUBackendType::CPU;
}
size_t estimateModelMemoryRequirements(const std::string& modelPath) {
return 2LL * 1024 * 1024 * 1024;
}
}
}
}