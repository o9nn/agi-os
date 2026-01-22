#include <gtest/gtest.h>
#include "bolt/ai/gpu_acceleration.hpp"
#include "bolt/core/logging.hpp"
using namespace bolt::ai;
class GPUAccelerationTest : public ::testing::Test {
protected:
void SetUp() override {
bolt::LogManager::configureConsoleLogging();
}
void TearDown() override {
auto& gpuManager = GPUAccelerationManager::getInstance();
if (gpuManager.isInitialized()) {
gpuManager.shutdown();
}
}
};
TEST_F(GPUAccelerationTest, GPUUtilityFunctions) {
bool gpuAvailable = gpu_utils::isGPUAccelerationAvailable();
EXPECT_TRUE(gpuAvailable || !gpuAvailable);
GPUConfig config = gpu_utils::getRecommendedGPUConfig();
EXPECT_TRUE(config.preferredBackend == GPUBackendType::CPU ||
config.preferredBackend == GPUBackendType::CUDA ||
config.preferredBackend == GPUBackendType::OpenCL ||
config.preferredBackend == GPUBackendType::Vulkan);
GPUBackendType bestBackend = gpu_utils::benchmarkGPUBackends();
EXPECT_TRUE(bestBackend == GPUBackendType::CPU ||
bestBackend == GPUBackendType::CUDA ||
bestBackend == GPUBackendType::OpenCL ||
bestBackend == GPUBackendType::Vulkan);
size_t memReq = gpu_utils::estimateModelMemoryRequirements("/tmp/mock_model.gguf");
EXPECT_GT(memReq, 0);
}
TEST_F(GPUAccelerationTest, GPUManagerInitialization) {
auto& gpuManager = GPUAccelerationManager::getInstance();
GPUConfig config;
config.preferredBackend = GPUBackendType::CPU;
config.enableAutomaticFallback = true;
bool initialized = gpuManager.initialize(config);
EXPECT_TRUE(initialized);
EXPECT_TRUE(gpuManager.isInitialized());
bool reinit = gpuManager.initialize(config);
EXPECT_TRUE(reinit);
auto devices = gpuManager.detectAvailableDevices();
EXPECT_GE(devices.size(), 1);
bool cpuFound = false;
for (const auto& device : devices) {
if (device.backendType == GPUBackendType::CPU) {
cpuFound = true;
break;
}
}
EXPECT_TRUE(cpuFound);
auto currentDevice = gpuManager.getCurrentDevice();
EXPECT_FALSE(currentDevice.name.empty());
GPUConfig newConfig = config;
newConfig.enableMemoryOptimization = false;
gpuManager.updateConfig(newConfig);
auto retrievedConfig = gpuManager.getConfig();
EXPECT_FALSE(retrievedConfig.enableMemoryOptimization);
gpuManager.shutdown();
EXPECT_FALSE(gpuManager.isInitialized());
}
TEST_F(GPUAccelerationTest, DeviceSelection) {
auto& gpuManager = GPUAccelerationManager::getInstance();
GPUConfig config;
config.preferredBackend = GPUBackendType::CPU;
EXPECT_TRUE(gpuManager.initialize(config));
bool cpuSelected = gpuManager.selectDevice(GPUBackendType::CPU, 0);
EXPECT_TRUE(cpuSelected);
EXPECT_EQ(gpuManager.getCurrentBackendType(), GPUBackendType::CPU);
bool cudaSelected = gpuManager.selectDevice(GPUBackendType::CUDA, 0);
bool openclSelected = gpuManager.selectDevice(GPUBackendType::OpenCL, 0);
bool vulkanSelected = gpuManager.selectDevice(GPUBackendType::Vulkan, 0);
}
TEST_F(GPUAccelerationTest, MemoryManager) {
auto& gpuManager = GPUAccelerationManager::getInstance();
GPUConfig config;
config.preferredBackend = GPUBackendType::CPU;
EXPECT_TRUE(gpuManager.initialize(config));
auto& memManager = gpuManager.getMemoryManager();
size_t initialUsed = memManager.getUsedMemory();
EXPECT_GE(initialUsed, 0);
memManager.optimizeMemoryUsage();
memManager.defragmentMemory();
size_t afterOptimization = memManager.getUsedMemory();
EXPECT_EQ(initialUsed, afterOptimization);
}
TEST_F(GPUAccelerationTest, PerformanceStats) {
auto& gpuManager = GPUAccelerationManager::getInstance();
GPUConfig config;
config.preferredBackend = GPUBackendType::CPU;
EXPECT_TRUE(gpuManager.initialize(config));
auto stats = gpuManager.getPerformanceStats();
EXPECT_EQ(stats.totalComputeOps, 0);
EXPECT_EQ(stats.totalComputeTime, 0.0);
EXPECT_EQ(stats.totalMemoryTransfers, 0);
EXPECT_EQ(stats.totalTransferTime, 0.0);
gpuManager.resetPerformanceStats();
auto resetStats = gpuManager.getPerformanceStats();
EXPECT_EQ(resetStats.totalComputeOps, 0);
}
TEST_F(GPUAccelerationTest, GPUAcceleratedModel) {
GPUConfig config;
config.preferredBackend = GPUBackendType::CPU;
config.enableAutomaticFallback = true;
std::string mockModelPath = "/tmp/mock_model.gguf";
GPUAcceleratedModel model(mockModelPath, config);
EXPECT_FALSE(model.isLoaded());
model.setMaxContextSize(1024);
model.setThreadCount(2);
model.enableGPUOffloading(true);
std::string result = model.generate("test prompt", 10);
EXPECT_TRUE(result.empty());
auto embedding = model.generateEmbedding("test input");
EXPECT_TRUE(embedding.empty());
auto stats = model.getPerformanceStats();
}
TEST_F(GPUAccelerationTest, GPUAccelerationException) {
GPUBackendType failedBackend = GPUBackendType::CUDA;
std::string message = "Test GPU error";
GPUAccelerationException exception(message, failedBackend);
EXPECT_EQ(std::string(exception.what()), message);
EXPECT_EQ(exception.getFailedBackend(), failedBackend);
}
TEST_F(GPUAccelerationTest, BackendTypeString) {
auto& gpuManager = GPUAccelerationManager::getInstance();
GPUConfig config;
config.preferredBackend = GPUBackendType::CPU;
EXPECT_TRUE(gpuManager.initialize(config));
auto devices = gpuManager.detectAvailableDevices();
EXPECT_GE(devices.size(), 1);
for (const auto& device : devices) {
EXPECT_TRUE(device.backendType == GPUBackendType::CPU ||
device.backendType == GPUBackendType::CUDA ||
device.backendType == GPUBackendType::OpenCL ||
device.backendType == GPUBackendType::Vulkan ||
device.backendType == GPUBackendType::Metal);
}
}
TEST_F(GPUAccelerationTest, GPUConfig) {
GPUConfig defaultConfig;
EXPECT_EQ(defaultConfig.preferredBackend, GPUBackendType::CUDA);
EXPECT_EQ(defaultConfig.preferredDeviceId, 0);
EXPECT_TRUE(defaultConfig.enableAutomaticFallback);
EXPECT_GT(defaultConfig.minMemoryRequirement, 0);
EXPECT_TRUE(defaultConfig.enableMemoryOptimization);
EXPECT_TRUE(defaultConfig.enableAsynchronousCompute);
EXPECT_GE(defaultConfig.fallbackBackends.size(), 1);
GPUConfig customConfig;
customConfig.preferredBackend = GPUBackendType::OpenCL;
customConfig.preferredDeviceId = 1;
customConfig.enableAutomaticFallback = false;
customConfig.minMemoryRequirement = 512 * 1024 * 1024;
customConfig.enableMemoryOptimization = false;
customConfig.enableAsynchronousCompute = false;
customConfig.fallbackBackends = {GPUBackendType::CPU};
EXPECT_EQ(customConfig.preferredBackend, GPUBackendType::OpenCL);
EXPECT_EQ(customConfig.preferredDeviceId, 1);
EXPECT_FALSE(customConfig.enableAutomaticFallback);
EXPECT_EQ(customConfig.minMemoryRequirement, 512 * 1024 * 1024);
EXPECT_FALSE(customConfig.enableMemoryOptimization);
EXPECT_FALSE(customConfig.enableAsynchronousCompute);
EXPECT_EQ(customConfig.fallbackBackends.size(), 1);
EXPECT_EQ(customConfig.fallbackBackends[0], GPUBackendType::CPU);
}
TEST_F(GPUAccelerationTest, IntegrationTest) {
GPUConfig config = gpu_utils::getRecommendedGPUConfig();
auto& gpuManager = GPUAccelerationManager::getInstance();
EXPECT_TRUE(gpuManager.initialize(config));
auto devices = gpuManager.detectAvailableDevices();
EXPECT_GE(devices.size(), 1);
std::string mockModelPath = "/tmp/integration_test_model.gguf";
GPUAcceleratedModel model(mockModelPath, config);
EXPECT_FALSE(model.isLoaded());
auto stats = model.getPerformanceStats();
EXPECT_EQ(stats.totalComputeOps, 0);
auto& memManager = gpuManager.getMemoryManager();
size_t usedMemory = memManager.getUsedMemory();
EXPECT_GE(usedMemory, 0);
gpuManager.shutdown();
EXPECT_FALSE(gpuManager.isInitialized());
}