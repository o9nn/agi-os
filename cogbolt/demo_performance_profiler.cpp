#include "bolt/core/performance_profiler.hpp"
#include "bolt/core/logging.hpp"
#include <iostream>
#include <thread>
#include <chrono>
#include <vector>
#include <random>
void simulateFileLoading(const std::string& filename, int loadTimeMs) {
BOLT_PROFILE_EDITOR("file_load:" + filename);
std::this_thread::sleep_for(std::chrono::milliseconds(loadTimeMs));
auto& profiler = bolt::PerformanceProfiler::getInstance();
auto currentSession = profiler.getCurrentSession();
if (currentSession) {
}
}
void simulateAIInference(const std::string& modelName, int inferenceTimeMs) {
BOLT_PROFILE_AI("inference:" + modelName);
std::vector<float> data(1000000);
std::this_thread::sleep_for(std::chrono::milliseconds(inferenceTimeMs));
data.clear();
}
void simulateNetworkRequest(const std::string& endpoint, int requestTimeMs) {
BOLT_PROFILE_NETWORK("http_request:" + endpoint);
std::this_thread::sleep_for(std::chrono::milliseconds(requestTimeMs));
}
void simulateMemoryOperations() {
BOLT_PROFILE_MEMORY("memory_intensive_operations");
std::vector<std::vector<int>> largeData;
for (int i = 0; i < 100; ++i) {
largeData.emplace_back(10000, i);
if (i % 10 == 0) {
std::this_thread::sleep_for(std::chrono::milliseconds(5));
}
}
largeData.clear();
}
void demonstrateBasicProfiling() {
std::cout << "\\n=== Basic Profiling Demo ===" << std::endl;
auto& profiler = bolt::PerformanceProfiler::getInstance();
profiler.enable();
auto simpleTask = []() {
BOLT_PROFILE_FUNCTION();
std::this_thread::sleep_for(std::chrono::milliseconds(50));
};
simpleTask();
{
BOLT_PROFILE_SCOPE("complex_calculation");
volatile double result = 0.0;
for (int i = 0; i < 1000000; ++i) {
result += std::sin(i) * std::cos(i);
}
}
{
BOLT_PROFILE_CORE("core_initialization");
std::this_thread::sleep_for(std::chrono::milliseconds(30));
}
{
BOLT_PROFILE_GUI("ui_update");
std::this_thread::sleep_for(std::chrono::milliseconds(16));
}
std::cout << "Basic profiling completed. Metrics recorded: "
<< profiler.getTotalMetricsCount() << std::endl;
}
void demonstrateSessionProfiling() {
std::cout << "\\n=== Session-Based Profiling Demo ===" << std::endl;
auto& profiler = bolt::PerformanceProfiler::getInstance();
std::random_device rd;
std::mt19937 gen(rd());
{
BOLT_PROFILE_SESSION("file_processing_workflow");
std::vector<std::string> files = {"document.txt", "image.png", "data.csv", "config.json"};
std::uniform_int_distribution<> loadTime(20, 100);
for (const auto& file : files) {
simulateFileLoading(file, loadTime(gen));
}
{
BOLT_PROFILE_SCOPE("batch_processing");
std::this_thread::sleep_for(std::chrono::milliseconds(150));
}
auto session = profiler.getCurrentSession();
if (session) {
std::cout << "Session '" << session->getName() << "' has "
<< session->getMetricsCount() << " metrics" << std::endl;
}
}
{
BOLT_PROFILE_SESSION("ai_inference_session");
std::vector<std::string> models = {"text_classifier", "sentiment_analyzer", "summarizer"};
std::uniform_int_distribution<> inferenceTime(50, 200);
for (const auto& model : models) {
simulateAIInference(model, inferenceTime(gen));
}
auto session = profiler.getCurrentSession();
if (session) {
std::cout << "AI session completed with "
<< session->getMetricsCount() << " inferences in "
<< session->getTotalDurationMs() << " ms" << std::endl;
}
}
}
void demonstrateSystemMonitoring() {
std::cout << "\\n=== System Monitoring Demo ===" << std::endl;
auto& profiler = bolt::PerformanceProfiler::getInstance();
profiler.enableSystemMonitoring();
simulateMemoryOperations();
std::vector<std::string> endpoints = {"/api/users", "/api/data", "/api/status"};
std::random_device rd;
std::mt19937 gen(rd());
std::uniform_int_distribution<> requestTime(30, 120);
for (const auto& endpoint : endpoints) {
simulateNetworkRequest(endpoint, requestTime(gen));
}
std::cout << "System monitoring completed. Check detailed report for memory/CPU data." << std::endl;
}
void demonstrateConcurrentProfiling() {
std::cout << "\\n=== Concurrent Profiling Demo ===" << std::endl;
const int numWorkers = 4;
std::vector<std::thread> workers;
for (int i = 0; i < numWorkers; ++i) {
workers.emplace_back([i]() {
std::string sessionName = "worker_thread_" + std::to_string(i);
auto session = bolt::PerformanceProfiler::getInstance().createSession(sessionName);
bolt::PerformanceProfiler::getInstance().setCurrentSession(sessionName);
std::random_device rd;
std::mt19937 gen(rd());
std::uniform_int_distribution<> workTime(10, 50);
for (int j = 0; j < 5; ++j) {
std::string taskName = "task_" + std::to_string(j);
if (i % 2 == 0) {
simulateAIInference("thread_model_" + std::to_string(i), workTime(gen));
} else {
simulateFileLoading("thread_file_" + std::to_string(i) + "_" + std::to_string(j) + ".dat", workTime(gen));
}
}
bolt::PerformanceProfiler::getInstance().endSession(sessionName);
});
}
for (auto& worker : workers) {
worker.join();
}
std::cout << "Concurrent profiling completed with " << numWorkers << " worker threads." << std::endl;
}
void demonstrateDataExport() {
std::cout << "\\n=== Data Export Demo ===" << std::endl;
auto& profiler = bolt::PerformanceProfiler::getInstance();
std::string jsonFile = "/tmp/bolt_profiler_demo.json";
std::string csvFile = "/tmp/bolt_profiler_demo.csv";
profiler.exportToJson(jsonFile);
profiler.exportToCsv(csvFile);
std::cout << "Profiling data exported to:" << std::endl;
std::cout << "  JSON: " << jsonFile << std::endl;
std::cout << "  CSV:  " << csvFile << std::endl;
std::cout << "\\n--- Summary Report ---" << std::endl;
profiler.printSummary();
std::cout << "\\n--- Detailed Report ---" << std::endl;
profiler.printDetailedReport();
}
void demonstrateIntegrationWithLogging() {
std::cout << "\\n=== Integration with Logging System Demo ===" << std::endl;
bolt::LogManager::configureConsoleLogging(bolt::LogLevel::INFO, true);
auto& profiler = bolt::PerformanceProfiler::getInstance();
{
BOLT_PROFILE_SCOPE("logged_operation");
std::this_thread::sleep_for(std::chrono::milliseconds(25));
}
auto session = profiler.createSession("manual_logging_session");
profiler.setCurrentSession("manual_logging_session");
{
BOLT_PROFILE_SCOPE("session_work");
std::this_thread::sleep_for(std::chrono::milliseconds(35));
}
profiler.endSession("manual_logging_session");
profiler.logSessionSummary(*session, bolt::LogLevel::INFO);
std::cout << "Integration with logging system demonstrated." << std::endl;
}
void demonstrateAnalyticsAndFiltering() {
std::cout << "\\n=== Analytics and Filtering Demo ===" << std::endl;
auto& profiler = bolt::PerformanceProfiler::getInstance();
std::cout << "Total metrics recorded: " << profiler.getTotalMetricsCount() << std::endl;
std::cout << "Average metric duration: " << profiler.getAverageMetricDuration() << " ms" << std::endl;
auto categories = profiler.getCategories();
std::cout << "Categories found: ";
for (const auto& category : categories) {
std::cout << category << " ";
}
std::cout << std::endl;
for (const auto& category : categories) {
auto metrics = profiler.getMetricsByCategory(category);
if (!metrics.empty()) {
double totalDuration = 0.0;
for (const auto& metric : metrics) {
totalDuration += metric->getDurationMs();
}
double avgDuration = totalDuration / metrics.size();
std::cout << "Category " << category << ": "
<< metrics.size() << " metrics, avg "
<< avgDuration << " ms" << std::endl;
}
}
}
int main() {
std::cout << "Bolt Performance Profiler Demo" << std::endl;
std::cout << "==============================" << std::endl;
try {
bolt::LogManager::configureConsoleLogging(bolt::LogLevel::WARN, false);
auto& profiler = bolt::PerformanceProfiler::getInstance();
profiler.reset();
demonstrateBasicProfiling();
demonstrateSessionProfiling();
demonstrateSystemMonitoring();
demonstrateConcurrentProfiling();
demonstrateIntegrationWithLogging();
demonstrateAnalyticsAndFiltering();
demonstrateDataExport();
std::cout << "\\n=== Demo Complete ===" << std::endl;
std::cout << "Performance profiler demonstration completed successfully!" << std::endl;
std::cout << "Total metrics collected: " << profiler.getTotalMetricsCount() << std::endl;
std::cout << "\\nCleaning up metrics older than 1 minute..." << std::endl;
profiler.clearOldMetrics(std::chrono::minutes(1));
std::cout << "Metrics after cleanup: " << profiler.getTotalMetricsCount() << std::endl;
} catch (const std::exception& e) {
std::cerr << "Demo failed with exception: " << e.what() << std::endl;
return 1;
}
return 0;
}