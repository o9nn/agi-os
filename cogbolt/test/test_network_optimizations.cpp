#include <gtest/gtest.h>
#include "bolt/network/connection_pool.hpp"
#include "bolt/network/message_compression.hpp"
#include "bolt/network/network_buffer.hpp"
#include "bolt/network/network_metrics.hpp"
#include <thread>
#include <chrono>

namespace bolt {
namespace test {

class NetworkOptimizationsTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Reset metrics before each test
        NetworkMetrics::getInstance().resetAllStats();
    }
    
    void TearDown() override {
        // Clean up after each test
    }
};

// Connection Pool Tests
TEST_F(NetworkOptimizationsTest, ConnectionPoolBasicOperations) {
    auto& pool = ConnectionPool::getInstance();
    pool.setMaxConnectionsPerHost(5);
    
    // Test connection creation
    auto conn1 = pool.getConnection("127.0.0.1", 8080);
    EXPECT_NE(conn1, nullptr);
    EXPECT_EQ(conn1->host, "127.0.0.1");
    EXPECT_EQ(conn1->port, 8080);
    EXPECT_TRUE(conn1->inUse);
    
    // Test connection reuse
    pool.releaseConnection(conn1);
    EXPECT_FALSE(conn1->inUse);
    
    auto conn2 = pool.getConnection("127.0.0.1", 8080);
    EXPECT_EQ(conn1.get(), conn2.get()); // Should reuse the same connection
    
    pool.closeConnection(conn2);
}

TEST_F(NetworkOptimizationsTest, ConnectionPoolStats) {
    auto& pool = ConnectionPool::getInstance();
    pool.resetStats();
    
    auto initialStats = pool.getStats();
    EXPECT_EQ(initialStats.connectionsCreated, 0);
    EXPECT_EQ(initialStats.connectionsReused, 0);
    
    // Create a connection
    auto conn1 = pool.getConnection("127.0.0.1", 8080);
    auto statsAfterCreate = pool.getStats();
    EXPECT_EQ(statsAfterCreate.connectionsCreated, 1);
    
    // Release and get again (should reuse)
    pool.releaseConnection(conn1);
    auto conn2 = pool.getConnection("127.0.0.1", 8080);
    auto statsAfterReuse = pool.getStats();
    EXPECT_EQ(statsAfterReuse.connectionsReused, 1);
    
    pool.closeConnection(conn2);
}

// Message Compression Tests
TEST_F(NetworkOptimizationsTest, MessageCompressionBasic) {
    MessageCompressor compressor(CompressionType::GZIP, 6);
    
    std::string originalMessage = "This is a test message that should compress well when repeated. "
                                 "This is a test message that should compress well when repeated. "
                                 "This is a test message that should compress well when repeated.";
    
    auto compressed = compressor.compress(originalMessage);
    EXPECT_LT(compressed.size(), originalMessage.size()); // Should be smaller
    
    auto decompressed = compressor.decompress(compressed);
    EXPECT_EQ(decompressed, originalMessage);
}

TEST_F(NetworkOptimizationsTest, MessageCompressionThreshold) {
    MessageCompressor compressor(CompressionType::GZIP, 6);
    compressor.setMinCompressionSize(100);
    
    std::string shortMessage = "Short";
    auto compressedShort = compressor.compress(shortMessage);
    EXPECT_EQ(compressedShort.size(), shortMessage.size()); // Should not compress
    
    std::string longMessage(200, 'A'); // 200 'A' characters
    auto compressedLong = compressor.compress(longMessage);
    EXPECT_LT(compressedLong.size(), longMessage.size()); // Should compress
}

TEST_F(NetworkOptimizationsTest, MessageCompressionStats) {
    MessageCompressor compressor(CompressionType::GZIP, 6);
    compressor.resetStats();
    
    auto initialStats = compressor.getStats();
    EXPECT_EQ(initialStats.compressionCalls, 0);
    EXPECT_EQ(initialStats.decompressionCalls, 0);
    
    std::string message(500, 'X');
    auto compressed = compressor.compress(message);
    auto decompressed = compressor.decompress(compressed);
    
    auto finalStats = compressor.getStats();
    EXPECT_EQ(finalStats.compressionCalls, 1);
    EXPECT_EQ(finalStats.decompressionCalls, 1);
    EXPECT_GT(finalStats.totalBytesIn, 0);
    EXPECT_GT(finalStats.totalBytesOut, 0);
}

// Network Buffer Tests
TEST_F(NetworkOptimizationsTest, NetworkBufferBasicOperations) {
    NetworkBuffer buffer(1024);
    
    // Test append operations
    std::string testData = "Hello, World!";
    buffer.append(testData);
    EXPECT_EQ(buffer.size(), testData.size());
    EXPECT_FALSE(buffer.empty());
    
    // Test consume operations
    auto consumed = buffer.consumeString(5);
    EXPECT_EQ(consumed, "Hello");
    EXPECT_EQ(buffer.size(), testData.size()); // Size unchanged until compact
    
    // Test compact
    buffer.compact();
    EXPECT_EQ(buffer.size(), testData.size() - 5);
    
    buffer.clear();
    EXPECT_TRUE(buffer.empty());
    EXPECT_EQ(buffer.size(), 0);
}

TEST_F(NetworkOptimizationsTest, NetworkBufferGrowth) {
    NetworkBuffer buffer(10); // Small initial size
    
    std::string largeData(100, 'A');
    buffer.append(largeData);
    
    EXPECT_GE(buffer.capacity(), largeData.size());
    EXPECT_EQ(buffer.size(), largeData.size());
    
    auto consumed = buffer.consumeString(largeData.size());
    EXPECT_EQ(consumed, largeData);
}

TEST_F(NetworkOptimizationsTest, NetworkBufferPool) {
    auto& pool = NetworkBufferPool::getInstance();
    
    // Get buffer from pool
    auto buffer1 = pool.getBuffer(1024);
    EXPECT_NE(buffer1, nullptr);
    EXPECT_GE(buffer1->capacity(), 1024);
    
    size_t activeCount = pool.getActiveBuffers();
    EXPECT_GT(activeCount, 0);
    
    // Return buffer to pool
    pool.returnBuffer(std::move(buffer1));
    
    // Get another buffer (should reuse from pool)
    auto buffer2 = pool.getBuffer(1024);
    EXPECT_NE(buffer2, nullptr);
    
    pool.returnBuffer(std::move(buffer2));
}

// Ring Buffer Tests
TEST_F(NetworkOptimizationsTest, RingBufferBasicOperations) {
    RingBuffer ringBuf(100);
    
    EXPECT_TRUE(ringBuf.empty());
    EXPECT_FALSE(ringBuf.full());
    EXPECT_EQ(ringBuf.readAvailable(), 0);
    EXPECT_GT(ringBuf.writeAvailable(), 0);
    
    // Write some data
    std::string testData = "Test data";
    size_t written = ringBuf.write(testData.data(), testData.size());
    EXPECT_EQ(written, testData.size());
    EXPECT_EQ(ringBuf.readAvailable(), testData.size());
    
    // Read data back
    char readBuffer[100];
    size_t read = ringBuf.read(readBuffer, testData.size());
    EXPECT_EQ(read, testData.size());
    EXPECT_EQ(std::string(readBuffer, read), testData);
    EXPECT_TRUE(ringBuf.empty());
}

TEST_F(NetworkOptimizationsTest, RingBufferWrapAround) {
    RingBuffer ringBuf(10);
    
    // Fill buffer almost to capacity
    std::string data1 = "12345678";
    ringBuf.write(data1.data(), data1.size());
    
    // Read some data to make space
    char readBuf[5];
    ringBuf.read(readBuf, 4);
    
    // Write more data that should wrap around
    std::string data2 = "ABCDEF";
    size_t written = ringBuf.write(data2.data(), data2.size());
    EXPECT_GT(written, 0); // Should write at least some data
    
    // Verify we can read the wrapped data
    size_t available = ringBuf.readAvailable();
    EXPECT_GT(available, 0);
}

// Network Metrics Tests
TEST_F(NetworkOptimizationsTest, NetworkMetricsBasic) {
    auto& metrics = NetworkMetrics::getInstance();
    metrics.resetAllStats();
    
    std::string endpoint = "test-endpoint";
    metrics.registerEndpoint(endpoint);
    
    // Record some events
    metrics.recordConnection(endpoint, true);
    metrics.recordDataSent(endpoint, 1024);
    metrics.recordDataReceived(endpoint, 512);
    metrics.recordLatency(endpoint, 1500); // 1.5ms
    
    auto stats = metrics.getEndpointStats(endpoint);
    EXPECT_EQ(stats.connectionsOpened, 1);
    EXPECT_EQ(stats.connectionsActive, 1);
    EXPECT_EQ(stats.bytesSent, 1024);
    EXPECT_EQ(stats.bytesReceived, 512);
    EXPECT_EQ(stats.latencySamples, 1);
    EXPECT_EQ(stats.getAverageLatency(), 1500.0);
    
    metrics.recordDisconnection(endpoint);
    stats = metrics.getEndpointStats(endpoint);
    EXPECT_EQ(stats.connectionsActive, 0);
    EXPECT_EQ(stats.connectionsClosed, 1);
}

TEST_F(NetworkOptimizationsTest, NetworkMetricsGlobal) {
    auto& metrics = NetworkMetrics::getInstance();
    metrics.resetAllStats();
    
    std::string endpoint1 = "endpoint1";
    std::string endpoint2 = "endpoint2";
    
    metrics.registerEndpoint(endpoint1);
    metrics.registerEndpoint(endpoint2);
    
    // Record events on both endpoints
    metrics.recordConnection(endpoint1, true);
    metrics.recordConnection(endpoint2, true);
    metrics.recordDataSent(endpoint1, 1000);
    metrics.recordDataSent(endpoint2, 2000);
    
    auto globalStats = metrics.getGlobalStats();
    EXPECT_EQ(globalStats.connectionsOpened, 2);
    EXPECT_EQ(globalStats.bytesSent, 3000);
}

TEST_F(NetworkOptimizationsTest, NetworkMetricsReport) {
    auto& metrics = NetworkMetrics::getInstance();
    metrics.resetAllStats();
    
    std::string endpoint = "test-endpoint";
    metrics.registerEndpoint(endpoint);
    metrics.recordConnection(endpoint, true);
    metrics.recordDataSent(endpoint, 1024);
    
    std::string report = metrics.generateReport();
    EXPECT_FALSE(report.empty());
    EXPECT_NE(report.find("Network Performance Report"), std::string::npos);
    EXPECT_NE(report.find("Global Statistics"), std::string::npos);
    
    std::string endpointReport = metrics.generateEndpointReport(endpoint);
    EXPECT_FALSE(endpointReport.empty());
    EXPECT_NE(endpointReport.find("Endpoint Report"), std::string::npos);
}

// Bandwidth Tracking Tests
TEST_F(NetworkOptimizationsTest, BandwidthTracking) {
    BandwidthTracker tracker(std::chrono::seconds(5));
    
    // Initially no bandwidth
    EXPECT_EQ(tracker.getCurrentBandwidth(), 0.0);
    
    // Record some data
    tracker.recordBytes(1000);
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    tracker.recordBytes(2000);
    
    // Should have some bandwidth now
    double bandwidth = tracker.getCurrentBandwidth();
    EXPECT_GT(bandwidth, 0.0);
    
    double peak = tracker.getPeakBandwidth();
    EXPECT_GE(peak, bandwidth);
}

// Zero-Copy Buffer Tests
TEST_F(NetworkOptimizationsTest, ZeroCopyBuffer) {
    ZeroCopyBuffer zcBuffer;
    
    std::string data1 = "Hello";
    std::string data2 = "World";
    
    zcBuffer.addReference(data1);
    zcBuffer.addReference(data2);
    
    EXPECT_EQ(zcBuffer.getTotalSize(), data1.size() + data2.size());
    
    auto combined = zcBuffer.copyToVector();
    std::string result(combined.begin(), combined.end());
    EXPECT_EQ(result, data1 + data2);
}

// Scatter-Gather Buffer Tests
TEST_F(NetworkOptimizationsTest, ScatterGatherBuffer) {
    ScatterGatherBuffer sgBuffer;
    
    std::string segment1 = "First";
    std::string segment2 = "Second";
    std::vector<uint8_t> segment3 = {'T', 'h', 'i', 'r', 'd'};
    
    sgBuffer.addSegment(segment1);
    sgBuffer.addSegment(segment2);
    sgBuffer.addSegment(segment3);
    
    EXPECT_EQ(sgBuffer.getSegmentCount(), 3);
    EXPECT_EQ(sgBuffer.getTotalSize(), segment1.size() + segment2.size() + segment3.size());
    
    auto flattened = sgBuffer.flatten();
    std::string result(flattened.begin(), flattened.end());
    EXPECT_EQ(result, "FirstSecondThird");
}

} // namespace test
} // namespace bolt