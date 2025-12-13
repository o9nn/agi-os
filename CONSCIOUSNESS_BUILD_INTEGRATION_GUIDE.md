# Consciousness Layer Build Integration Guide

## Overview

This guide provides step-by-step instructions for integrating the consciousness layer's Go-based build system with the existing AGI-OS CMake and shell-based build infrastructure.

## Prerequisites

### Required Software
- **Go**: Version 1.21 or later
- **CMake**: Version 3.15 or later (already required)
- **Make**: Standard GNU make
- **Git**: For dependency management

### Verification Commands
```bash
# Check Go installation
go version  # Should show 1.21+

# Check CMake
cmake --version  # Should show 3.15+

# Check Go environment
go env GOPATH
go env GOROOT
```

## Step-by-Step Integration

### Step 1: Update Root CMakeLists.txt

**File**: `/home/ubuntu/agi-os/CMakeLists.txt`

**Location**: Add after Layer 4 (CogBolt) and before Layer 5 (Cognitive-Grip)

**Code to Add**:

```cmake
# ==============================================================================
# Layer 6: Consciousness Layer (EchOllama)
# ==============================================================================

OPTION(BUILD_CONSCIOUSNESS "Build Consciousness Layer (EchOllama)" ON)

IF(BUILD_CONSCIOUSNESS)
    MESSAGE(STATUS "====================================")
    MESSAGE(STATUS "Layer 6: Consciousness (EchOllama)")
    MESSAGE(STATUS "====================================")
    
    # Find Go compiler
    find_program(GO_EXECUTABLE go)
    
    IF(GO_EXECUTABLE)
        MESSAGE(STATUS "  Go compiler found: ${GO_EXECUTABLE}")
        
        # Get Go version
        execute_process(
            COMMAND ${GO_EXECUTABLE} version
            OUTPUT_VARIABLE GO_VERSION_OUTPUT
            OUTPUT_STRIP_TRAILING_WHITESPACE
        )
        MESSAGE(STATUS "  Go version: ${GO_VERSION_OUTPUT}")
        
        # Set Go build flags
        set(GO_BUILD_FLAGS "-v" "-ldflags=-s -w")
        set(CONSCIOUSNESS_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/consciousness")
        set(CONSCIOUSNESS_BINARY_DIR "${CMAKE_BINARY_DIR}/consciousness")
        
        # Create binary directory
        file(MAKE_DIRECTORY ${CONSCIOUSNESS_BINARY_DIR})
        
        # Build main consciousness server
        add_custom_target(consciousness_server ALL
            COMMAND ${GO_EXECUTABLE} build ${GO_BUILD_FLAGS}
                    -o ${CONSCIOUSNESS_BINARY_DIR}/echollama-server
                    ${CONSCIOUSNESS_SOURCE_DIR}/server/simple/embodied_server_enhanced.go
            WORKING_DIRECTORY ${CONSCIOUSNESS_SOURCE_DIR}
            COMMENT "Building EchOllama consciousness server"
            VERBATIM
        )
        
        # Build autonomous agents
        add_custom_target(consciousness_agents ALL
            COMMAND ${GO_EXECUTABLE} build ${GO_BUILD_FLAGS}
                    -o ${CONSCIOUSNESS_BINARY_DIR}/echoself-v8
                    ${CONSCIOUSNESS_SOURCE_DIR}/cmd/autonomous_echoself_v8/main.go
            COMMAND ${GO_EXECUTABLE} build ${GO_BUILD_FLAGS}
                    -o ${CONSCIOUSNESS_BINARY_DIR}/echoself-unified
                    ${CONSCIOUSNESS_SOURCE_DIR}/cmd/autonomous_unified/main.go
            WORKING_DIRECTORY ${CONSCIOUSNESS_SOURCE_DIR}
            COMMENT "Building autonomous consciousness agents"
            VERBATIM
        )
        
        # Build EchoBridge for integration
        add_custom_target(consciousness_bridge ALL
            COMMAND ${GO_EXECUTABLE} build ${GO_BUILD_FLAGS}
                    -o ${CONSCIOUSNESS_BINARY_DIR}/echobridge
                    ${CONSCIOUSNESS_SOURCE_DIR}/cmd/echobridge_standalone/main.go
            WORKING_DIRECTORY ${CONSCIOUSNESS_SOURCE_DIR}
            COMMENT "Building EchoBridge integration component"
            VERBATIM
        )
        
        # Install binaries
        install(PROGRAMS 
            ${CONSCIOUSNESS_BINARY_DIR}/echollama-server
            DESTINATION bin
            RENAME agi-consciousness
        )
        
        install(PROGRAMS 
            ${CONSCIOUSNESS_BINARY_DIR}/echoself-v8
            ${CONSCIOUSNESS_BINARY_DIR}/echoself-unified
            DESTINATION bin
        )
        
        install(PROGRAMS 
            ${CONSCIOUSNESS_BINARY_DIR}/echobridge
            DESTINATION bin
            RENAME agi-echobridge
        )
        
        # Install web dashboard
        install(DIRECTORY ${CONSCIOUSNESS_SOURCE_DIR}/web/
            DESTINATION share/agi-os/consciousness/web
            FILES_MATCHING PATTERN "*.html" PATTERN "*.css" PATTERN "*.js"
        )
        
        # Install models (if present)
        if(EXISTS ${CONSCIOUSNESS_SOURCE_DIR}/models)
            install(DIRECTORY ${CONSCIOUSNESS_SOURCE_DIR}/models/
                DESTINATION share/agi-os/consciousness/models
                FILES_MATCHING PATTERN "*.gguf"
            )
        endif()
        
        # Install templates
        install(DIRECTORY ${CONSCIOUSNESS_SOURCE_DIR}/template/
            DESTINATION share/agi-os/consciousness/templates
        )
        
        MESSAGE(STATUS "  Consciousness layer configured successfully")
        MESSAGE(STATUS "  Binaries will be installed to: ${CMAKE_INSTALL_PREFIX}/bin")
        MESSAGE(STATUS "  Data will be installed to: ${CMAKE_INSTALL_PREFIX}/share/agi-os/consciousness")
        
    ELSE()
        MESSAGE(WARNING "  Go compiler not found!")
        MESSAGE(WARNING "  Consciousness layer will not be built")
        MESSAGE(WARNING "  Install Go 1.21+ to enable consciousness layer")
        MESSAGE(WARNING "  Visit: https://go.dev/doc/install")
    ENDIF()
    
ELSE()
    MESSAGE(STATUS "Skipping Layer 6: Consciousness (disabled)")
ENDIF()
```

### Step 2: Update build-agi-os.sh

**File**: `/home/ubuntu/agi-os/build-agi-os.sh`

**Location**: Add after CogBolt build section and before final summary

**Code to Add**:

```bash
# ==============================================================================
# Layer 6: Consciousness Layer (EchOllama)
# ==============================================================================

BUILD_CONSCIOUSNESS=1  # Enable by default

if [ $BUILD_CONSCIOUSNESS -eq 1 ]; then
    log_info "======================================"
    log_info "Layer 6: Consciousness (EchOllama)"
    log_info "======================================"
    
    # Check for Go compiler
    if ! command -v go &> /dev/null; then
        log_warning "Go compiler not found - skipping consciousness layer"
        log_warning "Install Go 1.21+ from https://go.dev/doc/install"
    else
        GO_VERSION=$(go version | awk '{print $3}')
        log_info "Found Go: $GO_VERSION"
        
        # Set up Go environment
        export GOPATH="${HOME}/go"
        export PATH="${GOPATH}/bin:${PATH}"
        
        # Create build directory
        mkdir -p "$BUILD_DIR/consciousness"
        cd "$BUILD_DIR/../consciousness"
        
        # Download Go dependencies
        log_info "Downloading Go dependencies..."
        go mod download || log_warning "Failed to download some dependencies"
        
        # Build main consciousness server
        log_info "Building consciousness server..."
        go build -v -ldflags="-s -w" \
                 -o "$BUILD_DIR/consciousness/echollama-server" \
                 server/simple/embodied_server_enhanced.go
        
        if [ $? -eq 0 ]; then
            log_success "Consciousness server built successfully"
        else
            log_error "Failed to build consciousness server"
            exit 1
        fi
        
        # Build autonomous agents
        log_info "Building autonomous agents..."
        
        # EchoSelf v8
        if [ -f "cmd/autonomous_echoself_v8/main.go" ]; then
            go build -v -ldflags="-s -w" \
                     -o "$BUILD_DIR/consciousness/echoself-v8" \
                     cmd/autonomous_echoself_v8/main.go
            log_success "Built EchoSelf v8 agent"
        fi
        
        # Unified autonomous agent
        if [ -f "cmd/autonomous_unified/main.go" ]; then
            go build -v -ldflags="-s -w" \
                     -o "$BUILD_DIR/consciousness/echoself-unified" \
                     cmd/autonomous_unified/main.go
            log_success "Built unified autonomous agent"
        fi
        
        # Build EchoBridge
        log_info "Building EchoBridge integration..."
        if [ -f "cmd/echobridge_standalone/main.go" ]; then
            go build -v -ldflags="-s -w" \
                     -o "$BUILD_DIR/consciousness/echobridge" \
                     cmd/echobridge_standalone/main.go
            log_success "Built EchoBridge"
        fi
        
        # Install binaries
        log_info "Installing consciousness binaries..."
        install -m 755 "$BUILD_DIR/consciousness/echollama-server" \
                "$INSTALL_PREFIX/bin/agi-consciousness"
        
        if [ -f "$BUILD_DIR/consciousness/echoself-v8" ]; then
            install -m 755 "$BUILD_DIR/consciousness/echoself-v8" \
                    "$INSTALL_PREFIX/bin/"
        fi
        
        if [ -f "$BUILD_DIR/consciousness/echoself-unified" ]; then
            install -m 755 "$BUILD_DIR/consciousness/echoself-unified" \
                    "$INSTALL_PREFIX/bin/"
        fi
        
        if [ -f "$BUILD_DIR/consciousness/echobridge" ]; then
            install -m 755 "$BUILD_DIR/consciousness/echobridge" \
                    "$INSTALL_PREFIX/bin/agi-echobridge"
        fi
        
        # Install web dashboard
        log_info "Installing web dashboard..."
        mkdir -p "$INSTALL_PREFIX/share/agi-os/consciousness/web"
        cp -r web/*.html web/*.css web/*.js \
              "$INSTALL_PREFIX/share/agi-os/consciousness/web/" 2>/dev/null || true
        
        # Install templates
        log_info "Installing templates..."
        mkdir -p "$INSTALL_PREFIX/share/agi-os/consciousness/templates"
        cp -r template/* \
              "$INSTALL_PREFIX/share/agi-os/consciousness/templates/" 2>/dev/null || true
        
        # Install models (if present)
        if [ -d "models" ]; then
            log_info "Installing models..."
            mkdir -p "$INSTALL_PREFIX/share/agi-os/consciousness/models"
            cp models/*.gguf \
               "$INSTALL_PREFIX/share/agi-os/consciousness/models/" 2>/dev/null || true
        fi
        
        log_success "Consciousness layer built and installed"
        
        cd "$BUILD_DIR/.."
    fi
fi
```

### Step 3: Create Go Module Configuration

**File**: `/home/ubuntu/agi-os/consciousness/go.mod` (if not exists)

Check if `go.mod` exists in the consciousness directory:

```bash
cd /home/ubuntu/agi-os/consciousness
ls -la go.mod
```

If it doesn't exist, create it:

```bash
cd /home/ubuntu/agi-os/consciousness
go mod init github.com/o9nn/agi-os/consciousness
go mod tidy
```

This will scan all Go files and automatically add required dependencies.

### Step 4: Create Consciousness Build Helper Script

**File**: `/home/ubuntu/agi-os/consciousness/build.sh`

**Purpose**: Standalone build script for development and testing

```bash
#!/bin/bash
# Consciousness Layer Build Script

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

# Check Go installation
if ! command -v go &> /dev/null; then
    log_error "Go compiler not found"
    log_error "Install Go 1.21+ from https://go.dev/doc/install"
    exit 1
fi

GO_VERSION=$(go version | awk '{print $3}')
log_info "Using Go: $GO_VERSION"

# Set build directory
BUILD_DIR="${BUILD_DIR:-./build}"
mkdir -p "$BUILD_DIR"

# Download dependencies
log_info "Downloading dependencies..."
go mod download

# Build consciousness server
log_info "Building consciousness server..."
go build -v -ldflags="-s -w" \
         -o "$BUILD_DIR/echollama-server" \
         server/simple/embodied_server_enhanced.go

# Build autonomous agents
log_info "Building autonomous agents..."

if [ -f "cmd/autonomous_echoself_v8/main.go" ]; then
    go build -v -ldflags="-s -w" \
             -o "$BUILD_DIR/echoself-v8" \
             cmd/autonomous_echoself_v8/main.go
    log_info "Built EchoSelf v8"
fi

if [ -f "cmd/autonomous_unified/main.go" ]; then
    go build -v -ldflags="-s -w" \
             -o "$BUILD_DIR/echoself-unified" \
             cmd/autonomous_unified/main.go
    log_info "Built unified agent"
fi

# Build EchoBridge
if [ -f "cmd/echobridge_standalone/main.go" ]; then
    go build -v -ldflags="-s -w" \
             -o "$BUILD_DIR/echobridge" \
             cmd/echobridge_standalone/main.go
    log_info "Built EchoBridge"
fi

log_info "Build complete! Binaries in: $BUILD_DIR"
log_info ""
log_info "To run consciousness server:"
log_info "  $BUILD_DIR/echollama-server"
log_info ""
log_info "To install system-wide:"
log_info "  sudo cp $BUILD_DIR/echollama-server /usr/local/bin/agi-consciousness"
```

Make it executable:
```bash
chmod +x /home/ubuntu/agi-os/consciousness/build.sh
```

### Step 5: Update Cognitive-Grip Integration

**File**: `/home/ubuntu/agi-os/core/integration/cognitive-grip/CMakeLists.txt`

Add consciousness bridge to the source files:

```cmake
# Add after cogbolt_bridge.cpp
set(SOURCES
    src/cognitive_grip.cpp
    src/machspace_bridge.cpp
    src/hurdcog_bridge.cpp
    src/cognumach_bridge.cpp
    src/cogbolt_bridge.cpp
    src/consciousness_bridge.cpp  # NEW
    src/unified_config.cpp
)
```

### Step 6: Create Consciousness Bridge Implementation

**File**: `/home/ubuntu/agi-os/core/integration/cognitive-grip/src/consciousness_bridge.cpp`

```cpp
#include <consciousness_bridge.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <map>
#include <curl/curl.h>
#include <json/json.h>

namespace agi_os {
namespace cognitive_grip {

class ConsciousnessBridge::Impl {
public:
    std::string consciousness_url;
    std::shared_ptr<opencog::AtomSpace> atomspace;
    
    Impl(const std::string& url, std::shared_ptr<opencog::AtomSpace> as)
        : consciousness_url(url), atomspace(as) {
        curl_global_init(CURL_GLOBAL_DEFAULT);
    }
    
    ~Impl() {
        curl_global_cleanup();
    }
    
    // HTTP POST helper
    std::string httpPost(const std::string& endpoint, const std::string& json_data) {
        CURL* curl = curl_easy_init();
        std::string response;
        
        if (curl) {
            std::string url = consciousness_url + endpoint;
            
            curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
            curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json_data.c_str());
            
            struct curl_slist* headers = nullptr;
            headers = curl_slist_append(headers, "Content-Type: application/json");
            curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
            
            // Response callback
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, 
                [](char* ptr, size_t size, size_t nmemb, std::string* data) -> size_t {
                    data->append(ptr, size * nmemb);
                    return size * nmemb;
                });
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
            
            CURLcode res = curl_easy_perform(curl);
            
            curl_slist_free_all(headers);
            curl_easy_cleanup(curl);
            
            if (res != CURLE_OK) {
                std::cerr << "CURL error: " << curl_easy_strerror(res) << std::endl;
                return "";
            }
        }
        
        return response;
    }
};

ConsciousnessBridge::ConsciousnessBridge(
    const std::string& consciousness_url,
    std::shared_ptr<opencog::AtomSpace> atomspace)
    : impl_(std::make_unique<Impl>(consciousness_url, atomspace)) {
}

ConsciousnessBridge::~ConsciousnessBridge() = default;

bool ConsciousnessBridge::registerConsciousnessState(
    const std::string& state_type,
    const std::map<std::string, std::string>& state_data) {
    
    // Create JSON request
    Json::Value request;
    request["model"] = "local";
    request["prompt"] = "Register state: " + state_type;
    
    for (const auto& [key, value] : state_data) {
        request["context"][key] = value;
    }
    
    Json::StreamWriterBuilder writer;
    std::string json_str = Json::writeString(writer, request);
    
    // Send to consciousness server
    std::string response = impl_->httpPost("/api/state/register", json_str);
    
    // Register in AtomSpace
    opencog::Handle state_node = impl_->atomspace->add_node(
        opencog::CONCEPT_NODE, 
        "ConsciousnessState:" + state_type
    );
    
    return !response.empty();
}

std::string ConsciousnessBridge::queryConsciousness(
    const std::string& query,
    const std::map<std::string, std::string>& context) {
    
    Json::Value request;
    request["model"] = "local";
    request["prompt"] = query;
    
    for (const auto& [key, value] : context) {
        request["context"][key] = value;
    }
    
    Json::StreamWriterBuilder writer;
    std::string json_str = Json::writeString(writer, request);
    
    std::string response = impl_->httpPost("/api/generate", json_str);
    
    // Parse response
    Json::CharReaderBuilder reader;
    Json::Value response_json;
    std::istringstream response_stream(response);
    
    if (Json::parseFromStream(reader, response_stream, &response_json, nullptr)) {
        return response_json["response"].asString();
    }
    
    return "";
}

std::map<std::string, float> ConsciousnessBridge::getConsciousnessMetrics() {
    std::map<std::string, float> metrics;
    
    std::string response = impl_->httpPost("/api/metrics", "{}");
    
    Json::CharReaderBuilder reader;
    Json::Value response_json;
    std::istringstream response_stream(response);
    
    if (Json::parseFromStream(reader, response_stream, &response_json, nullptr)) {
        metrics["awareness"] = response_json["awareness"].asFloat();
        metrics["attention"] = response_json["attention"].asFloat();
        metrics["coherence"] = response_json["coherence"].asFloat();
    }
    
    return metrics;
}

bool ConsciousnessBridge::triggerSelfReflection(const std::string& focus) {
    Json::Value request;
    request["model"] = "local";
    request["prompt"] = "Reflect on: " + focus;
    request["mode"] = "self_reflection";
    
    Json::StreamWriterBuilder writer;
    std::string json_str = Json::writeString(writer, request);
    
    std::string response = impl_->httpPost("/api/reflect", json_str);
    return !response.empty();
}

} // namespace cognitive_grip
} // namespace agi_os
```

**Header File**: `/home/ubuntu/agi-os/core/integration/cognitive-grip/include/consciousness_bridge.h`

```cpp
#ifndef AGI_OS_CONSCIOUSNESS_BRIDGE_H
#define AGI_OS_CONSCIOUSNESS_BRIDGE_H

#include <memory>
#include <string>
#include <vector>
#include <map>

namespace opencog {
    class AtomSpace;
    class Handle;
}

namespace agi_os {
namespace cognitive_grip {

class ConsciousnessBridge {
public:
    ConsciousnessBridge(
        const std::string& consciousness_url,
        std::shared_ptr<opencog::AtomSpace> atomspace
    );
    
    ~ConsciousnessBridge();
    
    // Register consciousness state in AtomSpace
    bool registerConsciousnessState(
        const std::string& state_type,
        const std::map<std::string, std::string>& state_data
    );
    
    // Query consciousness layer
    std::string queryConsciousness(
        const std::string& query,
        const std::map<std::string, std::string>& context = {}
    );
    
    // Get consciousness metrics
    std::map<std::string, float> getConsciousnessMetrics();
    
    // Trigger self-reflection
    bool triggerSelfReflection(const std::string& focus);
    
private:
    class Impl;
    std::unique_ptr<Impl> impl_;
};

} // namespace cognitive_grip
} // namespace agi_os

#endif // AGI_OS_CONSCIOUSNESS_BRIDGE_H
```

### Step 7: Create Systemd Service (Optional)

**File**: `/etc/systemd/system/agi-consciousness.service`

```ini
[Unit]
Description=AGI-OS Consciousness Layer (EchOllama)
After=network.target

[Service]
Type=simple
User=agi-os
Group=agi-os
WorkingDirectory=/usr/share/agi-os/consciousness
ExecStart=/usr/local/bin/agi-consciousness
Restart=on-failure
RestartSec=10
Environment="CONSCIOUSNESS_PORT=5000"
Environment="CONSCIOUSNESS_HOST=0.0.0.0"

[Install]
WantedBy=multi-user.target
```

Enable and start:
```bash
sudo systemctl daemon-reload
sudo systemctl enable agi-consciousness
sudo systemctl start agi-consciousness
```

### Step 8: Testing the Integration

**Test Script**: `/home/ubuntu/agi-os/test_consciousness_integration.sh`

```bash
#!/bin/bash

echo "Testing Consciousness Layer Integration"
echo "========================================"

# Test 1: Check Go compiler
echo "Test 1: Checking Go compiler..."
if command -v go &> /dev/null; then
    echo "✓ Go compiler found: $(go version)"
else
    echo "✗ Go compiler not found"
    exit 1
fi

# Test 2: Build consciousness layer
echo ""
echo "Test 2: Building consciousness layer..."
cd consciousness
./build.sh
if [ $? -eq 0 ]; then
    echo "✓ Build successful"
else
    echo "✗ Build failed"
    exit 1
fi

# Test 3: Start consciousness server
echo ""
echo "Test 3: Starting consciousness server..."
./build/echollama-server &
SERVER_PID=$!
sleep 5

# Test 4: Query consciousness server
echo ""
echo "Test 4: Querying consciousness server..."
RESPONSE=$(curl -s -X POST http://localhost:5000/api/generate \
  -H "Content-Type: application/json" \
  -d '{"model": "local", "prompt": "Hello"}')

if [ -n "$RESPONSE" ]; then
    echo "✓ Server responding"
    echo "Response: $RESPONSE"
else
    echo "✗ Server not responding"
    kill $SERVER_PID
    exit 1
fi

# Test 5: Stop server
echo ""
echo "Test 5: Stopping server..."
kill $SERVER_PID
echo "✓ Server stopped"

echo ""
echo "========================================"
echo "All tests passed!"
```

Make it executable:
```bash
chmod +x /home/ubuntu/agi-os/test_consciousness_integration.sh
```

## Build Order Summary

The updated build order with consciousness layer:

```
0. MIG (Mach Interface Generator)
0.5. GGML (AI Inference Engine)
1. CogNumach (Microkernel)
2. HurdCog (Operating System)
3. OpenCog Collection (Foundation → Storage → Network → Reasoning → Learning)
4. CogBolt (AI-Powered IDE)
6. Consciousness (EchOllama) ⭐ NEW
5. Cognitive-Grip (Integration Layer) - Updated to include consciousness bridge
```

## Execution Commands

### Full Build
```bash
cd /home/ubuntu/agi-os
./build-agi-os.sh --all
```

### CMake Build
```bash
mkdir build && cd build
cmake .. -DBUILD_CONSCIOUSNESS=ON
make -j$(nproc)
sudo make install
```

### Standalone Consciousness Build
```bash
cd /home/ubuntu/agi-os/consciousness
./build.sh
```

### Run Consciousness Server
```bash
# After installation
agi-consciousness

# Or directly
/usr/local/bin/agi-consciousness

# Or from build directory
./consciousness/build/echollama-server
```

## Troubleshooting

### Issue 1: Go Not Found
**Error**: `go: command not found`

**Solution**:
```bash
# Install Go
wget https://go.dev/dl/go1.21.5.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.21.5.linux-amd64.tar.gz
export PATH=$PATH:/usr/local/go/bin
echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.bashrc
```

### Issue 2: Go Module Errors
**Error**: `go.mod file not found`

**Solution**:
```bash
cd /home/ubuntu/agi-os/consciousness
go mod init github.com/o9nn/agi-os/consciousness
go mod tidy
```

### Issue 3: Missing Dependencies
**Error**: `cannot find package`

**Solution**:
```bash
cd /home/ubuntu/agi-os/consciousness
go mod download
go get -u ./...
```

### Issue 4: Build Fails
**Error**: Various build errors

**Solution**:
```bash
# Clean and rebuild
cd /home/ubuntu/agi-os/consciousness
rm -rf build/
go clean -cache
./build.sh
```

### Issue 5: Server Won't Start
**Error**: `address already in use`

**Solution**:
```bash
# Check what's using port 5000
sudo lsof -i :5000

# Kill existing process
kill $(lsof -t -i:5000)

# Or use different port
export CONSCIOUSNESS_PORT=5001
agi-consciousness
```

## Next Steps

1. **Implement the CMakeLists.txt changes** (Step 1)
2. **Update build-agi-os.sh** (Step 2)
3. **Create consciousness build script** (Step 4)
4. **Test standalone build** (Step 8)
5. **Implement Cognitive-Grip bridge** (Steps 5-6)
6. **Test full integration** (Step 8)
7. **Create systemd service** (Step 7)
8. **Update documentation** (BUILD_DEPENDENCY_ANALYSIS.md)

## Estimated Time

- CMake integration: 1 hour
- Build script updates: 1 hour
- Cognitive-Grip bridge: 2 hours
- Testing and debugging: 2 hours
- Documentation: 1 hour
- **Total**: ~7 hours

## Conclusion

This guide provides complete integration of the Go-based consciousness layer into the AGI-OS build system. The integration maintains consistency with existing CMake patterns while properly handling Go's unique build requirements.

---

**Last Updated**: December 12, 2025  
**Status**: Ready for implementation  
**Priority**: High (enables Layer 6 functionality)
