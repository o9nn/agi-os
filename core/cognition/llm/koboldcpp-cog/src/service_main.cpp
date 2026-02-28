/**
 * @file service_main.cpp
 * @brief Standalone service launcher for KoboldCpp-Cog
 *
 * Launches the KoboldCpp cognitive inference bridge as a standalone
 * service, independent of CogServer. Useful for development and testing.
 *
 * Usage:
 *   koboldcpp-cog-service [--endpoint URL] [--port PORT]
 */

#include "opencog/koboldcpp/koboldcpp_cog_module.h"
#include "opencog/koboldcpp/cognitive_inference.h"
#include "opencog/koboldcpp/koboldcpp_client.h"

#include <iostream>
#include <string>
#include <csignal>
#include <atomic>

static std::atomic<bool> running{true};

void signal_handler(int) {
    running = false;
}

void print_usage(const char* prog) {
    std::cout << "Usage: " << prog << " [options]\n"
              << "\n"
              << "KoboldCpp Cognitive Inference Service\n"
              << "\n"
              << "Options:\n"
              << "  --endpoint URL   KoboldCpp server URL (default: http://localhost:5001)\n"
              << "  --port PORT      Service listen port (default: 5100)\n"
              << "  --help           Show this help\n"
              << "\n"
              << "This service bridges KoboldCpp LLM inference with OpenCog AtomSpace,\n"
              << "providing cognitive context-aware text generation.\n";
}

int main(int argc, char* argv[]) {
    std::string endpoint = "http://localhost:5001";
    int port = 5100;

    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "--endpoint" && i + 1 < argc) {
            endpoint = argv[++i];
        } else if (arg == "--port" && i + 1 < argc) {
            port = std::stoi(argv[++i]);
        } else if (arg == "--help") {
            print_usage(argv[0]);
            return 0;
        }
    }

    std::signal(SIGINT, signal_handler);
    std::signal(SIGTERM, signal_handler);

    std::cout << "=== KoboldCpp Cognitive Inference Service ===" << std::endl;
    std::cout << "KoboldCpp endpoint: " << endpoint << std::endl;
    std::cout << "Service port: " << port << std::endl;
    std::cout << std::endl;

    // Initialize module
    opencog::koboldcpp::KoboldCppCogModule module(endpoint, true);
    module.init();

    auto inference = module.inference();
    if (inference && inference->is_ready()) {
        std::cout << "Cognitive inference engine ready." << std::endl;
    } else {
        std::cout << "Warning: KoboldCpp not available. "
                  << "Start KoboldCpp at " << endpoint << std::endl;
    }

    std::cout << "Service running. Press Ctrl+C to stop." << std::endl;

    // Simple interactive loop for testing
    while (running) {
        std::cout << "\ncog-infer> ";
        std::string query;
        if (!std::getline(std::cin, query) || query.empty()) {
            continue;
        }

        if (query == "quit" || query == "exit") {
            break;
        }

        if (query == "info") {
            auto client = inference->client();
            auto info = client->get_info();
            std::cout << "Connected: " << (info.connected ? "yes" : "no") << std::endl;
            std::cout << "Model: " << info.model_name << std::endl;
            std::cout << "Max context: " << info.max_context_length << std::endl;
            continue;
        }

        auto result = inference->infer(query);
        std::cout << "\n[" << result.inference_mode << "] "
                  << "(confidence: " << result.confidence << ")\n"
                  << result.response_text << std::endl;
    }

    module.shutdown();
    std::cout << "Service stopped." << std::endl;

    return 0;
}
