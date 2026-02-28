/**
 * @file koboldcpp_client.h
 * @brief HTTP client for KoboldCpp inference API
 *
 * Provides a C++ interface to KoboldCpp's OpenAI-compatible API endpoints,
 * supporting both synchronous and streaming text generation, chat completion,
 * and multimodal operations (image gen, whisper, TTS).
 *
 * Endpoints supported:
 *   - POST /v1/completions       (OpenAI text completion)
 *   - POST /v1/chat/completions  (OpenAI chat completion)
 *   - POST /api/v1/generate      (KoboldAI native)
 *   - GET  /api/v1/info          (Server info)
 *   - POST /api/extra/whisper    (Speech-to-text)
 *   - POST /api/extra/tts        (Text-to-speech)
 *
 * @see https://github.com/LostRuins/koboldcpp
 */

#ifndef _OPENCOG_KOBOLDCPP_CLIENT_H
#define _OPENCOG_KOBOLDCPP_CLIENT_H

#include <string>
#include <vector>
#include <functional>
#include <memory>
#include <optional>

namespace opencog {
namespace koboldcpp {

/**
 * Generation parameters for LLM inference.
 */
struct GenerationParams {
    float temperature = 0.7f;
    float top_p = 0.9f;
    int top_k = 40;
    float rep_pen = 1.1f;
    int rep_pen_range = 512;
    int max_tokens = 256;
    std::vector<std::string> stop_sequences;
    bool stream = false;
};

/**
 * Chat message for chat completion API.
 */
struct ChatMessage {
    std::string role;    // "system", "user", "assistant"
    std::string content;
};

/**
 * Generation result from the inference API.
 */
struct GenerationResult {
    std::string text;
    int prompt_tokens = 0;
    int completion_tokens = 0;
    std::string finish_reason;  // "stop", "length"
    bool success = false;
    std::string error;
};

/**
 * Server information from /api/v1/info.
 */
struct ServerInfo {
    std::string model_name;
    int max_context_length = 0;
    int max_length = 0;
    bool connected = false;
};

/**
 * Streaming callback: receives partial text as it arrives.
 */
using StreamCallback = std::function<void(const std::string& partial_text)>;

/**
 * @class KoboldCppClient
 * @brief HTTP client for KoboldCpp LLM inference server.
 *
 * Connects to a running KoboldCpp instance and provides methods for
 * text generation, chat completion, and server management.
 *
 * Usage:
 * @code
 *   KoboldCppClient client("http://localhost:5001");
 *   if (client.is_connected()) {
 *       auto result = client.generate("Hello, world!", {});
 *       std::cout << result.text << std::endl;
 *   }
 * @endcode
 */
class KoboldCppClient {
public:
    /**
     * Construct a client connected to the given endpoint.
     * @param endpoint Base URL (e.g., "http://localhost:5001")
     * @param timeout_ms HTTP timeout in milliseconds (default: 30000)
     */
    explicit KoboldCppClient(const std::string& endpoint = "http://localhost:5001",
                              int timeout_ms = 30000);
    ~KoboldCppClient();

    // Non-copyable, movable
    KoboldCppClient(const KoboldCppClient&) = delete;
    KoboldCppClient& operator=(const KoboldCppClient&) = delete;
    KoboldCppClient(KoboldCppClient&&) noexcept;
    KoboldCppClient& operator=(KoboldCppClient&&) noexcept;

    /** Check if the server is reachable. */
    bool is_connected() const;

    /** Get server information. */
    ServerInfo get_info() const;

    /** Get the endpoint URL. */
    const std::string& endpoint() const;

    /**
     * Generate text completion (OpenAI /v1/completions).
     * @param prompt The input prompt
     * @param params Generation parameters
     * @return GenerationResult with generated text
     */
    GenerationResult generate(const std::string& prompt,
                              const GenerationParams& params = {}) const;

    /**
     * Chat completion (OpenAI /v1/chat/completions).
     * @param messages Conversation history
     * @param params Generation parameters
     * @return GenerationResult with assistant response
     */
    GenerationResult chat(const std::vector<ChatMessage>& messages,
                          const GenerationParams& params = {}) const;

    /**
     * Generate with streaming callback.
     * @param prompt The input prompt
     * @param params Generation parameters (stream=true forced)
     * @param callback Called with each partial text chunk
     * @return Final GenerationResult
     */
    GenerationResult generate_stream(const std::string& prompt,
                                     const GenerationParams& params,
                                     StreamCallback callback) const;

    /**
     * Native KoboldAI generation (/api/v1/generate).
     * @param prompt The input prompt
     * @param params Generation parameters
     * @return GenerationResult
     */
    GenerationResult generate_native(const std::string& prompt,
                                     const GenerationParams& params = {}) const;

private:
    struct Impl;
    std::unique_ptr<Impl> pimpl_;
};

} // namespace koboldcpp
} // namespace opencog

#endif // _OPENCOG_KOBOLDCPP_CLIENT_H
