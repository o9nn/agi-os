/**
 * @file koboldcpp_client.cpp
 * @brief Implementation of KoboldCpp HTTP client
 */

#include "opencog/koboldcpp/koboldcpp_client.h"

#include <sstream>
#include <stdexcept>

#ifdef HAVE_CURL
#include <curl/curl.h>
#endif

namespace opencog {
namespace koboldcpp {

struct KoboldCppClient::Impl {
    std::string endpoint;
    int timeout_ms;
    mutable bool connected = false;

    Impl(const std::string& ep, int timeout)
        : endpoint(ep), timeout_ms(timeout) {}

    // HTTP POST helper
    std::string http_post(const std::string& path,
                          const std::string& json_body) const {
#ifdef HAVE_CURL
        CURL* curl = curl_easy_init();
        if (!curl) {
            throw std::runtime_error("Failed to initialize CURL");
        }

        std::string url = endpoint + path;
        std::string response_body;

        struct curl_slist* headers = nullptr;
        headers = curl_slist_append(headers, "Content-Type: application/json");

        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json_body.c_str());
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
        curl_easy_setopt(curl, CURLOPT_TIMEOUT_MS, timeout_ms);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION,
            +[](char* ptr, size_t size, size_t nmemb, void* userdata) -> size_t {
                auto* body = static_cast<std::string*>(userdata);
                body->append(ptr, size * nmemb);
                return size * nmemb;
            });
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_body);

        CURLcode res = curl_easy_perform(curl);
        curl_slist_free_all(headers);
        curl_easy_cleanup(curl);

        if (res != CURLE_OK) {
            throw std::runtime_error(
                std::string("HTTP request failed: ") + curl_easy_strerror(res));
        }

        return response_body;
#else
        // Fallback: use system curl command
        std::ostringstream cmd;
        cmd << "curl -s -X POST -H 'Content-Type: application/json' "
            << "-d '" << json_body << "' "
            << "--max-time " << (timeout_ms / 1000) << " "
            << "'" << endpoint << path << "' 2>/dev/null";

        FILE* pipe = popen(cmd.str().c_str(), "r");
        if (!pipe) {
            throw std::runtime_error("Failed to execute curl command");
        }

        std::string result;
        char buffer[4096];
        while (fgets(buffer, sizeof(buffer), pipe)) {
            result += buffer;
        }
        pclose(pipe);
        return result;
#endif
    }

    // HTTP GET helper
    std::string http_get(const std::string& path) const {
#ifdef HAVE_CURL
        CURL* curl = curl_easy_init();
        if (!curl) {
            throw std::runtime_error("Failed to initialize CURL");
        }

        std::string url = endpoint + path;
        std::string response_body;

        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_TIMEOUT_MS, timeout_ms);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION,
            +[](char* ptr, size_t size, size_t nmemb, void* userdata) -> size_t {
                auto* body = static_cast<std::string*>(userdata);
                body->append(ptr, size * nmemb);
                return size * nmemb;
            });
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_body);

        CURLcode res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);

        if (res != CURLE_OK) {
            return "";
        }

        return response_body;
#else
        std::ostringstream cmd;
        cmd << "curl -s --max-time " << (timeout_ms / 1000)
            << " '" << endpoint << path << "' 2>/dev/null";

        FILE* pipe = popen(cmd.str().c_str(), "r");
        if (!pipe) return "";

        std::string result;
        char buffer[4096];
        while (fgets(buffer, sizeof(buffer), pipe)) {
            result += buffer;
        }
        pclose(pipe);
        return result;
#endif
    }
};

KoboldCppClient::KoboldCppClient(const std::string& endpoint, int timeout_ms)
    : pimpl_(std::make_unique<Impl>(endpoint, timeout_ms)) {}

KoboldCppClient::~KoboldCppClient() = default;

KoboldCppClient::KoboldCppClient(KoboldCppClient&&) noexcept = default;
KoboldCppClient& KoboldCppClient::operator=(KoboldCppClient&&) noexcept = default;

bool KoboldCppClient::is_connected() const {
    try {
        auto info = get_info();
        return info.connected;
    } catch (...) {
        return false;
    }
}

ServerInfo KoboldCppClient::get_info() const {
    ServerInfo info;
    try {
        std::string response = pimpl_->http_get("/api/v1/info");
        if (!response.empty()) {
            info.connected = true;
            // TODO: Parse JSON response for model_name, max_context_length, etc.
        }
    } catch (...) {
        info.connected = false;
    }
    return info;
}

const std::string& KoboldCppClient::endpoint() const {
    return pimpl_->endpoint;
}

GenerationResult KoboldCppClient::generate(const std::string& prompt,
                                            const GenerationParams& params) const {
    GenerationResult result;

    std::ostringstream json;
    json << "{\"prompt\":\"" << prompt << "\""
         << ",\"max_tokens\":" << params.max_tokens
         << ",\"temperature\":" << params.temperature
         << ",\"top_p\":" << params.top_p
         << "}";

    try {
        std::string response = pimpl_->http_post("/v1/completions", json.str());
        // TODO: Parse JSON response
        result.text = response;
        result.success = true;
    } catch (const std::exception& e) {
        result.success = false;
        result.error = e.what();
    }

    return result;
}

GenerationResult KoboldCppClient::chat(const std::vector<ChatMessage>& messages,
                                        const GenerationParams& params) const {
    GenerationResult result;

    std::ostringstream json;
    json << "{\"messages\":[";
    for (size_t i = 0; i < messages.size(); ++i) {
        if (i > 0) json << ",";
        json << "{\"role\":\"" << messages[i].role
             << "\",\"content\":\"" << messages[i].content << "\"}";
    }
    json << "],\"max_tokens\":" << params.max_tokens
         << ",\"temperature\":" << params.temperature
         << "}";

    try {
        std::string response = pimpl_->http_post("/v1/chat/completions", json.str());
        result.text = response;
        result.success = true;
    } catch (const std::exception& e) {
        result.success = false;
        result.error = e.what();
    }

    return result;
}

GenerationResult KoboldCppClient::generate_stream(const std::string& prompt,
                                                    const GenerationParams& params,
                                                    StreamCallback callback) const {
    // TODO: Implement SSE streaming
    auto result = generate(prompt, params);
    if (result.success && callback) {
        callback(result.text);
    }
    return result;
}

GenerationResult KoboldCppClient::generate_native(const std::string& prompt,
                                                    const GenerationParams& params) const {
    GenerationResult result;

    std::ostringstream json;
    json << "{\"prompt\":\"" << prompt << "\""
         << ",\"max_length\":" << params.max_tokens
         << ",\"temperature\":" << params.temperature
         << ",\"top_p\":" << params.top_p
         << ",\"top_k\":" << params.top_k
         << ",\"rep_pen\":" << params.rep_pen
         << ",\"rep_pen_range\":" << params.rep_pen_range
         << "}";

    try {
        std::string response = pimpl_->http_post("/api/v1/generate", json.str());
        result.text = response;
        result.success = true;
    } catch (const std::exception& e) {
        result.success = false;
        result.error = e.what();
    }

    return result;
}

} // namespace koboldcpp
} // namespace opencog
