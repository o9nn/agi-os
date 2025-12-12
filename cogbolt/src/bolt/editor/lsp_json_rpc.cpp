#include "bolt/editor/lsp_json_rpc.hpp"
#include <sstream>
#include <iostream>

namespace bolt {
namespace lsp {

// Basic JSON Value implementation
std::string JsonValue::toString() const {
    switch (type_) {
        case Null:
            return "null";
        case Bool:
            return boolValue_ ? "true" : "false";
        case Number:
            return std::to_string(numberValue_);
        case String: {
            std::ostringstream oss;
            oss << "\"";
            // Basic string escaping
            for (char c : stringValue_) {
                switch (c) {
                    case '"': oss << "\\\""; break;
                    case '\\': oss << "\\\\"; break;
                    case '\n': oss << "\\n"; break;
                    case '\r': oss << "\\r"; break;
                    case '\t': oss << "\\t"; break;
                    default: oss << c; break;
                }
            }
            oss << "\"";
            return oss.str();
        }
        case Array: {
            std::ostringstream oss;
            oss << "[";
            for (size_t i = 0; i < arrayValue_.size(); ++i) {
                if (i > 0) oss << ",";
                oss << arrayValue_[i]->toString();
            }
            oss << "]";
            return oss.str();
        }
        case Object: {
            std::ostringstream oss;
            oss << "{";
            bool first = true;
            for (const auto& pair : objectValue_) {
                if (!first) oss << ",";
                first = false;
                oss << "\"" << pair.first << "\":" << pair.second->toString();
            }
            oss << "}";
            return oss.str();
        }
    }
    return "null";
}

std::shared_ptr<JsonValue> JsonValue::fromString(const std::string& json) {
    // Very basic JSON parsing - in a real implementation, use a proper JSON library
    auto value = std::make_shared<JsonValue>();
    
    std::string trimmed = json;
    // Remove whitespace
    trimmed.erase(0, trimmed.find_first_not_of(" \t\n\r"));
    trimmed.erase(trimmed.find_last_not_of(" \t\n\r") + 1);
    
    if (trimmed.empty() || trimmed == "null") {
        return value; // null
    }
    
    if (trimmed == "true") {
        value->setBool(true);
        return value;
    }
    
    if (trimmed == "false") {
        value->setBool(false);
        return value;
    }
    
    if (trimmed[0] == '"' && trimmed.back() == '"') {
        // String
        std::string str = trimmed.substr(1, trimmed.length() - 2);
        // Basic unescaping
        std::string unescaped;
        for (size_t i = 0; i < str.length(); ++i) {
            if (str[i] == '\\' && i + 1 < str.length()) {
                switch (str[i + 1]) {
                    case '"': unescaped += '"'; i++; break;
                    case '\\': unescaped += '\\'; i++; break;
                    case 'n': unescaped += '\n'; i++; break;
                    case 'r': unescaped += '\r'; i++; break;
                    case 't': unescaped += '\t'; i++; break;
                    default: unescaped += str[i]; break;
                }
            } else {
                unescaped += str[i];
            }
        }
        value->setString(unescaped);
        return value;
    }
    
    if ((trimmed[0] >= '0' && trimmed[0] <= '9') || trimmed[0] == '-') {
        // Number
        try {
            double num = std::stod(trimmed);
            value->setNumber(num);
            return value;
        } catch (...) {
            // Invalid number, return null
        }
    }
    
    if (trimmed[0] == '{') {
        // Object - very basic parsing
        value->setObject();
        // In a real implementation, properly parse the object
        return value;
    }
    
    if (trimmed[0] == '[') {
        // Array - very basic parsing
        value->setArray();
        // In a real implementation, properly parse the array
        return value;
    }
    
    return value; // null for invalid JSON
}

// JSON-RPC Handler implementation
void JsonRpcHandler::registerRequestHandler(const std::string& method, RequestHandler handler) {
    requestHandlers_[method] = handler;
}

void JsonRpcHandler::registerNotificationHandler(const std::string& method, NotificationHandler handler) {
    notificationHandlers_[method] = handler;
}

std::string JsonRpcHandler::processMessage(const std::string& message) {
    try {
        auto json = JsonValue::fromString(message);
        if (!json || json->getType() != JsonValue::Object) {
            return createErrorResponse("", -32700, "Parse error");
        }
        
        auto methodProperty = json->getProperty("method");
        auto idProperty = json->getProperty("id");
        
        if (!methodProperty || methodProperty->getType() != JsonValue::String) {
            return createErrorResponse(idProperty ? idProperty->asString() : "", -32600, "Invalid Request");
        }
        
        std::string method = methodProperty->asString();
        auto paramsProperty = json->getProperty("params");
        
        // Check if it's a notification (no id) or request (has id)
        if (!idProperty) {
            // Notification
            auto it = notificationHandlers_.find(method);
            if (it != notificationHandlers_.end()) {
                it->second(method, paramsProperty);
            }
            return ""; // No response for notifications
        } else {
            // Request
            std::string id = idProperty->asString();
            auto it = requestHandlers_.find(method);
            if (it != requestHandlers_.end()) {
                auto result = it->second(method, paramsProperty);
                return createResponse(id, result);
            } else {
                return createErrorResponse(id, -32601, "Method not found");
            }
        }
    } catch (const std::exception& e) {
        return createErrorResponse("", -32603, "Internal error");
    }
}

std::string JsonRpcHandler::createRequest(const std::string& method, std::shared_ptr<JsonValue> params, const std::string& id) {
    auto request = std::make_shared<JsonValue>();
    request->setObject();
    request->setProperty("jsonrpc", std::make_shared<JsonValue>("2.0"));
    request->setProperty("method", std::make_shared<JsonValue>(method));
    if (!id.empty()) {
        request->setProperty("id", std::make_shared<JsonValue>(id));
    }
    if (params) {
        request->setProperty("params", params);
    }
    return request->toString();
}

std::string JsonRpcHandler::createNotification(const std::string& method, std::shared_ptr<JsonValue> params) {
    return createRequest(method, params, ""); // No id for notifications
}

std::string JsonRpcHandler::createResponse(const std::string& id, std::shared_ptr<JsonValue> result) {
    auto response = std::make_shared<JsonValue>();
    response->setObject();
    response->setProperty("jsonrpc", std::make_shared<JsonValue>("2.0"));
    response->setProperty("id", std::make_shared<JsonValue>(id));
    if (result) {
        response->setProperty("result", result);
    } else {
        response->setProperty("result", std::make_shared<JsonValue>()); // null
    }
    return response->toString();
}

std::string JsonRpcHandler::createErrorResponse(const std::string& id, int code, const std::string& message) {
    auto error = std::make_shared<JsonValue>();
    error->setObject();
    error->setProperty("code", std::make_shared<JsonValue>(static_cast<double>(code)));
    error->setProperty("message", std::make_shared<JsonValue>(message));
    
    auto response = std::make_shared<JsonValue>();
    response->setObject();
    response->setProperty("jsonrpc", std::make_shared<JsonValue>("2.0"));
    response->setProperty("id", std::make_shared<JsonValue>(id));
    response->setProperty("error", error);
    
    return response->toString();
}

} // namespace lsp
} // namespace bolt