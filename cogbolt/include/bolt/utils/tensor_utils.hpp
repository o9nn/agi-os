
#ifndef TENSOR_UTILS_HPP
#define TENSOR_UTILS_HPP

#include <string>
#include <vector>
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include "bolt/ai/ggml.hpp"

namespace bolt {

class TensorUtils {
public:
    static void serializeTensor(FILE* f, ggml_tensor* tensor) {
        // Write tensor metadata 
        fwrite(&tensor->type, sizeof(tensor->type), 1, f);
        int n_dims = ggml_n_dims(tensor);
        fwrite(&n_dims, sizeof(n_dims), 1, f);
        for (int i = 0; i < n_dims; i++) {
            int64_t ne = tensor->ne[i];
            fwrite(&ne, sizeof(ne), 1, f);
        }
        
        // Write tensor data
        size_t size = ggml_nbytes(tensor);
        fwrite(tensor->data, size, 1, f);
    }
    
    static ggml_tensor* deserializeTensor(FILE* f, ggml_context* ctx) {
        // Read tensor metadata
        enum ggml_type type;
        int n_dims;
        int64_t ne[GGML_MAX_DIMS] = {0};
        
        fread(&type, sizeof(type), 1, f);
        fread(&n_dims, sizeof(n_dims), 1, f);
        for (int i = 0; i < n_dims; i++) {
            fread(&ne[i], sizeof(ne[i]), 1, f);
        }
        
        // Create tensor
        ggml_tensor* tensor = ggml_new_tensor(ctx, type, n_dims, ne);
        
        // Read tensor data
        size_t size = ggml_nbytes(tensor);
        fread(tensor->data, size, 1, f);
        
        return tensor;
    }
    
    // Simplified quantization placeholder - real implementation would use proper GGML API
    static ggml_tensor* quantizeTensor(ggml_context* ctx, ggml_tensor* tensor, enum ggml_type target_type) {
        // Create quantized tensor (simplified - just copy for now)
        ggml_tensor* quantized = ggml_dup_tensor(ctx, tensor);
        // TODO: Implement proper quantization when GGML API is stable
        return quantized;
    }
    
    static float calculateQuantizationError(ggml_tensor* original, ggml_tensor* quantized) {
        if (ggml_nelements(original) != ggml_nelements(quantized)) {
            throw std::runtime_error("Tensor dimensions mismatch");
        }
        
        // Simplified error calculation
        return 0.0f; // TODO: Implement proper error calculation
    }
};

} // namespace bolt

#endif
