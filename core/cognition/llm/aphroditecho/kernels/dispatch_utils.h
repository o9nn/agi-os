#pragma once
#include <torch/torch.h>
#define AT_DISPATCH_FP8_CASE(enum_type, ...) \
AT_PRIVATE_CASE_TYPE_USING_HINT(enum_type, fp8_t, __VA_ARGS__)
#define APHRODITE_DISPATCH_CASE_FLOATING_TYPES(...)    \
AT_DISPATCH_CASE(at::ScalarType::Float, __VA_ARGS__) \
AT_DISPATCH_CASE(at::ScalarType::Half, __VA_ARGS__)  \
AT_DISPATCH_CASE(at::ScalarType::BFloat16, __VA_ARGS__)
#define APHRODITE_DISPATCH_FLOATING_TYPES(TYPE, NAME, ...) \
AT_DISPATCH_SWITCH(TYPE, NAME,                           \
APHRODITE_DISPATCH_CASE_FLOATING_TYPES(__VA_ARGS__))
#ifdef USE_ROCM
#define APHRODITE_DISPATCH_CASE_FP8_TYPES(...)                     \
AT_DISPATCH_FP8_CASE(at::ScalarType::Float8_e4m3fn, __VA_ARGS__) \
AT_DISPATCH_FP8_CASE(at::ScalarType::Float8_e4m3fnuz, __VA_ARGS__)
#define APHRODITE_DISPATCH_CASE_QUANT_TYPES(...)                 \
AT_DISPATCH_CASE(at::ScalarType::Float8_e4m3fn, __VA_ARGS__)   \
AT_DISPATCH_CASE(at::ScalarType::Float8_e4m3fnuz, __VA_ARGS__) \
AT_DISPATCH_CASE(at::ScalarType::Char, __VA_ARGS__)
#else
#define APHRODITE_DISPATCH_CASE_FP8_TYPES(...) \
AT_DISPATCH_FP8_CASE(at::ScalarType::Float8_e4m3fn, __VA_ARGS__)
#define APHRODITE_DISPATCH_CASE_QUANT_TYPES(...)               \
AT_DISPATCH_CASE(at::ScalarType::Float8_e4m3fn, __VA_ARGS__) \
AT_DISPATCH_CASE(at::ScalarType::Char, __VA_ARGS__)
#endif
#define APHRODITE_DISPATCH_FP8_TYPES(TYPE, NAME, ...) \
AT_DISPATCH_SWITCH(TYPE, NAME, APHRODITE_DISPATCH_CASE_FP8_TYPES(__VA_ARGS__))
#define APHRODITE_DISPATCH_QUANT_TYPES(TYPE, NAME, ...) \
AT_DISPATCH_SWITCH(TYPE, NAME,                        \
APHRODITE_DISPATCH_CASE_QUANT_TYPES(__VA_ARGS__))
#define APHRODITE_DISPATCH_CASE_FLOATING_AND_BYTE_TYPES(...) \
AT_DISPATCH_CASE(at::ScalarType::Float, __VA_ARGS__)       \
AT_DISPATCH_CASE(at::ScalarType::Half, __VA_ARGS__)        \
AT_DISPATCH_CASE(at::ScalarType::BFloat16, __VA_ARGS__)    \
AT_DISPATCH_CASE(at::ScalarType::Byte, __VA_ARGS__)
#define APHRODITE_DISPATCH_FLOATING_AND_BYTE_TYPES(TYPE, NAME, ...) \
AT_DISPATCH_SWITCH(                                               \
TYPE, NAME,                                                   \
APHRODITE_DISPATCH_CASE_FLOATING_AND_BYTE_TYPES(__VA_ARGS__))
#define APHRODITE_DISPATCH_CASE_INTEGRAL_TYPES(...)    \
AT_DISPATCH_CASE(at::ScalarType::Byte, __VA_ARGS__)  \
AT_DISPATCH_CASE(at::ScalarType::Char, __VA_ARGS__)  \
AT_DISPATCH_CASE(at::ScalarType::Short, __VA_ARGS__) \
AT_DISPATCH_CASE(at::ScalarType::Int, __VA_ARGS__)   \
AT_DISPATCH_CASE(at::ScalarType::Long, __VA_ARGS__)
#define APHRODITE_DISPATCH_CASE_INTEGRAL_AND_UNSIGNED_TYPES(...) \
AT_DISPATCH_CASE(at::ScalarType::Byte, __VA_ARGS__)            \
AT_DISPATCH_CASE(at::ScalarType::Char, __VA_ARGS__)            \
AT_DISPATCH_CASE(at::ScalarType::Short, __VA_ARGS__)           \
AT_DISPATCH_CASE(at::ScalarType::Int, __VA_ARGS__)             \
AT_DISPATCH_CASE(at::ScalarType::Long, __VA_ARGS__)            \
AT_DISPATCH_CASE(at::ScalarType::UInt16, __VA_ARGS__)          \
AT_DISPATCH_CASE(at::ScalarType::UInt32, __VA_ARGS__)          \
AT_DISPATCH_CASE(at::ScalarType::UInt64, __VA_ARGS__)
#define APHRODITE_DISPATCH_INTEGRAL_TYPES(TYPE, NAME, ...) \
AT_DISPATCH_SWITCH(TYPE, NAME,                           \
APHRODITE_DISPATCH_CASE_INTEGRAL_TYPES(__VA_ARGS__))
#define APHRODITE_DISPATCH_INTEGRAL_AND_UNSIGNED_TYPES(TYPE, NAME, ...) \
AT_DISPATCH_SWITCH(                                                   \
TYPE, NAME,                                                       \
APHRODITE_DISPATCH_CASE_INTEGRAL_AND_UNSIGNED_TYPES(__VA_ARGS__))