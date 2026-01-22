#pragma once
#include <torch/library.h>
template <typename T>
struct pytorch_library_compatible_type {
using type = T;
static T convert_from_type(T arg) { return arg; }
};
template <typename T>
using pytorch_library_compatible_type_t =
typename pytorch_library_compatible_type<T>::type;
template <typename T>
T convert_from_pytorch_compatible_type(
pytorch_library_compatible_type_t<T> arg) {
return pytorch_library_compatible_type<T>::convert_from_type(arg);
}
template <typename T>
struct pytorch_library_compatible_type<c10::optional<T>&> {
using type = const c10::optional<T>&;
static c10::optional<T>& convert_from_type(const c10::optional<T>& arg) {
return const_cast<c10::optional<T>&>(arg);
}
};
template <typename T>
struct pytorch_library_compatible_type<c10::optional<T>> {
using type = c10::optional<pytorch_library_compatible_type_t<T>>;
static c10::optional<pytorch_library_compatible_type_t<T>> convert_from_type(
c10::optional<T> arg) {
return arg;
}
};
template <>
struct pytorch_library_compatible_type<c10::optional<const at::Tensor>&> {
using type = const c10::optional<at::Tensor>&;
static c10::optional<const at::Tensor>& convert_from_type(
const c10::optional<at::Tensor>& arg) {
return const_cast<c10::optional<const at::Tensor>&>(
reinterpret_cast<const c10::optional<const at::Tensor>&>(arg));
}
};
template <>
struct pytorch_library_compatible_type<int> {
using type = int64_t;
static int convert_from_type(int64_t arg) {
TORCH_CHECK(arg <= std::numeric_limits<int>::max(),
"int64_t value is too large to be converted to int");
TORCH_CHECK(arg >= std::numeric_limits<int>::min(),
"int64_t value is too small to be converted to int");
return arg;
}
};
template <>
struct pytorch_library_compatible_type<float> {
using type = double;
static float convert_from_type(double arg) {
TORCH_CHECK(std::abs(arg) <= std::numeric_limits<float>::max(),
"double value is too large to be converted to float");
return arg;
}
};
template <typename Ret, typename... Args>
auto make_pytorch_shim(Ret (*fun)(Args... args)) {
return [fun](pytorch_library_compatible_type_t<Args>... args) {
return fun(convert_from_pytorch_compatible_type<Args>(args)...);
};
}