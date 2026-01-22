#include "env_utils.h"
#include <cstdlib>
static std::optional<int32_t> getIntEnv(char const* name) {
char const* const env = std::getenv(name);
if (env == nullptr) {
return std::nullopt;
}
int32_t const val = std::atoi(env);
if (val <= 0) {
return std::nullopt;
}
return {val};
};
bool forceXQAKernels() {
static bool const forceXQA = (getIntEnv("APHRODITE_FORCE_XQA").value_or(0) != 0);
return forceXQA;
}