#include "bolt/ai/gguf_loader.hpp"
#include "bolt/test/test_framework.hpp"
#include <iostream>
#include <fstream>
using namespace bolt;
BOLT_TEST(GGUFLoader, BasicCreation) {
GGUFLoader loader("nonexistent.gguf");
BOLT_ASSERT_FALSE(loader.isLoaded());
}
BOLT_TEST(GGUFLoader, InvalidFile) {
GGUFLoader loader("nonexistent.gguf");
bool result = loader.load();
BOLT_ASSERT_FALSE(result);
BOLT_ASSERT_FALSE(loader.getError().empty());
}
BOLT_TEST(GGUFLoader, MetadataAccess) {
GGUFLoader loader("test.gguf");
BOLT_ASSERT_FALSE(loader.hasMetadata("test.key"));
BOLT_ASSERT_EQUAL(loader.getMetadataString("test.key", "default"), std::string("default"));
BOLT_ASSERT_EQUAL(loader.getMetadataInt("test.key", 42), 42);
}
BOLT_TEST(GGUFLoader, TensorAccess) {
GGUFLoader loader("test.gguf");
BOLT_ASSERT_FALSE(loader.hasTensor("test.tensor"));
BOLT_ASSERT_EQUAL(loader.getTensorInfo("test.tensor"), nullptr);
BOLT_ASSERT_TRUE(loader.getTensorNames().empty());
}
BOLT_TEST(GGUFLoader, ModelInformation) {
GGUFLoader loader("test.gguf");
BOLT_ASSERT_EQUAL(loader.getNumLayers(), 0);
BOLT_ASSERT_EQUAL(loader.getEmbedDim(), 0);
BOLT_ASSERT_EQUAL(loader.getVocabSize(), 0);
BOLT_ASSERT_EQUAL(loader.getArchitecture(), std::string("unknown"));
}
int main() {
std::cout << "Running GGUF Loader tests..." << std::endl;
return bolt::test::run_all_tests();
}