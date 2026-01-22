#include <cstdio>
#include <string>
#include <unordered_map>
int main(int argc, char** argv) {
std::string filename = "main";
if (argc >= 1) {
filename = argv[0];
}
auto pos = filename.find_last_of("/\\");
if (pos != std::string::npos) {
filename = filename.substr(pos+1);
}
auto replacement_filename = "llama-" + filename;
if (filename == "main") {
replacement_filename = "llama-cli";
}
fprintf(stdout, "\n");
fprintf(stdout, "WARNING: The binary '%s' is deprecated.\n", filename.c_str());
fprintf(stdout, " Please use '%s' instead.\n", replacement_filename.c_str());
fprintf(stdout, " See https:
fprintf(stdout, "\n");
return EXIT_FAILURE;
}