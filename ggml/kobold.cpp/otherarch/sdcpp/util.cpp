#include "util.h"
#include <stdarg.h>
#include <algorithm>
#include <cmath>
#include <codecvt>
#include <fstream>
#include <locale>
#include <sstream>
#include <string>
#include <thread>
#include <unordered_set>
#include <vector>
#include "preprocessing.hpp"
#include <inttypes.h>
#include <cinttypes>
#if defined(__APPLE__) && defined(__MACH__)
#include <sys/sysctl.h>
#include <sys/types.h>
#endif
#if !defined(_WIN32)
#include <sys/ioctl.h>
#include <unistd.h>
#endif
#include "ggml-cpu.h"
#include "ggml.h"
#include "stable-diffusion.h"
#include "stb_image_resize.h"
bool ends_with(const std::string& str, const std::string& ending) {
if (str.length() >= ending.length()) {
return (str.compare(str.length() - ending.length(), ending.length(), ending) == 0);
} else {
return false;
}
}
bool starts_with(const std::string& str, const std::string& start) {
if (str.find(start) == 0) {
return true;
}
return false;
}
bool contains(const std::string& str, const std::string& substr) {
if (str.find(substr) != std::string::npos) {
return true;
}
return false;
}
void replace_all_chars(std::string& str, char target, char replacement) {
for (size_t i = 0; i < str.length(); ++i) {
if (str[i] == target) {
str[i] = replacement;
}
}
}
#ifdef _WIN32
#include <windows.h>
bool file_exists(const std::string& filename) {
DWORD attributes = GetFileAttributesA(filename.c_str());
return (attributes != INVALID_FILE_ATTRIBUTES && !(attributes & FILE_ATTRIBUTE_DIRECTORY));
}
bool is_directory(const std::string& path) {
DWORD attributes = GetFileAttributesA(path.c_str());
return (attributes != INVALID_FILE_ATTRIBUTES && (attributes & FILE_ATTRIBUTE_DIRECTORY));
}
std::string get_full_path(const std::string& dir, const std::string& filename) {
std::string full_path = dir + "\\" + filename;
WIN32_FIND_DATA find_file_data;
HANDLE hFind = FindFirstFile(full_path.c_str(), &find_file_data);
if (hFind != INVALID_HANDLE_VALUE) {
FindClose(hFind);
return full_path;
} else {
return "";
}
}
std::vector<std::string> get_files_from_dir(const std::string& dir) {
std::vector<std::string> files;
WIN32_FIND_DATA findFileData;
HANDLE hFind;
char currentDirectory[MAX_PATH];
GetCurrentDirectory(MAX_PATH, currentDirectory);
char directoryPath[MAX_PATH];
sprintf(directoryPath, "%s\\%s\\*", currentDirectory, dir.c_str());
hFind = FindFirstFile(directoryPath, &findFileData);
if (hFind == INVALID_HANDLE_VALUE) {
printf("Unable to find directory.\n");
return files;
}
do {
if (!(findFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
files.push_back(std::string(currentDirectory) + "\\" + dir + "\\" + std::string(findFileData.cFileName));
}
} while (FindNextFile(hFind, &findFileData) != 0);
FindClose(hFind);
sort(files.begin(), files.end());
return files;
}
#else
#include <dirent.h>
#include <sys/stat.h>
bool file_exists(const std::string& filename) {
struct stat buffer;
return (stat(filename.c_str(), &buffer) == 0 && S_ISREG(buffer.st_mode));
}
bool is_directory(const std::string& path) {
struct stat buffer;
return (stat(path.c_str(), &buffer) == 0 && S_ISDIR(buffer.st_mode));
}
std::string get_full_path(const std::string& dir, const std::string& filename) {
DIR* dp = opendir(dir.c_str());
if (dp != nullptr) {
struct dirent* entry;
while ((entry = readdir(dp)) != nullptr) {
if (strcasecmp(entry->d_name, filename.c_str()) == 0) {
closedir(dp);
return dir + "/" + entry->d_name;
}
}
closedir(dp);
}
return "";
}
std::vector<std::string> get_files_from_dir(const std::string& dir) {
std::vector<std::string> files;
DIR* dp = opendir(dir.c_str());
if (dp != nullptr) {
struct dirent* entry;
while ((entry = readdir(dp)) != nullptr) {
std::string fname = dir + "/" + entry->d_name;
if (!is_directory(fname))
files.push_back(fname);
}
closedir(dp);
}
sort(files.begin(), files.end());
return files;
}
#endif
int32_t sd_get_num_physical_cores() {
#ifdef __linux__
std::unordered_set<std::string> siblings;
for (uint32_t cpu = 0; cpu < UINT32_MAX; ++cpu) {
std::ifstream thread_siblings("/sys/devices/system/cpu" + std::to_string(cpu) + "/topology/thread_siblings");
if (!thread_siblings.is_open()) {
break;
}
std::string line;
if (std::getline(thread_siblings, line)) {
siblings.insert(line);
}
}
if (siblings.size() > 0) {
return static_cast<int32_t>(siblings.size());
}
#elif defined(__APPLE__) && defined(__MACH__)
int32_t num_physical_cores;
size_t len = sizeof(num_physical_cores);
int result = sysctlbyname("hw.perflevel0.physicalcpu", &num_physical_cores, &len, NULL, 0);
if (result == 0) {
return num_physical_cores;
}
result = sysctlbyname("hw.physicalcpu", &num_physical_cores, &len, NULL, 0);
if (result == 0) {
return num_physical_cores;
}
#elif defined(_WIN32)
#endif
unsigned int n_threads = std::thread::hardware_concurrency();
return n_threads > 0 ? (n_threads <= 4 ? n_threads : n_threads / 2) : 4;
}
static sd_progress_cb_t sd_progress_cb = NULL;
void* sd_progress_cb_data = NULL;
std::u32string utf8_to_utf32(const std::string& utf8_str) {
std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> converter;
return converter.from_bytes(utf8_str);
}
std::string utf32_to_utf8(const std::u32string& utf32_str) {
std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> converter;
return converter.to_bytes(utf32_str);
}
std::u32string unicode_value_to_utf32(int unicode_value) {
std::u32string utf32_string = {static_cast<char32_t>(unicode_value)};
return utf32_string;
}
static std::string sd_basename(const std::string& path) {
size_t pos = path.find_last_of('/');
if (pos != std::string::npos) {
return path.substr(pos + 1);
}
pos = path.find_last_of('\\');
if (pos != std::string::npos) {
return path.substr(pos + 1);
}
return path;
}
std::string path_join(const std::string& p1, const std::string& p2) {
if (p1.empty()) {
return p2;
}
if (p2.empty()) {
return p1;
}
if (p1[p1.length() - 1] == '/' || p1[p1.length() - 1] == '\\') {
return p1 + p2;
}
return p1 + "/" + p2;
}
std::vector<std::string> splitString(const std::string& str, char delimiter) {
std::vector<std::string> result;
size_t start = 0;
size_t end = str.find(delimiter);
while (end != std::string::npos) {
result.push_back(str.substr(start, end - start));
start = end + 1;
end = str.find(delimiter, start);
}
result.push_back(str.substr(start));
return result;
}
sd_image_t* preprocess_id_image(sd_image_t* img) {
int shortest_edge = 224;
int size = shortest_edge;
sd_image_t* resized = NULL;
uint32_t w = img->width;
uint32_t h = img->height;
uint32_t c = img->channel;
unsigned char* buf = (unsigned char*)malloc(sizeof(unsigned char) * 3 * size * size);
if (!stbir_resize_uint8(img->data, w, h, 0,
buf, size, size, 0,
c)) {
fprintf(stderr, "%s: resize operation failed \n ", __func__);
return resized;
}
resized = new sd_image_t{(uint32_t)shortest_edge,
(uint32_t)shortest_edge,
3,
buf};
return resized;
}
static int sdloglevel = 0;
static bool sdquiet = false;
void pretty_progress(int step, int steps, float time) {
if (sd_progress_cb) {
sd_progress_cb(step, steps, time, sd_progress_cb_data);
return;
}
if (step == 0) {
return;
}
if(sdloglevel<0 || sdquiet)
{
return;
}
std::string progress = "  |";
int max_progress = 50;
int32_t current = (int32_t)(step * 1.f * max_progress / steps);
for (int i = 0; i < 50; i++) {
if (i > current) {
progress += " ";
} else if (i == current && i != max_progress - 1) {
progress += ">";
} else {
progress += "=";
}
}
progress += "|";
printf(time > 1.0f ? "\r%s %i/%i - %.2fs/it" : "\r%s %i/%i - %.2fit/s",
progress.c_str(), step, steps,
time > 1.0f || time == 0 ? time : (1.0f / time));
fflush(stdout);
if (step == steps) {
printf("\n");
}
}
std::string ltrim(const std::string& s) {
auto it = std::find_if(s.begin(), s.end(), [](int ch) {
return !std::isspace(ch);
});
return std::string(it, s.end());
}
std::string rtrim(const std::string& s) {
auto it = std::find_if(s.rbegin(), s.rend(), [](int ch) {
return !std::isspace(ch);
});
return std::string(s.begin(), it.base());
}
std::string trim(const std::string& s) {
return rtrim(ltrim(s));
}
static sd_log_cb_t sd_log_cb = NULL;
void* sd_log_cb_data = NULL;
#define LOG_BUFFER_SIZE 1024
void log_message(const char* format, ...) {
if (sdloglevel>0) {
printf("\n");
va_list args;
va_start(args, format);
vprintf(format, args);
va_end(args);
fflush(stdout);
}
}
void set_sd_log_level(int log)
{
sdloglevel = log;
}
bool get_sd_log_level()
{
return sdloglevel;
}
void set_sd_quiet(bool quiet)
{
sdquiet = quiet;
}
void log_printf(sd_log_level_t level, const char* file, int line, const char* format, ...) {
va_list args;
va_start(args, format);
static char log_buffer[LOG_BUFFER_SIZE + 1];
int written = snprintf(log_buffer, LOG_BUFFER_SIZE, "%s:%-4d - ", sd_basename(file).c_str(), line);
if (written >= 0 && written < LOG_BUFFER_SIZE) {
vsnprintf(log_buffer + written, LOG_BUFFER_SIZE - written, format, args);
}
strncat(log_buffer, "\n", LOG_BUFFER_SIZE - strlen(log_buffer));
if (sd_log_cb) {
sd_log_cb(level, log_buffer, sd_log_cb_data);
}
va_end(args);
}
void sd_set_log_callback(sd_log_cb_t cb, void* data) {
sd_log_cb = cb;
sd_log_cb_data = data;
}
void sd_set_progress_callback(sd_progress_cb_t cb, void* data) {
sd_progress_cb = cb;
sd_progress_cb_data = data;
}
const char* sd_get_system_info() {
static char buffer[1024];
std::stringstream ss;
ss << "System Info: \n";
ss << "    SSE3 = " << ggml_cpu_has_sse3() << std::endl;
ss << "    AVX = " << ggml_cpu_has_avx() << std::endl;
ss << "    AVX2 = " << ggml_cpu_has_avx2() << std::endl;
ss << "    AVX512 = " << ggml_cpu_has_avx512() << std::endl;
ss << "    AVX512_VBMI = " << ggml_cpu_has_avx512_vbmi() << std::endl;
ss << "    AVX512_VNNI = " << ggml_cpu_has_avx512_vnni() << std::endl;
ss << "    FMA = " << ggml_cpu_has_fma() << std::endl;
ss << "    NEON = " << ggml_cpu_has_neon() << std::endl;
ss << "    ARM_FMA = " << ggml_cpu_has_arm_fma() << std::endl;
ss << "    F16C = " << ggml_cpu_has_f16c() << std::endl;
ss << "    FP16_VA = " << ggml_cpu_has_fp16_va() << std::endl;
ss << "    WASM_SIMD = " << ggml_cpu_has_wasm_simd() << std::endl;
ss << "    VSX = " << ggml_cpu_has_vsx() << std::endl;
snprintf(buffer, sizeof(buffer), "%s", ss.str().c_str());
return buffer;
}
const char* sd_type_name(enum sd_type_t type) {
return ggml_type_name((ggml_type)type);
}
sd_image_f32_t sd_image_t_to_sd_image_f32_t(sd_image_t image) {
sd_image_f32_t converted_image;
converted_image.width = image.width;
converted_image.height = image.height;
converted_image.channel = image.channel;
converted_image.data = (float*)malloc(image.width * image.height * image.channel * sizeof(float));
for (int i = 0; i < image.width * image.height * image.channel; i++) {
converted_image.data[i] = (float)image.data[i];
}
return converted_image;
}
float interpolate(float v1, float v2, float v3, float v4, float x_ratio, float y_ratio) {
return v1 * (1 - x_ratio) * (1 - y_ratio) + v2 * x_ratio * (1 - y_ratio) + v3 * (1 - x_ratio) * y_ratio + v4 * x_ratio * y_ratio;
}
sd_image_f32_t resize_sd_image_f32_t(sd_image_f32_t image, int target_width, int target_height) {
sd_image_f32_t resized_image;
resized_image.width = target_width;
resized_image.height = target_height;
resized_image.channel = image.channel;
resized_image.data = (float*)malloc(target_width * target_height * image.channel * sizeof(float));
for (int y = 0; y < target_height; y++) {
for (int x = 0; x < target_width; x++) {
float original_x = (float)x * image.width / target_width;
float original_y = (float)y * image.height / target_height;
int x1 = (int)original_x;
int y1 = (int)original_y;
int x2 = x1 + 1;
int y2 = y1 + 1;
for (int k = 0; k < image.channel; k++) {
float v1 = *(image.data + y1 * image.width * image.channel + x1 * image.channel + k);
float v2 = *(image.data + y1 * image.width * image.channel + x2 * image.channel + k);
float v3 = *(image.data + y2 * image.width * image.channel + x1 * image.channel + k);
float v4 = *(image.data + y2 * image.width * image.channel + x2 * image.channel + k);
float x_ratio = original_x - x1;
float y_ratio = original_y - y1;
float value = interpolate(v1, v2, v3, v4, x_ratio, y_ratio);
*(resized_image.data + y * target_width * image.channel + x * image.channel + k) = value;
}
}
}
return resized_image;
}
void normalize_sd_image_f32_t(sd_image_f32_t image, float means[3], float stds[3]) {
for (int y = 0; y < image.height; y++) {
for (int x = 0; x < image.width; x++) {
for (int k = 0; k < image.channel; k++) {
int index = (y * image.width + x) * image.channel + k;
image.data[index] = (image.data[index] - means[k]) / stds[k];
}
}
}
}
float means[3] = {0.48145466, 0.4578275, 0.40821073};
float stds[3] = {0.26862954, 0.26130258, 0.27577711};
sd_image_f32_t clip_preprocess(sd_image_f32_t image, int size) {
float scale = (float)size / fmin(image.width, image.height);
int new_width = (int)(scale * image.width);
int new_height = (int)(scale * image.height);
float* resized_data = (float*)malloc(new_width * new_height * image.channel * sizeof(float));
for (int y = 0; y < new_height; y++) {
for (int x = 0; x < new_width; x++) {
float original_x = (float)x * image.width / new_width;
float original_y = (float)y * image.height / new_height;
int x1 = (int)original_x;
int y1 = (int)original_y;
int x2 = x1 + 1;
int y2 = y1 + 1;
for (int k = 0; k < image.channel; k++) {
float v1 = *(image.data + y1 * image.width * image.channel + x1 * image.channel + k);
float v2 = *(image.data + y1 * image.width * image.channel + x2 * image.channel + k);
float v3 = *(image.data + y2 * image.width * image.channel + x1 * image.channel + k);
float v4 = *(image.data + y2 * image.width * image.channel + x2 * image.channel + k);
float x_ratio = original_x - x1;
float y_ratio = original_y - y1;
float value = interpolate(v1, v2, v3, v4, x_ratio, y_ratio);
*(resized_data + y * new_width * image.channel + x * image.channel + k) = value;
}
}
}
int h = (new_height - size) / 2;
int w = (new_width - size) / 2;
sd_image_f32_t result;
result.width = size;
result.height = size;
result.channel = image.channel;
result.data = (float*)malloc(size * size * image.channel * sizeof(float));
for (int k = 0; k < image.channel; k++) {
for (int i = 0; i < size; i++) {
for (int j = 0; j < size; j++) {
*(result.data + i * size * image.channel + j * image.channel + k) =
fmin(fmax(*(resized_data + (i + h) * new_width * image.channel + (j + w) * image.channel + k), 0.0f), 255.0f) / 255.0f;
}
}
}
free(resized_data);
for (int k = 0; k < image.channel; k++) {
for (int i = 0; i < size; i++) {
for (int j = 0; j < size; j++) {
int offset = i * size * image.channel + j * image.channel + k;
float value = *(result.data + offset);
value = (value - means[k]) / stds[k];
*(result.data + offset) = value;
}
}
}
return result;
}
std::vector<std::pair<std::string, float>> parse_prompt_attention(const std::string& text) {
std::vector<std::pair<std::string, float>> res;
std::vector<int> round_brackets;
std::vector<int> square_brackets;
float round_bracket_multiplier = 1.1f;
float square_bracket_multiplier = 1 / 1.1f;
std::regex re_attention(R"(\\\(|\\\)|\\\[|\\\]|\\\\|\\|\(|\[|:([+-]?[.\d]+)\)|\)|\]|[^\\()\[\]:]+|:)");
std::regex re_break(R"(\s*\bBREAK\b\s*)");
auto multiply_range = [&](int start_position, float multiplier) {
for (int p = start_position; p < res.size(); ++p) {
res[p].second *= multiplier;
}
};
std::smatch m;
std::string remaining_text = text;
while (std::regex_search(remaining_text, m, re_attention)) {
std::string text = m[0];
std::string weight = m[1];
if (text == "(") {
round_brackets.push_back((int)res.size());
} else if (text == "[") {
square_brackets.push_back((int)res.size());
} else if (!weight.empty()) {
if (!round_brackets.empty()) {
multiply_range(round_brackets.back(), std::stof(weight));
round_brackets.pop_back();
}
} else if (text == ")" && !round_brackets.empty()) {
multiply_range(round_brackets.back(), round_bracket_multiplier);
round_brackets.pop_back();
} else if (text == "]" && !square_brackets.empty()) {
multiply_range(square_brackets.back(), square_bracket_multiplier);
square_brackets.pop_back();
} else if (text == "\\(") {
res.push_back({text.substr(1), 1.0f});
} else {
res.push_back({text, 1.0f});
}
remaining_text = m.suffix();
}
for (int pos : round_brackets) {
multiply_range(pos, round_bracket_multiplier);
}
for (int pos : square_brackets) {
multiply_range(pos, square_bracket_multiplier);
}
if (res.empty()) {
res.push_back({"", 1.0f});
}
int i = 0;
while (i + 1 < res.size()) {
if (res[i].second == res[i + 1].second) {
res[i].first += res[i + 1].first;
res.erase(res.begin() + i + 1);
} else {
++i;
}
}
return res;
}