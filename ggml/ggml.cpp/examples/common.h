#pragma once
#include <string>
#include <map>
#include <vector>
#include <random>
#include <thread>
#include <ctime>
#include <fstream>
#include <sstream>
struct gpt_params {
int32_t seed         = -1;
int32_t n_threads    = std::min(4, (int32_t) std::thread::hardware_concurrency());
int32_t n_predict    = 200;
int32_t n_parallel   = 1;
int32_t n_batch      = 32;
int32_t n_ctx        = 2048;
int32_t n_gpu_layers = 0;
bool ignore_eos = false;
int32_t top_k          = 40;
float   top_p          = 0.9f;
float   temp           = 0.9f;
int32_t repeat_last_n  = 64;
float   repeat_penalty = 1.00f;
std::string model      = "models/gpt-2-117M/ggml-model.bin";
std::string prompt     = "";
std::string token_test = "";
bool    interactive      = false;
int32_t interactive_port = -1;
};
bool gpt_params_parse(int argc, char ** argv, gpt_params & params);
void gpt_print_usage(int argc, char ** argv, const gpt_params & params);
std::string gpt_random_prompt(std::mt19937 & rng);
std::string trim(const std::string & s);
std::string replace(
const std::string & s,
const std::string & from,
const std::string & to);
struct gpt_vocab {
using id    = int32_t;
using token = std::string;
std::map<token, id> token_to_id;
std::map<id, token> id_to_token;
std::vector<std::string> special_tokens;
void add_special_token(const std::string & token);
};
std::map<std::string, int32_t> json_parse(const std::string & fname);
std::string convert_to_utf8(const std::wstring & input);
std::wstring convert_to_wstring(const std::string & input);
void gpt_split_words(std::string str, std::vector<std::string>& words);
std::vector<gpt_vocab::id> gpt_tokenize(const gpt_vocab & vocab, const std::string & text);
void test_gpt_tokenizer(gpt_vocab & vocab, const std::string & fpath_test);
bool gpt_vocab_init(const std::string & fname, gpt_vocab & vocab);
gpt_vocab::id gpt_sample_top_k_top_p(
const gpt_vocab & vocab,
const float * logits,
int    top_k,
double top_p,
double temp,
std::mt19937 & rng);
gpt_vocab::id gpt_sample_top_k_top_p_repeat(
const gpt_vocab & vocab,
const float * logits,
const int32_t * last_n_tokens_data,
size_t last_n_tokens_data_size,
int    top_k,
double top_p,
double temp,
int repeat_last_n,
float repeat_penalty,
std::mt19937 & rng);
class wav_writer {
private:
std::ofstream file;
uint32_t dataSize = 0;
std::string wav_filename;
bool write_header(const uint32_t sample_rate,
const uint16_t bits_per_sample,
const uint16_t channels) {
file.write("RIFF", 4);
file.write("\0\0\0\0", 4);
file.write("WAVE", 4);
file.write("fmt ", 4);
const uint32_t sub_chunk_size = 16;
const uint16_t audio_format = 1;
const uint32_t byte_rate = sample_rate * channels * bits_per_sample / 8;
const uint16_t block_align = channels * bits_per_sample / 8;
file.write(reinterpret_cast<const char *>(&sub_chunk_size), 4);
file.write(reinterpret_cast<const char *>(&audio_format), 2);
file.write(reinterpret_cast<const char *>(&channels), 2);
file.write(reinterpret_cast<const char *>(&sample_rate), 4);
file.write(reinterpret_cast<const char *>(&byte_rate), 4);
file.write(reinterpret_cast<const char *>(&block_align), 2);
file.write(reinterpret_cast<const char *>(&bits_per_sample), 2);
file.write("data", 4);
file.write("\0\0\0\0", 4);
return true;
}
bool write_audio(const float * data, size_t length) {
for (size_t i = 0; i < length; ++i) {
const int16_t intSample = int16_t(data[i] * 32767);
file.write(reinterpret_cast<const char *>(&intSample), sizeof(int16_t));
dataSize += sizeof(int16_t);
}
if (file.is_open()) {
file.seekp(4, std::ios::beg);
uint32_t fileSize = 36 + dataSize;
file.write(reinterpret_cast<char *>(&fileSize), 4);
file.seekp(40, std::ios::beg);
file.write(reinterpret_cast<char *>(&dataSize), 4);
file.seekp(0, std::ios::end);
}
return true;
}
bool open_wav(const std::string & filename) {
if (filename != wav_filename) {
if (file.is_open()) {
file.close();
}
}
if (!file.is_open()) {
file.open(filename, std::ios::binary);
wav_filename = filename;
dataSize = 0;
}
return file.is_open();
}
public:
bool open(const std::string & filename,
const    uint32_t   sample_rate,
const    uint16_t   bits_per_sample,
const    uint16_t   channels) {
if (open_wav(filename)) {
write_header(sample_rate, bits_per_sample, channels);
} else {
return false;
}
return true;
}
bool close() {
file.close();
return true;
}
bool write(const float * data, size_t length) {
return write_audio(data, length);
}
~wav_writer() {
if (file.is_open()) {
file.close();
}
}
};
void high_pass_filter(
std::vector<float> & data,
float cutoff,
float sample_rate);
bool vad_simple(
std::vector<float> & pcmf32,
int   sample_rate,
int   last_ms,
float vad_thold,
float freq_thold,
bool  verbose);
float similarity(const std::string & s0, const std::string & s1);
#define SQR(X)    ((X) * (X))
#define UNCUBE(x) x < 48 ? 0 : x < 115 ? 1 : (x - 35) / 40
static int rgb2xterm256(int r, int g, int b) {
unsigned char cube[] = {0, 0137, 0207, 0257, 0327, 0377};
int av, ir, ig, ib, il, qr, qg, qb, ql;
av = r * .299 + g * .587 + b * .114 + .5;
ql = (il = av > 238 ? 23 : (av - 3) / 10) * 10 + 8;
qr = cube[(ir = UNCUBE(r))];
qg = cube[(ig = UNCUBE(g))];
qb = cube[(ib = UNCUBE(b))];
if (SQR(qr - r) + SQR(qg - g) + SQR(qb - b) <=
SQR(ql - r) + SQR(ql - g) + SQR(ql - b))
return ir * 36 + ig * 6 + ib + 020;
return il + 0350;
}
static std::string set_xterm256_foreground(int r, int g, int b) {
int x = rgb2xterm256(r, g, b);
std::ostringstream oss;
oss << "\033[38;5;" << x << "m";
return oss.str();
}
const std::vector<std::string> k_colors = {
set_xterm256_foreground(220,   5,  12),
set_xterm256_foreground(232,  96,  28),
set_xterm256_foreground(241, 147,  45),
set_xterm256_foreground(246, 193,  65),
set_xterm256_foreground(247, 240,  86),
set_xterm256_foreground(144, 201, 135),
set_xterm256_foreground( 78, 178, 101),
};
static std::string set_inverse() {
return "\033[7m";
}
static std::string set_underline() {
return "\033[4m";
}
static std::string set_dim() {
return "\033[2m";
}
const std::vector<std::string> k_styles = {
set_inverse(),
set_underline(),
set_dim(),
};
bool is_file_exist(const char * filename);