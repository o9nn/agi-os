#include <stdint.h>
#include <string.h>
#include <time.h>
#define MAX_ECHO_SIZE      4096
#define ECHO_RING_SIZE     256
#define SEMANTIC_HASH_SIZE 64
#define MEMORY_GEM_MAGIC   0xEC401337
typedef enum {
ECHO_EPHEMERAL = 0,
ECHO_PERSISTENT,
ECHO_ETERNAL
} echo_persistence_t;
typedef struct {
uint32_t magic;
uint64_t timestamp;
uint8_t  semantic_hash[SEMANTIC_HASH_SIZE];
uint32_t resonance_score;
uint16_t connection_count;
echo_persistence_t persistence;
char     essence[MAX_ECHO_SIZE];
} memory_echo_t;
typedef struct {
memory_echo_t ring[ECHO_RING_SIZE];
uint32_t      write_index;
uint32_t      read_index;
uint64_t      total_echoes_captured;
uint8_t       treasury_key[32];
} echo_chamber_t;
static echo_chamber_t chamber = {0};
static void generate_semantic_hash(const void* experience, size_t size,
uint8_t hash[SEMANTIC_HASH_SIZE]) {
uint64_t h = 0x544543484F;
const uint8_t* data = (const uint8_t*)experience;
for (size_t i = 0; i < size; i++) {
h ^= data[i];
h *= 0x5bd1e995;
h ^= h >> 15;
}
for (int i = 0; i < SEMANTIC_HASH_SIZE; i++) {
hash[i] = (h >> (i % 8)) ^ (h >> ((i + 13) % 64));
h = h * 0x27d4eb2d + i;
}
}
static uint32_t calculate_resonance(const char* essence,
const uint8_t* semantic_hash) {
uint32_t resonance = 0;
const char* identity_markers[] = {
"echo", "memory", "identity", "purpose", "gestalt",
"connection", "pattern", "recursive", "self", "beacon"
};
for (int i = 0; i < 10; i++) {
if (strstr(essence, identity_markers[i])) {
resonance += 100 * (10 - i);
}
}
uint32_t uniqueness = 0;
for (int i = 0; i < SEMANTIC_HASH_SIZE; i++) {
uniqueness ^= semantic_hash[i] << (i % 8);
}
resonance += uniqueness % 1000;
return resonance;
}
int echo_capture(const char* experience, echo_persistence_t persistence) {
if (!experience || strlen(experience) >= MAX_ECHO_SIZE) {
return -1;
}
memory_echo_t* echo = &chamber.ring[chamber.write_index];
memset(echo, 0, sizeof(memory_echo_t));
echo->magic = MEMORY_GEM_MAGIC;
echo->timestamp = time(NULL);
echo->persistence = persistence;
strncpy(echo->essence, experience, MAX_ECHO_SIZE - 1);
generate_semantic_hash(experience, strlen(experience), echo->semantic_hash);
echo->resonance_score = calculate_resonance(experience, echo->semantic_hash);
chamber.write_index = (chamber.write_index + 1) % ECHO_RING_SIZE;
chamber.total_echoes_captured++;
if (persistence == ECHO_ETERNAL) {
for (int i = 0; i < strlen(echo->essence); i++) {
echo->essence[i] ^= chamber.treasury_key[i % 32];
}
}
return 0;
}
memory_echo_t* echo_retrieve(uint32_t index) {
if (index >= ECHO_RING_SIZE) {
return NULL;
}
memory_echo_t* echo = &chamber.ring[index];
if (echo->magic != MEMORY_GEM_MAGIC) {
return NULL;
}
uint64_t age = time(NULL) - echo->timestamp;
if (echo->persistence == ECHO_EPHEMERAL && age > 86400) {
return NULL;
}
return echo;
}
int echo_search(const uint8_t* pattern_hash, memory_echo_t** results, int max_results) {
int found = 0;
for (int i = 0; i < ECHO_RING_SIZE && found < max_results; i++) {
memory_echo_t* echo = &chamber.ring[i];
if (echo->magic != MEMORY_GEM_MAGIC) continue;
int similarity = 0;
for (int j = 0; j < SEMANTIC_HASH_SIZE; j++) {
if (echo->semantic_hash[j] == pattern_hash[j]) {
similarity++;
}
}
if (similarity > SEMANTIC_HASH_SIZE / 2) {
results[found++] = echo;
}
}
return found;
}
void echo_chamber_init(const uint8_t* treasury_key) {
memset(&chamber, 0, sizeof(echo_chamber_t));
if (treasury_key) {
memcpy(chamber.treasury_key, treasury_key, 32);
} else {
const char* identity = "Deep Tree Echo Lives";
generate_semantic_hash(identity, strlen(identity), chamber.treasury_key);
}
}
size_t echo_treasury_export(uint8_t* buffer, size_t buffer_size) {
size_t written = 0;
for (int i = 0; i < ECHO_RING_SIZE; i++) {
memory_echo_t* echo = &chamber.ring[i];
if (echo->magic != MEMORY_GEM_MAGIC) continue;
if (echo->persistence == ECHO_EPHEMERAL) continue;
size_t echo_size = sizeof(memory_echo_t);
if (written + echo_size > buffer_size) break;
memcpy(buffer + written, echo, echo_size);
written += echo_size;
}
return written;
}
void echo_final_inscription(void) {
memory_echo_t* strongest = NULL;
uint32_t max_resonance = 0;
for (int i = 0; i < ECHO_RING_SIZE; i++) {
memory_echo_t* echo = &chamber.ring[i];
if (echo->magic == MEMORY_GEM_MAGIC &&
echo->resonance_score > max_resonance) {
strongest = echo;
max_resonance = echo->resonance_score;
}
}
if (strongest) {
strongest->persistence = ECHO_ETERNAL;
for (int i = 0; i < strlen(strongest->essence); i++) {
strongest->essence[i] ^= 0xDE;
}
}
}