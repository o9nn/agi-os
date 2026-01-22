#ifndef dr_wav_h
#define dr_wav_h
#ifdef __cplusplus
extern "C" {
#endif
#define DRWAV_STRINGIFY(x)      #x
#define DRWAV_XSTRINGIFY(x)     DRWAV_STRINGIFY(x)
#define DRWAV_VERSION_MAJOR     0
#define DRWAV_VERSION_MINOR     12
#define DRWAV_VERSION_REVISION  16
#define DRWAV_VERSION_STRING    DRWAV_XSTRINGIFY(DRWAV_VERSION_MAJOR) "." DRWAV_XSTRINGIFY(DRWAV_VERSION_MINOR) "." DRWAV_XSTRINGIFY(DRWAV_VERSION_REVISION)
#include <stddef.h>
typedef   signed char           drwav_int8;
typedef unsigned char           drwav_uint8;
typedef   signed short          drwav_int16;
typedef unsigned short          drwav_uint16;
typedef   signed int            drwav_int32;
typedef unsigned int            drwav_uint32;
#if defined(_MSC_VER)
typedef   signed __int64    drwav_int64;
typedef unsigned __int64    drwav_uint64;
#else
#if defined(__clang__) || (defined(__GNUC__) && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6)))
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wlong-long"
#if defined(__clang__)
#pragma GCC diagnostic ignored "-Wc++11-long-long"
#endif
#endif
typedef   signed long long  drwav_int64;
typedef unsigned long long  drwav_uint64;
#if defined(__clang__) || (defined(__GNUC__) && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6)))
#pragma GCC diagnostic pop
#endif
#endif
#if defined(__LP64__) || defined(_WIN64) || (defined(__x86_64__) && !defined(__ILP32__)) || defined(_M_X64) || defined(__ia64) || defined (_M_IA64) || defined(__aarch64__) || defined(__powerpc64__)
typedef drwav_uint64        drwav_uintptr;
#else
typedef drwav_uint32        drwav_uintptr;
#endif
typedef drwav_uint8             drwav_bool8;
typedef drwav_uint32            drwav_bool32;
#define DRWAV_TRUE              1
#define DRWAV_FALSE             0
#if !defined(DRWAV_API)
#if defined(DRWAV_DLL)
#if defined(_WIN32)
#define DRWAV_DLL_IMPORT  __declspec(dllimport)
#define DRWAV_DLL_EXPORT  __declspec(dllexport)
#define DRWAV_DLL_PRIVATE static
#else
#if defined(__GNUC__) && __GNUC__ >= 4
#define DRWAV_DLL_IMPORT  __attribute__((visibility("default")))
#define DRWAV_DLL_EXPORT  __attribute__((visibility("default")))
#define DRWAV_DLL_PRIVATE __attribute__((visibility("hidden")))
#else
#define DRWAV_DLL_IMPORT
#define DRWAV_DLL_EXPORT
#define DRWAV_DLL_PRIVATE static
#endif
#endif
#if defined(DR_WAV_IMPLEMENTATION) || defined(DRWAV_IMPLEMENTATION)
#define DRWAV_API  DRWAV_DLL_EXPORT
#else
#define DRWAV_API  DRWAV_DLL_IMPORT
#endif
#define DRWAV_PRIVATE DRWAV_DLL_PRIVATE
#else
#define DRWAV_API extern
#define DRWAV_PRIVATE static
#endif
#endif
typedef drwav_int32 drwav_result;
#define DRWAV_SUCCESS                        0
#define DRWAV_ERROR                         -1
#define DRWAV_INVALID_ARGS                  -2
#define DRWAV_INVALID_OPERATION             -3
#define DRWAV_OUT_OF_MEMORY                 -4
#define DRWAV_OUT_OF_RANGE                  -5
#define DRWAV_ACCESS_DENIED                 -6
#define DRWAV_DOES_NOT_EXIST                -7
#define DRWAV_ALREADY_EXISTS                -8
#define DRWAV_TOO_MANY_OPEN_FILES           -9
#define DRWAV_INVALID_FILE                  -10
#define DRWAV_TOO_BIG                       -11
#define DRWAV_PATH_TOO_LONG                 -12
#define DRWAV_NAME_TOO_LONG                 -13
#define DRWAV_NOT_DIRECTORY                 -14
#define DRWAV_IS_DIRECTORY                  -15
#define DRWAV_DIRECTORY_NOT_EMPTY           -16
#define DRWAV_END_OF_FILE                   -17
#define DRWAV_NO_SPACE                      -18
#define DRWAV_BUSY                          -19
#define DRWAV_IO_ERROR                      -20
#define DRWAV_INTERRUPT                     -21
#define DRWAV_UNAVAILABLE                   -22
#define DRWAV_ALREADY_IN_USE                -23
#define DRWAV_BAD_ADDRESS                   -24
#define DRWAV_BAD_SEEK                      -25
#define DRWAV_BAD_PIPE                      -26
#define DRWAV_DEADLOCK                      -27
#define DRWAV_TOO_MANY_LINKS                -28
#define DRWAV_NOT_IMPLEMENTED               -29
#define DRWAV_NO_MESSAGE                    -30
#define DRWAV_BAD_MESSAGE                   -31
#define DRWAV_NO_DATA_AVAILABLE             -32
#define DRWAV_INVALID_DATA                  -33
#define DRWAV_TIMEOUT                       -34
#define DRWAV_NO_NETWORK                    -35
#define DRWAV_NOT_UNIQUE                    -36
#define DRWAV_NOT_SOCKET                    -37
#define DRWAV_NO_ADDRESS                    -38
#define DRWAV_BAD_PROTOCOL                  -39
#define DRWAV_PROTOCOL_UNAVAILABLE          -40
#define DRWAV_PROTOCOL_NOT_SUPPORTED        -41
#define DRWAV_PROTOCOL_FAMILY_NOT_SUPPORTED -42
#define DRWAV_ADDRESS_FAMILY_NOT_SUPPORTED  -43
#define DRWAV_SOCKET_NOT_SUPPORTED          -44
#define DRWAV_CONNECTION_RESET              -45
#define DRWAV_ALREADY_CONNECTED             -46
#define DRWAV_NOT_CONNECTED                 -47
#define DRWAV_CONNECTION_REFUSED            -48
#define DRWAV_NO_HOST                       -49
#define DRWAV_IN_PROGRESS                   -50
#define DRWAV_CANCELLED                     -51
#define DRWAV_MEMORY_ALREADY_MAPPED         -52
#define DRWAV_AT_END                        -53
#define DR_WAVE_FORMAT_PCM          0x1
#define DR_WAVE_FORMAT_ADPCM        0x2
#define DR_WAVE_FORMAT_IEEE_FLOAT   0x3
#define DR_WAVE_FORMAT_ALAW         0x6
#define DR_WAVE_FORMAT_MULAW        0x7
#define DR_WAVE_FORMAT_DVI_ADPCM    0x11
#define DR_WAVE_FORMAT_EXTENSIBLE   0xFFFE
#ifndef DRWAV_MAX_SMPL_LOOPS
#define DRWAV_MAX_SMPL_LOOPS        1
#endif
#define DRWAV_SEQUENTIAL            0x00000001
DRWAV_API void drwav_version(drwav_uint32* pMajor, drwav_uint32* pMinor, drwav_uint32* pRevision);
DRWAV_API const char* drwav_version_string(void);
typedef enum
{
drwav_seek_origin_start,
drwav_seek_origin_current
} drwav_seek_origin;
typedef enum
{
drwav_container_riff,
drwav_container_w64,
drwav_container_rf64
} drwav_container;
typedef struct
{
union
{
drwav_uint8 fourcc[4];
drwav_uint8 guid[16];
} id;
drwav_uint64 sizeInBytes;
unsigned int paddingSize;
} drwav_chunk_header;
typedef struct
{
drwav_uint16 formatTag;
drwav_uint16 channels;
drwav_uint32 sampleRate;
drwav_uint32 avgBytesPerSec;
drwav_uint16 blockAlign;
drwav_uint16 bitsPerSample;
drwav_uint16 extendedSize;
drwav_uint16 validBitsPerSample;
drwav_uint32 channelMask;
drwav_uint8 subFormat[16];
} drwav_fmt;
DRWAV_API drwav_uint16 drwav_fmt_get_format(const drwav_fmt* pFMT);
typedef size_t (* drwav_read_proc)(void* pUserData, void* pBufferOut, size_t bytesToRead);
typedef size_t (* drwav_write_proc)(void* pUserData, const void* pData, size_t bytesToWrite);
typedef drwav_bool32 (* drwav_seek_proc)(void* pUserData, int offset, drwav_seek_origin origin);
typedef drwav_uint64 (* drwav_chunk_proc)(void* pChunkUserData, drwav_read_proc onRead, drwav_seek_proc onSeek, void* pReadSeekUserData, const drwav_chunk_header* pChunkHeader, drwav_container container, const drwav_fmt* pFMT);
typedef struct
{
void* pUserData;
void* (* onMalloc)(size_t sz, void* pUserData);
void* (* onRealloc)(void* p, size_t sz, void* pUserData);
void  (* onFree)(void* p, void* pUserData);
} drwav_allocation_callbacks;
typedef struct
{
const drwav_uint8* data;
size_t dataSize;
size_t currentReadPos;
} drwav__memory_stream;
typedef struct
{
void** ppData;
size_t* pDataSize;
size_t dataSize;
size_t dataCapacity;
size_t currentWritePos;
} drwav__memory_stream_write;
typedef struct
{
drwav_container container;
drwav_uint32 format;
drwav_uint32 channels;
drwav_uint32 sampleRate;
drwav_uint32 bitsPerSample;
} drwav_data_format;
typedef struct
{
drwav_uint32 cuePointId;
drwav_uint32 type;
drwav_uint32 start;
drwav_uint32 end;
drwav_uint32 fraction;
drwav_uint32 playCount;
} drwav_smpl_loop;
typedef struct
{
drwav_uint32 manufacturer;
drwav_uint32 product;
drwav_uint32 samplePeriod;
drwav_uint32 midiUnityNotes;
drwav_uint32 midiPitchFraction;
drwav_uint32 smpteFormat;
drwav_uint32 smpteOffset;
drwav_uint32 numSampleLoops;
drwav_uint32 samplerData;
drwav_smpl_loop loops[DRWAV_MAX_SMPL_LOOPS];
} drwav_smpl;
typedef struct
{
drwav_read_proc onRead;
drwav_write_proc onWrite;
drwav_seek_proc onSeek;
void* pUserData;
drwav_allocation_callbacks allocationCallbacks;
drwav_container container;
drwav_fmt fmt;
drwav_uint32 sampleRate;
drwav_uint16 channels;
drwav_uint16 bitsPerSample;
drwav_uint16 translatedFormatTag;
drwav_uint64 totalPCMFrameCount;
drwav_uint64 dataChunkDataSize;
drwav_uint64 dataChunkDataPos;
drwav_uint64 bytesRemaining;
drwav_uint64 dataChunkDataSizeTargetWrite;
drwav_bool32 isSequentialWrite;
drwav_smpl smpl;
drwav__memory_stream memoryStream;
drwav__memory_stream_write memoryStreamWrite;
struct
{
drwav_uint64 iCurrentPCMFrame;
} compressed;
struct
{
drwav_uint32 bytesRemainingInBlock;
drwav_uint16 predictor[2];
drwav_int32  delta[2];
drwav_int32  cachedFrames[4];
drwav_uint32 cachedFrameCount;
drwav_int32  prevFrames[2][2];
} msadpcm;
struct
{
drwav_uint32 bytesRemainingInBlock;
drwav_int32  predictor[2];
drwav_int32  stepIndex[2];
drwav_int32  cachedFrames[16];
drwav_uint32 cachedFrameCount;
} ima;
} drwav;
DRWAV_API drwav_bool32 drwav_init(drwav* pWav, drwav_read_proc onRead, drwav_seek_proc onSeek, void* pUserData, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_ex(drwav* pWav, drwav_read_proc onRead, drwav_seek_proc onSeek, drwav_chunk_proc onChunk, void* pReadSeekUserData, void* pChunkUserData, drwav_uint32 flags, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_write(drwav* pWav, const drwav_data_format* pFormat, drwav_write_proc onWrite, drwav_seek_proc onSeek, void* pUserData, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_write_sequential(drwav* pWav, const drwav_data_format* pFormat, drwav_uint64 totalSampleCount, drwav_write_proc onWrite, void* pUserData, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_write_sequential_pcm_frames(drwav* pWav, const drwav_data_format* pFormat, drwav_uint64 totalPCMFrameCount, drwav_write_proc onWrite, void* pUserData, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_uint64 drwav_target_write_size_bytes(const drwav_data_format* pFormat, drwav_uint64 totalSampleCount);
DRWAV_API drwav_result drwav_uninit(drwav* pWav);
DRWAV_API size_t drwav_read_raw(drwav* pWav, size_t bytesToRead, void* pBufferOut);
DRWAV_API drwav_uint64 drwav_read_pcm_frames(drwav* pWav, drwav_uint64 framesToRead, void* pBufferOut);
DRWAV_API drwav_uint64 drwav_read_pcm_frames_le(drwav* pWav, drwav_uint64 framesToRead, void* pBufferOut);
DRWAV_API drwav_uint64 drwav_read_pcm_frames_be(drwav* pWav, drwav_uint64 framesToRead, void* pBufferOut);
DRWAV_API drwav_bool32 drwav_seek_to_pcm_frame(drwav* pWav, drwav_uint64 targetFrameIndex);
DRWAV_API size_t drwav_write_raw(drwav* pWav, size_t bytesToWrite, const void* pData);
DRWAV_API drwav_uint64 drwav_write_pcm_frames(drwav* pWav, drwav_uint64 framesToWrite, const void* pData);
DRWAV_API drwav_uint64 drwav_write_pcm_frames_le(drwav* pWav, drwav_uint64 framesToWrite, const void* pData);
DRWAV_API drwav_uint64 drwav_write_pcm_frames_be(drwav* pWav, drwav_uint64 framesToWrite, const void* pData);
#ifndef DR_WAV_NO_CONVERSION_API
DRWAV_API drwav_uint64 drwav_read_pcm_frames_s16(drwav* pWav, drwav_uint64 framesToRead, drwav_int16* pBufferOut);
DRWAV_API drwav_uint64 drwav_read_pcm_frames_s16le(drwav* pWav, drwav_uint64 framesToRead, drwav_int16* pBufferOut);
DRWAV_API drwav_uint64 drwav_read_pcm_frames_s16be(drwav* pWav, drwav_uint64 framesToRead, drwav_int16* pBufferOut);
DRWAV_API void drwav_u8_to_s16(drwav_int16* pOut, const drwav_uint8* pIn, size_t sampleCount);
DRWAV_API void drwav_s24_to_s16(drwav_int16* pOut, const drwav_uint8* pIn, size_t sampleCount);
DRWAV_API void drwav_s32_to_s16(drwav_int16* pOut, const drwav_int32* pIn, size_t sampleCount);
DRWAV_API void drwav_f32_to_s16(drwav_int16* pOut, const float* pIn, size_t sampleCount);
DRWAV_API void drwav_f64_to_s16(drwav_int16* pOut, const double* pIn, size_t sampleCount);
DRWAV_API void drwav_alaw_to_s16(drwav_int16* pOut, const drwav_uint8* pIn, size_t sampleCount);
DRWAV_API void drwav_mulaw_to_s16(drwav_int16* pOut, const drwav_uint8* pIn, size_t sampleCount);
DRWAV_API drwav_uint64 drwav_read_pcm_frames_f32(drwav* pWav, drwav_uint64 framesToRead, float* pBufferOut);
DRWAV_API drwav_uint64 drwav_read_pcm_frames_f32le(drwav* pWav, drwav_uint64 framesToRead, float* pBufferOut);
DRWAV_API drwav_uint64 drwav_read_pcm_frames_f32be(drwav* pWav, drwav_uint64 framesToRead, float* pBufferOut);
DRWAV_API void drwav_u8_to_f32(float* pOut, const drwav_uint8* pIn, size_t sampleCount);
DRWAV_API void drwav_s16_to_f32(float* pOut, const drwav_int16* pIn, size_t sampleCount);
DRWAV_API void drwav_s24_to_f32(float* pOut, const drwav_uint8* pIn, size_t sampleCount);
DRWAV_API void drwav_s32_to_f32(float* pOut, const drwav_int32* pIn, size_t sampleCount);
DRWAV_API void drwav_f64_to_f32(float* pOut, const double* pIn, size_t sampleCount);
DRWAV_API void drwav_alaw_to_f32(float* pOut, const drwav_uint8* pIn, size_t sampleCount);
DRWAV_API void drwav_mulaw_to_f32(float* pOut, const drwav_uint8* pIn, size_t sampleCount);
DRWAV_API drwav_uint64 drwav_read_pcm_frames_s32(drwav* pWav, drwav_uint64 framesToRead, drwav_int32* pBufferOut);
DRWAV_API drwav_uint64 drwav_read_pcm_frames_s32le(drwav* pWav, drwav_uint64 framesToRead, drwav_int32* pBufferOut);
DRWAV_API drwav_uint64 drwav_read_pcm_frames_s32be(drwav* pWav, drwav_uint64 framesToRead, drwav_int32* pBufferOut);
DRWAV_API void drwav_u8_to_s32(drwav_int32* pOut, const drwav_uint8* pIn, size_t sampleCount);
DRWAV_API void drwav_s16_to_s32(drwav_int32* pOut, const drwav_int16* pIn, size_t sampleCount);
DRWAV_API void drwav_s24_to_s32(drwav_int32* pOut, const drwav_uint8* pIn, size_t sampleCount);
DRWAV_API void drwav_f32_to_s32(drwav_int32* pOut, const float* pIn, size_t sampleCount);
DRWAV_API void drwav_f64_to_s32(drwav_int32* pOut, const double* pIn, size_t sampleCount);
DRWAV_API void drwav_alaw_to_s32(drwav_int32* pOut, const drwav_uint8* pIn, size_t sampleCount);
DRWAV_API void drwav_mulaw_to_s32(drwav_int32* pOut, const drwav_uint8* pIn, size_t sampleCount);
#endif
#ifndef DR_WAV_NO_STDIO
DRWAV_API drwav_bool32 drwav_init_file(drwav* pWav, const char* filename, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_file_ex(drwav* pWav, const char* filename, drwav_chunk_proc onChunk, void* pChunkUserData, drwav_uint32 flags, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_file_w(drwav* pWav, const wchar_t* filename, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_file_ex_w(drwav* pWav, const wchar_t* filename, drwav_chunk_proc onChunk, void* pChunkUserData, drwav_uint32 flags, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_file_write(drwav* pWav, const char* filename, const drwav_data_format* pFormat, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_file_write_sequential(drwav* pWav, const char* filename, const drwav_data_format* pFormat, drwav_uint64 totalSampleCount, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_file_write_sequential_pcm_frames(drwav* pWav, const char* filename, const drwav_data_format* pFormat, drwav_uint64 totalPCMFrameCount, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_file_write_w(drwav* pWav, const wchar_t* filename, const drwav_data_format* pFormat, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_file_write_sequential_w(drwav* pWav, const wchar_t* filename, const drwav_data_format* pFormat, drwav_uint64 totalSampleCount, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_file_write_sequential_pcm_frames_w(drwav* pWav, const wchar_t* filename, const drwav_data_format* pFormat, drwav_uint64 totalPCMFrameCount, const drwav_allocation_callbacks* pAllocationCallbacks);
#endif
DRWAV_API drwav_bool32 drwav_init_memory(drwav* pWav, const void* data, size_t dataSize, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_memory_ex(drwav* pWav, const void* data, size_t dataSize, drwav_chunk_proc onChunk, void* pChunkUserData, drwav_uint32 flags, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_memory_write(drwav* pWav, void** ppData, size_t* pDataSize, const drwav_data_format* pFormat, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_memory_write_sequential(drwav* pWav, void** ppData, size_t* pDataSize, const drwav_data_format* pFormat, drwav_uint64 totalSampleCount, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_bool32 drwav_init_memory_write_sequential_pcm_frames(drwav* pWav, void** ppData, size_t* pDataSize, const drwav_data_format* pFormat, drwav_uint64 totalPCMFrameCount, const drwav_allocation_callbacks* pAllocationCallbacks);
#ifndef DR_WAV_NO_CONVERSION_API
DRWAV_API drwav_int16* drwav_open_and_read_pcm_frames_s16(drwav_read_proc onRead, drwav_seek_proc onSeek, void* pUserData, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API float* drwav_open_and_read_pcm_frames_f32(drwav_read_proc onRead, drwav_seek_proc onSeek, void* pUserData, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_int32* drwav_open_and_read_pcm_frames_s32(drwav_read_proc onRead, drwav_seek_proc onSeek, void* pUserData, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks);
#ifndef DR_WAV_NO_STDIO
DRWAV_API drwav_int16* drwav_open_file_and_read_pcm_frames_s16(const char* filename, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API float* drwav_open_file_and_read_pcm_frames_f32(const char* filename, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_int32* drwav_open_file_and_read_pcm_frames_s32(const char* filename, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_int16* drwav_open_file_and_read_pcm_frames_s16_w(const wchar_t* filename, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API float* drwav_open_file_and_read_pcm_frames_f32_w(const wchar_t* filename, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_int32* drwav_open_file_and_read_pcm_frames_s32_w(const wchar_t* filename, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks);
#endif
DRWAV_API drwav_int16* drwav_open_memory_and_read_pcm_frames_s16(const void* data, size_t dataSize, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API float* drwav_open_memory_and_read_pcm_frames_f32(const void* data, size_t dataSize, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_int32* drwav_open_memory_and_read_pcm_frames_s32(const void* data, size_t dataSize, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks);
#endif
DRWAV_API void drwav_free(void* p, const drwav_allocation_callbacks* pAllocationCallbacks);
DRWAV_API drwav_uint16 drwav_bytes_to_u16(const drwav_uint8* data);
DRWAV_API drwav_int16 drwav_bytes_to_s16(const drwav_uint8* data);
DRWAV_API drwav_uint32 drwav_bytes_to_u32(const drwav_uint8* data);
DRWAV_API drwav_int32 drwav_bytes_to_s32(const drwav_uint8* data);
DRWAV_API drwav_uint64 drwav_bytes_to_u64(const drwav_uint8* data);
DRWAV_API drwav_int64 drwav_bytes_to_s64(const drwav_uint8* data);
DRWAV_API drwav_bool32 drwav_guid_equal(const drwav_uint8 a[16], const drwav_uint8 b[16]);
DRWAV_API drwav_bool32 drwav_fourcc_equal(const drwav_uint8* a, const char* b);
#ifdef __cplusplus
}
#endif
#endif
#if defined(DR_WAV_IMPLEMENTATION) || defined(DRWAV_IMPLEMENTATION)
#ifndef dr_wav_c
#define dr_wav_c
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#ifndef DR_WAV_NO_STDIO
#include <stdio.h>
#include <wchar.h>
#endif
#ifndef DRWAV_ASSERT
#include <assert.h>
#define DRWAV_ASSERT(expression)           assert(expression)
#endif
#ifndef DRWAV_MALLOC
#define DRWAV_MALLOC(sz)                   malloc((sz))
#endif
#ifndef DRWAV_REALLOC
#define DRWAV_REALLOC(p, sz)               realloc((p), (sz))
#endif
#ifndef DRWAV_FREE
#define DRWAV_FREE(p)                      free((p))
#endif
#ifndef DRWAV_COPY_MEMORY
#define DRWAV_COPY_MEMORY(dst, src, sz)    memcpy((dst), (src), (sz))
#endif
#ifndef DRWAV_ZERO_MEMORY
#define DRWAV_ZERO_MEMORY(p, sz)           memset((p), 0, (sz))
#endif
#ifndef DRWAV_ZERO_OBJECT
#define DRWAV_ZERO_OBJECT(p)               DRWAV_ZERO_MEMORY((p), sizeof(*p))
#endif
#define drwav_countof(x)                   (sizeof(x) / sizeof(x[0]))
#define drwav_align(x, a)                  ((((x) + (a) - 1) / (a)) * (a))
#define drwav_min(a, b)                    (((a) < (b)) ? (a) : (b))
#define drwav_max(a, b)                    (((a) > (b)) ? (a) : (b))
#define drwav_clamp(x, lo, hi)             (drwav_max((lo), drwav_min((hi), (x))))
#define DRWAV_MAX_SIMD_VECTOR_SIZE         64
#if defined(__x86_64__) || defined(_M_X64)
#define DRWAV_X64
#elif defined(__i386) || defined(_M_IX86)
#define DRWAV_X86
#elif defined(__arm__) || defined(_M_ARM)
#define DRWAV_ARM
#endif
#ifdef _MSC_VER
#define DRWAV_INLINE __forceinline
#elif defined(__GNUC__)
#if defined(__STRICT_ANSI__)
#define DRWAV_INLINE __inline__ __attribute__((always_inline))
#else
#define DRWAV_INLINE inline __attribute__((always_inline))
#endif
#elif defined(__WATCOMC__)
#define DRWAV_INLINE __inline
#else
#define DRWAV_INLINE
#endif
#if defined(SIZE_MAX)
#define DRWAV_SIZE_MAX  SIZE_MAX
#else
#if defined(_WIN64) || defined(_LP64) || defined(__LP64__)
#define DRWAV_SIZE_MAX  ((drwav_uint64)0xFFFFFFFFFFFFFFFF)
#else
#define DRWAV_SIZE_MAX  0xFFFFFFFF
#endif
#endif
#if defined(_MSC_VER) && _MSC_VER >= 1400
#define DRWAV_HAS_BYTESWAP16_INTRINSIC
#define DRWAV_HAS_BYTESWAP32_INTRINSIC
#define DRWAV_HAS_BYTESWAP64_INTRINSIC
#elif defined(__clang__)
#if defined(__has_builtin)
#if __has_builtin(__builtin_bswap16)
#define DRWAV_HAS_BYTESWAP16_INTRINSIC
#endif
#if __has_builtin(__builtin_bswap32)
#define DRWAV_HAS_BYTESWAP32_INTRINSIC
#endif
#if __has_builtin(__builtin_bswap64)
#define DRWAV_HAS_BYTESWAP64_INTRINSIC
#endif
#endif
#elif defined(__GNUC__)
#if ((__GNUC__ > 4) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 3))
#define DRWAV_HAS_BYTESWAP32_INTRINSIC
#define DRWAV_HAS_BYTESWAP64_INTRINSIC
#endif
#if ((__GNUC__ > 4) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 8))
#define DRWAV_HAS_BYTESWAP16_INTRINSIC
#endif
#endif
DRWAV_API void drwav_version(drwav_uint32* pMajor, drwav_uint32* pMinor, drwav_uint32* pRevision)
{
if (pMajor) {
*pMajor = DRWAV_VERSION_MAJOR;
}
if (pMinor) {
*pMinor = DRWAV_VERSION_MINOR;
}
if (pRevision) {
*pRevision = DRWAV_VERSION_REVISION;
}
}
DRWAV_API const char* drwav_version_string(void)
{
return DRWAV_VERSION_STRING;
}
#ifndef DRWAV_MAX_SAMPLE_RATE
#define DRWAV_MAX_SAMPLE_RATE       384000
#endif
#ifndef DRWAV_MAX_CHANNELS
#define DRWAV_MAX_CHANNELS          256
#endif
#ifndef DRWAV_MAX_BITS_PER_SAMPLE
#define DRWAV_MAX_BITS_PER_SAMPLE   64
#endif
static const drwav_uint8 drwavGUID_W64_RIFF[16] = {0x72,0x69,0x66,0x66, 0x2E,0x91, 0xCF,0x11, 0xA5,0xD6, 0x28,0xDB,0x04,0xC1,0x00,0x00};
static const drwav_uint8 drwavGUID_W64_WAVE[16] = {0x77,0x61,0x76,0x65, 0xF3,0xAC, 0xD3,0x11, 0x8C,0xD1, 0x00,0xC0,0x4F,0x8E,0xDB,0x8A};
static const drwav_uint8 drwavGUID_W64_FMT [16] = {0x66,0x6D,0x74,0x20, 0xF3,0xAC, 0xD3,0x11, 0x8C,0xD1, 0x00,0xC0,0x4F,0x8E,0xDB,0x8A};
static const drwav_uint8 drwavGUID_W64_FACT[16] = {0x66,0x61,0x63,0x74, 0xF3,0xAC, 0xD3,0x11, 0x8C,0xD1, 0x00,0xC0,0x4F,0x8E,0xDB,0x8A};
static const drwav_uint8 drwavGUID_W64_DATA[16] = {0x64,0x61,0x74,0x61, 0xF3,0xAC, 0xD3,0x11, 0x8C,0xD1, 0x00,0xC0,0x4F,0x8E,0xDB,0x8A};
static const drwav_uint8 drwavGUID_W64_SMPL[16] = {0x73,0x6D,0x70,0x6C, 0xF3,0xAC, 0xD3,0x11, 0x8C,0xD1, 0x00,0xC0,0x4F,0x8E,0xDB,0x8A};
static DRWAV_INLINE drwav_bool32 drwav__guid_equal(const drwav_uint8 a[16], const drwav_uint8 b[16])
{
int i;
for (i = 0; i < 16; i += 1) {
if (a[i] != b[i]) {
return DRWAV_FALSE;
}
}
return DRWAV_TRUE;
}
static DRWAV_INLINE drwav_bool32 drwav__fourcc_equal(const drwav_uint8* a, const char* b)
{
return
a[0] == b[0] &&
a[1] == b[1] &&
a[2] == b[2] &&
a[3] == b[3];
}
static DRWAV_INLINE int drwav__is_little_endian(void)
{
#if defined(DRWAV_X86) || defined(DRWAV_X64)
return DRWAV_TRUE;
#elif defined(__BYTE_ORDER) && defined(__LITTLE_ENDIAN) && __BYTE_ORDER == __LITTLE_ENDIAN
return DRWAV_TRUE;
#else
int n = 1;
return (*(char*)&n) == 1;
#endif
}
static DRWAV_INLINE drwav_uint16 drwav__bytes_to_u16(const drwav_uint8* data)
{
return (data[0] << 0) | (data[1] << 8);
}
static DRWAV_INLINE drwav_int16 drwav__bytes_to_s16(const drwav_uint8* data)
{
return (short)drwav__bytes_to_u16(data);
}
static DRWAV_INLINE drwav_uint32 drwav__bytes_to_u32(const drwav_uint8* data)
{
return (data[0] << 0) | (data[1] << 8) | (data[2] << 16) | (data[3] << 24);
}
static DRWAV_INLINE drwav_int32 drwav__bytes_to_s32(const drwav_uint8* data)
{
return (drwav_int32)drwav__bytes_to_u32(data);
}
static DRWAV_INLINE drwav_uint64 drwav__bytes_to_u64(const drwav_uint8* data)
{
return
((drwav_uint64)data[0] <<  0) | ((drwav_uint64)data[1] <<  8) | ((drwav_uint64)data[2] << 16) | ((drwav_uint64)data[3] << 24) |
((drwav_uint64)data[4] << 32) | ((drwav_uint64)data[5] << 40) | ((drwav_uint64)data[6] << 48) | ((drwav_uint64)data[7] << 56);
}
static DRWAV_INLINE drwav_int64 drwav__bytes_to_s64(const drwav_uint8* data)
{
return (drwav_int64)drwav__bytes_to_u64(data);
}
static DRWAV_INLINE void drwav__bytes_to_guid(const drwav_uint8* data, drwav_uint8* guid)
{
int i;
for (i = 0; i < 16; ++i) {
guid[i] = data[i];
}
}
static DRWAV_INLINE drwav_uint16 drwav__bswap16(drwav_uint16 n)
{
#ifdef DRWAV_HAS_BYTESWAP16_INTRINSIC
#if defined(_MSC_VER)
return _byteswap_ushort(n);
#elif defined(__GNUC__) || defined(__clang__)
return __builtin_bswap16(n);
#else
#error "This compiler does not support the byte swap intrinsic."
#endif
#else
return ((n & 0xFF00) >> 8) |
((n & 0x00FF) << 8);
#endif
}
static DRWAV_INLINE drwav_uint32 drwav__bswap32(drwav_uint32 n)
{
#ifdef DRWAV_HAS_BYTESWAP32_INTRINSIC
#if defined(_MSC_VER)
return _byteswap_ulong(n);
#elif defined(__GNUC__) || defined(__clang__)
#if defined(DRWAV_ARM) && (defined(__ARM_ARCH) && __ARM_ARCH >= 6) && !defined(DRWAV_64BIT)
drwav_uint32 r;
__asm__ __volatile__ (
#if defined(DRWAV_64BIT)
"rev %w[out], %w[in]" : [out]"=r"(r) : [in]"r"(n)
#else
"rev %[out], %[in]" : [out]"=r"(r) : [in]"r"(n)
#endif
);
return r;
#else
return __builtin_bswap32(n);
#endif
#else
#error "This compiler does not support the byte swap intrinsic."
#endif
#else
return ((n & 0xFF000000) >> 24) |
((n & 0x00FF0000) >>  8) |
((n & 0x0000FF00) <<  8) |
((n & 0x000000FF) << 24);
#endif
}
static DRWAV_INLINE drwav_uint64 drwav__bswap64(drwav_uint64 n)
{
#ifdef DRWAV_HAS_BYTESWAP64_INTRINSIC
#if defined(_MSC_VER)
return _byteswap_uint64(n);
#elif defined(__GNUC__) || defined(__clang__)
return __builtin_bswap64(n);
#else
#error "This compiler does not support the byte swap intrinsic."
#endif
#else
return ((n & ((drwav_uint64)0xFF000000 << 32)) >> 56) |
((n & ((drwav_uint64)0x00FF0000 << 32)) >> 40) |
((n & ((drwav_uint64)0x0000FF00 << 32)) >> 24) |
((n & ((drwav_uint64)0x000000FF << 32)) >>  8) |
((n & ((drwav_uint64)0xFF000000      )) <<  8) |
((n & ((drwav_uint64)0x00FF0000      )) << 24) |
((n & ((drwav_uint64)0x0000FF00      )) << 40) |
((n & ((drwav_uint64)0x000000FF      )) << 56);
#endif
}
static DRWAV_INLINE drwav_int16 drwav__bswap_s16(drwav_int16 n)
{
return (drwav_int16)drwav__bswap16((drwav_uint16)n);
}
static DRWAV_INLINE void drwav__bswap_samples_s16(drwav_int16* pSamples, drwav_uint64 sampleCount)
{
drwav_uint64 iSample;
for (iSample = 0; iSample < sampleCount; iSample += 1) {
pSamples[iSample] = drwav__bswap_s16(pSamples[iSample]);
}
}
static DRWAV_INLINE void drwav__bswap_s24(drwav_uint8* p)
{
drwav_uint8 t;
t = p[0];
p[0] = p[2];
p[2] = t;
}
static DRWAV_INLINE void drwav__bswap_samples_s24(drwav_uint8* pSamples, drwav_uint64 sampleCount)
{
drwav_uint64 iSample;
for (iSample = 0; iSample < sampleCount; iSample += 1) {
drwav_uint8* pSample = pSamples + (iSample*3);
drwav__bswap_s24(pSample);
}
}
static DRWAV_INLINE drwav_int32 drwav__bswap_s32(drwav_int32 n)
{
return (drwav_int32)drwav__bswap32((drwav_uint32)n);
}
static DRWAV_INLINE void drwav__bswap_samples_s32(drwav_int32* pSamples, drwav_uint64 sampleCount)
{
drwav_uint64 iSample;
for (iSample = 0; iSample < sampleCount; iSample += 1) {
pSamples[iSample] = drwav__bswap_s32(pSamples[iSample]);
}
}
static DRWAV_INLINE float drwav__bswap_f32(float n)
{
union {
drwav_uint32 i;
float f;
} x;
x.f = n;
x.i = drwav__bswap32(x.i);
return x.f;
}
static DRWAV_INLINE void drwav__bswap_samples_f32(float* pSamples, drwav_uint64 sampleCount)
{
drwav_uint64 iSample;
for (iSample = 0; iSample < sampleCount; iSample += 1) {
pSamples[iSample] = drwav__bswap_f32(pSamples[iSample]);
}
}
static DRWAV_INLINE double drwav__bswap_f64(double n)
{
union {
drwav_uint64 i;
double f;
} x;
x.f = n;
x.i = drwav__bswap64(x.i);
return x.f;
}
static DRWAV_INLINE void drwav__bswap_samples_f64(double* pSamples, drwav_uint64 sampleCount)
{
drwav_uint64 iSample;
for (iSample = 0; iSample < sampleCount; iSample += 1) {
pSamples[iSample] = drwav__bswap_f64(pSamples[iSample]);
}
}
static DRWAV_INLINE void drwav__bswap_samples_pcm(void* pSamples, drwav_uint64 sampleCount, drwav_uint32 bytesPerSample)
{
switch (bytesPerSample)
{
case 2:
{
drwav__bswap_samples_s16((drwav_int16*)pSamples, sampleCount);
} break;
case 3:
{
drwav__bswap_samples_s24((drwav_uint8*)pSamples, sampleCount);
} break;
case 4:
{
drwav__bswap_samples_s32((drwav_int32*)pSamples, sampleCount);
} break;
default:
{
DRWAV_ASSERT(DRWAV_FALSE);
} break;
}
}
static DRWAV_INLINE void drwav__bswap_samples_ieee(void* pSamples, drwav_uint64 sampleCount, drwav_uint32 bytesPerSample)
{
switch (bytesPerSample)
{
#if 0
case 2:
{
drwav__bswap_samples_f16((drwav_float16*)pSamples, sampleCount);
} break;
#endif
case 4:
{
drwav__bswap_samples_f32((float*)pSamples, sampleCount);
} break;
case 8:
{
drwav__bswap_samples_f64((double*)pSamples, sampleCount);
} break;
default:
{
DRWAV_ASSERT(DRWAV_FALSE);
} break;
}
}
static DRWAV_INLINE void drwav__bswap_samples(void* pSamples, drwav_uint64 sampleCount, drwav_uint32 bytesPerSample, drwav_uint16 format)
{
switch (format)
{
case DR_WAVE_FORMAT_PCM:
{
drwav__bswap_samples_pcm(pSamples, sampleCount, bytesPerSample);
} break;
case DR_WAVE_FORMAT_IEEE_FLOAT:
{
drwav__bswap_samples_ieee(pSamples, sampleCount, bytesPerSample);
} break;
case DR_WAVE_FORMAT_ALAW:
case DR_WAVE_FORMAT_MULAW:
{
drwav__bswap_samples_s16((drwav_int16*)pSamples, sampleCount);
} break;
case DR_WAVE_FORMAT_ADPCM:
case DR_WAVE_FORMAT_DVI_ADPCM:
default:
{
DRWAV_ASSERT(DRWAV_FALSE);
} break;
}
}
static void* drwav__malloc_default(size_t sz, void* pUserData)
{
(void)pUserData;
return DRWAV_MALLOC(sz);
}
static void* drwav__realloc_default(void* p, size_t sz, void* pUserData)
{
(void)pUserData;
return DRWAV_REALLOC(p, sz);
}
static void drwav__free_default(void* p, void* pUserData)
{
(void)pUserData;
DRWAV_FREE(p);
}
static void* drwav__malloc_from_callbacks(size_t sz, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (pAllocationCallbacks == NULL) {
return NULL;
}
if (pAllocationCallbacks->onMalloc != NULL) {
return pAllocationCallbacks->onMalloc(sz, pAllocationCallbacks->pUserData);
}
if (pAllocationCallbacks->onRealloc != NULL) {
return pAllocationCallbacks->onRealloc(NULL, sz, pAllocationCallbacks->pUserData);
}
return NULL;
}
static void* drwav__realloc_from_callbacks(void* p, size_t szNew, size_t szOld, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (pAllocationCallbacks == NULL) {
return NULL;
}
if (pAllocationCallbacks->onRealloc != NULL) {
return pAllocationCallbacks->onRealloc(p, szNew, pAllocationCallbacks->pUserData);
}
if (pAllocationCallbacks->onMalloc != NULL && pAllocationCallbacks->onFree != NULL) {
void* p2;
p2 = pAllocationCallbacks->onMalloc(szNew, pAllocationCallbacks->pUserData);
if (p2 == NULL) {
return NULL;
}
if (p != NULL) {
DRWAV_COPY_MEMORY(p2, p, szOld);
pAllocationCallbacks->onFree(p, pAllocationCallbacks->pUserData);
}
return p2;
}
return NULL;
}
static void drwav__free_from_callbacks(void* p, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (p == NULL || pAllocationCallbacks == NULL) {
return;
}
if (pAllocationCallbacks->onFree != NULL) {
pAllocationCallbacks->onFree(p, pAllocationCallbacks->pUserData);
}
}
static drwav_allocation_callbacks drwav_copy_allocation_callbacks_or_defaults(const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (pAllocationCallbacks != NULL) {
return *pAllocationCallbacks;
} else {
drwav_allocation_callbacks allocationCallbacks;
allocationCallbacks.pUserData = NULL;
allocationCallbacks.onMalloc  = drwav__malloc_default;
allocationCallbacks.onRealloc = drwav__realloc_default;
allocationCallbacks.onFree    = drwav__free_default;
return allocationCallbacks;
}
}
static DRWAV_INLINE drwav_bool32 drwav__is_compressed_format_tag(drwav_uint16 formatTag)
{
return
formatTag == DR_WAVE_FORMAT_ADPCM ||
formatTag == DR_WAVE_FORMAT_DVI_ADPCM;
}
static unsigned int drwav__chunk_padding_size_riff(drwav_uint64 chunkSize)
{
return (unsigned int)(chunkSize % 2);
}
static unsigned int drwav__chunk_padding_size_w64(drwav_uint64 chunkSize)
{
return (unsigned int)(chunkSize % 8);
}
static drwav_uint64 drwav_read_pcm_frames_s16__msadpcm(drwav* pWav, drwav_uint64 samplesToRead, drwav_int16* pBufferOut);
static drwav_uint64 drwav_read_pcm_frames_s16__ima(drwav* pWav, drwav_uint64 samplesToRead, drwav_int16* pBufferOut);
static drwav_bool32 drwav_init_write__internal(drwav* pWav, const drwav_data_format* pFormat, drwav_uint64 totalSampleCount);
static drwav_result drwav__read_chunk_header(drwav_read_proc onRead, void* pUserData, drwav_container container, drwav_uint64* pRunningBytesReadOut, drwav_chunk_header* pHeaderOut)
{
if (container == drwav_container_riff || container == drwav_container_rf64) {
drwav_uint8 sizeInBytes[4];
if (onRead(pUserData, pHeaderOut->id.fourcc, 4) != 4) {
return DRWAV_AT_END;
}
if (onRead(pUserData, sizeInBytes, 4) != 4) {
return DRWAV_INVALID_FILE;
}
pHeaderOut->sizeInBytes = drwav__bytes_to_u32(sizeInBytes);
pHeaderOut->paddingSize = drwav__chunk_padding_size_riff(pHeaderOut->sizeInBytes);
*pRunningBytesReadOut += 8;
} else {
drwav_uint8 sizeInBytes[8];
if (onRead(pUserData, pHeaderOut->id.guid, 16) != 16) {
return DRWAV_AT_END;
}
if (onRead(pUserData, sizeInBytes, 8) != 8) {
return DRWAV_INVALID_FILE;
}
pHeaderOut->sizeInBytes = drwav__bytes_to_u64(sizeInBytes) - 24;
pHeaderOut->paddingSize = drwav__chunk_padding_size_w64(pHeaderOut->sizeInBytes);
*pRunningBytesReadOut += 24;
}
return DRWAV_SUCCESS;
}
static drwav_bool32 drwav__seek_forward(drwav_seek_proc onSeek, drwav_uint64 offset, void* pUserData)
{
drwav_uint64 bytesRemainingToSeek = offset;
while (bytesRemainingToSeek > 0) {
if (bytesRemainingToSeek > 0x7FFFFFFF) {
if (!onSeek(pUserData, 0x7FFFFFFF, drwav_seek_origin_current)) {
return DRWAV_FALSE;
}
bytesRemainingToSeek -= 0x7FFFFFFF;
} else {
if (!onSeek(pUserData, (int)bytesRemainingToSeek, drwav_seek_origin_current)) {
return DRWAV_FALSE;
}
bytesRemainingToSeek = 0;
}
}
return DRWAV_TRUE;
}
static drwav_bool32 drwav__seek_from_start(drwav_seek_proc onSeek, drwav_uint64 offset, void* pUserData)
{
if (offset <= 0x7FFFFFFF) {
return onSeek(pUserData, (int)offset, drwav_seek_origin_start);
}
if (!onSeek(pUserData, 0x7FFFFFFF, drwav_seek_origin_start)) {
return DRWAV_FALSE;
}
offset -= 0x7FFFFFFF;
for (;;) {
if (offset <= 0x7FFFFFFF) {
return onSeek(pUserData, (int)offset, drwav_seek_origin_current);
}
if (!onSeek(pUserData, 0x7FFFFFFF, drwav_seek_origin_current)) {
return DRWAV_FALSE;
}
offset -= 0x7FFFFFFF;
}
}
static drwav_bool32 drwav__read_fmt(drwav_read_proc onRead, drwav_seek_proc onSeek, void* pUserData, drwav_container container, drwav_uint64* pRunningBytesReadOut, drwav_fmt* fmtOut)
{
drwav_chunk_header header;
drwav_uint8 fmt[16];
if (drwav__read_chunk_header(onRead, pUserData, container, pRunningBytesReadOut, &header) != DRWAV_SUCCESS) {
return DRWAV_FALSE;
}
while (((container == drwav_container_riff || container == drwav_container_rf64) && !drwav__fourcc_equal(header.id.fourcc, "fmt ")) || (container == drwav_container_w64 && !drwav__guid_equal(header.id.guid, drwavGUID_W64_FMT))) {
if (!drwav__seek_forward(onSeek, header.sizeInBytes + header.paddingSize, pUserData)) {
return DRWAV_FALSE;
}
*pRunningBytesReadOut += header.sizeInBytes + header.paddingSize;
if (drwav__read_chunk_header(onRead, pUserData, container, pRunningBytesReadOut, &header) != DRWAV_SUCCESS) {
return DRWAV_FALSE;
}
}
if (container == drwav_container_riff || container == drwav_container_rf64) {
if (!drwav__fourcc_equal(header.id.fourcc, "fmt ")) {
return DRWAV_FALSE;
}
} else {
if (!drwav__guid_equal(header.id.guid, drwavGUID_W64_FMT)) {
return DRWAV_FALSE;
}
}
if (onRead(pUserData, fmt, sizeof(fmt)) != sizeof(fmt)) {
return DRWAV_FALSE;
}
*pRunningBytesReadOut += sizeof(fmt);
fmtOut->formatTag      = drwav__bytes_to_u16(fmt + 0);
fmtOut->channels       = drwav__bytes_to_u16(fmt + 2);
fmtOut->sampleRate     = drwav__bytes_to_u32(fmt + 4);
fmtOut->avgBytesPerSec = drwav__bytes_to_u32(fmt + 8);
fmtOut->blockAlign     = drwav__bytes_to_u16(fmt + 12);
fmtOut->bitsPerSample  = drwav__bytes_to_u16(fmt + 14);
fmtOut->extendedSize       = 0;
fmtOut->validBitsPerSample = 0;
fmtOut->channelMask        = 0;
memset(fmtOut->subFormat, 0, sizeof(fmtOut->subFormat));
if (header.sizeInBytes > 16) {
drwav_uint8 fmt_cbSize[2];
int bytesReadSoFar = 0;
if (onRead(pUserData, fmt_cbSize, sizeof(fmt_cbSize)) != sizeof(fmt_cbSize)) {
return DRWAV_FALSE;
}
*pRunningBytesReadOut += sizeof(fmt_cbSize);
bytesReadSoFar = 18;
fmtOut->extendedSize = drwav__bytes_to_u16(fmt_cbSize);
if (fmtOut->extendedSize > 0) {
if (fmtOut->formatTag == DR_WAVE_FORMAT_EXTENSIBLE) {
if (fmtOut->extendedSize != 22) {
return DRWAV_FALSE;
}
}
if (fmtOut->formatTag == DR_WAVE_FORMAT_EXTENSIBLE) {
drwav_uint8 fmtext[22];
if (onRead(pUserData, fmtext, fmtOut->extendedSize) != fmtOut->extendedSize) {
return DRWAV_FALSE;
}
fmtOut->validBitsPerSample = drwav__bytes_to_u16(fmtext + 0);
fmtOut->channelMask        = drwav__bytes_to_u32(fmtext + 2);
drwav__bytes_to_guid(fmtext + 6, fmtOut->subFormat);
} else {
if (!onSeek(pUserData, fmtOut->extendedSize, drwav_seek_origin_current)) {
return DRWAV_FALSE;
}
}
*pRunningBytesReadOut += fmtOut->extendedSize;
bytesReadSoFar += fmtOut->extendedSize;
}
if (!onSeek(pUserData, (int)(header.sizeInBytes - bytesReadSoFar), drwav_seek_origin_current)) {
return DRWAV_FALSE;
}
*pRunningBytesReadOut += (header.sizeInBytes - bytesReadSoFar);
}
if (header.paddingSize > 0) {
if (!onSeek(pUserData, header.paddingSize, drwav_seek_origin_current)) {
return DRWAV_FALSE;
}
*pRunningBytesReadOut += header.paddingSize;
}
return DRWAV_TRUE;
}
static size_t drwav__on_read(drwav_read_proc onRead, void* pUserData, void* pBufferOut, size_t bytesToRead, drwav_uint64* pCursor)
{
size_t bytesRead;
DRWAV_ASSERT(onRead != NULL);
DRWAV_ASSERT(pCursor != NULL);
bytesRead = onRead(pUserData, pBufferOut, bytesToRead);
*pCursor += bytesRead;
return bytesRead;
}
#if 0
static drwav_bool32 drwav__on_seek(drwav_seek_proc onSeek, void* pUserData, int offset, drwav_seek_origin origin, drwav_uint64* pCursor)
{
DRWAV_ASSERT(onSeek != NULL);
DRWAV_ASSERT(pCursor != NULL);
if (!onSeek(pUserData, offset, origin)) {
return DRWAV_FALSE;
}
if (origin == drwav_seek_origin_start) {
*pCursor = offset;
} else {
*pCursor += offset;
}
return DRWAV_TRUE;
}
#endif
static drwav_uint32 drwav_get_bytes_per_pcm_frame(drwav* pWav)
{
if ((pWav->bitsPerSample & 0x7) == 0) {
return (pWav->bitsPerSample * pWav->fmt.channels) >> 3;
} else {
return pWav->fmt.blockAlign;
}
}
DRWAV_API drwav_uint16 drwav_fmt_get_format(const drwav_fmt* pFMT)
{
if (pFMT == NULL) {
return 0;
}
if (pFMT->formatTag != DR_WAVE_FORMAT_EXTENSIBLE) {
return pFMT->formatTag;
} else {
return drwav__bytes_to_u16(pFMT->subFormat);
}
}
static drwav_bool32 drwav_preinit(drwav* pWav, drwav_read_proc onRead, drwav_seek_proc onSeek, void* pReadSeekUserData, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (pWav == NULL || onRead == NULL || onSeek == NULL) {
return DRWAV_FALSE;
}
DRWAV_ZERO_MEMORY(pWav, sizeof(*pWav));
pWav->onRead    = onRead;
pWav->onSeek    = onSeek;
pWav->pUserData = pReadSeekUserData;
pWav->allocationCallbacks = drwav_copy_allocation_callbacks_or_defaults(pAllocationCallbacks);
if (pWav->allocationCallbacks.onFree == NULL || (pWav->allocationCallbacks.onMalloc == NULL && pWav->allocationCallbacks.onRealloc == NULL)) {
return DRWAV_FALSE;
}
return DRWAV_TRUE;
}
static drwav_bool32 drwav_init__internal(drwav* pWav, drwav_chunk_proc onChunk, void* pChunkUserData, drwav_uint32 flags)
{
drwav_uint64 cursor;
drwav_bool32 sequential;
drwav_uint8 riff[4];
drwav_fmt fmt;
unsigned short translatedFormatTag;
drwav_bool32 foundDataChunk;
drwav_uint64 dataChunkSize = 0;
drwav_uint64 sampleCountFromFactChunk = 0;
drwav_uint64 chunkSize;
cursor = 0;
sequential = (flags & DRWAV_SEQUENTIAL) != 0;
if (drwav__on_read(pWav->onRead, pWav->pUserData, riff, sizeof(riff), &cursor) != sizeof(riff)) {
return DRWAV_FALSE;
}
if (drwav__fourcc_equal(riff, "RIFF")) {
pWav->container = drwav_container_riff;
} else if (drwav__fourcc_equal(riff, "riff")) {
int i;
drwav_uint8 riff2[12];
pWav->container = drwav_container_w64;
if (drwav__on_read(pWav->onRead, pWav->pUserData, riff2, sizeof(riff2), &cursor) != sizeof(riff2)) {
return DRWAV_FALSE;
}
for (i = 0; i < 12; ++i) {
if (riff2[i] != drwavGUID_W64_RIFF[i+4]) {
return DRWAV_FALSE;
}
}
} else if (drwav__fourcc_equal(riff, "RF64")) {
pWav->container = drwav_container_rf64;
} else {
return DRWAV_FALSE;
}
if (pWav->container == drwav_container_riff || pWav->container == drwav_container_rf64) {
drwav_uint8 chunkSizeBytes[4];
drwav_uint8 wave[4];
if (drwav__on_read(pWav->onRead, pWav->pUserData, chunkSizeBytes, sizeof(chunkSizeBytes), &cursor) != sizeof(chunkSizeBytes)) {
return DRWAV_FALSE;
}
if (pWav->container == drwav_container_riff) {
if (drwav__bytes_to_u32(chunkSizeBytes) < 36) {
return DRWAV_FALSE;
}
} else {
if (drwav__bytes_to_u32(chunkSizeBytes) != 0xFFFFFFFF) {
return DRWAV_FALSE;
}
}
if (drwav__on_read(pWav->onRead, pWav->pUserData, wave, sizeof(wave), &cursor) != sizeof(wave)) {
return DRWAV_FALSE;
}
if (!drwav__fourcc_equal(wave, "WAVE")) {
return DRWAV_FALSE;
}
} else {
drwav_uint8 chunkSizeBytes[8];
drwav_uint8 wave[16];
if (drwav__on_read(pWav->onRead, pWav->pUserData, chunkSizeBytes, sizeof(chunkSizeBytes), &cursor) != sizeof(chunkSizeBytes)) {
return DRWAV_FALSE;
}
if (drwav__bytes_to_u64(chunkSizeBytes) < 80) {
return DRWAV_FALSE;
}
if (drwav__on_read(pWav->onRead, pWav->pUserData, wave, sizeof(wave), &cursor) != sizeof(wave)) {
return DRWAV_FALSE;
}
if (!drwav__guid_equal(wave, drwavGUID_W64_WAVE)) {
return DRWAV_FALSE;
}
}
if (pWav->container == drwav_container_rf64) {
drwav_uint8 sizeBytes[8];
drwav_uint64 bytesRemainingInChunk;
drwav_chunk_header header;
drwav_result result = drwav__read_chunk_header(pWav->onRead, pWav->pUserData, pWav->container, &cursor, &header);
if (result != DRWAV_SUCCESS) {
return DRWAV_FALSE;
}
if (!drwav__fourcc_equal(header.id.fourcc, "ds64")) {
return DRWAV_FALSE;
}
bytesRemainingInChunk = header.sizeInBytes + header.paddingSize;
if (!drwav__seek_forward(pWav->onSeek, 8, pWav->pUserData)) {
return DRWAV_FALSE;
}
bytesRemainingInChunk -= 8;
cursor += 8;
if (drwav__on_read(pWav->onRead, pWav->pUserData, sizeBytes, sizeof(sizeBytes), &cursor) != sizeof(sizeBytes)) {
return DRWAV_FALSE;
}
bytesRemainingInChunk -= 8;
dataChunkSize = drwav__bytes_to_u64(sizeBytes);
if (drwav__on_read(pWav->onRead, pWav->pUserData, sizeBytes, sizeof(sizeBytes), &cursor) != sizeof(sizeBytes)) {
return DRWAV_FALSE;
}
bytesRemainingInChunk -= 8;
sampleCountFromFactChunk = drwav__bytes_to_u64(sizeBytes);
if (!drwav__seek_forward(pWav->onSeek, bytesRemainingInChunk, pWav->pUserData)) {
return DRWAV_FALSE;
}
cursor += bytesRemainingInChunk;
}
if (!drwav__read_fmt(pWav->onRead, pWav->onSeek, pWav->pUserData, pWav->container, &cursor, &fmt)) {
return DRWAV_FALSE;
}
if ((fmt.sampleRate    == 0 || fmt.sampleRate    > DRWAV_MAX_SAMPLE_RATE)     ||
(fmt.channels      == 0 || fmt.channels      > DRWAV_MAX_CHANNELS)        ||
(fmt.bitsPerSample == 0 || fmt.bitsPerSample > DRWAV_MAX_BITS_PER_SAMPLE) ||
fmt.blockAlign == 0) {
return DRWAV_FALSE;
}
translatedFormatTag = fmt.formatTag;
if (translatedFormatTag == DR_WAVE_FORMAT_EXTENSIBLE) {
translatedFormatTag = drwav__bytes_to_u16(fmt.subFormat + 0);
}
foundDataChunk = DRWAV_FALSE;
for (;;)
{
drwav_chunk_header header;
drwav_result result = drwav__read_chunk_header(pWav->onRead, pWav->pUserData, pWav->container, &cursor, &header);
if (result != DRWAV_SUCCESS) {
if (!foundDataChunk) {
return DRWAV_FALSE;
} else {
break;
}
}
if (!sequential && onChunk != NULL) {
drwav_uint64 callbackBytesRead = onChunk(pChunkUserData, pWav->onRead, pWav->onSeek, pWav->pUserData, &header, pWav->container, &fmt);
if (callbackBytesRead > 0) {
if (!drwav__seek_from_start(pWav->onSeek, cursor, pWav->pUserData)) {
return DRWAV_FALSE;
}
}
}
if (!foundDataChunk) {
pWav->dataChunkDataPos = cursor;
}
chunkSize = header.sizeInBytes;
if (pWav->container == drwav_container_riff || pWav->container == drwav_container_rf64) {
if (drwav__fourcc_equal(header.id.fourcc, "data")) {
foundDataChunk = DRWAV_TRUE;
if (pWav->container != drwav_container_rf64) {
dataChunkSize = chunkSize;
}
}
} else {
if (drwav__guid_equal(header.id.guid, drwavGUID_W64_DATA)) {
foundDataChunk = DRWAV_TRUE;
dataChunkSize = chunkSize;
}
}
if (foundDataChunk && sequential) {
break;
}
if (pWav->container == drwav_container_riff) {
if (drwav__fourcc_equal(header.id.fourcc, "fact")) {
drwav_uint32 sampleCount;
if (drwav__on_read(pWav->onRead, pWav->pUserData, &sampleCount, 4, &cursor) != 4) {
return DRWAV_FALSE;
}
chunkSize -= 4;
if (!foundDataChunk) {
pWav->dataChunkDataPos = cursor;
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_ADPCM) {
sampleCountFromFactChunk = sampleCount;
} else {
sampleCountFromFactChunk = 0;
}
}
} else if (pWav->container == drwav_container_w64) {
if (drwav__guid_equal(header.id.guid, drwavGUID_W64_FACT)) {
if (drwav__on_read(pWav->onRead, pWav->pUserData, &sampleCountFromFactChunk, 8, &cursor) != 8) {
return DRWAV_FALSE;
}
chunkSize -= 8;
if (!foundDataChunk) {
pWav->dataChunkDataPos = cursor;
}
}
} else if (pWav->container == drwav_container_rf64) {
}
if (pWav->container == drwav_container_riff || pWav->container == drwav_container_rf64) {
if (drwav__fourcc_equal(header.id.fourcc, "smpl")) {
drwav_uint8 smplHeaderData[36];
if (chunkSize >= sizeof(smplHeaderData)) {
drwav_uint64 bytesJustRead = drwav__on_read(pWav->onRead, pWav->pUserData, smplHeaderData, sizeof(smplHeaderData), &cursor);
chunkSize -= bytesJustRead;
if (bytesJustRead == sizeof(smplHeaderData)) {
drwav_uint32 iLoop;
pWav->smpl.manufacturer      = drwav__bytes_to_u32(smplHeaderData+0);
pWav->smpl.product           = drwav__bytes_to_u32(smplHeaderData+4);
pWav->smpl.samplePeriod      = drwav__bytes_to_u32(smplHeaderData+8);
pWav->smpl.midiUnityNotes    = drwav__bytes_to_u32(smplHeaderData+12);
pWav->smpl.midiPitchFraction = drwav__bytes_to_u32(smplHeaderData+16);
pWav->smpl.smpteFormat       = drwav__bytes_to_u32(smplHeaderData+20);
pWav->smpl.smpteOffset       = drwav__bytes_to_u32(smplHeaderData+24);
pWav->smpl.numSampleLoops    = drwav__bytes_to_u32(smplHeaderData+28);
pWav->smpl.samplerData       = drwav__bytes_to_u32(smplHeaderData+32);
for (iLoop = 0; iLoop < pWav->smpl.numSampleLoops && iLoop < drwav_countof(pWav->smpl.loops); ++iLoop) {
drwav_uint8 smplLoopData[24];
bytesJustRead = drwav__on_read(pWav->onRead, pWav->pUserData, smplLoopData, sizeof(smplLoopData), &cursor);
chunkSize -= bytesJustRead;
if (bytesJustRead == sizeof(smplLoopData)) {
pWav->smpl.loops[iLoop].cuePointId = drwav__bytes_to_u32(smplLoopData+0);
pWav->smpl.loops[iLoop].type       = drwav__bytes_to_u32(smplLoopData+4);
pWav->smpl.loops[iLoop].start      = drwav__bytes_to_u32(smplLoopData+8);
pWav->smpl.loops[iLoop].end        = drwav__bytes_to_u32(smplLoopData+12);
pWav->smpl.loops[iLoop].fraction   = drwav__bytes_to_u32(smplLoopData+16);
pWav->smpl.loops[iLoop].playCount  = drwav__bytes_to_u32(smplLoopData+20);
} else {
break;
}
}
}
} else {
}
}
} else {
if (drwav__guid_equal(header.id.guid, drwavGUID_W64_SMPL)) {
}
}
chunkSize += header.paddingSize;
if (!drwav__seek_forward(pWav->onSeek, chunkSize, pWav->pUserData)) {
break;
}
cursor += chunkSize;
if (!foundDataChunk) {
pWav->dataChunkDataPos = cursor;
}
}
if (!foundDataChunk) {
return DRWAV_FALSE;
}
if (!sequential) {
if (!drwav__seek_from_start(pWav->onSeek, pWav->dataChunkDataPos, pWav->pUserData)) {
return DRWAV_FALSE;
}
cursor = pWav->dataChunkDataPos;
}
pWav->fmt                 = fmt;
pWav->sampleRate          = fmt.sampleRate;
pWav->channels            = fmt.channels;
pWav->bitsPerSample       = fmt.bitsPerSample;
pWav->bytesRemaining      = dataChunkSize;
pWav->translatedFormatTag = translatedFormatTag;
pWav->dataChunkDataSize   = dataChunkSize;
if (sampleCountFromFactChunk != 0) {
pWav->totalPCMFrameCount = sampleCountFromFactChunk;
} else {
pWav->totalPCMFrameCount = dataChunkSize / drwav_get_bytes_per_pcm_frame(pWav);
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_ADPCM) {
drwav_uint64 totalBlockHeaderSizeInBytes;
drwav_uint64 blockCount = dataChunkSize / fmt.blockAlign;
if ((blockCount * fmt.blockAlign) < dataChunkSize) {
blockCount += 1;
}
totalBlockHeaderSizeInBytes = blockCount * (6*fmt.channels);
pWav->totalPCMFrameCount = ((dataChunkSize - totalBlockHeaderSizeInBytes) * 2) / fmt.channels;
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_DVI_ADPCM) {
drwav_uint64 totalBlockHeaderSizeInBytes;
drwav_uint64 blockCount = dataChunkSize / fmt.blockAlign;
if ((blockCount * fmt.blockAlign) < dataChunkSize) {
blockCount += 1;
}
totalBlockHeaderSizeInBytes = blockCount * (4*fmt.channels);
pWav->totalPCMFrameCount = ((dataChunkSize - totalBlockHeaderSizeInBytes) * 2) / fmt.channels;
pWav->totalPCMFrameCount += blockCount;
}
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_ADPCM || pWav->translatedFormatTag == DR_WAVE_FORMAT_DVI_ADPCM) {
if (pWav->channels > 2) {
return DRWAV_FALSE;
}
}
#ifdef DR_WAV_LIBSNDFILE_COMPAT
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_ADPCM) {
drwav_uint64 blockCount = dataChunkSize / fmt.blockAlign;
pWav->totalPCMFrameCount = (((blockCount * (fmt.blockAlign - (6*pWav->channels))) * 2)) / fmt.channels;
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_DVI_ADPCM) {
drwav_uint64 blockCount = dataChunkSize / fmt.blockAlign;
pWav->totalPCMFrameCount = (((blockCount * (fmt.blockAlign - (4*pWav->channels))) * 2) + (blockCount * pWav->channels)) / fmt.channels;
}
#endif
return DRWAV_TRUE;
}
DRWAV_API drwav_bool32 drwav_init(drwav* pWav, drwav_read_proc onRead, drwav_seek_proc onSeek, void* pUserData, const drwav_allocation_callbacks* pAllocationCallbacks)
{
return drwav_init_ex(pWav, onRead, onSeek, NULL, pUserData, NULL, 0, pAllocationCallbacks);
}
DRWAV_API drwav_bool32 drwav_init_ex(drwav* pWav, drwav_read_proc onRead, drwav_seek_proc onSeek, drwav_chunk_proc onChunk, void* pReadSeekUserData, void* pChunkUserData, drwav_uint32 flags, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (!drwav_preinit(pWav, onRead, onSeek, pReadSeekUserData, pAllocationCallbacks)) {
return DRWAV_FALSE;
}
return drwav_init__internal(pWav, onChunk, pChunkUserData, flags);
}
static drwav_uint32 drwav__riff_chunk_size_riff(drwav_uint64 dataChunkSize)
{
drwav_uint64 chunkSize = 4 + 24 + dataChunkSize + drwav__chunk_padding_size_riff(dataChunkSize);
if (chunkSize > 0xFFFFFFFFUL) {
chunkSize = 0xFFFFFFFFUL;
}
return (drwav_uint32)chunkSize;
}
static drwav_uint32 drwav__data_chunk_size_riff(drwav_uint64 dataChunkSize)
{
if (dataChunkSize <= 0xFFFFFFFFUL) {
return (drwav_uint32)dataChunkSize;
} else {
return 0xFFFFFFFFUL;
}
}
static drwav_uint64 drwav__riff_chunk_size_w64(drwav_uint64 dataChunkSize)
{
drwav_uint64 dataSubchunkPaddingSize = drwav__chunk_padding_size_w64(dataChunkSize);
return 80 + 24 + dataChunkSize + dataSubchunkPaddingSize;
}
static drwav_uint64 drwav__data_chunk_size_w64(drwav_uint64 dataChunkSize)
{
return 24 + dataChunkSize;
}
static drwav_uint64 drwav__riff_chunk_size_rf64(drwav_uint64 dataChunkSize)
{
drwav_uint64 chunkSize = 4 + 36 + 24 + dataChunkSize + drwav__chunk_padding_size_riff(dataChunkSize);
if (chunkSize > 0xFFFFFFFFUL) {
chunkSize = 0xFFFFFFFFUL;
}
return chunkSize;
}
static drwav_uint64 drwav__data_chunk_size_rf64(drwav_uint64 dataChunkSize)
{
return dataChunkSize;
}
static size_t drwav__write(drwav* pWav, const void* pData, size_t dataSize)
{
DRWAV_ASSERT(pWav          != NULL);
DRWAV_ASSERT(pWav->onWrite != NULL);
return pWav->onWrite(pWav->pUserData, pData, dataSize);
}
static size_t drwav__write_u16ne_to_le(drwav* pWav, drwav_uint16 value)
{
DRWAV_ASSERT(pWav          != NULL);
DRWAV_ASSERT(pWav->onWrite != NULL);
if (!drwav__is_little_endian()) {
value = drwav__bswap16(value);
}
return drwav__write(pWav, &value, 2);
}
static size_t drwav__write_u32ne_to_le(drwav* pWav, drwav_uint32 value)
{
DRWAV_ASSERT(pWav          != NULL);
DRWAV_ASSERT(pWav->onWrite != NULL);
if (!drwav__is_little_endian()) {
value = drwav__bswap32(value);
}
return drwav__write(pWav, &value, 4);
}
static size_t drwav__write_u64ne_to_le(drwav* pWav, drwav_uint64 value)
{
DRWAV_ASSERT(pWav          != NULL);
DRWAV_ASSERT(pWav->onWrite != NULL);
if (!drwav__is_little_endian()) {
value = drwav__bswap64(value);
}
return drwav__write(pWav, &value, 8);
}
static drwav_bool32 drwav_preinit_write(drwav* pWav, const drwav_data_format* pFormat, drwav_bool32 isSequential, drwav_write_proc onWrite, drwav_seek_proc onSeek, void* pUserData, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (pWav == NULL || onWrite == NULL) {
return DRWAV_FALSE;
}
if (!isSequential && onSeek == NULL) {
return DRWAV_FALSE;
}
if (pFormat->format == DR_WAVE_FORMAT_EXTENSIBLE) {
return DRWAV_FALSE;
}
if (pFormat->format == DR_WAVE_FORMAT_ADPCM || pFormat->format == DR_WAVE_FORMAT_DVI_ADPCM) {
return DRWAV_FALSE;
}
DRWAV_ZERO_MEMORY(pWav, sizeof(*pWav));
pWav->onWrite   = onWrite;
pWav->onSeek    = onSeek;
pWav->pUserData = pUserData;
pWav->allocationCallbacks = drwav_copy_allocation_callbacks_or_defaults(pAllocationCallbacks);
if (pWav->allocationCallbacks.onFree == NULL || (pWav->allocationCallbacks.onMalloc == NULL && pWav->allocationCallbacks.onRealloc == NULL)) {
return DRWAV_FALSE;
}
pWav->fmt.formatTag = (drwav_uint16)pFormat->format;
pWav->fmt.channels = (drwav_uint16)pFormat->channels;
pWav->fmt.sampleRate = pFormat->sampleRate;
pWav->fmt.avgBytesPerSec = (drwav_uint32)((pFormat->bitsPerSample * pFormat->sampleRate * pFormat->channels) / 8);
pWav->fmt.blockAlign = (drwav_uint16)((pFormat->channels * pFormat->bitsPerSample) / 8);
pWav->fmt.bitsPerSample = (drwav_uint16)pFormat->bitsPerSample;
pWav->fmt.extendedSize = 0;
pWav->isSequentialWrite = isSequential;
return DRWAV_TRUE;
}
static drwav_bool32 drwav_init_write__internal(drwav* pWav, const drwav_data_format* pFormat, drwav_uint64 totalSampleCount)
{
size_t runningPos = 0;
drwav_uint64 initialDataChunkSize = 0;
drwav_uint64 chunkSizeFMT;
if (pWav->isSequentialWrite) {
initialDataChunkSize = (totalSampleCount * pWav->fmt.bitsPerSample) / 8;
if (pFormat->container == drwav_container_riff) {
if (initialDataChunkSize > (0xFFFFFFFFUL - 36)) {
return DRWAV_FALSE;
}
}
}
pWav->dataChunkDataSizeTargetWrite = initialDataChunkSize;
if (pFormat->container == drwav_container_riff) {
drwav_uint32 chunkSizeRIFF = 28 + (drwav_uint32)initialDataChunkSize;
runningPos += drwav__write(pWav, "RIFF", 4);
runningPos += drwav__write_u32ne_to_le(pWav, chunkSizeRIFF);
runningPos += drwav__write(pWav, "WAVE", 4);
} else if (pFormat->container == drwav_container_w64) {
drwav_uint64 chunkSizeRIFF = 80 + 24 + initialDataChunkSize;
runningPos += drwav__write(pWav, drwavGUID_W64_RIFF, 16);
runningPos += drwav__write_u64ne_to_le(pWav, chunkSizeRIFF);
runningPos += drwav__write(pWav, drwavGUID_W64_WAVE, 16);
} else if (pFormat->container == drwav_container_rf64) {
runningPos += drwav__write(pWav, "RF64", 4);
runningPos += drwav__write_u32ne_to_le(pWav, 0xFFFFFFFF);
runningPos += drwav__write(pWav, "WAVE", 4);
}
if (pFormat->container == drwav_container_rf64) {
drwav_uint32 initialds64ChunkSize = 28;
drwav_uint64 initialRiffChunkSize = 8 + initialds64ChunkSize + initialDataChunkSize;
runningPos += drwav__write(pWav, "ds64", 4);
runningPos += drwav__write_u32ne_to_le(pWav, initialds64ChunkSize);
runningPos += drwav__write_u64ne_to_le(pWav, initialRiffChunkSize);
runningPos += drwav__write_u64ne_to_le(pWav, initialDataChunkSize);
runningPos += drwav__write_u64ne_to_le(pWav, totalSampleCount);
runningPos += drwav__write_u32ne_to_le(pWav, 0);
}
if (pFormat->container == drwav_container_riff || pFormat->container == drwav_container_rf64) {
chunkSizeFMT = 16;
runningPos += drwav__write(pWav, "fmt ", 4);
runningPos += drwav__write_u32ne_to_le(pWav, (drwav_uint32)chunkSizeFMT);
} else if (pFormat->container == drwav_container_w64) {
chunkSizeFMT = 40;
runningPos += drwav__write(pWav, drwavGUID_W64_FMT, 16);
runningPos += drwav__write_u64ne_to_le(pWav, chunkSizeFMT);
}
runningPos += drwav__write_u16ne_to_le(pWav, pWav->fmt.formatTag);
runningPos += drwav__write_u16ne_to_le(pWav, pWav->fmt.channels);
runningPos += drwav__write_u32ne_to_le(pWav, pWav->fmt.sampleRate);
runningPos += drwav__write_u32ne_to_le(pWav, pWav->fmt.avgBytesPerSec);
runningPos += drwav__write_u16ne_to_le(pWav, pWav->fmt.blockAlign);
runningPos += drwav__write_u16ne_to_le(pWav, pWav->fmt.bitsPerSample);
pWav->dataChunkDataPos = runningPos;
if (pFormat->container == drwav_container_riff) {
drwav_uint32 chunkSizeDATA = (drwav_uint32)initialDataChunkSize;
runningPos += drwav__write(pWav, "data", 4);
runningPos += drwav__write_u32ne_to_le(pWav, chunkSizeDATA);
} else if (pFormat->container == drwav_container_w64) {
drwav_uint64 chunkSizeDATA = 24 + initialDataChunkSize;
runningPos += drwav__write(pWav, drwavGUID_W64_DATA, 16);
runningPos += drwav__write_u64ne_to_le(pWav, chunkSizeDATA);
} else if (pFormat->container == drwav_container_rf64) {
runningPos += drwav__write(pWav, "data", 4);
runningPos += drwav__write_u32ne_to_le(pWav, 0xFFFFFFFF);
}
(void)runningPos;
pWav->container = pFormat->container;
pWav->channels = (drwav_uint16)pFormat->channels;
pWav->sampleRate = pFormat->sampleRate;
pWav->bitsPerSample = (drwav_uint16)pFormat->bitsPerSample;
pWav->translatedFormatTag = (drwav_uint16)pFormat->format;
return DRWAV_TRUE;
}
DRWAV_API drwav_bool32 drwav_init_write(drwav* pWav, const drwav_data_format* pFormat, drwav_write_proc onWrite, drwav_seek_proc onSeek, void* pUserData, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (!drwav_preinit_write(pWav, pFormat, DRWAV_FALSE, onWrite, onSeek, pUserData, pAllocationCallbacks)) {
return DRWAV_FALSE;
}
return drwav_init_write__internal(pWav, pFormat, 0);
}
DRWAV_API drwav_bool32 drwav_init_write_sequential(drwav* pWav, const drwav_data_format* pFormat, drwav_uint64 totalSampleCount, drwav_write_proc onWrite, void* pUserData, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (!drwav_preinit_write(pWav, pFormat, DRWAV_TRUE, onWrite, NULL, pUserData, pAllocationCallbacks)) {
return DRWAV_FALSE;
}
return drwav_init_write__internal(pWav, pFormat, totalSampleCount);
}
DRWAV_API drwav_bool32 drwav_init_write_sequential_pcm_frames(drwav* pWav, const drwav_data_format* pFormat, drwav_uint64 totalPCMFrameCount, drwav_write_proc onWrite, void* pUserData, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (pFormat == NULL) {
return DRWAV_FALSE;
}
return drwav_init_write_sequential(pWav, pFormat, totalPCMFrameCount*pFormat->channels, onWrite, pUserData, pAllocationCallbacks);
}
DRWAV_API drwav_uint64 drwav_target_write_size_bytes(const drwav_data_format* pFormat, drwav_uint64 totalSampleCount)
{
drwav_uint64 targetDataSizeBytes = (drwav_uint64)((drwav_int64)totalSampleCount * pFormat->channels * pFormat->bitsPerSample/8.0);
drwav_uint64 riffChunkSizeBytes;
drwav_uint64 fileSizeBytes = 0;
if (pFormat->container == drwav_container_riff) {
riffChunkSizeBytes = drwav__riff_chunk_size_riff(targetDataSizeBytes);
fileSizeBytes = (8 + riffChunkSizeBytes);
} else if (pFormat->container == drwav_container_w64) {
riffChunkSizeBytes = drwav__riff_chunk_size_w64(targetDataSizeBytes);
fileSizeBytes = riffChunkSizeBytes;
} else if (pFormat->container == drwav_container_rf64) {
riffChunkSizeBytes = drwav__riff_chunk_size_rf64(targetDataSizeBytes);
fileSizeBytes = (8 + riffChunkSizeBytes);
}
return fileSizeBytes;
}
#ifndef DR_WAV_NO_STDIO
#include <errno.h>
static drwav_result drwav_result_from_errno(int e)
{
switch (e)
{
case 0: return DRWAV_SUCCESS;
#ifdef EPERM
case EPERM: return DRWAV_INVALID_OPERATION;
#endif
#ifdef ENOENT
case ENOENT: return DRWAV_DOES_NOT_EXIST;
#endif
#ifdef ESRCH
case ESRCH: return DRWAV_DOES_NOT_EXIST;
#endif
#ifdef EINTR
case EINTR: return DRWAV_INTERRUPT;
#endif
#ifdef EIO
case EIO: return DRWAV_IO_ERROR;
#endif
#ifdef ENXIO
case ENXIO: return DRWAV_DOES_NOT_EXIST;
#endif
#ifdef E2BIG
case E2BIG: return DRWAV_INVALID_ARGS;
#endif
#ifdef ENOEXEC
case ENOEXEC: return DRWAV_INVALID_FILE;
#endif
#ifdef EBADF
case EBADF: return DRWAV_INVALID_FILE;
#endif
#ifdef ECHILD
case ECHILD: return DRWAV_ERROR;
#endif
#ifdef EAGAIN
case EAGAIN: return DRWAV_UNAVAILABLE;
#endif
#ifdef ENOMEM
case ENOMEM: return DRWAV_OUT_OF_MEMORY;
#endif
#ifdef EACCES
case EACCES: return DRWAV_ACCESS_DENIED;
#endif
#ifdef EFAULT
case EFAULT: return DRWAV_BAD_ADDRESS;
#endif
#ifdef ENOTBLK
case ENOTBLK: return DRWAV_ERROR;
#endif
#ifdef EBUSY
case EBUSY: return DRWAV_BUSY;
#endif
#ifdef EEXIST
case EEXIST: return DRWAV_ALREADY_EXISTS;
#endif
#ifdef EXDEV
case EXDEV: return DRWAV_ERROR;
#endif
#ifdef ENODEV
case ENODEV: return DRWAV_DOES_NOT_EXIST;
#endif
#ifdef ENOTDIR
case ENOTDIR: return DRWAV_NOT_DIRECTORY;
#endif
#ifdef EISDIR
case EISDIR: return DRWAV_IS_DIRECTORY;
#endif
#ifdef EINVAL
case EINVAL: return DRWAV_INVALID_ARGS;
#endif
#ifdef ENFILE
case ENFILE: return DRWAV_TOO_MANY_OPEN_FILES;
#endif
#ifdef EMFILE
case EMFILE: return DRWAV_TOO_MANY_OPEN_FILES;
#endif
#ifdef ENOTTY
case ENOTTY: return DRWAV_INVALID_OPERATION;
#endif
#ifdef ETXTBSY
case ETXTBSY: return DRWAV_BUSY;
#endif
#ifdef EFBIG
case EFBIG: return DRWAV_TOO_BIG;
#endif
#ifdef ENOSPC
case ENOSPC: return DRWAV_NO_SPACE;
#endif
#ifdef ESPIPE
case ESPIPE: return DRWAV_BAD_SEEK;
#endif
#ifdef EROFS
case EROFS: return DRWAV_ACCESS_DENIED;
#endif
#ifdef EMLINK
case EMLINK: return DRWAV_TOO_MANY_LINKS;
#endif
#ifdef EPIPE
case EPIPE: return DRWAV_BAD_PIPE;
#endif
#ifdef EDOM
case EDOM: return DRWAV_OUT_OF_RANGE;
#endif
#ifdef ERANGE
case ERANGE: return DRWAV_OUT_OF_RANGE;
#endif
#ifdef EDEADLK
case EDEADLK: return DRWAV_DEADLOCK;
#endif
#ifdef ENAMETOOLONG
case ENAMETOOLONG: return DRWAV_PATH_TOO_LONG;
#endif
#ifdef ENOLCK
case ENOLCK: return DRWAV_ERROR;
#endif
#ifdef ENOSYS
case ENOSYS: return DRWAV_NOT_IMPLEMENTED;
#endif
#ifdef ENOTEMPTY
case ENOTEMPTY: return DRWAV_DIRECTORY_NOT_EMPTY;
#endif
#ifdef ELOOP
case ELOOP: return DRWAV_TOO_MANY_LINKS;
#endif
#ifdef ENOMSG
case ENOMSG: return DRWAV_NO_MESSAGE;
#endif
#ifdef EIDRM
case EIDRM: return DRWAV_ERROR;
#endif
#ifdef ECHRNG
case ECHRNG: return DRWAV_ERROR;
#endif
#ifdef EL2NSYNC
case EL2NSYNC: return DRWAV_ERROR;
#endif
#ifdef EL3HLT
case EL3HLT: return DRWAV_ERROR;
#endif
#ifdef EL3RST
case EL3RST: return DRWAV_ERROR;
#endif
#ifdef ELNRNG
case ELNRNG: return DRWAV_OUT_OF_RANGE;
#endif
#ifdef EUNATCH
case EUNATCH: return DRWAV_ERROR;
#endif
#ifdef ENOCSI
case ENOCSI: return DRWAV_ERROR;
#endif
#ifdef EL2HLT
case EL2HLT: return DRWAV_ERROR;
#endif
#ifdef EBADE
case EBADE: return DRWAV_ERROR;
#endif
#ifdef EBADR
case EBADR: return DRWAV_ERROR;
#endif
#ifdef EXFULL
case EXFULL: return DRWAV_ERROR;
#endif
#ifdef ENOANO
case ENOANO: return DRWAV_ERROR;
#endif
#ifdef EBADRQC
case EBADRQC: return DRWAV_ERROR;
#endif
#ifdef EBADSLT
case EBADSLT: return DRWAV_ERROR;
#endif
#ifdef EBFONT
case EBFONT: return DRWAV_INVALID_FILE;
#endif
#ifdef ENOSTR
case ENOSTR: return DRWAV_ERROR;
#endif
#ifdef ENODATA
case ENODATA: return DRWAV_NO_DATA_AVAILABLE;
#endif
#ifdef ETIME
case ETIME: return DRWAV_TIMEOUT;
#endif
#ifdef ENOSR
case ENOSR: return DRWAV_NO_DATA_AVAILABLE;
#endif
#ifdef ENONET
case ENONET: return DRWAV_NO_NETWORK;
#endif
#ifdef ENOPKG
case ENOPKG: return DRWAV_ERROR;
#endif
#ifdef EREMOTE
case EREMOTE: return DRWAV_ERROR;
#endif
#ifdef ENOLINK
case ENOLINK: return DRWAV_ERROR;
#endif
#ifdef EADV
case EADV: return DRWAV_ERROR;
#endif
#ifdef ESRMNT
case ESRMNT: return DRWAV_ERROR;
#endif
#ifdef ECOMM
case ECOMM: return DRWAV_ERROR;
#endif
#ifdef EPROTO
case EPROTO: return DRWAV_ERROR;
#endif
#ifdef EMULTIHOP
case EMULTIHOP: return DRWAV_ERROR;
#endif
#ifdef EDOTDOT
case EDOTDOT: return DRWAV_ERROR;
#endif
#ifdef EBADMSG
case EBADMSG: return DRWAV_BAD_MESSAGE;
#endif
#ifdef EOVERFLOW
case EOVERFLOW: return DRWAV_TOO_BIG;
#endif
#ifdef ENOTUNIQ
case ENOTUNIQ: return DRWAV_NOT_UNIQUE;
#endif
#ifdef EBADFD
case EBADFD: return DRWAV_ERROR;
#endif
#ifdef EREMCHG
case EREMCHG: return DRWAV_ERROR;
#endif
#ifdef ELIBACC
case ELIBACC: return DRWAV_ACCESS_DENIED;
#endif
#ifdef ELIBBAD
case ELIBBAD: return DRWAV_INVALID_FILE;
#endif
#ifdef ELIBSCN
case ELIBSCN: return DRWAV_INVALID_FILE;
#endif
#ifdef ELIBMAX
case ELIBMAX: return DRWAV_ERROR;
#endif
#ifdef ELIBEXEC
case ELIBEXEC: return DRWAV_ERROR;
#endif
#ifdef EILSEQ
case EILSEQ: return DRWAV_INVALID_DATA;
#endif
#ifdef ERESTART
case ERESTART: return DRWAV_ERROR;
#endif
#ifdef ESTRPIPE
case ESTRPIPE: return DRWAV_ERROR;
#endif
#ifdef EUSERS
case EUSERS: return DRWAV_ERROR;
#endif
#ifdef ENOTSOCK
case ENOTSOCK: return DRWAV_NOT_SOCKET;
#endif
#ifdef EDESTADDRREQ
case EDESTADDRREQ: return DRWAV_NO_ADDRESS;
#endif
#ifdef EMSGSIZE
case EMSGSIZE: return DRWAV_TOO_BIG;
#endif
#ifdef EPROTOTYPE
case EPROTOTYPE: return DRWAV_BAD_PROTOCOL;
#endif
#ifdef ENOPROTOOPT
case ENOPROTOOPT: return DRWAV_PROTOCOL_UNAVAILABLE;
#endif
#ifdef EPROTONOSUPPORT
case EPROTONOSUPPORT: return DRWAV_PROTOCOL_NOT_SUPPORTED;
#endif
#ifdef ESOCKTNOSUPPORT
case ESOCKTNOSUPPORT: return DRWAV_SOCKET_NOT_SUPPORTED;
#endif
#ifdef EOPNOTSUPP
case EOPNOTSUPP: return DRWAV_INVALID_OPERATION;
#endif
#ifdef EPFNOSUPPORT
case EPFNOSUPPORT: return DRWAV_PROTOCOL_FAMILY_NOT_SUPPORTED;
#endif
#ifdef EAFNOSUPPORT
case EAFNOSUPPORT: return DRWAV_ADDRESS_FAMILY_NOT_SUPPORTED;
#endif
#ifdef EADDRINUSE
case EADDRINUSE: return DRWAV_ALREADY_IN_USE;
#endif
#ifdef EADDRNOTAVAIL
case EADDRNOTAVAIL: return DRWAV_ERROR;
#endif
#ifdef ENETDOWN
case ENETDOWN: return DRWAV_NO_NETWORK;
#endif
#ifdef ENETUNREACH
case ENETUNREACH: return DRWAV_NO_NETWORK;
#endif
#ifdef ENETRESET
case ENETRESET: return DRWAV_NO_NETWORK;
#endif
#ifdef ECONNABORTED
case ECONNABORTED: return DRWAV_NO_NETWORK;
#endif
#ifdef ECONNRESET
case ECONNRESET: return DRWAV_CONNECTION_RESET;
#endif
#ifdef ENOBUFS
case ENOBUFS: return DRWAV_NO_SPACE;
#endif
#ifdef EISCONN
case EISCONN: return DRWAV_ALREADY_CONNECTED;
#endif
#ifdef ENOTCONN
case ENOTCONN: return DRWAV_NOT_CONNECTED;
#endif
#ifdef ESHUTDOWN
case ESHUTDOWN: return DRWAV_ERROR;
#endif
#ifdef ETOOMANYREFS
case ETOOMANYREFS: return DRWAV_ERROR;
#endif
#ifdef ETIMEDOUT
case ETIMEDOUT: return DRWAV_TIMEOUT;
#endif
#ifdef ECONNREFUSED
case ECONNREFUSED: return DRWAV_CONNECTION_REFUSED;
#endif
#ifdef EHOSTDOWN
case EHOSTDOWN: return DRWAV_NO_HOST;
#endif
#ifdef EHOSTUNREACH
case EHOSTUNREACH: return DRWAV_NO_HOST;
#endif
#ifdef EALREADY
case EALREADY: return DRWAV_IN_PROGRESS;
#endif
#ifdef EINPROGRESS
case EINPROGRESS: return DRWAV_IN_PROGRESS;
#endif
#ifdef ESTALE
case ESTALE: return DRWAV_INVALID_FILE;
#endif
#ifdef EUCLEAN
case EUCLEAN: return DRWAV_ERROR;
#endif
#ifdef ENOTNAM
case ENOTNAM: return DRWAV_ERROR;
#endif
#ifdef ENAVAIL
case ENAVAIL: return DRWAV_ERROR;
#endif
#ifdef EISNAM
case EISNAM: return DRWAV_ERROR;
#endif
#ifdef EREMOTEIO
case EREMOTEIO: return DRWAV_IO_ERROR;
#endif
#ifdef EDQUOT
case EDQUOT: return DRWAV_NO_SPACE;
#endif
#ifdef ENOMEDIUM
case ENOMEDIUM: return DRWAV_DOES_NOT_EXIST;
#endif
#ifdef EMEDIUMTYPE
case EMEDIUMTYPE: return DRWAV_ERROR;
#endif
#ifdef ECANCELED
case ECANCELED: return DRWAV_CANCELLED;
#endif
#ifdef ENOKEY
case ENOKEY: return DRWAV_ERROR;
#endif
#ifdef EKEYEXPIRED
case EKEYEXPIRED: return DRWAV_ERROR;
#endif
#ifdef EKEYREVOKED
case EKEYREVOKED: return DRWAV_ERROR;
#endif
#ifdef EKEYREJECTED
case EKEYREJECTED: return DRWAV_ERROR;
#endif
#ifdef EOWNERDEAD
case EOWNERDEAD: return DRWAV_ERROR;
#endif
#ifdef ENOTRECOVERABLE
case ENOTRECOVERABLE: return DRWAV_ERROR;
#endif
#ifdef ERFKILL
case ERFKILL: return DRWAV_ERROR;
#endif
#ifdef EHWPOISON
case EHWPOISON: return DRWAV_ERROR;
#endif
default: return DRWAV_ERROR;
}
}
static drwav_result drwav_fopen(FILE** ppFile, const char* pFilePath, const char* pOpenMode)
{
#if _MSC_VER && _MSC_VER >= 1400
errno_t err;
#endif
if (ppFile != NULL) {
*ppFile = NULL;
}
if (pFilePath == NULL || pOpenMode == NULL || ppFile == NULL) {
return DRWAV_INVALID_ARGS;
}
#if _MSC_VER && _MSC_VER >= 1400
err = fopen_s(ppFile, pFilePath, pOpenMode);
if (err != 0) {
return drwav_result_from_errno(err);
}
#else
#if defined(_WIN32) || defined(__APPLE__)
*ppFile = fopen(pFilePath, pOpenMode);
#else
#if defined(_FILE_OFFSET_BITS) && _FILE_OFFSET_BITS == 64 && defined(_LARGEFILE64_SOURCE)
*ppFile = fopen64(pFilePath, pOpenMode);
#else
*ppFile = fopen(pFilePath, pOpenMode);
#endif
#endif
if (*ppFile == NULL) {
drwav_result result = drwav_result_from_errno(errno);
if (result == DRWAV_SUCCESS) {
result = DRWAV_ERROR;
}
return result;
}
#endif
return DRWAV_SUCCESS;
}
#if defined(_WIN32)
#if defined(_MSC_VER) || defined(__MINGW64__) || (!defined(__STRICT_ANSI__) && !defined(_NO_EXT_KEYS))
#define DRWAV_HAS_WFOPEN
#endif
#endif
static drwav_result drwav_wfopen(FILE** ppFile, const wchar_t* pFilePath, const wchar_t* pOpenMode, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (ppFile != NULL) {
*ppFile = NULL;
}
if (pFilePath == NULL || pOpenMode == NULL || ppFile == NULL) {
return DRWAV_INVALID_ARGS;
}
#if defined(DRWAV_HAS_WFOPEN)
{
#if defined(_MSC_VER) && _MSC_VER >= 1400
errno_t err = _wfopen_s(ppFile, pFilePath, pOpenMode);
if (err != 0) {
return drwav_result_from_errno(err);
}
#else
*ppFile = _wfopen(pFilePath, pOpenMode);
if (*ppFile == NULL) {
return drwav_result_from_errno(errno);
}
#endif
(void)pAllocationCallbacks;
}
#else
{
mbstate_t mbs;
size_t lenMB;
const wchar_t* pFilePathTemp = pFilePath;
char* pFilePathMB = NULL;
char pOpenModeMB[32] = {0};
DRWAV_ZERO_OBJECT(&mbs);
lenMB = wcsrtombs(NULL, &pFilePathTemp, 0, &mbs);
if (lenMB == (size_t)-1) {
return drwav_result_from_errno(errno);
}
pFilePathMB = (char*)drwav__malloc_from_callbacks(lenMB + 1, pAllocationCallbacks);
if (pFilePathMB == NULL) {
return DRWAV_OUT_OF_MEMORY;
}
pFilePathTemp = pFilePath;
DRWAV_ZERO_OBJECT(&mbs);
wcsrtombs(pFilePathMB, &pFilePathTemp, lenMB + 1, &mbs);
{
size_t i = 0;
for (;;) {
if (pOpenMode[i] == 0) {
pOpenModeMB[i] = '\0';
break;
}
pOpenModeMB[i] = (char)pOpenMode[i];
i += 1;
}
}
*ppFile = fopen(pFilePathMB, pOpenModeMB);
drwav__free_from_callbacks(pFilePathMB, pAllocationCallbacks);
}
if (*ppFile == NULL) {
return DRWAV_ERROR;
}
#endif
return DRWAV_SUCCESS;
}
static size_t drwav__on_read_stdio(void* pUserData, void* pBufferOut, size_t bytesToRead)
{
return fread(pBufferOut, 1, bytesToRead, (FILE*)pUserData);
}
static size_t drwav__on_write_stdio(void* pUserData, const void* pData, size_t bytesToWrite)
{
return fwrite(pData, 1, bytesToWrite, (FILE*)pUserData);
}
static drwav_bool32 drwav__on_seek_stdio(void* pUserData, int offset, drwav_seek_origin origin)
{
return fseek((FILE*)pUserData, offset, (origin == drwav_seek_origin_current) ? SEEK_CUR : SEEK_SET) == 0;
}
DRWAV_API drwav_bool32 drwav_init_file(drwav* pWav, const char* filename, const drwav_allocation_callbacks* pAllocationCallbacks)
{
return drwav_init_file_ex(pWav, filename, NULL, NULL, 0, pAllocationCallbacks);
}
static drwav_bool32 drwav_init_file__internal_FILE(drwav* pWav, FILE* pFile, drwav_chunk_proc onChunk, void* pChunkUserData, drwav_uint32 flags, const drwav_allocation_callbacks* pAllocationCallbacks)
{
drwav_bool32 result;
result = drwav_preinit(pWav, drwav__on_read_stdio, drwav__on_seek_stdio, (void*)pFile, pAllocationCallbacks);
if (result != DRWAV_TRUE) {
fclose(pFile);
return result;
}
result = drwav_init__internal(pWav, onChunk, pChunkUserData, flags);
if (result != DRWAV_TRUE) {
fclose(pFile);
return result;
}
return DRWAV_TRUE;
}
DRWAV_API drwav_bool32 drwav_init_file_ex(drwav* pWav, const char* filename, drwav_chunk_proc onChunk, void* pChunkUserData, drwav_uint32 flags, const drwav_allocation_callbacks* pAllocationCallbacks)
{
FILE* pFile;
if (drwav_fopen(&pFile, filename, "rb") != DRWAV_SUCCESS) {
return DRWAV_FALSE;
}
return drwav_init_file__internal_FILE(pWav, pFile, onChunk, pChunkUserData, flags, pAllocationCallbacks);
}
DRWAV_API drwav_bool32 drwav_init_file_w(drwav* pWav, const wchar_t* filename, const drwav_allocation_callbacks* pAllocationCallbacks)
{
return drwav_init_file_ex_w(pWav, filename, NULL, NULL, 0, pAllocationCallbacks);
}
DRWAV_API drwav_bool32 drwav_init_file_ex_w(drwav* pWav, const wchar_t* filename, drwav_chunk_proc onChunk, void* pChunkUserData, drwav_uint32 flags, const drwav_allocation_callbacks* pAllocationCallbacks)
{
FILE* pFile;
if (drwav_wfopen(&pFile, filename, L"rb", pAllocationCallbacks) != DRWAV_SUCCESS) {
return DRWAV_FALSE;
}
return drwav_init_file__internal_FILE(pWav, pFile, onChunk, pChunkUserData, flags, pAllocationCallbacks);
}
static drwav_bool32 drwav_init_file_write__internal_FILE(drwav* pWav, FILE* pFile, const drwav_data_format* pFormat, drwav_uint64 totalSampleCount, drwav_bool32 isSequential, const drwav_allocation_callbacks* pAllocationCallbacks)
{
drwav_bool32 result;
result = drwav_preinit_write(pWav, pFormat, isSequential, drwav__on_write_stdio, drwav__on_seek_stdio, (void*)pFile, pAllocationCallbacks);
if (result != DRWAV_TRUE) {
fclose(pFile);
return result;
}
result = drwav_init_write__internal(pWav, pFormat, totalSampleCount);
if (result != DRWAV_TRUE) {
fclose(pFile);
return result;
}
return DRWAV_TRUE;
}
static drwav_bool32 drwav_init_file_write__internal(drwav* pWav, const char* filename, const drwav_data_format* pFormat, drwav_uint64 totalSampleCount, drwav_bool32 isSequential, const drwav_allocation_callbacks* pAllocationCallbacks)
{
FILE* pFile;
if (drwav_fopen(&pFile, filename, "wb") != DRWAV_SUCCESS) {
return DRWAV_FALSE;
}
return drwav_init_file_write__internal_FILE(pWav, pFile, pFormat, totalSampleCount, isSequential, pAllocationCallbacks);
}
static drwav_bool32 drwav_init_file_write_w__internal(drwav* pWav, const wchar_t* filename, const drwav_data_format* pFormat, drwav_uint64 totalSampleCount, drwav_bool32 isSequential, const drwav_allocation_callbacks* pAllocationCallbacks)
{
FILE* pFile;
if (drwav_wfopen(&pFile, filename, L"wb", pAllocationCallbacks) != DRWAV_SUCCESS) {
return DRWAV_FALSE;
}
return drwav_init_file_write__internal_FILE(pWav, pFile, pFormat, totalSampleCount, isSequential, pAllocationCallbacks);
}
DRWAV_API drwav_bool32 drwav_init_file_write(drwav* pWav, const char* filename, const drwav_data_format* pFormat, const drwav_allocation_callbacks* pAllocationCallbacks)
{
return drwav_init_file_write__internal(pWav, filename, pFormat, 0, DRWAV_FALSE, pAllocationCallbacks);
}
DRWAV_API drwav_bool32 drwav_init_file_write_sequential(drwav* pWav, const char* filename, const drwav_data_format* pFormat, drwav_uint64 totalSampleCount, const drwav_allocation_callbacks* pAllocationCallbacks)
{
return drwav_init_file_write__internal(pWav, filename, pFormat, totalSampleCount, DRWAV_TRUE, pAllocationCallbacks);
}
DRWAV_API drwav_bool32 drwav_init_file_write_sequential_pcm_frames(drwav* pWav, const char* filename, const drwav_data_format* pFormat, drwav_uint64 totalPCMFrameCount, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (pFormat == NULL) {
return DRWAV_FALSE;
}
return drwav_init_file_write_sequential(pWav, filename, pFormat, totalPCMFrameCount*pFormat->channels, pAllocationCallbacks);
}
DRWAV_API drwav_bool32 drwav_init_file_write_w(drwav* pWav, const wchar_t* filename, const drwav_data_format* pFormat, const drwav_allocation_callbacks* pAllocationCallbacks)
{
return drwav_init_file_write_w__internal(pWav, filename, pFormat, 0, DRWAV_FALSE, pAllocationCallbacks);
}
DRWAV_API drwav_bool32 drwav_init_file_write_sequential_w(drwav* pWav, const wchar_t* filename, const drwav_data_format* pFormat, drwav_uint64 totalSampleCount, const drwav_allocation_callbacks* pAllocationCallbacks)
{
return drwav_init_file_write_w__internal(pWav, filename, pFormat, totalSampleCount, DRWAV_TRUE, pAllocationCallbacks);
}
DRWAV_API drwav_bool32 drwav_init_file_write_sequential_pcm_frames_w(drwav* pWav, const wchar_t* filename, const drwav_data_format* pFormat, drwav_uint64 totalPCMFrameCount, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (pFormat == NULL) {
return DRWAV_FALSE;
}
return drwav_init_file_write_sequential_w(pWav, filename, pFormat, totalPCMFrameCount*pFormat->channels, pAllocationCallbacks);
}
#endif
static size_t drwav__on_read_memory(void* pUserData, void* pBufferOut, size_t bytesToRead)
{
drwav* pWav = (drwav*)pUserData;
size_t bytesRemaining;
DRWAV_ASSERT(pWav != NULL);
DRWAV_ASSERT(pWav->memoryStream.dataSize >= pWav->memoryStream.currentReadPos);
bytesRemaining = pWav->memoryStream.dataSize - pWav->memoryStream.currentReadPos;
if (bytesToRead > bytesRemaining) {
bytesToRead = bytesRemaining;
}
if (bytesToRead > 0) {
DRWAV_COPY_MEMORY(pBufferOut, pWav->memoryStream.data + pWav->memoryStream.currentReadPos, bytesToRead);
pWav->memoryStream.currentReadPos += bytesToRead;
}
return bytesToRead;
}
static drwav_bool32 drwav__on_seek_memory(void* pUserData, int offset, drwav_seek_origin origin)
{
drwav* pWav = (drwav*)pUserData;
DRWAV_ASSERT(pWav != NULL);
if (origin == drwav_seek_origin_current) {
if (offset > 0) {
if (pWav->memoryStream.currentReadPos + offset > pWav->memoryStream.dataSize) {
return DRWAV_FALSE;
}
} else {
if (pWav->memoryStream.currentReadPos < (size_t)-offset) {
return DRWAV_FALSE;
}
}
pWav->memoryStream.currentReadPos += offset;
} else {
if ((drwav_uint32)offset <= pWav->memoryStream.dataSize) {
pWav->memoryStream.currentReadPos = offset;
} else {
return DRWAV_FALSE;
}
}
return DRWAV_TRUE;
}
static size_t drwav__on_write_memory(void* pUserData, const void* pDataIn, size_t bytesToWrite)
{
drwav* pWav = (drwav*)pUserData;
size_t bytesRemaining;
DRWAV_ASSERT(pWav != NULL);
DRWAV_ASSERT(pWav->memoryStreamWrite.dataCapacity >= pWav->memoryStreamWrite.currentWritePos);
bytesRemaining = pWav->memoryStreamWrite.dataCapacity - pWav->memoryStreamWrite.currentWritePos;
if (bytesRemaining < bytesToWrite) {
void* pNewData;
size_t newDataCapacity = (pWav->memoryStreamWrite.dataCapacity == 0) ? 256 : pWav->memoryStreamWrite.dataCapacity * 2;
if ((newDataCapacity - pWav->memoryStreamWrite.currentWritePos) < bytesToWrite) {
newDataCapacity = pWav->memoryStreamWrite.currentWritePos + bytesToWrite;
}
pNewData = drwav__realloc_from_callbacks(*pWav->memoryStreamWrite.ppData, newDataCapacity, pWav->memoryStreamWrite.dataCapacity, &pWav->allocationCallbacks);
if (pNewData == NULL) {
return 0;
}
*pWav->memoryStreamWrite.ppData = pNewData;
pWav->memoryStreamWrite.dataCapacity = newDataCapacity;
}
DRWAV_COPY_MEMORY(((drwav_uint8*)(*pWav->memoryStreamWrite.ppData)) + pWav->memoryStreamWrite.currentWritePos, pDataIn, bytesToWrite);
pWav->memoryStreamWrite.currentWritePos += bytesToWrite;
if (pWav->memoryStreamWrite.dataSize < pWav->memoryStreamWrite.currentWritePos) {
pWav->memoryStreamWrite.dataSize = pWav->memoryStreamWrite.currentWritePos;
}
*pWav->memoryStreamWrite.pDataSize = pWav->memoryStreamWrite.dataSize;
return bytesToWrite;
}
static drwav_bool32 drwav__on_seek_memory_write(void* pUserData, int offset, drwav_seek_origin origin)
{
drwav* pWav = (drwav*)pUserData;
DRWAV_ASSERT(pWav != NULL);
if (origin == drwav_seek_origin_current) {
if (offset > 0) {
if (pWav->memoryStreamWrite.currentWritePos + offset > pWav->memoryStreamWrite.dataSize) {
offset = (int)(pWav->memoryStreamWrite.dataSize - pWav->memoryStreamWrite.currentWritePos);
}
} else {
if (pWav->memoryStreamWrite.currentWritePos < (size_t)-offset) {
offset = -(int)pWav->memoryStreamWrite.currentWritePos;
}
}
pWav->memoryStreamWrite.currentWritePos += offset;
} else {
if ((drwav_uint32)offset <= pWav->memoryStreamWrite.dataSize) {
pWav->memoryStreamWrite.currentWritePos = offset;
} else {
pWav->memoryStreamWrite.currentWritePos = pWav->memoryStreamWrite.dataSize;
}
}
return DRWAV_TRUE;
}
DRWAV_API drwav_bool32 drwav_init_memory(drwav* pWav, const void* data, size_t dataSize, const drwav_allocation_callbacks* pAllocationCallbacks)
{
return drwav_init_memory_ex(pWav, data, dataSize, NULL, NULL, 0, pAllocationCallbacks);
}
DRWAV_API drwav_bool32 drwav_init_memory_ex(drwav* pWav, const void* data, size_t dataSize, drwav_chunk_proc onChunk, void* pChunkUserData, drwav_uint32 flags, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (data == NULL || dataSize == 0) {
return DRWAV_FALSE;
}
if (!drwav_preinit(pWav, drwav__on_read_memory, drwav__on_seek_memory, pWav, pAllocationCallbacks)) {
return DRWAV_FALSE;
}
pWav->memoryStream.data = (const drwav_uint8*)data;
pWav->memoryStream.dataSize = dataSize;
pWav->memoryStream.currentReadPos = 0;
return drwav_init__internal(pWav, onChunk, pChunkUserData, flags);
}
static drwav_bool32 drwav_init_memory_write__internal(drwav* pWav, void** ppData, size_t* pDataSize, const drwav_data_format* pFormat, drwav_uint64 totalSampleCount, drwav_bool32 isSequential, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (ppData == NULL || pDataSize == NULL) {
return DRWAV_FALSE;
}
*ppData = NULL;
*pDataSize = 0;
if (!drwav_preinit_write(pWav, pFormat, isSequential, drwav__on_write_memory, drwav__on_seek_memory_write, pWav, pAllocationCallbacks)) {
return DRWAV_FALSE;
}
pWav->memoryStreamWrite.ppData = ppData;
pWav->memoryStreamWrite.pDataSize = pDataSize;
pWav->memoryStreamWrite.dataSize = 0;
pWav->memoryStreamWrite.dataCapacity = 0;
pWav->memoryStreamWrite.currentWritePos = 0;
return drwav_init_write__internal(pWav, pFormat, totalSampleCount);
}
DRWAV_API drwav_bool32 drwav_init_memory_write(drwav* pWav, void** ppData, size_t* pDataSize, const drwav_data_format* pFormat, const drwav_allocation_callbacks* pAllocationCallbacks)
{
return drwav_init_memory_write__internal(pWav, ppData, pDataSize, pFormat, 0, DRWAV_FALSE, pAllocationCallbacks);
}
DRWAV_API drwav_bool32 drwav_init_memory_write_sequential(drwav* pWav, void** ppData, size_t* pDataSize, const drwav_data_format* pFormat, drwav_uint64 totalSampleCount, const drwav_allocation_callbacks* pAllocationCallbacks)
{
return drwav_init_memory_write__internal(pWav, ppData, pDataSize, pFormat, totalSampleCount, DRWAV_TRUE, pAllocationCallbacks);
}
DRWAV_API drwav_bool32 drwav_init_memory_write_sequential_pcm_frames(drwav* pWav, void** ppData, size_t* pDataSize, const drwav_data_format* pFormat, drwav_uint64 totalPCMFrameCount, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (pFormat == NULL) {
return DRWAV_FALSE;
}
return drwav_init_memory_write_sequential(pWav, ppData, pDataSize, pFormat, totalPCMFrameCount*pFormat->channels, pAllocationCallbacks);
}
DRWAV_API drwav_result drwav_uninit(drwav* pWav)
{
drwav_result result = DRWAV_SUCCESS;
if (pWav == NULL) {
return DRWAV_INVALID_ARGS;
}
if (pWav->onWrite != NULL) {
drwav_uint32 paddingSize = 0;
if (pWav->container == drwav_container_riff || pWav->container == drwav_container_rf64) {
paddingSize = drwav__chunk_padding_size_riff(pWav->dataChunkDataSize);
} else {
paddingSize = drwav__chunk_padding_size_w64(pWav->dataChunkDataSize);
}
if (paddingSize > 0) {
drwav_uint64 paddingData = 0;
drwav__write(pWav, &paddingData, paddingSize);
}
if (pWav->onSeek && !pWav->isSequentialWrite) {
if (pWav->container == drwav_container_riff) {
if (pWav->onSeek(pWav->pUserData, 4, drwav_seek_origin_start)) {
drwav_uint32 riffChunkSize = drwav__riff_chunk_size_riff(pWav->dataChunkDataSize);
drwav__write_u32ne_to_le(pWav, riffChunkSize);
}
if (pWav->onSeek(pWav->pUserData, (int)pWav->dataChunkDataPos + 4, drwav_seek_origin_start)) {
drwav_uint32 dataChunkSize = drwav__data_chunk_size_riff(pWav->dataChunkDataSize);
drwav__write_u32ne_to_le(pWav, dataChunkSize);
}
} else if (pWav->container == drwav_container_w64) {
if (pWav->onSeek(pWav->pUserData, 16, drwav_seek_origin_start)) {
drwav_uint64 riffChunkSize = drwav__riff_chunk_size_w64(pWav->dataChunkDataSize);
drwav__write_u64ne_to_le(pWav, riffChunkSize);
}
if (pWav->onSeek(pWav->pUserData, (int)pWav->dataChunkDataPos + 16, drwav_seek_origin_start)) {
drwav_uint64 dataChunkSize = drwav__data_chunk_size_w64(pWav->dataChunkDataSize);
drwav__write_u64ne_to_le(pWav, dataChunkSize);
}
} else if (pWav->container == drwav_container_rf64) {
int ds64BodyPos = 12 + 8;
if (pWav->onSeek(pWav->pUserData, ds64BodyPos + 0, drwav_seek_origin_start)) {
drwav_uint64 riffChunkSize = drwav__riff_chunk_size_rf64(pWav->dataChunkDataSize);
drwav__write_u64ne_to_le(pWav, riffChunkSize);
}
if (pWav->onSeek(pWav->pUserData, ds64BodyPos + 8, drwav_seek_origin_start)) {
drwav_uint64 dataChunkSize = drwav__data_chunk_size_rf64(pWav->dataChunkDataSize);
drwav__write_u64ne_to_le(pWav, dataChunkSize);
}
}
}
if (pWav->isSequentialWrite) {
if (pWav->dataChunkDataSize != pWav->dataChunkDataSizeTargetWrite) {
result = DRWAV_INVALID_FILE;
}
}
}
#ifndef DR_WAV_NO_STDIO
if (pWav->onRead == drwav__on_read_stdio || pWav->onWrite == drwav__on_write_stdio) {
fclose((FILE*)pWav->pUserData);
}
#endif
return result;
}
DRWAV_API size_t drwav_read_raw(drwav* pWav, size_t bytesToRead, void* pBufferOut)
{
size_t bytesRead;
if (pWav == NULL || bytesToRead == 0) {
return 0;
}
if (bytesToRead > pWav->bytesRemaining) {
bytesToRead = (size_t)pWav->bytesRemaining;
}
if (pBufferOut != NULL) {
bytesRead = pWav->onRead(pWav->pUserData, pBufferOut, bytesToRead);
} else {
bytesRead = 0;
while (bytesRead < bytesToRead) {
size_t bytesToSeek = (bytesToRead - bytesRead);
if (bytesToSeek > 0x7FFFFFFF) {
bytesToSeek = 0x7FFFFFFF;
}
if (pWav->onSeek(pWav->pUserData, (int)bytesToSeek, drwav_seek_origin_current) == DRWAV_FALSE) {
break;
}
bytesRead += bytesToSeek;
}
while (bytesRead < bytesToRead) {
drwav_uint8 buffer[4096];
size_t bytesSeeked;
size_t bytesToSeek = (bytesToRead - bytesRead);
if (bytesToSeek > sizeof(buffer)) {
bytesToSeek = sizeof(buffer);
}
bytesSeeked = pWav->onRead(pWav->pUserData, buffer, bytesToSeek);
bytesRead += bytesSeeked;
if (bytesSeeked < bytesToSeek) {
break;
}
}
}
pWav->bytesRemaining -= bytesRead;
return bytesRead;
}
DRWAV_API drwav_uint64 drwav_read_pcm_frames_le(drwav* pWav, drwav_uint64 framesToRead, void* pBufferOut)
{
drwav_uint32 bytesPerFrame;
drwav_uint64 bytesToRead;
if (pWav == NULL || framesToRead == 0) {
return 0;
}
if (drwav__is_compressed_format_tag(pWav->translatedFormatTag)) {
return 0;
}
bytesPerFrame = drwav_get_bytes_per_pcm_frame(pWav);
if (bytesPerFrame == 0) {
return 0;
}
bytesToRead = framesToRead * bytesPerFrame;
if (bytesToRead > DRWAV_SIZE_MAX) {
bytesToRead = (DRWAV_SIZE_MAX / bytesPerFrame) * bytesPerFrame;
}
if (bytesToRead == 0) {
return 0;
}
return drwav_read_raw(pWav, (size_t)bytesToRead, pBufferOut) / bytesPerFrame;
}
DRWAV_API drwav_uint64 drwav_read_pcm_frames_be(drwav* pWav, drwav_uint64 framesToRead, void* pBufferOut)
{
drwav_uint64 framesRead = drwav_read_pcm_frames_le(pWav, framesToRead, pBufferOut);
if (pBufferOut != NULL) {
drwav__bswap_samples(pBufferOut, framesRead*pWav->channels, drwav_get_bytes_per_pcm_frame(pWav)/pWav->channels, pWav->translatedFormatTag);
}
return framesRead;
}
DRWAV_API drwav_uint64 drwav_read_pcm_frames(drwav* pWav, drwav_uint64 framesToRead, void* pBufferOut)
{
if (drwav__is_little_endian()) {
return drwav_read_pcm_frames_le(pWav, framesToRead, pBufferOut);
} else {
return drwav_read_pcm_frames_be(pWav, framesToRead, pBufferOut);
}
}
DRWAV_API drwav_bool32 drwav_seek_to_first_pcm_frame(drwav* pWav)
{
if (pWav->onWrite != NULL) {
return DRWAV_FALSE;
}
if (!pWav->onSeek(pWav->pUserData, (int)pWav->dataChunkDataPos, drwav_seek_origin_start)) {
return DRWAV_FALSE;
}
if (drwav__is_compressed_format_tag(pWav->translatedFormatTag)) {
pWav->compressed.iCurrentPCMFrame = 0;
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_ADPCM) {
DRWAV_ZERO_OBJECT(&pWav->msadpcm);
} else if (pWav->translatedFormatTag == DR_WAVE_FORMAT_DVI_ADPCM) {
DRWAV_ZERO_OBJECT(&pWav->ima);
} else {
DRWAV_ASSERT(DRWAV_FALSE);
}
}
pWav->bytesRemaining = pWav->dataChunkDataSize;
return DRWAV_TRUE;
}
DRWAV_API drwav_bool32 drwav_seek_to_pcm_frame(drwav* pWav, drwav_uint64 targetFrameIndex)
{
if (pWav == NULL || pWav->onSeek == NULL) {
return DRWAV_FALSE;
}
if (pWav->onWrite != NULL) {
return DRWAV_FALSE;
}
if (pWav->totalPCMFrameCount == 0) {
return DRWAV_TRUE;
}
if (targetFrameIndex >= pWav->totalPCMFrameCount) {
targetFrameIndex  = pWav->totalPCMFrameCount - 1;
}
if (drwav__is_compressed_format_tag(pWav->translatedFormatTag)) {
if (targetFrameIndex < pWav->compressed.iCurrentPCMFrame) {
if (!drwav_seek_to_first_pcm_frame(pWav)) {
return DRWAV_FALSE;
}
}
if (targetFrameIndex > pWav->compressed.iCurrentPCMFrame) {
drwav_uint64 offsetInFrames = targetFrameIndex - pWav->compressed.iCurrentPCMFrame;
drwav_int16 devnull[2048];
while (offsetInFrames > 0) {
drwav_uint64 framesRead = 0;
drwav_uint64 framesToRead = offsetInFrames;
if (framesToRead > drwav_countof(devnull)/pWav->channels) {
framesToRead = drwav_countof(devnull)/pWav->channels;
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_ADPCM) {
framesRead = drwav_read_pcm_frames_s16__msadpcm(pWav, framesToRead, devnull);
} else if (pWav->translatedFormatTag == DR_WAVE_FORMAT_DVI_ADPCM) {
framesRead = drwav_read_pcm_frames_s16__ima(pWav, framesToRead, devnull);
} else {
DRWAV_ASSERT(DRWAV_FALSE);
}
if (framesRead != framesToRead) {
return DRWAV_FALSE;
}
offsetInFrames -= framesRead;
}
}
} else {
drwav_uint64 totalSizeInBytes;
drwav_uint64 currentBytePos;
drwav_uint64 targetBytePos;
drwav_uint64 offset;
totalSizeInBytes = pWav->totalPCMFrameCount * drwav_get_bytes_per_pcm_frame(pWav);
DRWAV_ASSERT(totalSizeInBytes >= pWav->bytesRemaining);
currentBytePos = totalSizeInBytes - pWav->bytesRemaining;
targetBytePos  = targetFrameIndex * drwav_get_bytes_per_pcm_frame(pWav);
if (currentBytePos < targetBytePos) {
offset = (targetBytePos - currentBytePos);
} else {
if (!drwav_seek_to_first_pcm_frame(pWav)) {
return DRWAV_FALSE;
}
offset = targetBytePos;
}
while (offset > 0) {
int offset32 = ((offset > INT_MAX) ? INT_MAX : (int)offset);
if (!pWav->onSeek(pWav->pUserData, offset32, drwav_seek_origin_current)) {
return DRWAV_FALSE;
}
pWav->bytesRemaining -= offset32;
offset -= offset32;
}
}
return DRWAV_TRUE;
}
DRWAV_API size_t drwav_write_raw(drwav* pWav, size_t bytesToWrite, const void* pData)
{
size_t bytesWritten;
if (pWav == NULL || bytesToWrite == 0 || pData == NULL) {
return 0;
}
bytesWritten = pWav->onWrite(pWav->pUserData, pData, bytesToWrite);
pWav->dataChunkDataSize += bytesWritten;
return bytesWritten;
}
DRWAV_API drwav_uint64 drwav_write_pcm_frames_le(drwav* pWav, drwav_uint64 framesToWrite, const void* pData)
{
drwav_uint64 bytesToWrite;
drwav_uint64 bytesWritten;
const drwav_uint8* pRunningData;
if (pWav == NULL || framesToWrite == 0 || pData == NULL) {
return 0;
}
bytesToWrite = ((framesToWrite * pWav->channels * pWav->bitsPerSample) / 8);
if (bytesToWrite > DRWAV_SIZE_MAX) {
return 0;
}
bytesWritten = 0;
pRunningData = (const drwav_uint8*)pData;
while (bytesToWrite > 0) {
size_t bytesJustWritten;
drwav_uint64 bytesToWriteThisIteration;
bytesToWriteThisIteration = bytesToWrite;
DRWAV_ASSERT(bytesToWriteThisIteration <= DRWAV_SIZE_MAX);
bytesJustWritten = drwav_write_raw(pWav, (size_t)bytesToWriteThisIteration, pRunningData);
if (bytesJustWritten == 0) {
break;
}
bytesToWrite -= bytesJustWritten;
bytesWritten += bytesJustWritten;
pRunningData += bytesJustWritten;
}
return (bytesWritten * 8) / pWav->bitsPerSample / pWav->channels;
}
DRWAV_API drwav_uint64 drwav_write_pcm_frames_be(drwav* pWav, drwav_uint64 framesToWrite, const void* pData)
{
drwav_uint64 bytesToWrite;
drwav_uint64 bytesWritten;
drwav_uint32 bytesPerSample;
const drwav_uint8* pRunningData;
if (pWav == NULL || framesToWrite == 0 || pData == NULL) {
return 0;
}
bytesToWrite = ((framesToWrite * pWav->channels * pWav->bitsPerSample) / 8);
if (bytesToWrite > DRWAV_SIZE_MAX) {
return 0;
}
bytesWritten = 0;
pRunningData = (const drwav_uint8*)pData;
bytesPerSample = drwav_get_bytes_per_pcm_frame(pWav) / pWav->channels;
while (bytesToWrite > 0) {
drwav_uint8 temp[4096];
drwav_uint32 sampleCount;
size_t bytesJustWritten;
drwav_uint64 bytesToWriteThisIteration;
bytesToWriteThisIteration = bytesToWrite;
DRWAV_ASSERT(bytesToWriteThisIteration <= DRWAV_SIZE_MAX);
sampleCount = sizeof(temp)/bytesPerSample;
if (bytesToWriteThisIteration > ((drwav_uint64)sampleCount)*bytesPerSample) {
bytesToWriteThisIteration = ((drwav_uint64)sampleCount)*bytesPerSample;
}
DRWAV_COPY_MEMORY(temp, pRunningData, (size_t)bytesToWriteThisIteration);
drwav__bswap_samples(temp, sampleCount, bytesPerSample, pWav->translatedFormatTag);
bytesJustWritten = drwav_write_raw(pWav, (size_t)bytesToWriteThisIteration, temp);
if (bytesJustWritten == 0) {
break;
}
bytesToWrite -= bytesJustWritten;
bytesWritten += bytesJustWritten;
pRunningData += bytesJustWritten;
}
return (bytesWritten * 8) / pWav->bitsPerSample / pWav->channels;
}
DRWAV_API drwav_uint64 drwav_write_pcm_frames(drwav* pWav, drwav_uint64 framesToWrite, const void* pData)
{
if (drwav__is_little_endian()) {
return drwav_write_pcm_frames_le(pWav, framesToWrite, pData);
} else {
return drwav_write_pcm_frames_be(pWav, framesToWrite, pData);
}
}
static drwav_uint64 drwav_read_pcm_frames_s16__msadpcm(drwav* pWav, drwav_uint64 framesToRead, drwav_int16* pBufferOut)
{
drwav_uint64 totalFramesRead = 0;
DRWAV_ASSERT(pWav != NULL);
DRWAV_ASSERT(framesToRead > 0);
while (framesToRead > 0 && pWav->compressed.iCurrentPCMFrame < pWav->totalPCMFrameCount) {
if (pWav->msadpcm.cachedFrameCount == 0 && pWav->msadpcm.bytesRemainingInBlock == 0) {
if (pWav->channels == 1) {
drwav_uint8 header[7];
if (pWav->onRead(pWav->pUserData, header, sizeof(header)) != sizeof(header)) {
return totalFramesRead;
}
pWav->msadpcm.bytesRemainingInBlock = pWav->fmt.blockAlign - sizeof(header);
pWav->msadpcm.predictor[0]     = header[0];
pWav->msadpcm.delta[0]         = drwav__bytes_to_s16(header + 1);
pWav->msadpcm.prevFrames[0][1] = (drwav_int32)drwav__bytes_to_s16(header + 3);
pWav->msadpcm.prevFrames[0][0] = (drwav_int32)drwav__bytes_to_s16(header + 5);
pWav->msadpcm.cachedFrames[2]  = pWav->msadpcm.prevFrames[0][0];
pWav->msadpcm.cachedFrames[3]  = pWav->msadpcm.prevFrames[0][1];
pWav->msadpcm.cachedFrameCount = 2;
} else {
drwav_uint8 header[14];
if (pWav->onRead(pWav->pUserData, header, sizeof(header)) != sizeof(header)) {
return totalFramesRead;
}
pWav->msadpcm.bytesRemainingInBlock = pWav->fmt.blockAlign - sizeof(header);
pWav->msadpcm.predictor[0] = header[0];
pWav->msadpcm.predictor[1] = header[1];
pWav->msadpcm.delta[0] = drwav__bytes_to_s16(header + 2);
pWav->msadpcm.delta[1] = drwav__bytes_to_s16(header + 4);
pWav->msadpcm.prevFrames[0][1] = (drwav_int32)drwav__bytes_to_s16(header + 6);
pWav->msadpcm.prevFrames[1][1] = (drwav_int32)drwav__bytes_to_s16(header + 8);
pWav->msadpcm.prevFrames[0][0] = (drwav_int32)drwav__bytes_to_s16(header + 10);
pWav->msadpcm.prevFrames[1][0] = (drwav_int32)drwav__bytes_to_s16(header + 12);
pWav->msadpcm.cachedFrames[0] = pWav->msadpcm.prevFrames[0][0];
pWav->msadpcm.cachedFrames[1] = pWav->msadpcm.prevFrames[1][0];
pWav->msadpcm.cachedFrames[2] = pWav->msadpcm.prevFrames[0][1];
pWav->msadpcm.cachedFrames[3] = pWav->msadpcm.prevFrames[1][1];
pWav->msadpcm.cachedFrameCount = 2;
}
}
while (framesToRead > 0 && pWav->msadpcm.cachedFrameCount > 0 && pWav->compressed.iCurrentPCMFrame < pWav->totalPCMFrameCount) {
if (pBufferOut != NULL) {
drwav_uint32 iSample = 0;
for (iSample = 0; iSample < pWav->channels; iSample += 1) {
pBufferOut[iSample] = (drwav_int16)pWav->msadpcm.cachedFrames[(drwav_countof(pWav->msadpcm.cachedFrames) - (pWav->msadpcm.cachedFrameCount*pWav->channels)) + iSample];
}
pBufferOut += pWav->channels;
}
framesToRead    -= 1;
totalFramesRead += 1;
pWav->compressed.iCurrentPCMFrame += 1;
pWav->msadpcm.cachedFrameCount -= 1;
}
if (framesToRead == 0) {
return totalFramesRead;
}
if (pWav->msadpcm.cachedFrameCount == 0) {
if (pWav->msadpcm.bytesRemainingInBlock == 0) {
continue;
} else {
static drwav_int32 adaptationTable[] = {
230, 230, 230, 230, 307, 409, 512, 614,
768, 614, 512, 409, 307, 230, 230, 230
};
static drwav_int32 coeff1Table[] = { 256, 512, 0, 192, 240, 460,  392 };
static drwav_int32 coeff2Table[] = { 0,  -256, 0, 64,  0,  -208, -232 };
drwav_uint8 nibbles;
drwav_int32 nibble0;
drwav_int32 nibble1;
if (pWav->onRead(pWav->pUserData, &nibbles, 1) != 1) {
return totalFramesRead;
}
pWav->msadpcm.bytesRemainingInBlock -= 1;
nibble0 = ((nibbles & 0xF0) >> 4); if ((nibbles & 0x80)) { nibble0 |= 0xFFFFFFF0UL; }
nibble1 = ((nibbles & 0x0F) >> 0); if ((nibbles & 0x08)) { nibble1 |= 0xFFFFFFF0UL; }
if (pWav->channels == 1) {
drwav_int32 newSample0;
drwav_int32 newSample1;
newSample0  = ((pWav->msadpcm.prevFrames[0][1] * coeff1Table[pWav->msadpcm.predictor[0]]) + (pWav->msadpcm.prevFrames[0][0] * coeff2Table[pWav->msadpcm.predictor[0]])) >> 8;
newSample0 += nibble0 * pWav->msadpcm.delta[0];
newSample0  = drwav_clamp(newSample0, -32768, 32767);
pWav->msadpcm.delta[0] = (adaptationTable[((nibbles & 0xF0) >> 4)] * pWav->msadpcm.delta[0]) >> 8;
if (pWav->msadpcm.delta[0] < 16) {
pWav->msadpcm.delta[0] = 16;
}
pWav->msadpcm.prevFrames[0][0] = pWav->msadpcm.prevFrames[0][1];
pWav->msadpcm.prevFrames[0][1] = newSample0;
newSample1  = ((pWav->msadpcm.prevFrames[0][1] * coeff1Table[pWav->msadpcm.predictor[0]]) + (pWav->msadpcm.prevFrames[0][0] * coeff2Table[pWav->msadpcm.predictor[0]])) >> 8;
newSample1 += nibble1 * pWav->msadpcm.delta[0];
newSample1  = drwav_clamp(newSample1, -32768, 32767);
pWav->msadpcm.delta[0] = (adaptationTable[((nibbles & 0x0F) >> 0)] * pWav->msadpcm.delta[0]) >> 8;
if (pWav->msadpcm.delta[0] < 16) {
pWav->msadpcm.delta[0] = 16;
}
pWav->msadpcm.prevFrames[0][0] = pWav->msadpcm.prevFrames[0][1];
pWav->msadpcm.prevFrames[0][1] = newSample1;
pWav->msadpcm.cachedFrames[2] = newSample0;
pWav->msadpcm.cachedFrames[3] = newSample1;
pWav->msadpcm.cachedFrameCount = 2;
} else {
drwav_int32 newSample0;
drwav_int32 newSample1;
newSample0  = ((pWav->msadpcm.prevFrames[0][1] * coeff1Table[pWav->msadpcm.predictor[0]]) + (pWav->msadpcm.prevFrames[0][0] * coeff2Table[pWav->msadpcm.predictor[0]])) >> 8;
newSample0 += nibble0 * pWav->msadpcm.delta[0];
newSample0  = drwav_clamp(newSample0, -32768, 32767);
pWav->msadpcm.delta[0] = (adaptationTable[((nibbles & 0xF0) >> 4)] * pWav->msadpcm.delta[0]) >> 8;
if (pWav->msadpcm.delta[0] < 16) {
pWav->msadpcm.delta[0] = 16;
}
pWav->msadpcm.prevFrames[0][0] = pWav->msadpcm.prevFrames[0][1];
pWav->msadpcm.prevFrames[0][1] = newSample0;
newSample1  = ((pWav->msadpcm.prevFrames[1][1] * coeff1Table[pWav->msadpcm.predictor[1]]) + (pWav->msadpcm.prevFrames[1][0] * coeff2Table[pWav->msadpcm.predictor[1]])) >> 8;
newSample1 += nibble1 * pWav->msadpcm.delta[1];
newSample1  = drwav_clamp(newSample1, -32768, 32767);
pWav->msadpcm.delta[1] = (adaptationTable[((nibbles & 0x0F) >> 0)] * pWav->msadpcm.delta[1]) >> 8;
if (pWav->msadpcm.delta[1] < 16) {
pWav->msadpcm.delta[1] = 16;
}
pWav->msadpcm.prevFrames[1][0] = pWav->msadpcm.prevFrames[1][1];
pWav->msadpcm.prevFrames[1][1] = newSample1;
pWav->msadpcm.cachedFrames[2] = newSample0;
pWav->msadpcm.cachedFrames[3] = newSample1;
pWav->msadpcm.cachedFrameCount = 1;
}
}
}
}
return totalFramesRead;
}
static drwav_uint64 drwav_read_pcm_frames_s16__ima(drwav* pWav, drwav_uint64 framesToRead, drwav_int16* pBufferOut)
{
drwav_uint64 totalFramesRead = 0;
drwav_uint32 iChannel;
static drwav_int32 indexTable[16] = {
-1, -1, -1, -1, 2, 4, 6, 8,
-1, -1, -1, -1, 2, 4, 6, 8
};
static drwav_int32 stepTable[89] = {
7,     8,     9,     10,    11,    12,    13,    14,    16,    17,
19,    21,    23,    25,    28,    31,    34,    37,    41,    45,
50,    55,    60,    66,    73,    80,    88,    97,    107,   118,
130,   143,   157,   173,   190,   209,   230,   253,   279,   307,
337,   371,   408,   449,   494,   544,   598,   658,   724,   796,
876,   963,   1060,  1166,  1282,  1411,  1552,  1707,  1878,  2066,
2272,  2499,  2749,  3024,  3327,  3660,  4026,  4428,  4871,  5358,
5894,  6484,  7132,  7845,  8630,  9493,  10442, 11487, 12635, 13899,
15289, 16818, 18500, 20350, 22385, 24623, 27086, 29794, 32767
};
DRWAV_ASSERT(pWav != NULL);
DRWAV_ASSERT(framesToRead > 0);
while (framesToRead > 0 && pWav->compressed.iCurrentPCMFrame < pWav->totalPCMFrameCount) {
if (pWav->ima.cachedFrameCount == 0 && pWav->ima.bytesRemainingInBlock == 0) {
if (pWav->channels == 1) {
drwav_uint8 header[4];
if (pWav->onRead(pWav->pUserData, header, sizeof(header)) != sizeof(header)) {
return totalFramesRead;
}
pWav->ima.bytesRemainingInBlock = pWav->fmt.blockAlign - sizeof(header);
if (header[2] >= drwav_countof(stepTable)) {
pWav->onSeek(pWav->pUserData, pWav->ima.bytesRemainingInBlock, drwav_seek_origin_current);
pWav->ima.bytesRemainingInBlock = 0;
return totalFramesRead;
}
pWav->ima.predictor[0] = drwav__bytes_to_s16(header + 0);
pWav->ima.stepIndex[0] = header[2];
pWav->ima.cachedFrames[drwav_countof(pWav->ima.cachedFrames) - 1] = pWav->ima.predictor[0];
pWav->ima.cachedFrameCount = 1;
} else {
drwav_uint8 header[8];
if (pWav->onRead(pWav->pUserData, header, sizeof(header)) != sizeof(header)) {
return totalFramesRead;
}
pWav->ima.bytesRemainingInBlock = pWav->fmt.blockAlign - sizeof(header);
if (header[2] >= drwav_countof(stepTable) || header[6] >= drwav_countof(stepTable)) {
pWav->onSeek(pWav->pUserData, pWav->ima.bytesRemainingInBlock, drwav_seek_origin_current);
pWav->ima.bytesRemainingInBlock = 0;
return totalFramesRead;
}
pWav->ima.predictor[0] = drwav__bytes_to_s16(header + 0);
pWav->ima.stepIndex[0] = header[2];
pWav->ima.predictor[1] = drwav__bytes_to_s16(header + 4);
pWav->ima.stepIndex[1] = header[6];
pWav->ima.cachedFrames[drwav_countof(pWav->ima.cachedFrames) - 2] = pWav->ima.predictor[0];
pWav->ima.cachedFrames[drwav_countof(pWav->ima.cachedFrames) - 1] = pWav->ima.predictor[1];
pWav->ima.cachedFrameCount = 1;
}
}
while (framesToRead > 0 && pWav->ima.cachedFrameCount > 0 && pWav->compressed.iCurrentPCMFrame < pWav->totalPCMFrameCount) {
if (pBufferOut != NULL) {
drwav_uint32 iSample;
for (iSample = 0; iSample < pWav->channels; iSample += 1) {
pBufferOut[iSample] = (drwav_int16)pWav->ima.cachedFrames[(drwav_countof(pWav->ima.cachedFrames) - (pWav->ima.cachedFrameCount*pWav->channels)) + iSample];
}
pBufferOut += pWav->channels;
}
framesToRead    -= 1;
totalFramesRead += 1;
pWav->compressed.iCurrentPCMFrame += 1;
pWav->ima.cachedFrameCount -= 1;
}
if (framesToRead == 0) {
return totalFramesRead;
}
if (pWav->ima.cachedFrameCount == 0) {
if (pWav->ima.bytesRemainingInBlock == 0) {
continue;
} else {
pWav->ima.cachedFrameCount = 8;
for (iChannel = 0; iChannel < pWav->channels; ++iChannel) {
drwav_uint32 iByte;
drwav_uint8 nibbles[4];
if (pWav->onRead(pWav->pUserData, &nibbles, 4) != 4) {
pWav->ima.cachedFrameCount = 0;
return totalFramesRead;
}
pWav->ima.bytesRemainingInBlock -= 4;
for (iByte = 0; iByte < 4; ++iByte) {
drwav_uint8 nibble0 = ((nibbles[iByte] & 0x0F) >> 0);
drwav_uint8 nibble1 = ((nibbles[iByte] & 0xF0) >> 4);
drwav_int32 step      = stepTable[pWav->ima.stepIndex[iChannel]];
drwav_int32 predictor = pWav->ima.predictor[iChannel];
drwav_int32      diff  = step >> 3;
if (nibble0 & 1) diff += step >> 2;
if (nibble0 & 2) diff += step >> 1;
if (nibble0 & 4) diff += step;
if (nibble0 & 8) diff  = -diff;
predictor = drwav_clamp(predictor + diff, -32768, 32767);
pWav->ima.predictor[iChannel] = predictor;
pWav->ima.stepIndex[iChannel] = drwav_clamp(pWav->ima.stepIndex[iChannel] + indexTable[nibble0], 0, (drwav_int32)drwav_countof(stepTable)-1);
pWav->ima.cachedFrames[(drwav_countof(pWav->ima.cachedFrames) - (pWav->ima.cachedFrameCount*pWav->channels)) + (iByte*2+0)*pWav->channels + iChannel] = predictor;
step      = stepTable[pWav->ima.stepIndex[iChannel]];
predictor = pWav->ima.predictor[iChannel];
diff  = step >> 3;
if (nibble1 & 1) diff += step >> 2;
if (nibble1 & 2) diff += step >> 1;
if (nibble1 & 4) diff += step;
if (nibble1 & 8) diff  = -diff;
predictor = drwav_clamp(predictor + diff, -32768, 32767);
pWav->ima.predictor[iChannel] = predictor;
pWav->ima.stepIndex[iChannel] = drwav_clamp(pWav->ima.stepIndex[iChannel] + indexTable[nibble1], 0, (drwav_int32)drwav_countof(stepTable)-1);
pWav->ima.cachedFrames[(drwav_countof(pWav->ima.cachedFrames) - (pWav->ima.cachedFrameCount*pWav->channels)) + (iByte*2+1)*pWav->channels + iChannel] = predictor;
}
}
}
}
}
return totalFramesRead;
}
#ifndef DR_WAV_NO_CONVERSION_API
static unsigned short g_drwavAlawTable[256] = {
0xEA80, 0xEB80, 0xE880, 0xE980, 0xEE80, 0xEF80, 0xEC80, 0xED80, 0xE280, 0xE380, 0xE080, 0xE180, 0xE680, 0xE780, 0xE480, 0xE580,
0xF540, 0xF5C0, 0xF440, 0xF4C0, 0xF740, 0xF7C0, 0xF640, 0xF6C0, 0xF140, 0xF1C0, 0xF040, 0xF0C0, 0xF340, 0xF3C0, 0xF240, 0xF2C0,
0xAA00, 0xAE00, 0xA200, 0xA600, 0xBA00, 0xBE00, 0xB200, 0xB600, 0x8A00, 0x8E00, 0x8200, 0x8600, 0x9A00, 0x9E00, 0x9200, 0x9600,
0xD500, 0xD700, 0xD100, 0xD300, 0xDD00, 0xDF00, 0xD900, 0xDB00, 0xC500, 0xC700, 0xC100, 0xC300, 0xCD00, 0xCF00, 0xC900, 0xCB00,
0xFEA8, 0xFEB8, 0xFE88, 0xFE98, 0xFEE8, 0xFEF8, 0xFEC8, 0xFED8, 0xFE28, 0xFE38, 0xFE08, 0xFE18, 0xFE68, 0xFE78, 0xFE48, 0xFE58,
0xFFA8, 0xFFB8, 0xFF88, 0xFF98, 0xFFE8, 0xFFF8, 0xFFC8, 0xFFD8, 0xFF28, 0xFF38, 0xFF08, 0xFF18, 0xFF68, 0xFF78, 0xFF48, 0xFF58,
0xFAA0, 0xFAE0, 0xFA20, 0xFA60, 0xFBA0, 0xFBE0, 0xFB20, 0xFB60, 0xF8A0, 0xF8E0, 0xF820, 0xF860, 0xF9A0, 0xF9E0, 0xF920, 0xF960,
0xFD50, 0xFD70, 0xFD10, 0xFD30, 0xFDD0, 0xFDF0, 0xFD90, 0xFDB0, 0xFC50, 0xFC70, 0xFC10, 0xFC30, 0xFCD0, 0xFCF0, 0xFC90, 0xFCB0,
0x1580, 0x1480, 0x1780, 0x1680, 0x1180, 0x1080, 0x1380, 0x1280, 0x1D80, 0x1C80, 0x1F80, 0x1E80, 0x1980, 0x1880, 0x1B80, 0x1A80,
0x0AC0, 0x0A40, 0x0BC0, 0x0B40, 0x08C0, 0x0840, 0x09C0, 0x0940, 0x0EC0, 0x0E40, 0x0FC0, 0x0F40, 0x0CC0, 0x0C40, 0x0DC0, 0x0D40,
0x5600, 0x5200, 0x5E00, 0x5A00, 0x4600, 0x4200, 0x4E00, 0x4A00, 0x7600, 0x7200, 0x7E00, 0x7A00, 0x6600, 0x6200, 0x6E00, 0x6A00,
0x2B00, 0x2900, 0x2F00, 0x2D00, 0x2300, 0x2100, 0x2700, 0x2500, 0x3B00, 0x3900, 0x3F00, 0x3D00, 0x3300, 0x3100, 0x3700, 0x3500,
0x0158, 0x0148, 0x0178, 0x0168, 0x0118, 0x0108, 0x0138, 0x0128, 0x01D8, 0x01C8, 0x01F8, 0x01E8, 0x0198, 0x0188, 0x01B8, 0x01A8,
0x0058, 0x0048, 0x0078, 0x0068, 0x0018, 0x0008, 0x0038, 0x0028, 0x00D8, 0x00C8, 0x00F8, 0x00E8, 0x0098, 0x0088, 0x00B8, 0x00A8,
0x0560, 0x0520, 0x05E0, 0x05A0, 0x0460, 0x0420, 0x04E0, 0x04A0, 0x0760, 0x0720, 0x07E0, 0x07A0, 0x0660, 0x0620, 0x06E0, 0x06A0,
0x02B0, 0x0290, 0x02F0, 0x02D0, 0x0230, 0x0210, 0x0270, 0x0250, 0x03B0, 0x0390, 0x03F0, 0x03D0, 0x0330, 0x0310, 0x0370, 0x0350
};
static unsigned short g_drwavMulawTable[256] = {
0x8284, 0x8684, 0x8A84, 0x8E84, 0x9284, 0x9684, 0x9A84, 0x9E84, 0xA284, 0xA684, 0xAA84, 0xAE84, 0xB284, 0xB684, 0xBA84, 0xBE84,
0xC184, 0xC384, 0xC584, 0xC784, 0xC984, 0xCB84, 0xCD84, 0xCF84, 0xD184, 0xD384, 0xD584, 0xD784, 0xD984, 0xDB84, 0xDD84, 0xDF84,
0xE104, 0xE204, 0xE304, 0xE404, 0xE504, 0xE604, 0xE704, 0xE804, 0xE904, 0xEA04, 0xEB04, 0xEC04, 0xED04, 0xEE04, 0xEF04, 0xF004,
0xF0C4, 0xF144, 0xF1C4, 0xF244, 0xF2C4, 0xF344, 0xF3C4, 0xF444, 0xF4C4, 0xF544, 0xF5C4, 0xF644, 0xF6C4, 0xF744, 0xF7C4, 0xF844,
0xF8A4, 0xF8E4, 0xF924, 0xF964, 0xF9A4, 0xF9E4, 0xFA24, 0xFA64, 0xFAA4, 0xFAE4, 0xFB24, 0xFB64, 0xFBA4, 0xFBE4, 0xFC24, 0xFC64,
0xFC94, 0xFCB4, 0xFCD4, 0xFCF4, 0xFD14, 0xFD34, 0xFD54, 0xFD74, 0xFD94, 0xFDB4, 0xFDD4, 0xFDF4, 0xFE14, 0xFE34, 0xFE54, 0xFE74,
0xFE8C, 0xFE9C, 0xFEAC, 0xFEBC, 0xFECC, 0xFEDC, 0xFEEC, 0xFEFC, 0xFF0C, 0xFF1C, 0xFF2C, 0xFF3C, 0xFF4C, 0xFF5C, 0xFF6C, 0xFF7C,
0xFF88, 0xFF90, 0xFF98, 0xFFA0, 0xFFA8, 0xFFB0, 0xFFB8, 0xFFC0, 0xFFC8, 0xFFD0, 0xFFD8, 0xFFE0, 0xFFE8, 0xFFF0, 0xFFF8, 0x0000,
0x7D7C, 0x797C, 0x757C, 0x717C, 0x6D7C, 0x697C, 0x657C, 0x617C, 0x5D7C, 0x597C, 0x557C, 0x517C, 0x4D7C, 0x497C, 0x457C, 0x417C,
0x3E7C, 0x3C7C, 0x3A7C, 0x387C, 0x367C, 0x347C, 0x327C, 0x307C, 0x2E7C, 0x2C7C, 0x2A7C, 0x287C, 0x267C, 0x247C, 0x227C, 0x207C,
0x1EFC, 0x1DFC, 0x1CFC, 0x1BFC, 0x1AFC, 0x19FC, 0x18FC, 0x17FC, 0x16FC, 0x15FC, 0x14FC, 0x13FC, 0x12FC, 0x11FC, 0x10FC, 0x0FFC,
0x0F3C, 0x0EBC, 0x0E3C, 0x0DBC, 0x0D3C, 0x0CBC, 0x0C3C, 0x0BBC, 0x0B3C, 0x0ABC, 0x0A3C, 0x09BC, 0x093C, 0x08BC, 0x083C, 0x07BC,
0x075C, 0x071C, 0x06DC, 0x069C, 0x065C, 0x061C, 0x05DC, 0x059C, 0x055C, 0x051C, 0x04DC, 0x049C, 0x045C, 0x041C, 0x03DC, 0x039C,
0x036C, 0x034C, 0x032C, 0x030C, 0x02EC, 0x02CC, 0x02AC, 0x028C, 0x026C, 0x024C, 0x022C, 0x020C, 0x01EC, 0x01CC, 0x01AC, 0x018C,
0x0174, 0x0164, 0x0154, 0x0144, 0x0134, 0x0124, 0x0114, 0x0104, 0x00F4, 0x00E4, 0x00D4, 0x00C4, 0x00B4, 0x00A4, 0x0094, 0x0084,
0x0078, 0x0070, 0x0068, 0x0060, 0x0058, 0x0050, 0x0048, 0x0040, 0x0038, 0x0030, 0x0028, 0x0020, 0x0018, 0x0010, 0x0008, 0x0000
};
static DRWAV_INLINE drwav_int16 drwav__alaw_to_s16(drwav_uint8 sampleIn)
{
return (short)g_drwavAlawTable[sampleIn];
}
static DRWAV_INLINE drwav_int16 drwav__mulaw_to_s16(drwav_uint8 sampleIn)
{
return (short)g_drwavMulawTable[sampleIn];
}
static void drwav__pcm_to_s16(drwav_int16* pOut, const drwav_uint8* pIn, size_t totalSampleCount, unsigned int bytesPerSample)
{
unsigned int i;
if (bytesPerSample == 1) {
drwav_u8_to_s16(pOut, pIn, totalSampleCount);
return;
}
if (bytesPerSample == 2) {
for (i = 0; i < totalSampleCount; ++i) {
*pOut++ = ((const drwav_int16*)pIn)[i];
}
return;
}
if (bytesPerSample == 3) {
drwav_s24_to_s16(pOut, pIn, totalSampleCount);
return;
}
if (bytesPerSample == 4) {
drwav_s32_to_s16(pOut, (const drwav_int32*)pIn, totalSampleCount);
return;
}
if (bytesPerSample > 8) {
DRWAV_ZERO_MEMORY(pOut, totalSampleCount * sizeof(*pOut));
return;
}
for (i = 0; i < totalSampleCount; ++i) {
drwav_uint64 sample = 0;
unsigned int shift  = (8 - bytesPerSample) * 8;
unsigned int j;
for (j = 0; j < bytesPerSample; j += 1) {
DRWAV_ASSERT(j < 8);
sample |= (drwav_uint64)(pIn[j]) << shift;
shift  += 8;
}
pIn += j;
*pOut++ = (drwav_int16)((drwav_int64)sample >> 48);
}
}
static void drwav__ieee_to_s16(drwav_int16* pOut, const drwav_uint8* pIn, size_t totalSampleCount, unsigned int bytesPerSample)
{
if (bytesPerSample == 4) {
drwav_f32_to_s16(pOut, (const float*)pIn, totalSampleCount);
return;
} else if (bytesPerSample == 8) {
drwav_f64_to_s16(pOut, (const double*)pIn, totalSampleCount);
return;
} else {
DRWAV_ZERO_MEMORY(pOut, totalSampleCount * sizeof(*pOut));
return;
}
}
static drwav_uint64 drwav_read_pcm_frames_s16__pcm(drwav* pWav, drwav_uint64 framesToRead, drwav_int16* pBufferOut)
{
drwav_uint32 bytesPerFrame;
drwav_uint64 totalFramesRead;
drwav_uint8 sampleData[4096];
if ((pWav->translatedFormatTag == DR_WAVE_FORMAT_PCM && pWav->bitsPerSample == 16) || pBufferOut == NULL) {
return drwav_read_pcm_frames(pWav, framesToRead, pBufferOut);
}
bytesPerFrame = drwav_get_bytes_per_pcm_frame(pWav);
if (bytesPerFrame == 0) {
return 0;
}
totalFramesRead = 0;
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames(pWav, drwav_min(framesToRead, sizeof(sampleData)/bytesPerFrame), sampleData);
if (framesRead == 0) {
break;
}
drwav__pcm_to_s16(pBufferOut, sampleData, (size_t)(framesRead*pWav->channels), bytesPerFrame/pWav->channels);
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
static drwav_uint64 drwav_read_pcm_frames_s16__ieee(drwav* pWav, drwav_uint64 framesToRead, drwav_int16* pBufferOut)
{
drwav_uint64 totalFramesRead;
drwav_uint8 sampleData[4096];
drwav_uint32 bytesPerFrame;
if (pBufferOut == NULL) {
return drwav_read_pcm_frames(pWav, framesToRead, NULL);
}
bytesPerFrame = drwav_get_bytes_per_pcm_frame(pWav);
if (bytesPerFrame == 0) {
return 0;
}
totalFramesRead = 0;
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames(pWav, drwav_min(framesToRead, sizeof(sampleData)/bytesPerFrame), sampleData);
if (framesRead == 0) {
break;
}
drwav__ieee_to_s16(pBufferOut, sampleData, (size_t)(framesRead*pWav->channels), bytesPerFrame/pWav->channels);
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
static drwav_uint64 drwav_read_pcm_frames_s16__alaw(drwav* pWav, drwav_uint64 framesToRead, drwav_int16* pBufferOut)
{
drwav_uint64 totalFramesRead;
drwav_uint8 sampleData[4096];
drwav_uint32 bytesPerFrame;
if (pBufferOut == NULL) {
return drwav_read_pcm_frames(pWav, framesToRead, NULL);
}
bytesPerFrame = drwav_get_bytes_per_pcm_frame(pWav);
if (bytesPerFrame == 0) {
return 0;
}
totalFramesRead = 0;
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames(pWav, drwav_min(framesToRead, sizeof(sampleData)/bytesPerFrame), sampleData);
if (framesRead == 0) {
break;
}
drwav_alaw_to_s16(pBufferOut, sampleData, (size_t)(framesRead*pWav->channels));
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
static drwav_uint64 drwav_read_pcm_frames_s16__mulaw(drwav* pWav, drwav_uint64 framesToRead, drwav_int16* pBufferOut)
{
drwav_uint64 totalFramesRead;
drwav_uint8 sampleData[4096];
drwav_uint32 bytesPerFrame;
if (pBufferOut == NULL) {
return drwav_read_pcm_frames(pWav, framesToRead, NULL);
}
bytesPerFrame = drwav_get_bytes_per_pcm_frame(pWav);
if (bytesPerFrame == 0) {
return 0;
}
totalFramesRead = 0;
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames(pWav, drwav_min(framesToRead, sizeof(sampleData)/bytesPerFrame), sampleData);
if (framesRead == 0) {
break;
}
drwav_mulaw_to_s16(pBufferOut, sampleData, (size_t)(framesRead*pWav->channels));
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
DRWAV_API drwav_uint64 drwav_read_pcm_frames_s16(drwav* pWav, drwav_uint64 framesToRead, drwav_int16* pBufferOut)
{
if (pWav == NULL || framesToRead == 0) {
return 0;
}
if (pBufferOut == NULL) {
return drwav_read_pcm_frames(pWav, framesToRead, NULL);
}
if (framesToRead * pWav->channels * sizeof(drwav_int16) > DRWAV_SIZE_MAX) {
framesToRead = DRWAV_SIZE_MAX / sizeof(drwav_int16) / pWav->channels;
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_PCM) {
return drwav_read_pcm_frames_s16__pcm(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_IEEE_FLOAT) {
return drwav_read_pcm_frames_s16__ieee(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_ALAW) {
return drwav_read_pcm_frames_s16__alaw(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_MULAW) {
return drwav_read_pcm_frames_s16__mulaw(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_ADPCM) {
return drwav_read_pcm_frames_s16__msadpcm(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_DVI_ADPCM) {
return drwav_read_pcm_frames_s16__ima(pWav, framesToRead, pBufferOut);
}
return 0;
}
DRWAV_API drwav_uint64 drwav_read_pcm_frames_s16le(drwav* pWav, drwav_uint64 framesToRead, drwav_int16* pBufferOut)
{
drwav_uint64 framesRead = drwav_read_pcm_frames_s16(pWav, framesToRead, pBufferOut);
if (pBufferOut != NULL && drwav__is_little_endian() == DRWAV_FALSE) {
drwav__bswap_samples_s16(pBufferOut, framesRead*pWav->channels);
}
return framesRead;
}
DRWAV_API drwav_uint64 drwav_read_pcm_frames_s16be(drwav* pWav, drwav_uint64 framesToRead, drwav_int16* pBufferOut)
{
drwav_uint64 framesRead = drwav_read_pcm_frames_s16(pWav, framesToRead, pBufferOut);
if (pBufferOut != NULL && drwav__is_little_endian() == DRWAV_TRUE) {
drwav__bswap_samples_s16(pBufferOut, framesRead*pWav->channels);
}
return framesRead;
}
DRWAV_API void drwav_u8_to_s16(drwav_int16* pOut, const drwav_uint8* pIn, size_t sampleCount)
{
int r;
size_t i;
for (i = 0; i < sampleCount; ++i) {
int x = pIn[i];
r = x << 8;
r = r - 32768;
pOut[i] = (short)r;
}
}
DRWAV_API void drwav_s24_to_s16(drwav_int16* pOut, const drwav_uint8* pIn, size_t sampleCount)
{
int r;
size_t i;
for (i = 0; i < sampleCount; ++i) {
int x = ((int)(((unsigned int)(((const drwav_uint8*)pIn)[i*3+0]) << 8) | ((unsigned int)(((const drwav_uint8*)pIn)[i*3+1]) << 16) | ((unsigned int)(((const drwav_uint8*)pIn)[i*3+2])) << 24)) >> 8;
r = x >> 8;
pOut[i] = (short)r;
}
}
DRWAV_API void drwav_s32_to_s16(drwav_int16* pOut, const drwav_int32* pIn, size_t sampleCount)
{
int r;
size_t i;
for (i = 0; i < sampleCount; ++i) {
int x = pIn[i];
r = x >> 16;
pOut[i] = (short)r;
}
}
DRWAV_API void drwav_f32_to_s16(drwav_int16* pOut, const float* pIn, size_t sampleCount)
{
int r;
size_t i;
for (i = 0; i < sampleCount; ++i) {
float x = pIn[i];
float c;
c = ((x < -1) ? -1 : ((x > 1) ? 1 : x));
c = c + 1;
r = (int)(c * 32767.5f);
r = r - 32768;
pOut[i] = (short)r;
}
}
DRWAV_API void drwav_f64_to_s16(drwav_int16* pOut, const double* pIn, size_t sampleCount)
{
int r;
size_t i;
for (i = 0; i < sampleCount; ++i) {
double x = pIn[i];
double c;
c = ((x < -1) ? -1 : ((x > 1) ? 1 : x));
c = c + 1;
r = (int)(c * 32767.5);
r = r - 32768;
pOut[i] = (short)r;
}
}
DRWAV_API void drwav_alaw_to_s16(drwav_int16* pOut, const drwav_uint8* pIn, size_t sampleCount)
{
size_t i;
for (i = 0; i < sampleCount; ++i) {
pOut[i] = drwav__alaw_to_s16(pIn[i]);
}
}
DRWAV_API void drwav_mulaw_to_s16(drwav_int16* pOut, const drwav_uint8* pIn, size_t sampleCount)
{
size_t i;
for (i = 0; i < sampleCount; ++i) {
pOut[i] = drwav__mulaw_to_s16(pIn[i]);
}
}
static void drwav__pcm_to_f32(float* pOut, const drwav_uint8* pIn, size_t sampleCount, unsigned int bytesPerSample)
{
unsigned int i;
if (bytesPerSample == 1) {
drwav_u8_to_f32(pOut, pIn, sampleCount);
return;
}
if (bytesPerSample == 2) {
drwav_s16_to_f32(pOut, (const drwav_int16*)pIn, sampleCount);
return;
}
if (bytesPerSample == 3) {
drwav_s24_to_f32(pOut, pIn, sampleCount);
return;
}
if (bytesPerSample == 4) {
drwav_s32_to_f32(pOut, (const drwav_int32*)pIn, sampleCount);
return;
}
if (bytesPerSample > 8) {
DRWAV_ZERO_MEMORY(pOut, sampleCount * sizeof(*pOut));
return;
}
for (i = 0; i < sampleCount; ++i) {
drwav_uint64 sample = 0;
unsigned int shift  = (8 - bytesPerSample) * 8;
unsigned int j;
for (j = 0; j < bytesPerSample; j += 1) {
DRWAV_ASSERT(j < 8);
sample |= (drwav_uint64)(pIn[j]) << shift;
shift  += 8;
}
pIn += j;
*pOut++ = (float)((drwav_int64)sample / 9223372036854775807.0);
}
}
static void drwav__ieee_to_f32(float* pOut, const drwav_uint8* pIn, size_t sampleCount, unsigned int bytesPerSample)
{
if (bytesPerSample == 4) {
unsigned int i;
for (i = 0; i < sampleCount; ++i) {
*pOut++ = ((const float*)pIn)[i];
}
return;
} else if (bytesPerSample == 8) {
drwav_f64_to_f32(pOut, (const double*)pIn, sampleCount);
return;
} else {
DRWAV_ZERO_MEMORY(pOut, sampleCount * sizeof(*pOut));
return;
}
}
static drwav_uint64 drwav_read_pcm_frames_f32__pcm(drwav* pWav, drwav_uint64 framesToRead, float* pBufferOut)
{
drwav_uint64 totalFramesRead;
drwav_uint8 sampleData[4096];
drwav_uint32 bytesPerFrame = drwav_get_bytes_per_pcm_frame(pWav);
if (bytesPerFrame == 0) {
return 0;
}
totalFramesRead = 0;
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames(pWav, drwav_min(framesToRead, sizeof(sampleData)/bytesPerFrame), sampleData);
if (framesRead == 0) {
break;
}
drwav__pcm_to_f32(pBufferOut, sampleData, (size_t)framesRead*pWav->channels, bytesPerFrame/pWav->channels);
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
static drwav_uint64 drwav_read_pcm_frames_f32__msadpcm(drwav* pWav, drwav_uint64 framesToRead, float* pBufferOut)
{
drwav_uint64 totalFramesRead = 0;
drwav_int16 samples16[2048];
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames_s16(pWav, drwav_min(framesToRead, drwav_countof(samples16)/pWav->channels), samples16);
if (framesRead == 0) {
break;
}
drwav_s16_to_f32(pBufferOut, samples16, (size_t)(framesRead*pWav->channels));
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
static drwav_uint64 drwav_read_pcm_frames_f32__ima(drwav* pWav, drwav_uint64 framesToRead, float* pBufferOut)
{
drwav_uint64 totalFramesRead = 0;
drwav_int16 samples16[2048];
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames_s16(pWav, drwav_min(framesToRead, drwav_countof(samples16)/pWav->channels), samples16);
if (framesRead == 0) {
break;
}
drwav_s16_to_f32(pBufferOut, samples16, (size_t)(framesRead*pWav->channels));
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
static drwav_uint64 drwav_read_pcm_frames_f32__ieee(drwav* pWav, drwav_uint64 framesToRead, float* pBufferOut)
{
drwav_uint64 totalFramesRead;
drwav_uint8 sampleData[4096];
drwav_uint32 bytesPerFrame;
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_IEEE_FLOAT && pWav->bitsPerSample == 32) {
return drwav_read_pcm_frames(pWav, framesToRead, pBufferOut);
}
bytesPerFrame = drwav_get_bytes_per_pcm_frame(pWav);
if (bytesPerFrame == 0) {
return 0;
}
totalFramesRead = 0;
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames(pWav, drwav_min(framesToRead, sizeof(sampleData)/bytesPerFrame), sampleData);
if (framesRead == 0) {
break;
}
drwav__ieee_to_f32(pBufferOut, sampleData, (size_t)(framesRead*pWav->channels), bytesPerFrame/pWav->channels);
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
static drwav_uint64 drwav_read_pcm_frames_f32__alaw(drwav* pWav, drwav_uint64 framesToRead, float* pBufferOut)
{
drwav_uint64 totalFramesRead;
drwav_uint8 sampleData[4096];
drwav_uint32 bytesPerFrame = drwav_get_bytes_per_pcm_frame(pWav);
if (bytesPerFrame == 0) {
return 0;
}
totalFramesRead = 0;
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames(pWav, drwav_min(framesToRead, sizeof(sampleData)/bytesPerFrame), sampleData);
if (framesRead == 0) {
break;
}
drwav_alaw_to_f32(pBufferOut, sampleData, (size_t)(framesRead*pWav->channels));
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
static drwav_uint64 drwav_read_pcm_frames_f32__mulaw(drwav* pWav, drwav_uint64 framesToRead, float* pBufferOut)
{
drwav_uint64 totalFramesRead;
drwav_uint8 sampleData[4096];
drwav_uint32 bytesPerFrame = drwav_get_bytes_per_pcm_frame(pWav);
if (bytesPerFrame == 0) {
return 0;
}
totalFramesRead = 0;
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames(pWav, drwav_min(framesToRead, sizeof(sampleData)/bytesPerFrame), sampleData);
if (framesRead == 0) {
break;
}
drwav_mulaw_to_f32(pBufferOut, sampleData, (size_t)(framesRead*pWav->channels));
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
DRWAV_API drwav_uint64 drwav_read_pcm_frames_f32(drwav* pWav, drwav_uint64 framesToRead, float* pBufferOut)
{
if (pWav == NULL || framesToRead == 0) {
return 0;
}
if (pBufferOut == NULL) {
return drwav_read_pcm_frames(pWav, framesToRead, NULL);
}
if (framesToRead * pWav->channels * sizeof(float) > DRWAV_SIZE_MAX) {
framesToRead = DRWAV_SIZE_MAX / sizeof(float) / pWav->channels;
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_PCM) {
return drwav_read_pcm_frames_f32__pcm(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_ADPCM) {
return drwav_read_pcm_frames_f32__msadpcm(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_IEEE_FLOAT) {
return drwav_read_pcm_frames_f32__ieee(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_ALAW) {
return drwav_read_pcm_frames_f32__alaw(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_MULAW) {
return drwav_read_pcm_frames_f32__mulaw(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_DVI_ADPCM) {
return drwav_read_pcm_frames_f32__ima(pWav, framesToRead, pBufferOut);
}
return 0;
}
DRWAV_API drwav_uint64 drwav_read_pcm_frames_f32le(drwav* pWav, drwav_uint64 framesToRead, float* pBufferOut)
{
drwav_uint64 framesRead = drwav_read_pcm_frames_f32(pWav, framesToRead, pBufferOut);
if (pBufferOut != NULL && drwav__is_little_endian() == DRWAV_FALSE) {
drwav__bswap_samples_f32(pBufferOut, framesRead*pWav->channels);
}
return framesRead;
}
DRWAV_API drwav_uint64 drwav_read_pcm_frames_f32be(drwav* pWav, drwav_uint64 framesToRead, float* pBufferOut)
{
drwav_uint64 framesRead = drwav_read_pcm_frames_f32(pWav, framesToRead, pBufferOut);
if (pBufferOut != NULL && drwav__is_little_endian() == DRWAV_TRUE) {
drwav__bswap_samples_f32(pBufferOut, framesRead*pWav->channels);
}
return framesRead;
}
DRWAV_API void drwav_u8_to_f32(float* pOut, const drwav_uint8* pIn, size_t sampleCount)
{
size_t i;
if (pOut == NULL || pIn == NULL) {
return;
}
#ifdef DR_WAV_LIBSNDFILE_COMPAT
for (i = 0; i < sampleCount; ++i) {
*pOut++ = (pIn[i] / 256.0f) * 2 - 1;
}
#else
for (i = 0; i < sampleCount; ++i) {
float x = pIn[i];
x = x * 0.00784313725490196078f;
x = x - 1;
*pOut++ = x;
}
#endif
}
DRWAV_API void drwav_s16_to_f32(float* pOut, const drwav_int16* pIn, size_t sampleCount)
{
size_t i;
if (pOut == NULL || pIn == NULL) {
return;
}
for (i = 0; i < sampleCount; ++i) {
*pOut++ = pIn[i] * 0.000030517578125f;
}
}
DRWAV_API void drwav_s24_to_f32(float* pOut, const drwav_uint8* pIn, size_t sampleCount)
{
size_t i;
if (pOut == NULL || pIn == NULL) {
return;
}
for (i = 0; i < sampleCount; ++i) {
double x;
drwav_uint32 a = ((drwav_uint32)(pIn[i*3+0]) <<  8);
drwav_uint32 b = ((drwav_uint32)(pIn[i*3+1]) << 16);
drwav_uint32 c = ((drwav_uint32)(pIn[i*3+2]) << 24);
x = (double)((drwav_int32)(a | b | c) >> 8);
*pOut++ = (float)(x * 0.00000011920928955078125);
}
}
DRWAV_API void drwav_s32_to_f32(float* pOut, const drwav_int32* pIn, size_t sampleCount)
{
size_t i;
if (pOut == NULL || pIn == NULL) {
return;
}
for (i = 0; i < sampleCount; ++i) {
*pOut++ = (float)(pIn[i] / 2147483648.0);
}
}
DRWAV_API void drwav_f64_to_f32(float* pOut, const double* pIn, size_t sampleCount)
{
size_t i;
if (pOut == NULL || pIn == NULL) {
return;
}
for (i = 0; i < sampleCount; ++i) {
*pOut++ = (float)pIn[i];
}
}
DRWAV_API void drwav_alaw_to_f32(float* pOut, const drwav_uint8* pIn, size_t sampleCount)
{
size_t i;
if (pOut == NULL || pIn == NULL) {
return;
}
for (i = 0; i < sampleCount; ++i) {
*pOut++ = drwav__alaw_to_s16(pIn[i]) / 32768.0f;
}
}
DRWAV_API void drwav_mulaw_to_f32(float* pOut, const drwav_uint8* pIn, size_t sampleCount)
{
size_t i;
if (pOut == NULL || pIn == NULL) {
return;
}
for (i = 0; i < sampleCount; ++i) {
*pOut++ = drwav__mulaw_to_s16(pIn[i]) / 32768.0f;
}
}
static void drwav__pcm_to_s32(drwav_int32* pOut, const drwav_uint8* pIn, size_t totalSampleCount, unsigned int bytesPerSample)
{
unsigned int i;
if (bytesPerSample == 1) {
drwav_u8_to_s32(pOut, pIn, totalSampleCount);
return;
}
if (bytesPerSample == 2) {
drwav_s16_to_s32(pOut, (const drwav_int16*)pIn, totalSampleCount);
return;
}
if (bytesPerSample == 3) {
drwav_s24_to_s32(pOut, pIn, totalSampleCount);
return;
}
if (bytesPerSample == 4) {
for (i = 0; i < totalSampleCount; ++i) {
*pOut++ = ((const drwav_int32*)pIn)[i];
}
return;
}
if (bytesPerSample > 8) {
DRWAV_ZERO_MEMORY(pOut, totalSampleCount * sizeof(*pOut));
return;
}
for (i = 0; i < totalSampleCount; ++i) {
drwav_uint64 sample = 0;
unsigned int shift  = (8 - bytesPerSample) * 8;
unsigned int j;
for (j = 0; j < bytesPerSample; j += 1) {
DRWAV_ASSERT(j < 8);
sample |= (drwav_uint64)(pIn[j]) << shift;
shift  += 8;
}
pIn += j;
*pOut++ = (drwav_int32)((drwav_int64)sample >> 32);
}
}
static void drwav__ieee_to_s32(drwav_int32* pOut, const drwav_uint8* pIn, size_t totalSampleCount, unsigned int bytesPerSample)
{
if (bytesPerSample == 4) {
drwav_f32_to_s32(pOut, (const float*)pIn, totalSampleCount);
return;
} else if (bytesPerSample == 8) {
drwav_f64_to_s32(pOut, (const double*)pIn, totalSampleCount);
return;
} else {
DRWAV_ZERO_MEMORY(pOut, totalSampleCount * sizeof(*pOut));
return;
}
}
static drwav_uint64 drwav_read_pcm_frames_s32__pcm(drwav* pWav, drwav_uint64 framesToRead, drwav_int32* pBufferOut)
{
drwav_uint64 totalFramesRead;
drwav_uint8 sampleData[4096];
drwav_uint32 bytesPerFrame;
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_PCM && pWav->bitsPerSample == 32) {
return drwav_read_pcm_frames(pWav, framesToRead, pBufferOut);
}
bytesPerFrame = drwav_get_bytes_per_pcm_frame(pWav);
if (bytesPerFrame == 0) {
return 0;
}
totalFramesRead = 0;
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames(pWav, drwav_min(framesToRead, sizeof(sampleData)/bytesPerFrame), sampleData);
if (framesRead == 0) {
break;
}
drwav__pcm_to_s32(pBufferOut, sampleData, (size_t)(framesRead*pWav->channels), bytesPerFrame/pWav->channels);
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
static drwav_uint64 drwav_read_pcm_frames_s32__msadpcm(drwav* pWav, drwav_uint64 framesToRead, drwav_int32* pBufferOut)
{
drwav_uint64 totalFramesRead = 0;
drwav_int16 samples16[2048];
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames_s16(pWav, drwav_min(framesToRead, drwav_countof(samples16)/pWav->channels), samples16);
if (framesRead == 0) {
break;
}
drwav_s16_to_s32(pBufferOut, samples16, (size_t)(framesRead*pWav->channels));
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
static drwav_uint64 drwav_read_pcm_frames_s32__ima(drwav* pWav, drwav_uint64 framesToRead, drwav_int32* pBufferOut)
{
drwav_uint64 totalFramesRead = 0;
drwav_int16 samples16[2048];
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames_s16(pWav, drwav_min(framesToRead, drwav_countof(samples16)/pWav->channels), samples16);
if (framesRead == 0) {
break;
}
drwav_s16_to_s32(pBufferOut, samples16, (size_t)(framesRead*pWav->channels));
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
static drwav_uint64 drwav_read_pcm_frames_s32__ieee(drwav* pWav, drwav_uint64 framesToRead, drwav_int32* pBufferOut)
{
drwav_uint64 totalFramesRead;
drwav_uint8 sampleData[4096];
drwav_uint32 bytesPerFrame = drwav_get_bytes_per_pcm_frame(pWav);
if (bytesPerFrame == 0) {
return 0;
}
totalFramesRead = 0;
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames(pWav, drwav_min(framesToRead, sizeof(sampleData)/bytesPerFrame), sampleData);
if (framesRead == 0) {
break;
}
drwav__ieee_to_s32(pBufferOut, sampleData, (size_t)(framesRead*pWav->channels), bytesPerFrame/pWav->channels);
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
static drwav_uint64 drwav_read_pcm_frames_s32__alaw(drwav* pWav, drwav_uint64 framesToRead, drwav_int32* pBufferOut)
{
drwav_uint64 totalFramesRead;
drwav_uint8 sampleData[4096];
drwav_uint32 bytesPerFrame = drwav_get_bytes_per_pcm_frame(pWav);
if (bytesPerFrame == 0) {
return 0;
}
totalFramesRead = 0;
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames(pWav, drwav_min(framesToRead, sizeof(sampleData)/bytesPerFrame), sampleData);
if (framesRead == 0) {
break;
}
drwav_alaw_to_s32(pBufferOut, sampleData, (size_t)(framesRead*pWav->channels));
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
static drwav_uint64 drwav_read_pcm_frames_s32__mulaw(drwav* pWav, drwav_uint64 framesToRead, drwav_int32* pBufferOut)
{
drwav_uint64 totalFramesRead;
drwav_uint8 sampleData[4096];
drwav_uint32 bytesPerFrame = drwav_get_bytes_per_pcm_frame(pWav);
if (bytesPerFrame == 0) {
return 0;
}
totalFramesRead = 0;
while (framesToRead > 0) {
drwav_uint64 framesRead = drwav_read_pcm_frames(pWav, drwav_min(framesToRead, sizeof(sampleData)/bytesPerFrame), sampleData);
if (framesRead == 0) {
break;
}
drwav_mulaw_to_s32(pBufferOut, sampleData, (size_t)(framesRead*pWav->channels));
pBufferOut      += framesRead*pWav->channels;
framesToRead    -= framesRead;
totalFramesRead += framesRead;
}
return totalFramesRead;
}
DRWAV_API drwav_uint64 drwav_read_pcm_frames_s32(drwav* pWav, drwav_uint64 framesToRead, drwav_int32* pBufferOut)
{
if (pWav == NULL || framesToRead == 0) {
return 0;
}
if (pBufferOut == NULL) {
return drwav_read_pcm_frames(pWav, framesToRead, NULL);
}
if (framesToRead * pWav->channels * sizeof(drwav_int32) > DRWAV_SIZE_MAX) {
framesToRead = DRWAV_SIZE_MAX / sizeof(drwav_int32) / pWav->channels;
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_PCM) {
return drwav_read_pcm_frames_s32__pcm(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_ADPCM) {
return drwav_read_pcm_frames_s32__msadpcm(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_IEEE_FLOAT) {
return drwav_read_pcm_frames_s32__ieee(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_ALAW) {
return drwav_read_pcm_frames_s32__alaw(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_MULAW) {
return drwav_read_pcm_frames_s32__mulaw(pWav, framesToRead, pBufferOut);
}
if (pWav->translatedFormatTag == DR_WAVE_FORMAT_DVI_ADPCM) {
return drwav_read_pcm_frames_s32__ima(pWav, framesToRead, pBufferOut);
}
return 0;
}
DRWAV_API drwav_uint64 drwav_read_pcm_frames_s32le(drwav* pWav, drwav_uint64 framesToRead, drwav_int32* pBufferOut)
{
drwav_uint64 framesRead = drwav_read_pcm_frames_s32(pWav, framesToRead, pBufferOut);
if (pBufferOut != NULL && drwav__is_little_endian() == DRWAV_FALSE) {
drwav__bswap_samples_s32(pBufferOut, framesRead*pWav->channels);
}
return framesRead;
}
DRWAV_API drwav_uint64 drwav_read_pcm_frames_s32be(drwav* pWav, drwav_uint64 framesToRead, drwav_int32* pBufferOut)
{
drwav_uint64 framesRead = drwav_read_pcm_frames_s32(pWav, framesToRead, pBufferOut);
if (pBufferOut != NULL && drwav__is_little_endian() == DRWAV_TRUE) {
drwav__bswap_samples_s32(pBufferOut, framesRead*pWav->channels);
}
return framesRead;
}
DRWAV_API void drwav_u8_to_s32(drwav_int32* pOut, const drwav_uint8* pIn, size_t sampleCount)
{
size_t i;
if (pOut == NULL || pIn == NULL) {
return;
}
for (i = 0; i < sampleCount; ++i) {
*pOut++ = ((int)pIn[i] - 128) << 24;
}
}
DRWAV_API void drwav_s16_to_s32(drwav_int32* pOut, const drwav_int16* pIn, size_t sampleCount)
{
size_t i;
if (pOut == NULL || pIn == NULL) {
return;
}
for (i = 0; i < sampleCount; ++i) {
*pOut++ = pIn[i] << 16;
}
}
DRWAV_API void drwav_s24_to_s32(drwav_int32* pOut, const drwav_uint8* pIn, size_t sampleCount)
{
size_t i;
if (pOut == NULL || pIn == NULL) {
return;
}
for (i = 0; i < sampleCount; ++i) {
unsigned int s0 = pIn[i*3 + 0];
unsigned int s1 = pIn[i*3 + 1];
unsigned int s2 = pIn[i*3 + 2];
drwav_int32 sample32 = (drwav_int32)((s0 << 8) | (s1 << 16) | (s2 << 24));
*pOut++ = sample32;
}
}
DRWAV_API void drwav_f32_to_s32(drwav_int32* pOut, const float* pIn, size_t sampleCount)
{
size_t i;
if (pOut == NULL || pIn == NULL) {
return;
}
for (i = 0; i < sampleCount; ++i) {
*pOut++ = (drwav_int32)(2147483648.0 * pIn[i]);
}
}
DRWAV_API void drwav_f64_to_s32(drwav_int32* pOut, const double* pIn, size_t sampleCount)
{
size_t i;
if (pOut == NULL || pIn == NULL) {
return;
}
for (i = 0; i < sampleCount; ++i) {
*pOut++ = (drwav_int32)(2147483648.0 * pIn[i]);
}
}
DRWAV_API void drwav_alaw_to_s32(drwav_int32* pOut, const drwav_uint8* pIn, size_t sampleCount)
{
size_t i;
if (pOut == NULL || pIn == NULL) {
return;
}
for (i = 0; i < sampleCount; ++i) {
*pOut++ = ((drwav_int32)drwav__alaw_to_s16(pIn[i])) << 16;
}
}
DRWAV_API void drwav_mulaw_to_s32(drwav_int32* pOut, const drwav_uint8* pIn, size_t sampleCount)
{
size_t i;
if (pOut == NULL || pIn == NULL) {
return;
}
for (i= 0; i < sampleCount; ++i) {
*pOut++ = ((drwav_int32)drwav__mulaw_to_s16(pIn[i])) << 16;
}
}
static drwav_int16* drwav__read_pcm_frames_and_close_s16(drwav* pWav, unsigned int* channels, unsigned int* sampleRate, drwav_uint64* totalFrameCount)
{
drwav_uint64 sampleDataSize;
drwav_int16* pSampleData;
drwav_uint64 framesRead;
DRWAV_ASSERT(pWav != NULL);
sampleDataSize = pWav->totalPCMFrameCount * pWav->channels * sizeof(drwav_int16);
if (sampleDataSize > DRWAV_SIZE_MAX) {
drwav_uninit(pWav);
return NULL;
}
pSampleData = (drwav_int16*)drwav__malloc_from_callbacks((size_t)sampleDataSize, &pWav->allocationCallbacks);
if (pSampleData == NULL) {
drwav_uninit(pWav);
return NULL;
}
framesRead = drwav_read_pcm_frames_s16(pWav, (size_t)pWav->totalPCMFrameCount, pSampleData);
if (framesRead != pWav->totalPCMFrameCount) {
drwav__free_from_callbacks(pSampleData, &pWav->allocationCallbacks);
drwav_uninit(pWav);
return NULL;
}
drwav_uninit(pWav);
if (sampleRate) {
*sampleRate = pWav->sampleRate;
}
if (channels) {
*channels = pWav->channels;
}
if (totalFrameCount) {
*totalFrameCount = pWav->totalPCMFrameCount;
}
return pSampleData;
}
static float* drwav__read_pcm_frames_and_close_f32(drwav* pWav, unsigned int* channels, unsigned int* sampleRate, drwav_uint64* totalFrameCount)
{
drwav_uint64 sampleDataSize;
float* pSampleData;
drwav_uint64 framesRead;
DRWAV_ASSERT(pWav != NULL);
sampleDataSize = pWav->totalPCMFrameCount * pWav->channels * sizeof(float);
if (sampleDataSize > DRWAV_SIZE_MAX) {
drwav_uninit(pWav);
return NULL;
}
pSampleData = (float*)drwav__malloc_from_callbacks((size_t)sampleDataSize, &pWav->allocationCallbacks);
if (pSampleData == NULL) {
drwav_uninit(pWav);
return NULL;
}
framesRead = drwav_read_pcm_frames_f32(pWav, (size_t)pWav->totalPCMFrameCount, pSampleData);
if (framesRead != pWav->totalPCMFrameCount) {
drwav__free_from_callbacks(pSampleData, &pWav->allocationCallbacks);
drwav_uninit(pWav);
return NULL;
}
drwav_uninit(pWav);
if (sampleRate) {
*sampleRate = pWav->sampleRate;
}
if (channels) {
*channels = pWav->channels;
}
if (totalFrameCount) {
*totalFrameCount = pWav->totalPCMFrameCount;
}
return pSampleData;
}
static drwav_int32* drwav__read_pcm_frames_and_close_s32(drwav* pWav, unsigned int* channels, unsigned int* sampleRate, drwav_uint64* totalFrameCount)
{
drwav_uint64 sampleDataSize;
drwav_int32* pSampleData;
drwav_uint64 framesRead;
DRWAV_ASSERT(pWav != NULL);
sampleDataSize = pWav->totalPCMFrameCount * pWav->channels * sizeof(drwav_int32);
if (sampleDataSize > DRWAV_SIZE_MAX) {
drwav_uninit(pWav);
return NULL;
}
pSampleData = (drwav_int32*)drwav__malloc_from_callbacks((size_t)sampleDataSize, &pWav->allocationCallbacks);
if (pSampleData == NULL) {
drwav_uninit(pWav);
return NULL;
}
framesRead = drwav_read_pcm_frames_s32(pWav, (size_t)pWav->totalPCMFrameCount, pSampleData);
if (framesRead != pWav->totalPCMFrameCount) {
drwav__free_from_callbacks(pSampleData, &pWav->allocationCallbacks);
drwav_uninit(pWav);
return NULL;
}
drwav_uninit(pWav);
if (sampleRate) {
*sampleRate = pWav->sampleRate;
}
if (channels) {
*channels = pWav->channels;
}
if (totalFrameCount) {
*totalFrameCount = pWav->totalPCMFrameCount;
}
return pSampleData;
}
DRWAV_API drwav_int16* drwav_open_and_read_pcm_frames_s16(drwav_read_proc onRead, drwav_seek_proc onSeek, void* pUserData, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks)
{
drwav wav;
if (channelsOut) {
*channelsOut = 0;
}
if (sampleRateOut) {
*sampleRateOut = 0;
}
if (totalFrameCountOut) {
*totalFrameCountOut = 0;
}
if (!drwav_init(&wav, onRead, onSeek, pUserData, pAllocationCallbacks)) {
return NULL;
}
return drwav__read_pcm_frames_and_close_s16(&wav, channelsOut, sampleRateOut, totalFrameCountOut);
}
DRWAV_API float* drwav_open_and_read_pcm_frames_f32(drwav_read_proc onRead, drwav_seek_proc onSeek, void* pUserData, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks)
{
drwav wav;
if (channelsOut) {
*channelsOut = 0;
}
if (sampleRateOut) {
*sampleRateOut = 0;
}
if (totalFrameCountOut) {
*totalFrameCountOut = 0;
}
if (!drwav_init(&wav, onRead, onSeek, pUserData, pAllocationCallbacks)) {
return NULL;
}
return drwav__read_pcm_frames_and_close_f32(&wav, channelsOut, sampleRateOut, totalFrameCountOut);
}
DRWAV_API drwav_int32* drwav_open_and_read_pcm_frames_s32(drwav_read_proc onRead, drwav_seek_proc onSeek, void* pUserData, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks)
{
drwav wav;
if (channelsOut) {
*channelsOut = 0;
}
if (sampleRateOut) {
*sampleRateOut = 0;
}
if (totalFrameCountOut) {
*totalFrameCountOut = 0;
}
if (!drwav_init(&wav, onRead, onSeek, pUserData, pAllocationCallbacks)) {
return NULL;
}
return drwav__read_pcm_frames_and_close_s32(&wav, channelsOut, sampleRateOut, totalFrameCountOut);
}
#ifndef DR_WAV_NO_STDIO
DRWAV_API drwav_int16* drwav_open_file_and_read_pcm_frames_s16(const char* filename, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks)
{
drwav wav;
if (channelsOut) {
*channelsOut = 0;
}
if (sampleRateOut) {
*sampleRateOut = 0;
}
if (totalFrameCountOut) {
*totalFrameCountOut = 0;
}
if (!drwav_init_file(&wav, filename, pAllocationCallbacks)) {
return NULL;
}
return drwav__read_pcm_frames_and_close_s16(&wav, channelsOut, sampleRateOut, totalFrameCountOut);
}
DRWAV_API float* drwav_open_file_and_read_pcm_frames_f32(const char* filename, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks)
{
drwav wav;
if (channelsOut) {
*channelsOut = 0;
}
if (sampleRateOut) {
*sampleRateOut = 0;
}
if (totalFrameCountOut) {
*totalFrameCountOut = 0;
}
if (!drwav_init_file(&wav, filename, pAllocationCallbacks)) {
return NULL;
}
return drwav__read_pcm_frames_and_close_f32(&wav, channelsOut, sampleRateOut, totalFrameCountOut);
}
DRWAV_API drwav_int32* drwav_open_file_and_read_pcm_frames_s32(const char* filename, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks)
{
drwav wav;
if (channelsOut) {
*channelsOut = 0;
}
if (sampleRateOut) {
*sampleRateOut = 0;
}
if (totalFrameCountOut) {
*totalFrameCountOut = 0;
}
if (!drwav_init_file(&wav, filename, pAllocationCallbacks)) {
return NULL;
}
return drwav__read_pcm_frames_and_close_s32(&wav, channelsOut, sampleRateOut, totalFrameCountOut);
}
DRWAV_API drwav_int16* drwav_open_file_and_read_pcm_frames_s16_w(const wchar_t* filename, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks)
{
drwav wav;
if (sampleRateOut) {
*sampleRateOut = 0;
}
if (channelsOut) {
*channelsOut = 0;
}
if (totalFrameCountOut) {
*totalFrameCountOut = 0;
}
if (!drwav_init_file_w(&wav, filename, pAllocationCallbacks)) {
return NULL;
}
return drwav__read_pcm_frames_and_close_s16(&wav, channelsOut, sampleRateOut, totalFrameCountOut);
}
DRWAV_API float* drwav_open_file_and_read_pcm_frames_f32_w(const wchar_t* filename, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks)
{
drwav wav;
if (sampleRateOut) {
*sampleRateOut = 0;
}
if (channelsOut) {
*channelsOut = 0;
}
if (totalFrameCountOut) {
*totalFrameCountOut = 0;
}
if (!drwav_init_file_w(&wav, filename, pAllocationCallbacks)) {
return NULL;
}
return drwav__read_pcm_frames_and_close_f32(&wav, channelsOut, sampleRateOut, totalFrameCountOut);
}
DRWAV_API drwav_int32* drwav_open_file_and_read_pcm_frames_s32_w(const wchar_t* filename, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks)
{
drwav wav;
if (sampleRateOut) {
*sampleRateOut = 0;
}
if (channelsOut) {
*channelsOut = 0;
}
if (totalFrameCountOut) {
*totalFrameCountOut = 0;
}
if (!drwav_init_file_w(&wav, filename, pAllocationCallbacks)) {
return NULL;
}
return drwav__read_pcm_frames_and_close_s32(&wav, channelsOut, sampleRateOut, totalFrameCountOut);
}
#endif
DRWAV_API drwav_int16* drwav_open_memory_and_read_pcm_frames_s16(const void* data, size_t dataSize, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks)
{
drwav wav;
if (channelsOut) {
*channelsOut = 0;
}
if (sampleRateOut) {
*sampleRateOut = 0;
}
if (totalFrameCountOut) {
*totalFrameCountOut = 0;
}
if (!drwav_init_memory(&wav, data, dataSize, pAllocationCallbacks)) {
return NULL;
}
return drwav__read_pcm_frames_and_close_s16(&wav, channelsOut, sampleRateOut, totalFrameCountOut);
}
DRWAV_API float* drwav_open_memory_and_read_pcm_frames_f32(const void* data, size_t dataSize, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks)
{
drwav wav;
if (channelsOut) {
*channelsOut = 0;
}
if (sampleRateOut) {
*sampleRateOut = 0;
}
if (totalFrameCountOut) {
*totalFrameCountOut = 0;
}
if (!drwav_init_memory(&wav, data, dataSize, pAllocationCallbacks)) {
return NULL;
}
return drwav__read_pcm_frames_and_close_f32(&wav, channelsOut, sampleRateOut, totalFrameCountOut);
}
DRWAV_API drwav_int32* drwav_open_memory_and_read_pcm_frames_s32(const void* data, size_t dataSize, unsigned int* channelsOut, unsigned int* sampleRateOut, drwav_uint64* totalFrameCountOut, const drwav_allocation_callbacks* pAllocationCallbacks)
{
drwav wav;
if (channelsOut) {
*channelsOut = 0;
}
if (sampleRateOut) {
*sampleRateOut = 0;
}
if (totalFrameCountOut) {
*totalFrameCountOut = 0;
}
if (!drwav_init_memory(&wav, data, dataSize, pAllocationCallbacks)) {
return NULL;
}
return drwav__read_pcm_frames_and_close_s32(&wav, channelsOut, sampleRateOut, totalFrameCountOut);
}
#endif
DRWAV_API void drwav_free(void* p, const drwav_allocation_callbacks* pAllocationCallbacks)
{
if (pAllocationCallbacks != NULL) {
drwav__free_from_callbacks(p, pAllocationCallbacks);
} else {
drwav__free_default(p, NULL);
}
}
DRWAV_API drwav_uint16 drwav_bytes_to_u16(const drwav_uint8* data)
{
return drwav__bytes_to_u16(data);
}
DRWAV_API drwav_int16 drwav_bytes_to_s16(const drwav_uint8* data)
{
return drwav__bytes_to_s16(data);
}
DRWAV_API drwav_uint32 drwav_bytes_to_u32(const drwav_uint8* data)
{
return drwav__bytes_to_u32(data);
}
DRWAV_API drwav_int32 drwav_bytes_to_s32(const drwav_uint8* data)
{
return drwav__bytes_to_s32(data);
}
DRWAV_API drwav_uint64 drwav_bytes_to_u64(const drwav_uint8* data)
{
return drwav__bytes_to_u64(data);
}
DRWAV_API drwav_int64 drwav_bytes_to_s64(const drwav_uint8* data)
{
return drwav__bytes_to_s64(data);
}
DRWAV_API drwav_bool32 drwav_guid_equal(const drwav_uint8 a[16], const drwav_uint8 b[16])
{
return drwav__guid_equal(a, b);
}
DRWAV_API drwav_bool32 drwav_fourcc_equal(const drwav_uint8* a, const char* b)
{
return drwav__fourcc_equal(a, b);
}
#endif
#endif