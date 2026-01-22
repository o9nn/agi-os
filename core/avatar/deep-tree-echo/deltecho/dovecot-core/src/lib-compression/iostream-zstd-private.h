#ifndef IOSTREAM_ZSTD_PRIVATE_H
#define IOSTREAM_ZSTD_PRIVATE_H 1
static inline ZSTD_ErrorCode zstd_version_errcode(ZSTD_ErrorCode err)
{
#if ZSTD_VERSION_NUMBER < 10301
if (ZSTD_versionNumber() > 10300) {
if (err == 10)
return ZSTD_error_prefix_unknown;
if (err == 32)
return ZSTD_error_dictionary_wrong;
if (err == 62)
return ZSTD_error_init_missing;
if (err == 64)
return ZSTD_error_memory_allocation;
return ZSTD_error_GENERIC;
}
#endif
return err;
}
static inline void zstd_version_check(void)
{
if (ZSTD_VERSION_NUMBER < 10301 || ZSTD_versionNumber() < 10301)
if (ZSTD_versionNumber() / 100 != ZSTD_VERSION_NUMBER / 100)
i_warning("zstd: Compiled against %u, but %u installed!",
ZSTD_VERSION_NUMBER, ZSTD_versionNumber());
}
#endif