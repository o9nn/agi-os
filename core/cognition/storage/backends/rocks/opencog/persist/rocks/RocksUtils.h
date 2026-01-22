#define CHECK_OPEN \
if (nullptr == _rfile) \
throw IOException(TRACE_INFO, "RocksDB is not open! %s", \
_name.c_str());