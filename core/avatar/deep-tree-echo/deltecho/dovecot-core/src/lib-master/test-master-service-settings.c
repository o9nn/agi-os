#include "lib.h"
#include "write-full.h"
#include "env-util.h"
#include "master-interface.h"
#include "master-service.h"
#include "master-service-settings.h"
#include "test-common.h"
#define DATA(data) (const unsigned char *)data"\xff", sizeof(data"\xff")-2
static const struct {
const unsigned char *data;
size_t size;
const char *error;
} tests[] = {
{ DATA("D"),
"File header doesn't begin with DOVECOT-CONFIG line" },
{ DATA("DOVECOT-CONFIG\t"),
"File header doesn't begin with DOVECOT-CONFIG line" },
{ DATA("DOVECOT-CONFIG\t1.0"),
"File header doesn't begin with DOVECOT-CONFIG line" },
{ DATA("DOVECOT-CONFIG\t2.3\n"),
"Unsupported config file version '2.3'" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x01"),
"Full size mismatch" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x04"
"\x00\x00\x00"),
"Full size mismatch" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x04"
"\x00\x00\x10\x00"),
"'filter string' points outside area" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x0C"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00"),
"Area too small when reading size of 'block size'" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x0D"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x00"),
"'block name' points outside area" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x0D"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x01"),
"'block size' points outside are" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x0F"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x01"
"N"
"\x00"),
"Settings block doesn't end with NUL at offset" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x12"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x05"
"N\x00"
"\x00\x00\x00"),
"Area too small when reading uint of 'settings count'" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x13"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x06"
"N\x00"
"\x00\x00\x01\x00"),
"'setting key' points outside area" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x1C"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x0F"
"N\x00"
"\x00\x00\x00\x01"
"K\x00"
"\x00\x00\x00\x00\x00\x00\x00"),
"Area too small when reading size of 'base settings size'" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x1D"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x10"
"N\x00"
"\x00\x00\x00\x01"
"K\x00"
"\x00\x00\x00\x00\x00\x00\x00\x00"),
"'base settings error' points outside area" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x1F"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x12"
"N\x00"
"\x00\x00\x00\x01"
"K\x00"
"\x00\x00\x00\x00\x00\x00\x00\x01"
"E"
"\x00"),
"'base settings error' points outside area" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x21"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x14"
"N\x00"
"\x00\x00\x00\x01"
"K\x00"
"\x00\x00\x00\x00\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00"),
"Area too small when reading uint of 'filter count'" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x29"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x1B"
"N\x00"
"\x00\x00\x00\x01"
"K\x00"
"\x00\x00\x00\x00\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x01"
"\x00\x00\x00\x00\x00\x00\x00"),
"Area too small when reading size of 'filter settings size'" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x2A"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x1D"
"N\x00"
"\x00\x00\x00\x01"
"K\x00"
"\x00\x00\x00\x00\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x01"
"\x00\x00\x00\x00\x00\x00\x10\x00"),
"'filter settings size' points outside area" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x37"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x2A"
"N\x00"
"\x00\x00\x00\x01"
"K\x00"
"\x00\x00\x00\x00\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x01"
"\x00\x00\x00\x00\x00\x00\x00\x00"
"\x00\x00\x00\x00"
"\x00\x00\x00\x00\x00\x00\x00\x00"
"\x00"),
"'filter error string' points outside area" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x45"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x38"
"master_service\x00"
"\x00\x00\x00\x01"
"K\x00"
"\x00\x00\x00\x00\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x01"
"\x00\x00\x00\x00\x00\x00\x00\x01"
"E"
"\x00\x00\x00\x00"
"\x00\x00\x00\x00\x00\x00\x00\x00"
"\x00"),
"'filter error string' points outside area" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x39"
"\x00\x00\x00\x01"
"F\x00"
"\x00\x00\x00\x00\x00\x00\x00\x2B"
"N\x00"
"\x00\x00\x00\x01"
"K\x00"
"\x00\x00\x00\x00\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x01"
"\x00\x00\x00\x00\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00"
"\x00\x00\x00\x00\x00\x00\x00\x00"
"\x00"),
"Received invalid filter 'F' at index 0: event filter: syntax error" },
{ DATA("DOVECOT-CONFIG\t1.0\n"
"\x00\x00\x00\x00\x00\x00\x00\x42"
"\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x2B"
"N\x00"
"\x00\x00\x00\x01"
"K\x00"
"\x00\x00\x00\x00\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x01"
"\x00\x00\x00\x00\x00\x00\x00\x01"
"\x00"
"\x00\x00\x00\x00"
"\x00\x00\x00\x00\x00\x00\x00\x00"
"\x00"
"\x00\x00\x00\x00\x00\x00\x00\x02"
"N\x00"),
"Duplicate block name 'N'" },
};
static int test_input_to_fd(const unsigned char *data, size_t size)
{
int fd = test_create_temp_fd();
if (write_full(fd, data, size) < 0)
i_fatal("write(temp file) failed: %m");
if (lseek(fd, 0, SEEK_SET) < 0)
i_fatal("lseek(temp file) failed: %m");
return fd;
}
static void test_master_service_settings_read_binary_corruption(void)
{
const char *error;
test_begin("master_service_settings_read() - binary corruption");
for (unsigned int i = 0; i < N_ELEMENTS(tests); i++) {
struct master_service_settings_input input = {
.config_fd = test_input_to_fd(tests[i].data, tests[i].size),
.no_key_validation = TRUE,
};
struct master_service_settings_output output;
test_assert_idx(master_service_settings_read(master_service,
&input, &output, &error) == -1, i);
test_assert_idx(strstr(error, tests[i].error) != NULL, i);
if (strstr(error, tests[i].error) == NULL)
i_error("%s", error);
}
test_end();
}
int main(int argc, char *argv[])
{
static void (*const test_functions[])(void) = {
test_master_service_settings_read_binary_corruption,
NULL
};
const enum master_service_flags service_flags =
MASTER_SERVICE_FLAG_STANDALONE |
MASTER_SERVICE_FLAG_DONT_SEND_STATS |
MASTER_SERVICE_FLAG_NO_SSL_INIT;
master_service = master_service_init("test-master-service-settings",
service_flags, &argc, &argv, "");
int ret = test_run(test_functions);
master_service_deinit(&master_service);
return ret;
}