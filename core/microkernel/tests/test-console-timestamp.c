#include <testlib.h>
int main(int argc, char *argv[], int envc, char *envp[])
{
printf("=== Enhanced Console Timestamp Feature Test ===\n");
ASSERT(console_timestamp_is_enabled(), "Timestamps should be enabled by default");
ASSERT(console_timestamp_get_format() == TIMESTAMP_FORMAT_RELATIVE,
"Default format should be TIMESTAMP_FORMAT_RELATIVE");
printf("Testing basic message output\n");
printf("Multiple line output:\n");
printf("Line 1\n");
printf("Line 2\n");
printf("Line 3\n");
printf("Testing TIMESTAMP_FORMAT_SIMPLE...\n");
console_timestamp_set_format(TIMESTAMP_FORMAT_SIMPLE);
ASSERT(console_timestamp_get_format() == TIMESTAMP_FORMAT_SIMPLE,
"Format should be set to SIMPLE");
printf("Message with simple timestamp format\n");
printf("Testing TIMESTAMP_FORMAT_PRECISE...\n");
console_timestamp_set_format(TIMESTAMP_FORMAT_PRECISE);
ASSERT(console_timestamp_get_format() == TIMESTAMP_FORMAT_PRECISE,
"Format should be set to PRECISE");
printf("Message with precise timestamp format (microseconds)\n");
printf("Testing TIMESTAMP_FORMAT_UPTIME...\n");
console_timestamp_set_format(TIMESTAMP_FORMAT_UPTIME);
ASSERT(console_timestamp_get_format() == TIMESTAMP_FORMAT_UPTIME,
"Format should be set to UPTIME");
printf("Message with absolute uptime format\n");
console_timestamp_set_format(TIMESTAMP_FORMAT_RELATIVE);
printf("Reset to default format\n");
printf("Disabling timestamps...\n");
console_timestamp_enable(FALSE);
ASSERT(!console_timestamp_is_enabled(), "Timestamps should be disabled");
printf("This message should have no timestamp\n");
printf("Neither should this one\n");
printf("Re-enabling timestamps...\n");
console_timestamp_enable(TRUE);
ASSERT(console_timestamp_is_enabled(), "Timestamps should be re-enabled");
printf("This message should have timestamps again\n");
printf("And so should this one\n");
time_value64_t boot_time;
console_timestamp_get_boot_time(&boot_time);
printf("Boot time recorded: %d.%09d seconds\n",
(int)boot_time.seconds, (int)boot_time.nanoseconds);
printf("Testing mixed content: ");
printf("same line continuation\n");
printf("Boot parameters supported:\n");
printf("  notimestamps - disable completely\n");
printf("  console_timestamps=off/on - explicit control\n");
printf("  timestamp_format=simple/precise/uptime - format selection\n");
printf("%s: %s\n", TEST_SUCCESS_MARKER, "Enhanced console timestamp test completed successfully");
return 0;
}