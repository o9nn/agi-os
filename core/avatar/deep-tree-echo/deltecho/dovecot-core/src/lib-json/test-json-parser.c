#include "lib.h"
#include "str.h"
#include "istream.h"
#include "ostream.h"
#include "istream-base64.h"
#include "test-common.h"
#include "json-parser.h"
#include <unistd.h>
static bool debug = FALSE;
struct json_valid_parse_test {
const char *input;
struct json_limits limits;
enum json_parser_flags flags;
};
static const struct json_valid_parse_test
valid_parse_tests[] = {
{
.input = "[[]   ]",
},
{
.input = "[]",
},
{
.input = "[\"\"]",
},
{
.input = "[\"a\"]",
},
{
.input = "[false]",
},
{
.input = "[null, 1, \"1\", {}]",
},
{
.input = "[null]",
},
{
.input = "[1\n"
"]",
},
{
.input = " [1]",
},
{
.input = "[1,null,null,null,2]",
},
{
.input = "[2] ",
},
{
.input = "[0e+1]",
},
{
.input = "[0e1]",
},
{
.input = "[ 4]",
},
{
.input = "[-0.000000000000000000000000000000000000"
"000000000000000000000000000000000000000001]\n",
},
{
.input = "[20e1]",
},
{
.input = "[123e65]",
.flags = JSON_PARSER_FLAG_NUMBERS_AS_STRING,
},
{
.input = "[-0]",
},
{
.input = "[-123]",
},
{
.input = "[-1]",
},
{
.input = "[-0]",
},
{
.input = "[1E22]",
.flags = JSON_PARSER_FLAG_NUMBERS_AS_STRING,
},
{
.input = "[1E-2]",
},
{
.input = "[1E+2]",
},
{
.input = "[123e45]",
.flags = JSON_PARSER_FLAG_NUMBERS_AS_STRING,
},
{
.input = "[123.456e78]",
.flags = JSON_PARSER_FLAG_NUMBERS_AS_STRING,
},
{
.input = "[1e-2]",
},
{
.input = "[1e+2]",
},
{
.input = "[123]",
},
{
.input = "[123.456789]",
},
{
.input = "{\"asd\":\"sdf\"}",
},
{
.input = "{\"a\":\"b\",\"a\":\"b\"}",
},
{
.input = "{\"a\":\"b\",\"a\":\"c\"}",
},
{
.input = "{}",
},
{
.input = "{\"\":0}",
},
{
.input = "{ \"min\": -1.0e+28, \"max\": 1.0e+28 }",
.flags = JSON_PARSER_FLAG_NUMBERS_AS_STRING,
},
{
.input = "{\"asd\":\"sdf\", \"dfg\":\"fgh\"}",
},
{
.input = "{\"x\":[{\"id\": "
"\"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}], "
"\"id\": "
"\"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}",
},
{
.input = "{\"a\":[]}",
},
{
.input = "{\"title\":"
"\"\\u041f\\u043e\\u043b\\u0442\\u043e\\u0440\\u0430 "
"\\u0417\\u0435\\u043c\\u043b\\u0435\\u043a\\u043e"
"\\u043f\\u0430\" }",
},
{
.input = "{\n"
"\"a\": \"b\"\n"
"}",
},
{
.input = "[\"\\u0060\\u012a\\u12AB\"]",
},
{
.input = "[\"\\uD801\\udc37\"]",
},
{
.input = "[\"\\ud83d\\ude39\\ud83d\\udc8d\"]",
},
{
.input = "[\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"]",
},
{
.input = "[\"\\\\u0000\"]",
},
{
.input = "[\"\\\"\"]",
},
{
.input = "[\"ac
{
.input = "[\n"
"    \"JSON Test Pattern pass1\",\n"
"    {\"object with 1 member\":[\"array with 1 element\"]},\n"
"    {},\n"
"    [],\n"
"    -42,\n"
"    true,\n"
"    false,\n"
"    null,\n"
"    {\n"
"        \"integer\": 1234567890,\n"
"        \"real\": -9876.543210,\n"
"        \"e\": 0.123456789e-12,\n"
"        \"E\": 1.234567890E+34,\n"
"        \"\":  23456789012E66,\n"
"        \"zero\": 0,\n"
"        \"one\": 1,\n"
"        \"space\": \" \",\n"
"        \"quote\": \"\\\"\",\n"
"        \"backslash\": \"\\\\\",\n"
"        \"controls\": \"\\b\\f\\n\\r\\t\",\n"
"        \"slash\": \"/ & \\/\",\n"
"        \"alpha\": \"abcdefghijklmnopqrstuvwyz\",\n"
"        \"ALPHA\": \"ABCDEFGHIJKLMNOPQRSTUVWYZ\",\n"
"        \"digit\": \"0123456789\",\n"
"        \"0123456789\": \"digit\",\n"
"        \"special\": \"`1~!@#$%^&*()_+-={':[,]}|;.</>?\",\n"
"        \"hex\": \"\\u0123\\u4567\\u89AB\\uCDEF\\uabcd\\uef4A\",\n"
"        \"true\": true,\n"
"        \"false\": false,\n"
"        \"null\": null,\n"
"        \"array\":[  ],\n"
"        \"object\":{  },\n"
"        \"address\": \"50 St. James Street\",\n"
"        \"url\": \"http:
"        \"comment\": \"
"        \" s p a c e d \" :[1,2 , 3\n"
"\n"
",\n"
"\n"
"4 , 5        ,          6           ,7        ],"
"\"compact\":[1,2,3,4,5,6,7],\n"
"        \"jsontext\": \"{\\\"object with 1 member\\\":"
"[\\\"array with 1 element\\\"]}\",\n"
"        \"quotes\": \"&#34; \\u0022 %22 0x22 034 &#x22;\",\n"
"        \"\\/\\\\\\\"\\uCAFE\\uBABE\\uAB98\\uFCDE\\ubcda\\uef4A"
"\\b\\f\\n\\r\\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?\"\n"
": \"A key can be any string\"\n"
"    },\n"
"    0.5 ,98.6\n"
",\n"
"99.44\n"
",\n"
"\n"
"1066,\n"
"1e1,\n"
"0.1e1,\n"
"1e-1,\n"
"1e00,2e+00,2e-00\n"
",\"rosebud\"]",
.flags = JSON_PARSER_FLAG_NUMBERS_AS_STRING,
},
{
.input =
"[[[[[[[[[[[[[[[[[[[\"Not too deep\"]]]]]]]]]]]]]]]]]]]",
},
{
.input =
"{\n"
"    \"JSON Test Pattern pass3\": {\n"
"        \"The outermost value\": \"must be an object or array.\",\n"
"        \"In this test\": \"It is an object.\"\n"
"    }\n"
"}\n",
},
{
.input = "[\"\\uD834\\uDD1E surrogate, four-byte UTF-8\"]\n",
},
{
.input = "[1.8011670033376514e-308]\n",
},
{
.input = "[{}]\n",
},
{
.input = "[\"\\u002c one-byte UTF-8\"]\n",
},
{
.input = "[\"\\u0123 two-byte UTF-8\"]\n",
},
{
.input = "[1e+2]\n",
},
{
.input = "[-0]\n",
},
{
.input = "[1]\n",
},
{
.input = "[\"\\u0012 escaped control character\"]\n",
},
{
.input = "[\"\\u0821 three-byte UTF-8\"]\n",
},
{
.input = "{}\n",
},
{
.input = "[\"\"]\n",
},
{
.input = "[123e45]\n",
.flags = JSON_PARSER_FLAG_NUMBERS_AS_STRING,
},
{
.input = "[\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"]\n",
},
{
.input = "[\"abcdefghijklmnopqrstuvwxyz1234567890 \"]\n",
},
{
.input = "[1e-2]\n",
},
{
.input = "[123e-10000000]\n",
},
{
.input = "[null]\n",
},
{
.input = "[123.456e78]\n",
.flags = JSON_PARSER_FLAG_NUMBERS_AS_STRING,
},
{
.input = "[true]\n",
},
{
.input = "{\"a\":[]}\n",
},
{
.input = "[1E-2]\n",
},
{
.input = "[]\n",
},
{
.input = "[-1]\n",
},
{
.input = "[\"a\"]\n",
},
{
.input = "[123]\n",
},
{
.input = "[false]\n",
},
{
.input = "[0]\n",
},
{
.input = "[1E22]\n",
.flags = JSON_PARSER_FLAG_NUMBERS_AS_STRING,
},
{
.input = "[1,2,3,4,\n"
"\"a\", \"b\", \"c\",\n"
"{\"foo\": \"bar\", \"core\": \"dump\"},\n"
"true, false, true, true, null, false\n"
"]\n",
},
{
.input = "[1E+2]\n",
},
{
.input = "[-123]\n",
},
{
.input = "[\"\xe2\x82\xac\xc3\xbe\xc4\xb1\xc5\x93\xc9"
"\x99\xc3\x9f\xc3\xb0 some utf-8 \xc4\xb8\xca\x92"
"\xc3\x97\xc5\x8b\xc2\xb5\xc3\xa5\xc3\xa4\xc3\xb6"
"\xf0\x9d\x84\x9e\"]\n",
},
{
.input = "[123.456789]\n",
},
{
.input =
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]",
},
{
.input =
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
"]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
"]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]",
.limits = { .max_nesting = 105 },
},
{
.input =
"[1,2,3,4,5,6,7,8,9,0,\n"
" 1,2,3,4,5,6,7,8,9,0,\n"
" 1,2,3,4,5,6,7,8,9,0,\n"
" 1,2,3,4,5,6,7,8,9,0,\n"
" 1,2,3,4,5,6,7,8,9,0]\n",
.limits = { .max_list_items = 50 },
},
{
.input =
"\"123456789012345678901234567890"
"123456789012345678901234567890"
"123456789012345678901234567890\"",
.limits = { .max_string_size = 90 },
},
{
.input =
"123456789012345678901234567890"
"123456789012345678901234567890"
"123456789012345678901234567890",
.flags = JSON_PARSER_FLAG_NUMBERS_AS_STRING,
.limits = { .max_string_size = 90 },
},
{
.input =
"{\"123456789012345678901234567890"
"123456789012345678901234567890"
"123456789012345678901234567890\": 90}",
.limits = { .max_name_size = 90 },
},
{
.input = "0e11111111111111110",
.flags = JSON_PARSER_FLAG_STRICT,
},
};
static const unsigned int valid_parse_test_count =
N_ELEMENTS(valid_parse_tests);
static void test_json_parse_valid(void)
{
unsigned int i;
for (i = 0; i < valid_parse_test_count; i++) T_BEGIN {
const struct json_valid_parse_test *test;
struct istream *input;
struct json_parser *parser;
const char *text, *error = NULL;
unsigned int pos, text_len;
int ret = 0;
test = &valid_parse_tests[i];
text = test->input;
text_len = strlen(text);
input = test_istream_create_data(text, text_len);
test_begin(t_strdup_printf("json text valid [%d]", i));
parser = json_parser_init(input,
&test->limits, test->flags, NULL, NULL);
for (pos = 0; pos <= text_len && ret == 0; pos++) {
test_istream_set_size(input, pos);
ret = json_parse_more(parser, &error);
if (ret < 0) {
if (debug)
i_debug("DATA: `%s'", text);
break;
}
}
test_out_reason_quiet("parse success (trickle)",
ret > 0, error);
json_parser_deinit(&parser);
i_stream_seek(input, 0);
parser = json_parser_init(input,
&test->limits, test->flags, NULL, NULL);
test_istream_set_size(input, text_len);
ret = json_parse_more(parser, &error);
if (ret < 0) {
if (debug)
i_debug("DATA: `%s'", text);
}
test_out_reason_quiet("parse success (buffered)",
ret > 0, error);
json_parser_deinit(&parser);
test_end();
i_stream_unref(&input);
} T_END;
}
struct json_invalid_parse_test {
const char *input;
size_t input_len;
struct json_limits limits;
enum json_parser_flags flags;
bool base64;
};
static const struct json_invalid_parse_test
invalid_parse_tests[] = {
{
.input = "[1 true]",
},
{
.input = "[a\xe5]",
},
{
.input = "[\"\": 1]",
},
{
.input = "[\"\"],",
},
{
.input = "[,1]",
},
{
.input = "[1,,2]",
},
{
.input = "[\"x\",,]",
},
{
.input = "[\"x\"]]",
},
{
.input = "[\"\",]",
},
{
.input = "[x",
},
{
.input = "[\"x\"",
},
{
.input = "[3[4]]",
},
{
.input = "[\xff]",
},
{
.input = "[1:2]",
},
{
.input = "[,]",
},
{
.input = "[-]",
},
{
.input = "[   , \"\"]",
},
{
.input = "[\"a\",\n"
"4\n"
",1,",
},
{
.input = "[1,]",
},
{
.input = "[1,,]",
},
{
.input = "[\"\va\"\\f]",
},
{
.input = "[*]",
},
{
.input = "[\"\"",
},
{
.input = "[1,",
},
{
.input = "[1,\n"
"1\n"
",1",
},
{
.input = "[{}",
},
{
.input = "[fals]",
},
{
.input = "[nul]",
},
{
.input = "[tru]",
},
{
.input = "123\x00",
.input_len = 4,
},
{
.input = "[0.1.2]",
},
{
.input = "[-01]",
},
{
.input = "[0.3e]",
},
{
.input = "[0.3e+]",
},
{
.input = "[0E]",
},
{
.input = "[0E+]",
},
{
.input = "[0.e1]",
},
{
.input = "[0e]",
},
{
.input = "[0e+]",
},
{
.input = "[1 000.0]",
},
{
.input = "[1.0e-]",
},
{
.input = "[1.0e]",
},
{
.input = "[1.0e+]",
},
{
.input = "[-1.0.]",
},
{
.input = "[1eE2]",
},
{
.input = "[.-1]",
},
{
.input = "[+1]",
},
{
.input = "[.2e-3]",
},
{
.input = "[2.e-3]",
},
{
.input = "[2.e+3]",
},
{
.input = "[2.e3]",
},
{
.input = "[-2.]",
},
{
.input = "[9.e+]",
},
{
.input = "[1+2]",
},
{
.input = "[0x1]",
},
{
.input = "[0x42]",
},
{
.input = "[Infinity]",
},
{
.input = "[+Inf]",
},
{
.input = "[Inf]",
},
{
.input = "[0e+-1]",
},
{
.input = "[-123.123foo]",
},
{
.input = "[123\xe5]",
},
{
.input = "[1e1\xe5]",
},
{
.input = "[0\xe5]\n",
},
{
.input = "[++1234]",
},
{
.input = "[-Infinity]",
},
{
.input = "[-foo]",
},
{
.input = "[- 1]",
},
{
.input = "[-NaN]",
},
{
.input = "[NaN]",
},
{
.input = "[-012]",
},
{
.input = "[-.123]",
},
{
.input = "[-1x]",
},
{
.input = "[1ea]",
},
{
.input = "[1e\xe5]",
},
{
.input = "[1.]",
},
{
.input = "[.123]",
},
{
.input = "[\xef\xbc\x91]",
},
{
.input = "[1.8011670033376514H-308]",
},
{
.input = "[1.2a-3]",
},
{
.input = "[012]",
},
{
.input = "[\"x\", truth]",
},
{
.input = "{[: \"x\"}\n",
},
{
.input = "{\"x\", null}",
},
{
.input = "{\"x\"::\"b\"}",
},
{
.input = "{\xf0\x9f\x87\xa8\xf0\x9f\x87\xad}",
},
{
.input = "{\"a\":\"a\" 123}",
},
{
.input = "{key: 'value'}",
},
{
.input = "{\"a\" b}",
},
{
.input = "{:\"b\"}",
},
{
.input = "{\"a\" \"b\"}",
},
{
.input = "{\"a\":",
},
{
.input = "{\"a\"",
},
{
.input = "{9999E9999:1}",
},
{
.input = "{1:1}",
},
{
.input = "{\"\xb9\":\"0\",}",
},
{
.input = "{null:null,null:null}",
},
{
.input = "{\"id\":0,,,,,}",
},
{
.input = "{'a':0}",
},
{
.input = "{\"id\":0,}",
},
{
.input = "{\"a\":\"b\"}",
},
{
.input = "{\"a\":\"b\"}/",
},
{
.input = "{\"a\":\"b\"}/",
},
{
.input = "{\"a\":\"b\"}
},
{
.input = "{\"a\":\"b\",,\"c\":\"d\"}",
},
{
.input = "{a: \"b\"}",
},
{
.input = "{\"a\":\"a",
},
{
.input = "{ \"foo\" : \"bar\", \"a\" }",
},
{
.input = "{\"a\":\"b\"}#",
},
{
.input = " ",
},
{
.input = "[\"\\uD800\\\"]",
},
{
.input = "[\"\\uD800\\u1\"]",
},
{
.input = "[\"\\uD800\\u1x\"]",
},
{
.input = "[\"\\uD800\\u\"]",
},
{
.input = "[\xc3\xa9]",
},
{
.input = "[\"\\\x00\"]",
},
{
.input = "[\"\\\\\\\"]",
},
{
.input = "[\"\\\t\"]",
},
{
.input = "[\"\\\xf0\x9f\x8c\x80\"]",
},
{
.input = "[\"\\x00\"]",
},
{
.input = "[\"\\u00A\"]",
},
{
.input = "[\"\\\"]",
},
{
.input = "[\"\\uD800\\uD800\\x\"]",
},
{
.input = "[\"\\uD834\\uDd\"]",
},
{
.input = "[\"\\a\"]",
},
{
.input = "[\"\\uqqqq\"]",
},
{
.input = "[\"\\\xe5\"]",
},
{
.input = "[\"\\u\xe5\"]",
},
{
.input = "[\\u0020\"asd\"]",
},
{
.input = "[\\n]",
},
{
.input = "\"",
},
{
.input = "['single quote']",
},
{
.input = "abc",
},
{
.input = "[\"\\",
},
{
.input = "[\"a\x00a\"]",
},
{
.input = "[\"new\n"
"line\"]",
},
{
.input = "[\"\t\"]",
},
{
.input = "\"\\UA66D\"",
},
{
.input = "\"\"x",
},
{
.input = "<.>",
},
{
.input = "[<null>]",
},
{
.input = "[1]x",
},
{
.input = "[1]]",
},
{
.input = "[\"asd]",
},
{
.input = "a\xc3\xa5",
},
{
.input = "[True]",
},
{
.input = "1]",
},
{
.input = "{\"x\": true,",
},
{
.input = "[][]",
},
{
.input = "]",
},
{
.input = "\xef\xbb{}",
},
{
.input = "\xe5",
},
{
.input = "[",
},
{
.input = "",
},
{
.input = "[\x00]",
},
{
.input = "2@",
},
{
.input = "{}}",
},
{
.input = "{\"\":",
},
{
.input = "{\"a\":\"b\"}",
},
{
.input = "{\"a\": true} \"x\"",
},
{
.input = "['",
},
{
.input = "[,",
},
{
.input = "[{",
},
{
.input = "[\"a",
},
{
.input = "[\"a\"",
},
{
.input = "{]",
},
{
.input = "{,",
},
{
.input = "{",
},
{
.input = "{[",
},
{
.input = "{\"a",
},
{
.input = "{'a'",
},
{
.input = "[\"\\{[\"\\{[\"\\{[\"\\{",
},
{
.input = "\xe9",
},
{
.input = "*",
},
{
.input = "{\"a\":\"b\"}#{}",
},
{
.input = "[\xe2\x81\xa0]",
},
{
.input = "[\\u000A\"\"]",
},
{
.input = "[1",
},
{
.input = "[ false, nul",
},
{
.input = "[ true, fals",
},
{
.input = "[ false, tru",
},
{
.input = "{\"asd\":\"asd\"",
},
{
.input = "\xc3\xa5",
},
{
.input = "\xef\xbb\xbf",
},
{
.input = "[\f]",
},
{
.input = "[\xe2\x81\xa0]",
},
{
.input = "[0.4e0066999999999999999999999999999999999"
"99999999999999999999999999999999999999999999999"
"99999999999999999999999999999999999969999999006]",
},
{
.input = "[-1e+9999]",
},
{
.input = "[1.5e+9999]",
},
{
.input = "[-123123e100000]",
},
{
.input = "[123123e100000]",
#if 0
},
{
.input = "[123e-10000000]",
#endif
},
{
.input = "[-123123123123123123123123123123]",
},
{
.input = "[100000000000000000000]",
},
{
.input = "[-237462374673276894279832749832423479823246327846]",
},
{
.input = "{\"\\uDFAA\":0}",
},
{
.input = "[\"\\uDADA\"]",
},
{
.input = "[\"\\uD888\\u1234\"]",
},
{
.input = "[\"\\uD800\\n\"]",
},
{
.input = "[\"\\uDd1ea\"]",
},
{
.input = "[\"\\uD800\\uD800\\n\"]",
},
{
.input = "[\"\\ud800\"]",
},
{
.input = "[\"\\ud800abc\"]",
},
{
.input = "[\"\xff\"]",
},
{
.input = "[\"\\uDd1e\\uD834\"]",
},
{
.input = "[\"\xe9\"]",
},
{
.input = "[\"\\uDFAA\"]",
},
{
.input = "[\"\x81\"]",
},
{
.input = "[\"\xf4\xbf\xbf\xbf\"]",
},
{
.input = "[\"\xc0\xaf\"]",
},
{
.input = "[\"\xfc\x83\xbf\xbf\xbf\xbf\"]",
},
{
.input = "[\"\xfc\x80\x80\x80\x80\x80\"]",
},
{
.input = "[\"\xe0\xff\"]",
},
{
.input = "\x00[\x00\"\x00\xe9\x00\"\x00]",
.input_len = 10
},
{
.input = "[\x00\"\x00\xe9\x00\"\x00]\x00",
.input_len = 10
},
{
.input = "\xff\xfe[\x00\"\x00\xe9\x00\"\x00]\x00",
.input_len = 12
},
{
.input = "[\"\xe6\x97\xa5\xd1\x88\xfa\"]",
},
{
.input = "[\"\xed\xa0\x80\"]",
},
{
.input = "\xef\xbb\xbf{}",
},
{
.input = "[\"Unclosed array\"",
},
{
.input = "{unquoted_key: \"keys must be quoted\"}",
},
{
.input = "[\"extra comma\",]",
},
{
.input = "[\"double extra comma\",,]",
},
{
.input = "[   , \"<-- missing value\"]",
},
{
.input = "[\"Comma after the close\"],",
},
{
.input = "[\"Extra close\"]]",
},
{
.input = "{\"Extra comma\": true,}",
},
{
.input = "{\"Extra value after close\": true} \"misplaced quoted value\"",
},
{
.input = "{\"Illegal expression\": 1 + 2}",
},
{
.input = "{\"Illegal invocation\": alert()}",
},
{
.input = "{\"Numbers cannot have leading zeroes\": 013}",
},
{
.input = "{\"Numbers cannot be hex\": 0x14}",
},
{
.input = "[\"Illegal backslash escape: \\x15\"]",
},
{
.input = "[\\naked]",
},
{
.input = "[\"Illegal backslash escape: \\017\"]",
},
{
.input = "{\"Missing colon\" null}",
},
{
.input = "{\"Double colon\":: null}",
},
{
.input = "{\"Comma instead of colon\", null}",
},
{
.input = "[\"Colon instead of comma\": false]",
},
{
.input = "[\"Bad value\", truth]",
},
{
.input = "['single quote']",
},
{
.input = "[\"\ttab\tcharacter\tin\tstring\t\"]",
},
{
.input = "[\"tab\\   character\\   in\\  string\\  \"]",
},
{
.input = "[\"line\n"
"break\"]",
},
{
.input = "[\"line\\\n"
"break\"]",
},
{
.input = "[0e]",
},
{
.input = "[0e+]",
},
{
.input = "[0e+-1]",
},
{
.input = "{\"Comma instead if closing brace\": true,",
},
{
.input = "[\"mismatch\"}",
},
{
.input = "a\xc3\xa5\n",
},
{
.input = "{,\n",
},
{
.input = "[1,\n"
"2,\n"
"3,\n"
"4,\n"
"5,\n"
"]\n",
},
{
.input =
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[",
},
{
.input = "[1e]\n",
},
{
.input = "[{}\n",
},
{
.input = "[-123123123123123123123123123123]\n",
},
{
.input = "[\"a\n",
},
{
.input = "{[\n",
},
{
.input = "[-123foo]\n",
},
{
.input = "[-foo]\n",
},
{
.input = "[\"\\uD888\\u3210 "
"(first surrogate and invalid second surrogate)\"]\n",
},
{
.input = "{\"a\":\"a\n",
},
{
.input = "[\x00\n",
},
{
.input = "[1,]\n",
},
{
.input = "[1,2,3]\n"
"foo\n",
},
{
.input = "[-123123e100000]\n",
},
{
.input = "[1.]\n",
},
{
.input = "[\"\\uqqqq <-- invalid unicode escape\"]\n",
},
{
.input = "{'a'\n",
},
{
.input = "{\n",
},
{
.input = "[\"\\uDADA (first surrogate without the second)\"]\n",
},
{
.input = "[,\n",
},
{
.input = "[1ea]\n",
},
{
.input = "",
},
{
.input = "[1,2,3]foo\n",
},
{
.input = "{\"a\"\n",
},
{
.input = "{\"a\":\n",
},
{
.input = "[012]\n",
},
{
.input = "{\"\n",
},
{
.input = "[\"\\a <-- invalid escape\"]\n",
},
{
.input = "[\n",
},
{
.input = "[{\n",
},
{
.input = "[troo\n",
},
{
.input = "[123123123123123123123123123123]\n",
},
{
.input = "\xc3\xa5\n",
},
{
.input = "[\"null escape \\u0000 not allowed\"]\n",
},
{
.input = "[1,\n",
},
{
.input = "{\"a\n",
},
{
.input = "['\n",
},
{
.input = "[-123.123foo]\n",
},
{
.input = "[\"null byte \x00 not allowed\"]\n",
},
{
.input = "{\"foo\\u0000bar\": 42}",
},
{
.input = "[123123e100000]\n",
},
{
.input = "[\"\\uDFAA (second surrogate on it's own)\"]\n",
},
{
.input = "[-012]\n",
},
{
.input = "[\"\t <-- tab character\"]\n",
},
{
.input = "{\"a\":\"a\" 123}\n",
},
{
.input = "[\"a\"\n",
},
{
.input = "[\"\xfd\"]\n",
},
{
.input = "[\"\xed\xa2\xab <-- encoded surrogate half\"]\n",
},
{
.input = "[\"\xe0\x80\xa2 <-- overlong encoding\"]\n",
},
{
.input = "[a\xe5]\n",
},
{
.input = "\xe5\n",
},
{
.input = "[\"\xe5 <-- invalid UTF-8\"]\n",
},
{
.input = "[1e\xe5]\n",
},
{
.input = "[\"\xe0\xff <-- truncated UTF-8\"]\n",
},
{
.input = "[\"\\\xe5\"]\n",
},
{
.input = "[\"\xc1\"]\n",
},
{
.input = "[\"\\u\xe5\"]\n",
},
{
.input = "[\"\xf0\x80\x80\xa2 <-- overlong encoding\"]\n",
},
{
.input = "[1e1\xe5]\n",
},
{
.input = "[\"\x81\"]\n",
},
{
.input = "[0\xe5]\n",
},
{
.input = "[\xe5]\n",
},
{
.input = "[\"\xf4\xbf\xbf\xbf\"]\n",
},
{
.input = "[123\xe5]\n",
},
{
.input = "{",
},
{
.input = "{:}",
},
{
.input = "{\"foo\":}",
},
{
.input = "{\"foo\" []}",
},
{
.input = "{\"foo\": [1}",
},
{
.input = "{\"foo\": [1,]}",
},
{
.input = "{\"foo\": 1,}",
},
{
.input = "{\"foo\": 1.}}",
},
{
.input = "{\"foo\": 1},{}",
},
{
.input = "{\"foo\": \"\\ud808\"}",
},
{
.input = "{\"foo\": \"\\udfff\"}",
},
{
.input = "{\"foo\": \"\\uyyyy\"}",
},
{
.input = "{\"a\":\"",
},
{
.input = "{\"a\":nul",
},
{
.input = "{\"a\":fals",
},
{
.input = "{\"a\":tru",
},
{
.input =
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]",
.limits = { .max_nesting = 31 },
},
{
.input =
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[["
"]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
"]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
"]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]",
.limits = { .max_nesting = 104 },
},
{
.input =
"[1,2,3,4,5,6,7,8,9,0,\n"
" 1,2,3,4,5,6,7,8,9,0,\n"
" 1,2,3,4,5,6,7,8,9,0,\n"
" 1,2,3,4,5,6,7,8,9,0,\n"
" 1,2,3,4,5,6,7,8,9,0]\n",
.limits = { .max_list_items = 49 },
},
{
.input =
"\"123456789012345678901234567890"
"123456789012345678901234567890"
"123456789012345678901234567890\"",
.limits = { .max_string_size = 89 },
},
{
.input =
"123456789012345678901234567890"
"123456789012345678901234567890"
"123456789012345678901234567890",
.flags = JSON_PARSER_FLAG_NUMBERS_AS_STRING,
.limits = { .max_string_size = 89 },
},
{
.input =
"{\"123456789012345678901234567890"
"123456789012345678901234567890"
"123456789012345678901234567890\": 90}",
.limits = { .max_name_size = 89 },
},
{
.input = "\"\\xFF\\xFF\\xFF\"",
},
{
.input = "ICJ9XHU10QAAAPxlXQ==",
.flags = JSON_PARSER_FLAG_STRICT,
.base64 = TRUE,
},
{
.input = "IiBcdTBEMNk=",
.flags = JSON_PARSER_FLAG_STRICT,
.base64 = TRUE,
},
{
.input =
"Ilx1ZDgzZFx1ZGUzOVswLDMuNDZFMiw1ZTUsMCwzLDVlNSwzLjIs"
"Mjc4My42RTIsNWU1LDMuMjc4NUUwLDM2RTIsNSwzLjIsMiwyODUs"
"MzUsMy40NiwzLjQ2RTIsNWU1LDAsMy40NjYsMCwzLjQ2RTIsMy40"
"Miw1ZTUsMy4yLDI3ODVFMCwzLjQ4LDMuNDZFMCwzLjQ2RTIsNWU1"
"LDMuMjg1RTAsMy40ODVFMCwzLjQ2RTIsNWU1LDAsMy40Niw1ZTYs"
"My4wLDMuNkUyLDVlNSwzLjI3ODVFMCwzLjQ2LDAsMy41LDMuMiwy"
"Nzg1RTAsMy40NkUyLDVlNSwyLDUzLjI3ODVFMCwzLjYsMCwzLjUs"
"MCwzLjQsNTI1LDMuMjc4NUUwLDMuNDIsNWU1LDMuNCwzLjQ2NDZF"
"Miw1ZTUsMy41LDIsNWU1LDMuNDIsNWU1LDMuMiwyNzg1RTAsMy40"
"OCwzLjQ2RTAsMy40NkUyLDVlNSwzLjIsMjc4NUUwLDMuNDg1RTAs"
"My40NkUyLDVlNSwwLDMuNDYsNWU2LDMuMCwzLjZFMiw1ZTUsMy4y"
"Nzg1RTAsMy40NiwwLDMuNSwzLjIsMjc4NUUwLDMuNDZFMiw1ZTUs"
"Miw1My4yNzg1RTAsMy42LDAsMy41LDAsMy40LDUyNSwzLjI3ODVF"
"MCwzLjQ2RTIsNWU2LDVlNSwwLDMuNDY2LDAsMy40NkUxLDVlNSwz"
"LjUsMiw1ZTUsMy40Miw1ZTUsMy4yLDI3ODVFMCwzLjQ4LDMuNDZF"
"MCwzLjQ2RTIsNWU1LDMuMiwyNzg1RTAsMy40ODVFMCwzLjQ2RTIs"
"NWU1LDAsMy40Niw1ZTYsMy4wLDMuNkUyLDVlNSwzLjI3ODVFMCwz"
"LjQ2LDAsMy41LDMuMiwyNzg1RTAsMy40NkUyLDVlNSwyLDUzLjI3"
"ODVFMCwzLjYsMCwzLjUsMCwzLjQsNTI1LDMuMjc4NUUwLDMuNDZF"
"Miw1ZTYsMCwzLjU1NjgsMy40MCwzLjQ2RTIsNWU1LDMuNDYsMCwz"
"LjQ2RTIsNTMuNDZFMiwyNWU1LDUzLjI3ODVFMCwzLjYsMCwzLjUs"
"MCwzLjQsNTI1LDMuMjc4NUUwLDMuNDZFMiw1ZTYsMCwzRTIsNTU2"
"OCwzLjIsNWU1LDMuNSwyLDVlNSwzLjQyLDVlNSwzLjIsMjc4NUUw"
"LDMuMiwyNzg1RTAsMy40OCwzLjQ1RTAsMy4yLDI3ODVFMCw4LjQz"
"NUU1LDVlNSwwLDMuMjUsMy40NjQsMy40NjIsNTMuMjc4NUUwLDMu"
"NDZFMiwzNUUwMCwzLjQ2NiwwLDNlNSwzLjIsNiwwLDMuNDZFMiw1"
"ZTUsMy41LDIsNWU1XHVkODNkXHVkY2U5XHVkODNkXHVkZTM5XHVk"
"ODNkXHVkYzhkXHVkODNkXHVkZTM5XHVkODNkXHVkYzZlOVx1ZDgz"
"ZFx1ZGUzOFx1ZDgzZFx1ZGMzZFx1ZDgzZFx1ZGUzOVx1ZDgzZFx1"
"ZGM2ZTkMdWQ4M2RcdWRlMzlcdWQ4M2RcdWRjOGRcdWQ4M2RcdWRl"
"MzlcdWQ4M2RcdWRlMzlcdWQ4M2RcdWRjNmU5XHVkODNkXHVkZTM5"
"XHVkODNkXHVkYzhkXHVkODNkXHVkZTM5XHVkODNkXHVkYzY5XHVk"
"ODNkJXVkZTM4XHVkODNkXHVkYzZkXHVkLDMuNDIsNWU1LDMuMiw4"
"RTcsNTIwMy40OCwzLjQ2RTAsMy40NkUyLDVlNSwzLjIsMjc4NUUw"
"LDMuNDg1RTAsMy40NkUyLDVlNSwwLDMuNDYsNWU2LDMuMCwzLjZF"
"Miw1ZTUsMy4yNzg1RTAsMy40NiwwLDMuNSwzLjIsMjc4NUUwLDMu"
"NDZFMiw1ZTUsMiw1My4yNzg1RTAsMy42LDAsMy41LDAsMy40LDUy"
"NSwzLjI3ODVFMCwzLjQ2RTIsNWU2LDAsMzZFMiw1NTY4LDMuNDAs"
"My40NkUyLDVlNSwzLjQ2LDAsMy40NkUyLDUzLjQ0ODVFMCwzLjQ2"
"RTIsNWU1ODNkXHVkLDAsMy40Niw1ZTYsMy4wLDMuNkUyLDVlNSwz"
"LjI3ODVFMCwzLjQ2LDAsMy41LDMuMiwyNzg1RTAsMy40NkUyLDVl"
"NSwyLDUzLjI3ODVFMCwzLjYsMCwzLjUsMCwzLjQsNTI1LDMzLjQ2"
"MiwyLDI4NSwzNSwzLjQ2RTIsNWU1LDMuNCwzLjQ2RTIsMjc4NGUz"
"OVx1ZDgzZFx1ZGM2ZTlcdWQ4M2SuipuazMajimQ4M2RcdWRjOGRc"
"dWQ4M2RcdWRlMzlcdWQ4M2RcdWRjNmVcdWQ4M2RcdTZFMiwyNjYs"
"MCwzLjQ2NSwzLjQyLDVlNWRlMzhcdWQ4M2RcdWRjM2RcdWQ4M2Rc"
"dWRjNmU5XHVkODNkXHVkZTMsMy4yLDI3ODVFMCwzLjIsMjc4NUUw"
"LDMuNDgsMy40NUUwLDMuMiwyNzg1RTAsOC40MzVFNSw1ZTUsMCwz"
"LjIsMy40LDMuNDYyLDUzOFwuMjc4NUV1ZDgzZFx1MCwzLjQ2RTIs"
"MzVFMCwzLjQ2RTIsNWU1LDI1RTAsMy42RTIsNWVkNSwzLmMyMzc4"
"ZCgy",
.flags = JSON_PARSER_FLAG_STRICT,
.limits = {
.max_name_size = 1024U,
.max_string_size = 1024U,
.max_nesting = 10U,
.max_list_items = JSON_DEFAULT_MAX_LIST_ITEMS,
},
.base64 = TRUE,
},
};
static const unsigned int invalid_parse_test_count =
N_ELEMENTS(invalid_parse_tests);
static void test_json_parse_invalid(void)
{
unsigned int i;
for (i = 0; i < invalid_parse_test_count; i++) T_BEGIN {
const struct json_invalid_parse_test *test;
struct istream *input;
struct json_parser *parser;
const char *text, *error = NULL;
unsigned int pos, text_len;
int ret = 0;
test = &invalid_parse_tests[i];
text = test->input;
text_len = test->input_len;
if (text_len == 0)
text_len = strlen(text);
input = test_istream_create_data(text, text_len);
if (test->base64) {
struct istream *inputb64 =
i_stream_create_base64_decoder(input);
i_stream_unref(&input);
input = inputb64;
}
test_begin(t_strdup_printf("json text invalid [%d]", i));
parser = json_parser_init(input,
&test->limits, test->flags, NULL, NULL);
for (pos = 0; pos <= text_len && ret == 0; pos++) {
test_istream_set_size(input, pos);
ret = json_parse_more(parser, &error);
if (ret < 0)
break;
if (ret > 0) {
if (debug)
i_debug("DATA: `%s'", text);
}
}
test_out_reason_quiet("parse failure (trickle)",
ret < 0, error);
json_parser_deinit(&parser);
i_stream_seek(input, 0);
parser = json_parser_init(input,
&test->limits, test->flags, NULL, NULL);
test_istream_set_size(input, text_len);
ret = json_parse_more(parser, &error);
if (ret > 0) {
if (debug)
i_debug("DATA: `%s'", text);
}
test_out_reason_quiet("parse failure (buffered)",
ret < 0, error);
json_parser_deinit(&parser);
test_end();
i_stream_unref(&input);
} T_END;
}
struct json_stream_parse_test {
const char *input, *output;
struct json_limits limits;
enum json_parser_flags flags;
};
static const struct json_stream_parse_test
stream_parse_tests[] = {
{
.input = "\"AABBCC\"",
.output = "AABBCC"
},{
.input = "\""
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"\"",
.output =
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
},{
.input = "[\""
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"\"]",
.output =
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
},{
.input = "  [ \""
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"\" ]  ",
.output =
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ"
},{
.input = "\"foo\\\\\\\"\\b\\f\\n\\r\\t\\u0001\\uffff\"",
.output = "foo\\\"\b\f\n\r\t\001\xEF\xBF\xBF"
},{
.input = "\"\\ud801\\udc37\"",
.output = "\xf0\x90\x90\xb7"
},{
.input = "\"\"",
.output = ""
}
};
static const unsigned int stream_parse_test_count =
N_ELEMENTS(stream_parse_tests);
static void
test_parse_stream_parse_value(void *context,
void *parent_context ATTR_UNUSED,
const char *name ATTR_UNUSED, enum json_type type,
const struct json_value *value)
{
struct istream **str_stream_r = (struct istream **)context;
test_assert(type == JSON_TYPE_STRING);
test_assert(value->content_type == JSON_CONTENT_TYPE_STREAM);
*str_stream_r = value->content.stream;
i_stream_ref(value->content.stream);
}
static struct json_parser_callbacks parse_stream_callbacks = {
.parse_value = test_parse_stream_parse_value
};
static void test_json_parse_stream(void)
{
static const unsigned int trickle_steps[] = {1,2,3,4,5,10,20};
string_t *buffer;
unsigned int i, j;
buffer = str_new(default_pool, 256);
for (i = 0; i < stream_parse_test_count; i++) T_BEGIN {
const struct json_stream_parse_test *test;
struct istream *input, *str_input;
struct json_parser *parser;
const char *text, *error = NULL;
unsigned int pos, text_len;
int ret = 0;
test = &stream_parse_tests[i];
text = test->input;
text_len = strlen(text);
input = test_istream_create_data(text, text_len);
test_begin(t_strdup_printf("json parse stream [%u]", i));
for (j = 0; j < N_ELEMENTS(trickle_steps); j++) {
unsigned int trickle_step = trickle_steps[j];
i_stream_seek(input, 0);
str_input = NULL;
str_truncate(buffer, 0);
parser = json_parser_init(input,
NULL, 0, &parse_stream_callbacks, &str_input);
json_parser_enable_string_stream(parser, 0, 10);
ret = 0;
for (pos = 0; pos <= text_len+1000 && ret == 0; pos += trickle_step) {
test_istream_set_size(input, pos);
if (str_input == NULL) {
ret = json_parse_more(parser, &error);
if (ret < 0)
break;
}
if (str_input != NULL) {
const unsigned char *data;
size_t size;
while ((ret = i_stream_read_more(str_input,
&data, &size)) > 0) {
buffer_append(buffer, data, size);
i_stream_skip(str_input, size);
}
if (ret < 0) {
i_assert(!i_stream_have_bytes_left(str_input));
i_stream_skip(str_input, size);
i_stream_unref(&str_input);
ret = 0;
}
}
}
test_out_reason_quiet(
t_strdup_printf("parse success "
"(trickle, step=%u)",
trickle_step),
ret > 0, error);
test_out_quiet("stream output",
strcmp(str_c(buffer),
test->output) == 0);
json_parser_deinit(&parser);
}
i_stream_seek(input, 0);
str_truncate(buffer, 0);
parser = json_parser_init(input,
NULL, 0, &parse_stream_callbacks, &str_input);
json_parser_enable_string_stream(parser, 0, 10);
test_istream_set_size(input, text_len);
ret = json_parse_more(parser, &error);
test_out_reason_quiet("parse success (buffered) #1",
ret == 0, error);
if (ret == 0 && str_input != NULL) {
const unsigned char *data;
size_t size;
while ((ret = i_stream_read_more(str_input,
&data, &size)) > 0) {
buffer_append(buffer, data, size);
i_stream_skip(str_input, size);
}
i_assert (ret != 0);
if (ret < 0) {
i_assert(!i_stream_have_bytes_left(str_input));
i_stream_skip(str_input, size);
i_stream_unref(&str_input);
ret = 0;
}
}
if (ret == 0) {
ret = json_parse_more(parser, &error);
test_out_reason_quiet("parse success (buffered) #2",
ret > 0, error);
}
test_out_quiet("stream output",
strcmp(str_c(buffer), test->output) == 0);
json_parser_deinit(&parser);
test_end();
i_stream_unref(&input);
} T_END;
str_free(&buffer);
}
struct json_stream_parse_error_test {
const char *input;
struct json_limits limits;
enum json_parser_flags flags;
int stream_errno;
};
static const struct json_stream_parse_error_test
stream_parse_error_tests[] = {
{
.input = "\"foo\\?\"",
.stream_errno = EINVAL,
},{
.input = "\"",
.stream_errno = EPIPE,
},{
.input = "\"\\\"",
.stream_errno = EPIPE,
},{
.input = "\"foo",
.stream_errno = EPIPE,
},{
.input = "\"\\ud801",
.stream_errno = EPIPE,
},{
.input = "\"\\ud801\"",
.stream_errno = EINVAL,
},{
.input = "\"\\udced\\udc37\"",
.stream_errno = EINVAL,
},{
.input = "\"\\ud8011\\udc37\"",
.stream_errno = EINVAL,
},{
.input = "\"\\ud801\\t\\udc37\"",
.stream_errno = EINVAL,
},{
.input = "\"hello \\udc37\"",
.stream_errno = EINVAL,
},{
.input = "\"hello \\ud801",
.stream_errno = EPIPE,
},{
.input = "\"\\uabcg",
.stream_errno = EINVAL,
},{
.input = "\"\\xFF\\xFF\\xFF\"",
.stream_errno = EINVAL,
}
};
static const unsigned int stream_parse_error_test_count =
N_ELEMENTS(stream_parse_error_tests);
static void
test_parse_stream_parse_error_value(void *context,
void *parent_context ATTR_UNUSED,
const char *name ATTR_UNUSED,
enum json_type type,
const struct json_value *value)
{
struct istream **str_stream_r = (struct istream **)context;
test_assert(type == JSON_TYPE_STRING);
test_assert(value->content_type == JSON_CONTENT_TYPE_STREAM);
*str_stream_r = value->content.stream;
i_stream_ref(value->content.stream);
}
static struct json_parser_callbacks parse_stream_error_callbacks = {
.parse_value = test_parse_stream_parse_error_value
};
static void test_json_parse_stream_error(void)
{
static const unsigned int trickle_steps[] = {1,2,3,4,5,10,20};
string_t *buffer;
unsigned int i, j;
buffer = str_new(default_pool, 256);
for (i = 0; i < stream_parse_error_test_count; i++) T_BEGIN {
const struct json_stream_parse_error_test *test;
struct istream *input, *str_input;
struct json_parser *parser;
const char *text, *error = NULL;
unsigned int pos, text_len;
int ret = 0;
test = &stream_parse_error_tests[i];
text = test->input;
text_len = strlen(text);
input = test_istream_create_data(text, text_len);
test_begin(t_strdup_printf("json parse stream error [%u]", i));
for (j = 0; j < N_ELEMENTS(trickle_steps); j++) {
unsigned int trickle_step = trickle_steps[j];
i_stream_seek(input, 0);
str_input = NULL;
str_truncate(buffer, 0);
parser = json_parser_init(input,
NULL, 0, &parse_stream_error_callbacks, &str_input);
json_parser_enable_string_stream(parser, 0, 10);
ret = 0;
for (pos = 0; pos <= text_len+1000 && ret == 0; pos += trickle_step) {
test_istream_set_size(input, pos);
if (str_input == NULL) {
ret = json_parse_more(parser, &error);
if (ret < 0)
break;
}
if (str_input != NULL) {
const unsigned char *data;
size_t size;
while ((ret = i_stream_read_more(str_input,
&data, &size)) > 0) {
buffer_append(buffer, data, size);
i_stream_skip(str_input, size);
}
if (ret < 0) {
test_assert(str_input->stream_errno != 0);
test_out_quiet("stream errno",
str_input->stream_errno == test->stream_errno);
i_stream_skip(str_input, size);
i_stream_unref(&str_input);
ret = 0;
}
}
}
test_out_reason_quiet(
t_strdup_printf("parse failure "
"(trickle, step=%u)",
trickle_step),
ret < 0, error);
json_parser_deinit(&parser);
}
i_stream_seek(input, 0);
str_truncate(buffer, 0);
parser = json_parser_init(input,
NULL, 0, &parse_stream_error_callbacks, &str_input);
json_parser_enable_string_stream(parser, 0, 10);
test_istream_set_size(input, text_len);
ret = json_parse_more(parser, &error);
test_out_reason_quiet("parse failure (buffered) #1",
ret <= 0, error);
if (ret == 0 && str_input != NULL) {
const unsigned char *data;
size_t size;
while ((ret = i_stream_read_more(str_input,
&data, &size)) > 0) {
buffer_append(buffer, data, size);
i_stream_skip(str_input, size);
}
i_assert (ret != 0);
if (ret < 0) {
test_assert(str_input->stream_errno != 0);
test_out_quiet("stream errno",
str_input->stream_errno == test->stream_errno);
i_stream_skip(str_input, size);
i_stream_unref(&str_input);
ret = 0;
}
}
if (ret == 0) {
ret = json_parse_more(parser, &error);
test_out_reason_quiet("parse failure (buffered) #2",
ret < 0, error);
}
json_parser_deinit(&parser);
test_end();
i_stream_unref(&input);
} T_END;
str_free(&buffer);
}
int main(int argc, char *argv[])
{
int c;
static void (*test_functions[])(void) = {
test_json_parse_valid,
test_json_parse_invalid,
test_json_parse_stream,
test_json_parse_stream_error,
NULL
};
while ((c = getopt(argc, argv, "D")) > 0) {
switch (c) {
case 'D':
debug = TRUE;
break;
default:
i_fatal("Usage: %s [-D]", argv[0]);
}
}
return test_run(test_functions);
}