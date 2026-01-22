COMMON_CHAT_FORMAT_CONTENT_ONLY,
COMMON_REASONING_FORMAT_NONE,
false,
false,
});
assert_equals(false, builder.try_parse_reasoning("<tnk>", "</tnk>"));
assert_equals("<tnk>Cogito</tnk>Ergo sum", builder.consume_rest());
}
{
common_chat_msg_parser builder("<tnk>Cogito</tnk>Ergo sum", false, {
COMMON_CHAT_FORMAT_CONTENT_ONLY,
COMMON_REASONING_FORMAT_DEEPSEEK,
false,
false,
});
assert_equals(true, builder.try_parse_reasoning("<tnk>", "</tnk>"));
assert_equals(std::string("Cogito"), builder.result().reasoning_content);
assert_equals("Ergo sum", builder.consume_rest());
}
{
common_chat_msg_parser builder("Cogito</tnk>Ergo sum", false, {
COMMON_CHAT_FORMAT_CONTENT_ONLY,
COMMON_REASONING_FORMAT_NONE,
false,
false,
});
assert_equals(false, builder.try_parse_reasoning("<tnk>", "</tnk>"));
assert_equals("Cogito</tnk>Ergo sum", builder.consume_rest());
}
{
common_chat_msg_parser builder("Cogito</tnk>Ergo sum", false, {
COMMON_CHAT_FORMAT_CONTENT_ONLY,
COMMON_REASONING_FORMAT_DEEPSEEK,
false,
true,
});
assert_equals(true, builder.try_parse_reasoning("<tnk>", "</tnk>"));
assert_equals(std::string("Cogito"), builder.result().reasoning_content);
assert_equals("Ergo sum", builder.consume_rest());
}
{
common_chat_msg_parser builder("Cogito</tnk>Ergo sum", false, {
COMMON_CHAT_FORMAT_CONTENT_ONLY,
COMMON_REASONING_FORMAT_DEEPSEEK,
true,
true,
});
assert_equals(true, builder.try_parse_reasoning("<tnk>", "</tnk>"));
assert_equals("<think>Cogito</think>", builder.result().content);
assert_equals("Ergo sum", builder.consume_rest());
}
}
static void test_regex() {
auto test_throws = [](const std::string & input, const std::string & regex, const std::string & expected_exception_pattern = "") {
common_chat_msg_parser builder(input, false, {});
assert_throws([&]() { builder.consume_regex(common_regex(regex)); }, expected_exception_pattern);
};
test_throws("Hello, world!", "abc", "^abc$");
test_throws("Hello, world!", "e", "^e$");
{
common_chat_msg_parser builder("Hello, world!", false, {});
builder.consume_regex(common_regex("Hello"));
assert_equals(", world!", builder.consume_rest());
}
{
common_chat_msg_parser builder("Hello,", false, {});
assert_equals(false, builder.try_consume_regex(common_regex("Hello, world!")).has_value());
}
{
common_chat_msg_parser builder("Hello,", false, {});
auto res = builder.try_consume_regex(common_regex("H(el)l(?:o, world!)?"));
assert_equals(true, res.has_value());
assert_equals<size_t>(2, res->groups.size());
assert_equals("Hell", builder.str(res->groups[0]));
assert_equals("el", builder.str(res->groups[1]));
assert_equals<size_t>(4, builder.pos());
assert_equals("o,", builder.consume_rest());
}
{
common_chat_msg_parser builder("Hello,", true, {});
assert_throws([&]() {
builder.try_consume_regex(common_regex("Hello, world!"));
}, "^Hello, world!$");
}
for (const auto is_partial : {false, true}) {
common_chat_msg_parser builder("Hello,", is_partial, {});
assert_equals(false, builder.try_consume_regex(common_regex("a(b|c)(d|e)f")).has_value());
}
for (const auto is_partial : {false, true}) {
common_chat_msg_parser builder("Hello,", is_partial, {});
assert_equals(false, builder.try_consume_literal("Oh"));
}
}
const std::vector<std::string> barely_healable_jsons = {
"{",
"{\"",
"{\"\\",
"{\"n",
"{\"name\"",
"{\"name\":",
"{\"name\":\"",
"{\"name\":\"\\",
"{\"name\":\"python",
"{\"name\":\"python\\",
"{\",",
"{\":",
"{\"[",
"{\"]",
"{\"{",
"{\"}",
"{\"1",
"{\"name\":\",",
"{\"name\":\":",
"{\"name\":\"[",
"{\"name\":\"]",
"{\"name\":\"{",
"{\"name\":\"}",
"{\"name\":\"1",
};
static void test(const std::string & input, bool is_partial, const std::vector<std::vector<std::string>> & args_paths, const std::vector<std::vector<std::string>> & content_paths, const std::string & expected) {
common_chat_msg_parser builder(input, is_partial, {});
auto js = builder.try_consume_json_with_dumped_args(args_paths, content_paths);
assert_equals(true, js.has_value());
assert_equals(is_partial, js->is_partial);
assert_equals(expected, args_paths.size() == 1 && args_paths[0].empty() ? js->value.get<std::string>() : js->value.dump());
}
static void test_with_args(const std::string & input, const std::string & expected, bool parse_as_partial = true, bool is_partial = true) {
common_chat_msg_parser builder(input, parse_as_partial, {});
auto js = builder.try_consume_json_with_dumped_args({{"args"}}, {});
assert_equals(true, js.has_value());
assert_equals(is_partial, js->is_partial);
assert_equals(expected, js->value.dump());
}
static void test_json_with_dumped_args_no_args() {
test("{\"name\": \"python\"}", false, {}, {}, "{\"name\":\"python\"}");
test("{\"name\": \"python\"}", false, {{}}, {}, "{\"name\":\"python\"}");
for (const auto & src : barely_healable_jsons) {
test(src, true, {{"arguments"}}, {}, "{}");
}
test("{\"name\": \"python\"", true, {{"arguments"}}, {}, "{\"name\":\"python\"}");
}
static void test_json_with_dumped_args() {
test("{\"content\": \"t", true, {}, {{"content"}}, "{\"content\":\"t\"}");
test("{\"content\": \"", true, {}, {{"content"}}, "{\"content\":\"\"}");
test("{\"content\": ", true, {}, {{"content"}}, "{}");
test("{\"name\": \"python", true, {{}}, {}, "{\"name\":\"python");
for (const auto & src : barely_healable_jsons) {
test(src, true, {{}}, {}, src);
}
for (auto parse_as_partial : {true, false}) {
test_with_args(
R"({"name": "python", "args": {"arg1": 1}})",
R"({"name":"python","args":"{\"arg1\":1}"})",
parse_as_partial,
false
);
}
test_with_args(
R"({"foo": "bar", "args": {")",
R"({"foo":"bar","args":"{\""})"
);
test_with_args(
R"({"foo": "bar", "args": {"ar)",
R"({"foo":"bar","args":"{\"ar"})"
);
test_with_args(
R"({"foo": "bar", "args": {"arg1")",
R"({"foo":"bar","args":"{\"arg1\""})"
);
test_with_args(
R"({"foo": "bar", "args": {"arg1":)",
R"({"foo":"bar","args":"{\"arg1\":"})"
);
test_with_args(
R"({"foo": "bar", "args": {"arg1": )",
R"({"foo":"bar","args":"{\"arg1\":"})"
);
test_with_args(
R"({"foo": "bar", "args": {"arg1": 1)",
R"({"foo":"bar","args":"{\"arg1\":"})"
);
test_with_args(
R"({"foo": "bar", "args": {"arg1": 1 )",
R"({"foo":"bar","args":"{\"arg1\":1"})"
);
test_with_args(
R"({"foo": "bar", "args": {"arg1": ")",
R"({"foo":"bar","args":"{\"arg1\":\""})"
);
test_with_args(
R"({"foo": "bar", "args": {"arg1": "1")",
R"({"foo":"bar","args":"{\"arg1\":\"1\""})"
);
test_with_args(
R"({"foo": "bar", "args": [)",
R"({"foo":"bar","args":"["})"
);
test_with_args(
R"({"foo": "bar", "args": [1)",
R"({"foo":"bar","args":"["})"
);
test_with_args(
R"({"foo": "bar", "args": [1 )",
R"({"foo":"bar","args":"[1"})"
);
test_with_args(
R"({"foo": "bar", "args": ["1")",
R"({"foo":"bar","args":"[\"1\""})"
);
test_with_args(
R"({"foo": "bar", "args": [1,)",
R"({"foo":"bar","args":"[1,"})"
);
test_with_args(
R"({"foo": "bar", "args": {"arg1": [)",
R"({"foo":"bar","args":"{\"arg1\":["})"
);
}
static void test_positions() {
{
common_chat_msg_parser builder("Hello, world!", false, {});
assert_equals<size_t>(0, builder.pos());
assert_throws([&]() { builder.move_to(100); });
assert_equals<size_t>(0, builder.pos());
assert_throws([&]() { builder.move_back(1); });
assert_equals<size_t>(0, builder.pos());
builder.move_to(8);
assert_equals<size_t>(8, builder.pos());
builder.move_back(1);
assert_equals<size_t>(7, builder.pos());
assert_equals("world!", builder.consume_rest());
builder.move_to(0);
assert_equals<size_t>(0, builder.pos());
assert_throws([&]() { builder.finish(); });
assert_equals<size_t>(0, builder.pos());
builder.move_to(builder.input().size());
builder.finish();
}
{
common_chat_msg_parser builder("Hello, world!", true, {});
builder.move_to(builder.input().size());
assert_equals<size_t>(builder.input().size(), builder.pos());
builder.finish();
}
}
int main() {
test_positions();
test_json_with_dumped_args_no_args();
test_json_with_dumped_args();
test_reasoning();
test_regex();
std::cout << "All tests passed!\n";
return 0;
}