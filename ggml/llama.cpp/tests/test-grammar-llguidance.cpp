#ifdef NDEBUG
# undef NDEBUG
#endif
#include "sampling.h"
#include <cassert>
#include <string>
#include <vector>
static const llama_vocab * vocab;
static bool match_string(const std::string & input, llama_sampler * grammar) {
llama_sampler_reset(grammar);
auto tokens = common_tokenize(vocab, input, false, false);
auto n_vocab = llama_vocab_n_tokens(vocab);
std::vector<llama_token_data> cur;
cur.reserve(n_vocab);
for (llama_token token_id = 0; token_id < (llama_token) n_vocab; token_id++) {
cur.emplace_back(llama_token_data{ token_id, 0.0f, 0.0f });
}
auto tok_arr = llama_token_data_array{ cur.data(), cur.size(), -1, false };
for (const auto token : tokens) {
for (llama_token token_id = 0; token_id < (llama_token) n_vocab; token_id++) {
cur[token_id].logit = 0.0f;
}
llama_sampler_apply(grammar, &tok_arr);
if (cur[token].logit < 0.0f) {
return false;
}
llama_sampler_accept(grammar, token);
}
auto tok_eos = llama_vocab_eot(vocab);
if (tok_eos == LLAMA_TOKEN_NULL) {
tok_eos = llama_vocab_eos(vocab);
}
cur[tok_eos].logit = 0.0f;
llama_sampler_apply(grammar, &tok_arr);
return cur[tok_eos].logit >= 0.0f;
}
static void test(const std::string & test_desc, const std::string & grammar_str,
const std::vector<std::string> & passing_strings, const std::vector<std::string> & failing_strings) {
fprintf(stderr, "⚫ Testing %s\n%s\n", test_desc.c_str(), grammar_str.c_str());
fflush(stderr);
auto * grammar = llama_sampler_init_llg(vocab, "lark", grammar_str.c_str());
fprintf(stderr, "  🔵 Valid strings:\n");
for (const auto & test_string : passing_strings) {
fprintf(stderr, "    \"%s\" ", test_string.c_str());
fflush(stderr);
bool matched = match_string(test_string, grammar);
if (!matched) {
fprintf(stderr, "❌ (failed to match)\n");
FILE * grammar_file = fopen("test-grammar-integration.grammar.gbnf", "w");
if (grammar_file) {
fprintf(grammar_file, "%s", grammar_str.c_str());
fclose(grammar_file);
}
FILE * string_file = fopen("test-grammar-integration.string.txt", "w");
if (string_file) {
fprintf(string_file, "%s", test_string.c_str());
fclose(string_file);
}
fprintf(stderr,
"\n NOTE: Debug grammar file generated. To analyze this failure in detail, run the following "
"command:     ./test-gbnf-validator test-grammar-integration.grammar.gbnf "
"test-grammar-integration.string.txt\n\n");
} else {
fprintf(stdout, "✅︎\n");
}
assert(matched);
}
fprintf(stderr, "  🟠 Invalid strings:\n");
for (const auto & test_string : failing_strings) {
fprintf(stderr, "    \"%s\" ", test_string.c_str());
fflush(stderr);
bool matched = match_string(test_string, grammar);
if (matched) {
fprintf(stderr, "❌ (incorrectly matched)\n");
} else {
fprintf(stdout, "✅︎\n");
}
assert(!matched);
}
llama_sampler_free(grammar);
}
static void test_grammar(const std::string & test_desc, const std::string & grammar_str,
const std::vector<std::string> & passing_strings,
const std::vector<std::string> & failing_strings) {
test(test_desc + ". Grammar: " + grammar_str, grammar_str, passing_strings, failing_strings);
}
static void test_schema(const std::string & test_desc, const std::string & schema_str,
const std::vector<std::string> & passing_strings,
const std::vector<std::string> & failing_strings) {
test(test_desc + ". Schema: " + schema_str, "%llguidance {}\nstart: %json " + schema_str, passing_strings,
failing_strings);
}
static void test_simple_grammar() {
test_schema("min 0",
R"""({
"type": "integer",
"minimum": 0
})""",
{
"0",
"10",
"12",
"10000",
},
{
"-1",
"-10",
"-10000",
"-100000000000000000000000000000000",
"00",
"01",
"-0",
});
test_schema("min 2",
R"""({
"type": "integer",
"minimum": 2
})""",
{
"2",
"3",
"4",
"10",
"20",
"1234567890000000",
},
{
"0", "1", "-1", "-100", "0", "1", "01", "02",
});
test_schema("min 456",
R"""({
"type": "integer",
"minimum": 456
})""",
{
"456",
"4560",
"457",
"460",
"500",
},
{
"455",
"356",
"50",
"050",
"-1",
"-456",
});
test_schema("min -123",
R"""({
"type": "integer",
"minimum": -123
})""",
{
"-123",
"-122",
"-11",
"-1",
"0",
"1",
"123",
"1234",
"2345",
},
{
"-1234",
"-124",
});
test_schema("max 9999",
R"""({
"type": "integer",
"maximum": 9999
})""",
{
"-99999",
"0",
"9999",
},
{
"10000",
"99991",
});
test_schema("max -9999",
R"""({
"type": "integer",
"maximum": -9999
})""",
{
"-10000",
"-9999",
},
{
"-9998",
"0",
"9999",
});
test_schema("min 5 max 30",
R"""({
"type": "integer",
"minimum": 5,
"maximum": 30
})""",
{
"5",
"10",
"30",
},
{
"05",
"4",
"-1",
"31",
"123",
"0123",
});
test_schema("min -1 max 1",
R"""({
"type": "integer",
"minimum": -1,
"maximum": 1
})""",
{
"-1",
"0",
"1",
},
{
"-11",
"-10",
"-2",
"2",
"10",
"11",
});
test_schema("min -123 max 42",
R"""({
"type": "integer",
"minimum": -123,
"maximum": 42
})""",
{
"-123",
"-122",
"-13",
"-11",
"-2",
"-1",
"0",
"1",
"5",
"10",
"39",
"40",
"42",
},
{
"-0123",
"-124",
"-1123",
"-200",
"43",
"123",
"0123",
});
test_schema("exclusive min / max",
R"""({
"type": "integer",
"exclusiveMinimum": 0,
"exclusiveMaximum": 10000
})""",
{
"1",
"9999",
},
{
"0",
"01",
"10000",
"99999",
});
test_grammar("simple grammar",
R"""(
start: expr
expr: term ("+" term)*
term: number
number: /[0-9]+/ )""",
{
"42",
"1+2+3+4+5",
"123+456",
},
{
"+",
"/ 3",
"1+2+3+4+5+",
"12a45",
});
}
static void test_complex_grammar() {
test_grammar("medium complexity grammar",
R"""(
start: expression
expression: term ws (("+"|"-") ws term)*
term: factor ws (("*"|"/") ws factor)*
factor: number | variable | "(" expression ")" | function-call
number: /[0-9]+/
variable: /[a-zA-Z_][a-zA-Z0-9_]*/
function-call: variable ws "(" (expression ("," ws expression)*)? ")"
ws: /[ \t\n\r]?/ )""",
{ "42",
"1*2*3*4*5",
"x",
"x+10",
"x1+y2",
"(a+b)*(c-d)",
"func()",
"func(x,y+2)",
"a*(b+c)-d/e",
"f(g(x),h(y,z))",
"x + 10",
"x1 + y2",
"(a + b) * (c - d)",
"func()",
"func(x, y + 2)",
"a * (b + c) - d / e",
"f(g(x), h(y, z))",
"123+456",
"123*456*789-123/456+789*123",
"123+456*789-123/456+789*123-456/789+123*456-789/123+456*789-123/456+789*123-456" },
{
"+",
"/ 3x",
"x + + y",
"a * / b",
"func(,)",
"func(x y)",
"(a + b",
"x + y)",
"a + b * (c - d",
"42 +",
"x +",
"x + 10 +",
"(a + b) * (c - d",
"func(",
"func(x, y + 2",
"a * (b + c) - d /",
"f(g(x), h(y, z)",
"123+456*789-123/456+789*123-456/789+123*456-789/123+456*789-123/456+789*123-456/",
});
}
static void test_special_chars() {
test_grammar("special characters",
R"""(
start: /.../ "abc" /.../
)""",
{ "abcabcabc", "aaaabcccc",
"🔵🟠✅abc❌🟠🔵" },
{ "aaabcccc", "aaaaabcccc", "aaaabccc", "aaaabccccc", "🔵🟠✅❌abc❌✅🟠🔵", "🔵🟠abc🟠🔵" });
}
static void test_quantifiers() {
test_grammar(
"* quantifier",
R"""(start: "a"*)""",
{ "", "a", "aaaaa", "aaaaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" },
{ "b", "ab", "aab", "ba", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab" });
test_grammar(
"+ quantifier",
R"""(start: "a"+)""",
{ "a", "aaaaa", "aaaaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" },
{ "", "b", "ab", "aab", "ba", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab" });
test_grammar("? quantifier",
R"""(start: "a"?)""",
{ "", "a" },
{
"b",
"ab",
"aa",
"ba",
});
test_grammar("mixed quantifiers",
R"""(
start: cons+ vowel* cons? (vowel cons)*
vowel: /[aeiouy]/
cons: /[bcdfghjklmnpqrstvwxyz]/
)""",
{
"yes",
"no",
"noyes",
"crwth",
"four",
"bryyyy",
},
{
"yess",
"yesno",
"forty",
"catyyy",
});
test_grammar("simple exact repetition",
R"""(
start: /[ab]{4}/
)""",
{
"aaaa",
"bbbb",
"abab",
},
{
"a",
"b",
"aaaaa",
});
test_grammar("simple min repetition",
R"""(
start: /[ab]{4,}/
)""",
{
"aaaa",
"aaaaab",
"bbbb",
"ababab",
},
{
"",
"aba",
});
test_grammar("simple max repetition",
R"""(
start: /[ab]{0,4}/
)""",
{
"",
"a",
"aa",
"aaa",
"aaab",
},
{
"aaaaa",
});
}
static void test_json_schema() {
test_schema("empty schema (object)",
R"""(
{"type":"object"}
)""",
{
R"""({})""",
R"""({"foo": "bar"})""",
},
{
"",
"[]",
"null",
R"""("")""",
"true",
});
test_schema(
"exotic formats (list)",
R"""({
"items": [
{ "format": "date" },
{ "format": "uuid" },
{ "format": "time" },
{ "format": "date-time" }
]
})""",
{
R"""(["2012-04-23", "12345678-1234-1234-1234-1234567890ab", "18:25:43.511Z", "2012-04-23T18:25:43.511Z"])""",
},
{
R"""(["foo", "bar"])""",
R"""(["12345678-1234-1234-1234-1234567890ab"])""",
});
test_schema("string",
R"""({
"type": "string"
})""",
{
R"""("foo")""",
R"""("bar")""",
R"""("")""",
},
{
R"""({})""",
R"""("foo": "bar")""",
});
test_schema("string w/ min length 1",
R"""({
"type": "string",
"minLength": 1
})""",
{
R"""("foo")""",
R"""("bar")""",
},
{
R"""("")""",
R"""({})""",
R"""("foo": "bar")""",
});
test_schema("string w/ min length 3",
R"""({
"type": "string",
"minLength": 3
})""",
{
R"""("foo")""",
R"""("bar")""",
R"""("foobar")""",
},
{
R"""("")""",
R"""("f")""",
R"""("fo")""",
});
test_schema("string w/ max length",
R"""({
"type": "string",
"maxLength": 3
})""",
{
R"""("foo")""",
R"""("bar")""",
R"""("")""",
R"""("f")""",
R"""("fo")""",
},
{
R"""("foobar")""",
});
test_schema("string w/ min & max length",
R"""({
"type": "string",
"minLength": 1,
"maxLength": 4
})""",
{
R"""("foo")""",
R"""("bar")""",
R"""("f")""",
R"""("barf")""",
},
{
R"""("")""",
R"""("barfo")""",
R"""("foobar")""",
});
test_schema("boolean",
R"""({
"type": "boolean"
})""",
{
"true",
"false",
},
{
R"""("")""",
R"""("true")""",
R"""(True)""",
R"""(FALSE)""",
});
test_schema("integer",
R"""({
"type": "integer"
})""",
{
R"""(0)""",
R"""(12345)""",
R"""(1234567890123456)""",
},
{
R"""()""",
R"""(01)""",
R"""(007)""",
R"""(12345678901234567  )""",
});
test_schema("string const",
R"""({
"const": "foo"
})""",
{
R"""("foo")""",
},
{
R"""(foo)""",
R"""("bar")""",
});
test_schema("non-string const",
R"""({
"const": true
})""",
{
R"""(true)""",
},
{
R"""()""",
R"""(foo)""",
R"""("true")""",
});
test_schema("non-string const",
R"""({
"enum": ["red", "amber", "green", null, 42, ["foo"]]
})""",
{
R"""("red")""",
R"""(null)""",
R"""(42)""",
R"""(["foo"])""",
},
{
R"""()""",
R"""(420)""",
R"""(true)""",
R"""(foo)""",
});
test_schema("simple pattern",
R"""({
"pattern": "^[a-zA-Z0-9_-]*$"
})""",
{
R"""("")""",
R"""("He_llo-12")""",
},
{
R"""("!")""",
R"""("Hello World")""",
});
test_schema("pattern with escapes",
R"""({
"pattern": "^a\\^\\$\\.\\[\\]\\(\\)\\|\\{\\}\\*\\+\\?b$"
})""",
{
R"""("a^$.[]()|{}*+?b")""",
},
{
R"""("ab")""",
});
test_schema("",
R"""(
{
"type": ["array", "null"],
"items": { "type": "string" }
}
)""",
{
"null",
"[]",
"[\"123\"]",
"[\"foo\", \"bar\"]",
},
{
"",
"[123]",
"\"foo\"",
"[\"foo\", 42]",
});
test_schema("min+max items",
R"""({
"items": {
"type": ["number", "integer"]
},
"minItems": 3,
"maxItems": 5
})""",
{
R"""([1, 2, 3])""",
R"""([1, 2, 3, 4])""",
R"""([1, 2, 3, 4, 5])""",
R"""(1)""",
},
{
R"""([1, 2])""",
R"""([1, 2, 3, 4, 5, 6])""",
});
test_schema("object properties",
R"""({
"type": "object",
"properties": {
"number": { "type": "number" },
"street_name": { "type": "string" },
"street_type": { "enum": ["Street", "Avenue", "Boulevard"] }
},
"additionalProperties": false
})""",
{
R"""({ "number": 1600, "street_name": "Pennsylvania", "street_type":"Avenue"})""",
R"""({ "street_name": "Pennsylvania" })""",
R"""({ "number": 1600, "street_name": "Pennsylvania" })""",
R"""({})""",
R"""({ "number": 1600, "street_name": "Pennsylvania", "street_type": "Avenue" })""",
},
{
R"""({ "number": "1600", "street_name": "Pennsylvania", "street_type":"Avenue"})""",
R"""({ "street_name": "Pennsylvania", "number": 1600 })""",
R"""({ "number": "1600", "street_name": "Pennsylvania", "street_type":"Avenue"})""",
R"""({ "number": 1600, "street_name": "Pennsylvania", "street_type":"Avenue", "direction":"NW"})""",
});
test_schema("additional properties can't override other properties",
R"""({
"properties": {
"a": {"type": "integer"},
"b": {"type": "integer"}
},
"additionalProperties": true
})""",
{
R"""({"a": 42})""",
R"""({"c": ""})""",
R"""({"a": 42, "c": ""})""",
R"""({"a_": ""})""",
},
{
R"""()""",
R"""({"a": ""})""",
R"""({"a": "", "b": ""})""",
});
test_schema("object properties, additionalProperties: true",
R"""({
"type": "object",
"properties": {
"number": { "type": "number" },
"street_name": { "type": "string" },
"street_type": { "enum": ["Street", "Avenue", "Boulevard"] }
},
"additionalProperties": true
})""",
{
R"""({})""",
R"""({"number":1600,"street_name":"Pennsylvania","street_type":"Avenue"})""",
R"""({ "street_name": "Pennsylvania" })""",
R"""({ "number": 1600, "street_name": "Pennsylvania" })""",
R"""({ "number": 1600, "street_name": "Pennsylvania", "street_type":"Avenue", "direction":"NW"})""",
R"""({ "number": 1600, "street_name": "Pennsylvania", "street_type": "Avenue" })""",
},
{
R"""({ "number": "1600", "street_name": "Pennsylvania", "street_type":"Avenue"})""",
R"""({ "street_name": "Pennsylvania", "number": 1600, "street_type":"Avenue"})""",
});
test_schema(
"required + optional props each in original order",
R"""({
"type": "object",
"properties": {
"number": { "type": "number" },
"street_name": { "type": "string" },
"street_type": { "enum": ["Street", "Avenue", "Boulevard"] }
},
"additionalProperties": false
})""",
{
R"""({ "street_name": "Pennsylvania" })""",
R"""({ "number": 1600, "street_type":"Avenue"})""",
R"""({ "number": 1600, "street_name": "Pennsylvania" })""",
R"""({ "number": 1600, "street_name": "Pennsylvania", "street_type":"Avenue"})""",
R"""({ "number": 1600, "street_name": "Pennsylvania", "street_type": "Avenue" })""",
},
{
R"""({ "street_type": "Avenue", "number": 1600 })""",
R"""({ "number": 1600, "street_name": "Pennsylvania", "street_type": "Avenue", "direction": "NW" })""",
});
test_schema("required + optional props each in original order",
R"""({
"properties": {
"b": {"type": "string"},
"a": {"type": "string"},
"d": {"type": "string"},
"c": {"type": "string"}
},
"required": ["a", "b"],
"additionalProperties": false
})""",
{
R"""({"b": "foo", "a": "bar"})""",
R"""({"b":"foo","a":"bar","d":"qux"})""",
R"""({"b":"foo", "a":"bar", "d":"qux", "c":"baz"})""",
},
{
R"""({"a": "foo", "b": "bar"})""",
R"""({"b": "bar"})""",
R"""({"a": "foo", "c": "baz"})""",
R"""({"a":"foo", "b":"bar", "c":"baz", "d":"qux"})""",
});
test_schema(
"required props",
R"""({
"$schema": "https:
"$id": "https:
"title": "Product",
"description": "A product from Acme's catalog",
"type": "object",
"properties": {
"productId": {
"description": "The unique identifier for a product",
"type": "integer"
},
"productName": {
"description": "Name of the product",
"type": "string"
},
"price": {
"description": "The price of the product",
"type": "number",
"exclusiveMinimum": 0
},
"tags": {
"description": "Tags for the product",
"type": "array",
"items": {
"type": "string"
},
"minItems": 1,
"DISABLED_uniqueItems": true
},
"dimensions": {
"type": "object",
"properties": {
"length": {
"type": "number"
},
"width": {
"type": "number"
},
"height": {
"type": "number"
}
},
"required": [ "length", "width", "height" ]
}
},
"required": [ "productId", "productName", "price" ]
})""",
{
R"""({"productId": 1, "productName": "A green door", "price": 12.50})""",
R"""({"productId": 1, "productName": "A green door", "price": 12.50, "tags": ["home", "green"]})""",
R"""({"productId": 1, "productName": "A green door", "price": 12.50, "tags": ["home", "green"], "dimensions": {"length": 785, "width": 250.5, "height": -0.359}})""",
},
{
R"""({})""",
R"""({"productName": "A green door", "price": 12.50, "productId": 1})""",
R"""({"productId": 1, "productName": "A green door", "price": -12.50})""",
R"""({"productId": 1, "productName": "A green door"})""",
R"""({"productName": "A green door", "price": 12.50})""",
R"""({"productId": 1, "productName": "A green door", "price": 12.50, "tags": []})""",
R"""({"productId": 1, "productName": "A green door", "price": 12.50, "dimensions": {"length": 785, "width": 250.5, "height": -0.359}, "tags": ["home", "green"]})""",
});
}
static void one_hot(llama_token_data_array & tok_arr, llama_token selected) {
auto n_vocab = tok_arr.size;
tok_arr.selected = -1;
tok_arr.sorted = false;
for (llama_token token_id = 0; token_id < (llama_token) n_vocab; token_id++) {
tok_arr.data[token_id].id = token_id;
tok_arr.data[token_id].logit = 0.0f;
}
tok_arr.data[selected].logit = 100.0f;
}
static void test_sampler_chain(void) {
auto sparams = llama_sampler_chain_default_params();
sparams.no_perf = false;
llama_sampler * sampler = llama_sampler_chain_init(sparams);
const auto grammar_data = R"(%llguidance {}
start: /[A-Z ]*/)";
llama_sampler_chain_add(sampler, llama_sampler_init_llg(vocab, "lark", grammar_data));
llama_sampler_chain_add(sampler, llama_sampler_init_dist(42));
auto input = "ALL YOUR BASE ARE BELONG TO US";
auto tokens = common_tokenize(vocab, input, false, false);
auto n_vocab = llama_vocab_n_tokens(vocab);
std::vector<llama_token_data> cur;
cur.reserve(n_vocab);
for (llama_token token_id = 0; token_id < (llama_token) n_vocab; token_id++) {
cur.emplace_back(llama_token_data{ token_id, 0.0f, 0.0f });
}
auto tok_arr = llama_token_data_array{ cur.data(), cur.size(), -1, false };
for (const auto token : tokens) {
one_hot(tok_arr, token);
fprintf(stderr, "applying token: %d\n", token);
llama_sampler_apply(sampler, &tok_arr);
auto idx = tok_arr.selected;
fprintf(stderr, " -> %d %f\n", cur[idx].id, cur[idx].logit);
assert(cur[tok_arr.selected].id == token);
llama_sampler_accept(sampler, token);
}
auto tok_eos = llama_vocab_eot(vocab);
if (tok_eos == LLAMA_TOKEN_NULL) {
tok_eos = llama_vocab_eos(vocab);
}
one_hot(tok_arr, tok_eos);
llama_sampler_apply(sampler, &tok_arr);
assert(cur[tok_arr.selected].id == tok_eos);
}
int main(int argc, const char ** argv) {
fprintf(stdout, "Running llguidance integration tests...\n");
if (argc != 2) {
fprintf(stderr, "Usage: %s <vocab-file>\n", argv[0]);
return 1;
}
const char * vocab_file = argv[1];
fprintf(stderr, "reading vocab from: '%s'\n", vocab_file);
llama_model * model;
llama_context * ctx;
llama_backend_init();
{
auto mparams = llama_model_default_params();
mparams.vocab_only = true;
model = llama_model_load_from_file(vocab_file, mparams);
if (model == NULL) {
fprintf(stderr, "%s: error: failed to load vocab '%s'\n", __func__, vocab_file);
return 1;
}
auto cparams = llama_context_default_params();
ctx = llama_init_from_model(model, cparams);
if (ctx == NULL) {
fprintf(stderr, "%s: error: failed to load vocab '%s'\n", __func__, vocab_file);
llama_model_free(model);
return 1;
}
}
vocab = llama_model_get_vocab(model);
test_simple_grammar();
test_complex_grammar();
test_special_chars();
test_quantifiers();
test_json_schema();
test_sampler_chain();
fprintf(stdout, "All tests passed.\n");
return 0;
}