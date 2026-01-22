#ifdef NDEBUG
#undef NDEBUG
#endif
#include "json-schema-to-grammar.h"
#include "../src/unicode.h"
#include "../src/llama-grammar.h"
#include <nlohmann/json.hpp>
#include <cassert>
#include <string>
#include <vector>
using json = nlohmann::ordered_json;
static llama_grammar * build_grammar(const std::string & grammar_str) {
return llama_grammar_init_impl(nullptr, grammar_str.c_str(), "root", false, nullptr, 0, nullptr, 0);
}
static bool test_build_grammar_fails(const std::string & grammar_str) {
fprintf(stderr, "⚫ Testing failure for grammar: %s\n", grammar_str.c_str());
bool grammar_fails = false;
llama_grammar * grammar = build_grammar(grammar_str);
if (grammar != nullptr) {
fprintf(stderr, "  ❌ Expected build failure, but succeeded\n");
} else {
grammar_fails = true;
fprintf(stdout, "  ✅︎\n");
}
return grammar_fails;
}
static bool match_string(const std::string & input, llama_grammar * grammar) {
const auto cpts = unicode_cpts_from_utf8(input);
auto & stacks_cur = llama_grammar_get_stacks(grammar);
for (const auto & cpt : cpts) {
llama_grammar_accept(grammar, cpt);
if (stacks_cur.empty()) {
return false;
}
}
for (const auto & stack : stacks_cur) {
if (stack.empty()) {
return true;
}
}
return false;
}
static void test(const std::string & test_desc, const std::string & grammar_str, const std::vector<std::string> & passing_strings, const std::vector<std::string> & failing_strings) {
fprintf(stderr, "⚫ Testing %s\n%s\n", test_desc.c_str(), grammar_str.c_str());
fflush(stderr);
auto * grammar = build_grammar(grammar_str);
const llama_grammar_stacks stacks_org = llama_grammar_get_stacks(grammar);
llama_grammar_stacks & stacks_cur = llama_grammar_get_stacks(grammar);
fprintf(stderr, "  🔵 Valid strings:\n");
for (const auto & test_string : passing_strings) {
fprintf(stderr, "    \"%s\" ", test_string.c_str());
fflush(stderr);
bool matched = match_string(test_string, grammar);
if (!matched) {
fprintf(stderr, "❌ (failed to match)\n");
FILE* grammar_file = fopen("test-grammar-integration.grammar.gbnf", "w");
if (grammar_file) {
fprintf(grammar_file, "%s", grammar_str.c_str());
fclose(grammar_file);
}
FILE* string_file = fopen("test-grammar-integration.string.txt", "w");
if (string_file) {
fprintf(string_file, "%s", test_string.c_str());
fclose(string_file);
}
fprintf(stderr, "\n NOTE: Debug grammar file generated. To analyze this failure in detail, run the following command:     ./llama-gbnf-validator test-grammar-integration.grammar.gbnf test-grammar-integration.string.txt\n\n");
} else {
fprintf(stdout, "✅︎\n");
}
assert(matched);
stacks_cur = stacks_org;
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
stacks_cur = stacks_org;
}
llama_grammar_free_impl(grammar);
}
static void test_grammar(const std::string & test_desc, const std::string & grammar_str, const std::vector<std::string> & passing_strings, const std::vector<std::string> & failing_strings) {
test(test_desc + ". Grammar: " + grammar_str, grammar_str, passing_strings, failing_strings);
}
static void test_schema(const std::string & test_desc, const std::string & schema_str, const std::vector<std::string> & passing_strings, const std::vector<std::string> & failing_strings) {
test(test_desc + ". Schema: " + schema_str, json_schema_to_grammar(json::parse(schema_str), true), passing_strings, failing_strings);
}
static void test_simple_grammar() {
test_schema(
"min 0",
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
"100000000000000000000000000000000",
"00",
"01",
"-0",
}
);
test_schema(
"min 2",
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
"0",
"1",
"-1",
"-100",
"0",
"1",
"01",
"02",
"12345678900000000",
}
);
test_schema(
"min 456",
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
}
);
test_schema(
"min -123",
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
}
);
test_schema(
"max 9999",
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
}
);
test_schema(
"max -9999",
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
}
);
test_schema(
"min 5 max 30",
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
}
);
test_schema(
"min -1 max 1",
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
}
);
test_schema(
"min -123 max 42",
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
}
);
test_schema(
"exclusive min / max",
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
}
);
test_grammar(
"simple grammar",
R"""(
root ::= expr
expr ::= term ("+" term)*
term ::= number
number ::= [0-9]+)""",
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
}
);
}
static void test_complex_grammar() {
test_grammar(
"medium complexity grammar",
R"""(
root ::= expression
expression ::= term ws (("+"|"-") ws term)*
term ::= factor ws (("*"|"/") ws factor)*
factor ::= number | variable | "(" expression ")" | function-call
number ::= [0-9]+
variable ::= [a-zA-Z_][a-zA-Z0-9_]*
function-call ::= variable ws "(" (expression ("," ws expression)*)? ")"
ws ::= [ \t\n\r]?)""",
{
"42",
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
"123+456*789-123/456+789*123-456/789+123*456-789/123+456*789-123/456+789*123-456"
},
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
}
);
}
static void test_special_chars() {
test_grammar(
"special characters",
R"""(
root ::= ... "abc" ...
)""",
{
"abcabcabc",
"aaaabcccc",
"🔵🟠✅abc❌🟠🔵"
},
{
"aaabcccc",
"aaaaabcccc",
"aaaabccc",
"aaaabccccc",
"🔵🟠✅❌abc❌✅🟠🔵",
"🔵🟠abc🟠🔵"
}
);
}
static void test_quantifiers() {
test_grammar(
"* quantifier",
R"""(root ::= "a"*)""",
{
"",
"a",
"aaaaa",
"aaaaaaaaaaaaaaaaaa",
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
},
{
"b",
"ab",
"aab",
"ba",
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"
}
);
test_grammar(
"+ quantifier",
R"""(root ::= "a"+)""",
{
"a",
"aaaaa",
"aaaaaaaaaaaaaaaaaa",
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
},
{
"",
"b",
"ab",
"aab",
"ba",
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"
}
);
test_grammar(
"? quantifier",
R"""(root ::= "a"?)""",
{
"",
"a"
},
{
"b",
"ab",
"aa",
"ba",
}
);
test_grammar(
"mixed quantifiers",
R"""(
root ::= cons+ vowel* cons? (vowel cons)*
vowel ::= [aeiouy]
cons ::= [bcdfghjklmnpqrstvwxyz]
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
}
);
test_grammar(
"simple exact repetition",
R"""(
root ::= [ab]{4}
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
}
);
test_grammar(
"simple min repetition",
R"""(
root ::= [ab]{4,}
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
}
);
test_grammar(
"simple max repetition",
R"""(
root ::= [ab]{0,4}
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
}
);
test_grammar(
"min / max repetition",
R"""(
root ::= ("0x" [A-F0-9]{2} " "?){3,5}
)""",
{
"0xFF 0x12 0xAB",
"0xFF 0x12 0xAB 0x00 0x00",
},
{
"",
"0xFF",
"0xFF 0x12",
"0xFF 0x12 0xAB 0x00 0x00 0x00",
}
);
}
static void test_failure_missing_root() {
fprintf(stderr, "⚫ Testing missing root node:\n");
const std::string grammar_str = R"""(
rot ::= expr
expr ::= term ("+" term)*
term ::= number
number ::= [0-9]+)""";
llama_grammar_parser parsed_grammar;
parsed_grammar.parse(grammar_str.c_str());
assert(!parsed_grammar.rules.empty());
assert(parsed_grammar.symbol_ids.find("root") == parsed_grammar.symbol_ids.end());
fprintf(stderr, "  ✅︎ Passed\n");
}
static void test_failure_missing_reference() {
fprintf(stderr, "⚫ Testing missing reference node:\n");
const std::string grammar_str =
R"""(root ::= expr
expr ::= term ("+" term)*
term ::= numero
number ::= [0-9]+)""";
fprintf(stderr, "    Expected error:  ");
llama_grammar_parser parsed_grammar;
parsed_grammar.parse(grammar_str.c_str());
assert(parsed_grammar.rules.empty());
fprintf(stderr, "    End of expected error.\n");
fprintf(stderr, "  ✅︎ Passed\n");
}
static void test_failure_left_recursion() {
fprintf(stderr, "⚫ Testing left recursion detection:\n");
const std::string simple_str = R"""(root ::= "a" | root "a")""";
assert(test_build_grammar_fails(simple_str));
const std::string medium_str = R"""(
root ::= asdf
asdf ::= "a" | asdf "a"
)""";
assert(test_build_grammar_fails(medium_str));
const std::string hard_str = R"""(
root ::= asdf
asdf ::= "a" | foo "b"
foo ::= "c" | asdf "d" | "e")""";
assert(test_build_grammar_fails(hard_str));
const std::string hardest_str = R"""(
root ::= asdf
asdf ::= "a" | foo "b"
foo ::= "c" | empty asdf "d" | "e"
empty ::= "blah" | )""";
assert(test_build_grammar_fails(hardest_str));
fprintf(stderr, "  ✅︎ Passed\n");
}
static void test_json_schema() {
test_schema(
"empty schema (object)",
R"""(
{}
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
}
);
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
}
);
test_schema(
"string",
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
}
);
test_schema(
"string w/ min length 1",
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
}
);
test_schema(
"string w/ min length 3",
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
}
);
test_schema(
"string w/ max length",
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
}
);
test_schema(
"string w/ min & max length",
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
}
);
test_schema(
"boolean",
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
}
);
test_schema(
"integer",
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
}
);
test_schema(
"string const",
R"""({
"const": "foo"
})""",
{
R"""("foo")""",
},
{
R"""(foo)""",
R"""("bar")""",
}
);
test_schema(
"non-string const",
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
}
);
test_schema(
"non-string const",
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
}
);
test_schema(
"simple pattern",
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
}
);
test_schema(
"pattern with escapes",
R"""({
"pattern": "^a\\^\\$\\.\\[\\]\\(\\)\\|\\{\\}\\*\\+\\?b$"
})""",
{
R"""("a^$.[]()|{}*+?b")""",
},
{
R"""("ab")""",
}
);
test_schema(
"",
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
}
);
test_schema(
"min+max items",
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
},
{
R"""([1, 2])""",
R"""([1, 2, 3, 4, 5, 6])""",
R"""(1)""",
}
);
test_schema(
"object properties",
R"""({
"type": "object",
"properties": {
"number": { "type": "number" },
"street_name": { "type": "string" },
"street_type": { "enum": ["Street", "Avenue", "Boulevard"] }
}
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
}
);
test_schema(
"additional properties can't override other properties",
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
}
);
test_schema(
"object properties, additionalProperties: true",
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
}
);
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
}
);
test_schema(
"required + optional props each in original order",
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
}
);
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
"uniqueItems": true
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
R"""({"productId": 1, "productName": "A green door"})""",
R"""({"productName": "A green door", "price": 12.50})""",
R"""({"productId": 1, "productName": "A green door", "price": 12.50, "tags": []})""",
R"""({"productId": 1, "productName": "A green door", "price": 12.50, "dimensions": {"length": 785, "width": 250.5, "height": -0.359}, "tags": ["home", "green"]})""",
}
);
}
int main() {
fprintf(stdout, "Running grammar integration tests...\n");
test_simple_grammar();
test_complex_grammar();
test_special_chars();
test_quantifiers();
test_failure_missing_root();
test_failure_missing_reference();
test_failure_left_recursion();
test_json_schema();
fprintf(stdout, "All tests passed.\n");
return 0;
}