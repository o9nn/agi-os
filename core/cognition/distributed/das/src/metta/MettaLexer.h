#pragma once
#include <map>
#include <memory>
#include <queue>
#include <stack>
#include <string>
#include "Token.h"
using namespace std;
namespace metta {
enum LexerState { START, READING_LITERAL_STRING, READING_NUMBER, READING_SYMBOL_OR_VARIABLE };
class MettaLexer {
public:
MettaLexer(unsigned int input_buffer_size = DEFAULT_INPUT_BUFFER_SIZE);
MettaLexer(const string& metta_string);
~MettaLexer();
void attach_file(const string& file_name);
void attach_string(const string& metta_string);
unique_ptr<Token> next();
void stack_metta_string();
void pop_metta_string();
unsigned int line_number;
string current_metta_string;
private:
void _init(unsigned int input_buffer_size);
void _attach_string(const string& metta_string);
inline char _read_next_char();
void _rewind_input_buffer(unsigned int n);
void _feed_from_file();
bool _feed_input_buffer();
void _error(LexerState state, const string& error_message, char c);
static unsigned int DEFAULT_INPUT_BUFFER_SIZE;
char* input_buffer;
unsigned int input_buffer_size;
bool single_string_flag;
bool file_input_flag;
unsigned int reading_cursor;
unsigned int writing_cursor;
queue<string> attached_strings;
queue<string> attached_file_names;
map<string, long> current_offset;
stack<string> metta_string_stack;
};
}