#pragma once
#include <memory>
#include "MettaLexer.h"
#include "ParserActions.h"
using namespace std;
namespace metta {
class MettaParser {
public:
MettaParser(const string& metta_string, shared_ptr<ParserActions> parser_actions = nullptr);
MettaParser(shared_ptr<MettaLexer> lexer, shared_ptr<ParserActions> parser_actions = nullptr);
~MettaParser();
bool parse(bool throw_on_parse_error = true);
private:
void _init(shared_ptr<ParserActions> parser_actions);
void _error(bool throw_flag,
const string& error_message,
const string& token_text,
unsigned char token_type);
shared_ptr<MettaLexer> lexer;
shared_ptr<ParserActions> actions;
};
}