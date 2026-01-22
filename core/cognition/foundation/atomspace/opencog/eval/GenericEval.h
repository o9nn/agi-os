#ifndef _OPENCOG_GENERIC_EVAL_H
#define _OPENCOG_GENERIC_EVAL_H
#include <string>
namespace opencog {
class GenericEval
{
protected:
std::string _input_line;
std::string _error_string;
bool _pending_input;
bool _caught_error;
public:
GenericEval(void) :
_input_line(""),
_pending_input(false),
_caught_error(false) {}
virtual ~GenericEval() {}
virtual std::string get_name(void) const { return "GenericEval"; }
virtual bool input_pending()
{
return _pending_input;
}
virtual void clear_pending()
{
_input_line = "";
_error_string = "";
_pending_input = false;
_caught_error = false;
}
virtual bool eval_error(void)
{
return _caught_error;
}
virtual std::string get_error_string(void)
{
return _error_string;
}
virtual void begin_eval(void) = 0;
virtual void eval_expr(const std::string&) = 0;
virtual std::string poll_result(void) = 0;
virtual void interrupt(void) = 0;
};
}
#endif