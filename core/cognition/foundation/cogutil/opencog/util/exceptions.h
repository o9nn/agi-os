#ifndef _OPENCOG_EXCEPTIONS_H
#define _OPENCOG_EXCEPTIONS_H
#include <string>
#include <iostream>
#include <stdarg.h>
#include <string.h>
#include <opencog/util/macros.h>
namespace opencog
{
class StandardException : public std::exception
{
private:
mutable char * message;
protected:
void parse_error_message(const char* fmt, va_list ap,
bool logError=true);
void parse_error_message(const char * trace, const char* fmt,
va_list ap, bool logError=true);
public:
StandardException();
StandardException(const StandardException&);
StandardException& operator=(const StandardException&);
virtual ~StandardException() _GLIBCXX_USE_NOEXCEPT;
virtual const char* what() const _GLIBCXX_USE_NOEXCEPT {
return get_message();
}
const char* get_message() const;
void set_message(const char *) const;
};
class RuntimeException : public StandardException
{
public:
RuntimeException(const char*, const char*, ...);
RuntimeException(const char*, const char*, va_list);
RuntimeException();
};
class SyntaxException : public RuntimeException
{
public:
SyntaxException(const char*, const char*, ...);
SyntaxException(const char*, const char*, va_list);
};
class IOException : public RuntimeException
{
public:
IOException(const char*, const char*, ...);
IOException(const char*, const char*, va_list);
};
class ComboException : public RuntimeException
{
public:
ComboException(const char*, const char*, ...);
ComboException(const char*, const char*, va_list);
};
class IndexErrorException : public RuntimeException
{
public:
IndexErrorException(const char*, const char*, ...);
IndexErrorException(const char*, const char*, va_list);
};
class InvalidParamException : public RuntimeException
{
public:
InvalidParamException(const char*, const char*, ...);
InvalidParamException(const char*, const char*, va_list);
};
class InconsistenceException : public RuntimeException
{
public:
InconsistenceException(const char*, const char*, ...);
InconsistenceException(const char*, const char*, va_list);
};
class FatalErrorException : public StandardException
{
public:
FatalErrorException(const char*, const char*, ...);
FatalErrorException(const char*, const char*, va_list);
};
class NetworkException : public StandardException
{
public:
NetworkException(const char*, const char*, ...);
NetworkException(const char*, const char*, va_list);
};
class AssertionException : public StandardException
{
public:
AssertionException(const char*, ...);
AssertionException(const char*, va_list);
};
class SilentException : public RuntimeException
{
public:
SilentException(void) {}
};
class DeleteException : public SilentException
{
public:
DeleteException(void) {}
};
class NestingException : public SilentException
{
public:
NestingException(void) {}
};
class NotEvaluatableException : public SilentException
{
public:
NotEvaluatableException(void) {}
};
class NotFoundException : public SilentException
{
public:
NotFoundException(void) {}
NotFoundException(const char*, const char*, ...);
NotFoundException(const char*, const char*, va_list);
};
class TypeCheckException : public SilentException
{
public:
TypeCheckException(void) {}
};
inline std::ostream& operator<<(std::ostream& out,
const StandardException& ex)
{
out << ex.what();
return out;
}
}
#endif