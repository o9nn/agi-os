#ifndef _OPENCOG_LOGGER_H
#define _OPENCOG_LOGGER_H
#include <cstdarg>
#include <map>
#include <mutex>
#include <sstream>
#include <string>
#include <thread>
#include <opencog/util/concurrent_queue.h>
#ifdef DEBUG
#undef DEBUG
#endif
namespace opencog
{
class Logger
{
void set(const Logger&);
public:
enum Level { NONE, ERROR, WARN, INFO, DEBUG, FINE, BAD_LEVEL=255 };
static Level get_level_from_string(const std::string&);
static const char* get_level_string(const Level);
Logger(const std::string &fileName = "opencog.log",
Level level = INFO, bool timestampEnabled = true);
Logger(const Logger&);
~Logger();
Logger& operator=(const Logger& log);
void set_level(Level);
void set_level(const std::string& str) {
set_level(get_level_from_string(str));
}
Level get_level() const;
void set_backtrace_level(Level);
void set_backtrace_level(const std::string& str) {
set_backtrace_level(get_level_from_string(str));
}
Level get_backtrace_level() const;
void set_filename(const std::string&);
const std::string& get_filename() const;
void set_component(const std::string&);
const std::string& get_component() const;
void set_timestamp_flag(bool);
void set_thread_id_flag(bool);
bool get_thread_id_flag() const;
void set_print_to_stdout_flag(bool);
bool get_print_to_stdout_flag() const;
void set_print_level_flag(bool);
void set_sync_flag(bool);
void set_print_error_level_stdout();
void log(Level level, const std::string &);
void backtrace();
void logva(Level level, const char *, va_list args);
void log  (Level level, const char *, ...);
class Base
{
public:
Base(const Base& b) : logger(b.logger), lvl(b.lvl) {}
template<typename T> std::stringstream& operator<<(const T& v)
{
ss << v;
return ss;
}
~Base()
{
if (0 < ss.str().length())
logger.log(lvl, ss.str());
}
protected:
friend class Logger;
Base(Logger& l, Level v) : logger(l), lvl(v) {}
private:
Logger& logger;
std::stringstream ss;
Level lvl;
};
class Error : public Base
{
public:
void operator()(const std::string &txt) { logger.log(ERROR, txt); }
void operator()(const char *, ...);
Base operator()() { return *this; }
protected:
friend class Logger;
Error(Logger& l) : Base(l, ERROR) {}
};
Error error;
class Warn : public Base
{
public:
void operator()(const std::string &txt) { logger.log(WARN, txt); }
void operator()(const char *, ...);
Base operator()() { return *this; }
protected:
friend class Logger;
Warn(Logger& l) : Base(l, WARN) {}
};
Warn warn;
class Info : public Base
{
public:
void operator()(const std::string &txt) { logger.log(INFO, txt); }
void operator()(const char *, ...);
Base operator()() { return *this; }
protected:
friend class Logger;
Info(Logger& l) : Base(l, INFO) {}
};
Info info;
class Debug : public Base
{
public:
void operator()(const std::string &txt) { logger.log(DEBUG, txt); }
void operator()(const char *, ...);
Base operator()() { return *this; }
protected:
friend class Logger;
Debug(Logger& l) : Base(l, DEBUG) {}
};
Debug debug;
class Fine : public Base
{
public:
void operator()(const std::string &txt) { logger.log(FINE, txt); }
void operator()(const char *, ...);
Base operator()() { return *this; }
protected:
friend class Logger;
Fine(Logger& l) : Base(l, FINE) {}
};
Fine fine;
public:
bool is_enabled(Level level) const { return level <= currentLevel; }
bool is_error_enabled() const { return ERROR <= currentLevel; }
bool is_warn_enabled() const { return WARN <= currentLevel; }
bool is_info_enabled() const { return INFO <= currentLevel; }
bool is_debug_enabled() const { return DEBUG <= currentLevel; }
bool is_fine_enabled() const { return FINE <= currentLevel; }
void flush();
private:
std::string component;
Level currentLevel;
Level backTraceLevel;
bool timestampEnabled;
bool threadIdEnabled;
bool logEnabled;
bool printToStdout;
bool printLevel;
bool syncEnabled;
void enable();
void disable();
class LogWriter
{
std::string fileName;
FILE *logfile;
bool writingLoopActive;
std::thread writer_thread;
std::mutex the_mutex;
concurrent_queue< std::string* > msg_queue;
bool pending_write;
void start_write_loop();
void stop_write_loop();
void writing_loop();
void write_msg(const std::string&);
public:
LogWriter(void);
~LogWriter();
void setFileName(const std::string&);
const std::string& getFileName(void) const
{ return fileName; }
void qmsg(const std::string& str)
{ msg_queue.push(new std::string(str)); }
size_t size(void)
{ return msg_queue.size(); }
void flush();
};
LogWriter* _log_writer;
static std::mutex _loggers_mtx;
static std::map<std::string, LogWriter*> _loggers;
};
Logger& logger();
#define LAZY_LOG_ERROR if(logger().is_error_enabled()) logger().error()
#define LAZY_LOG_WARN if(logger().is_warn_enabled()) logger().warn()
#define LAZY_LOG_INFO if(logger().is_info_enabled()) logger().info()
#define LAZY_LOG_DEBUG if(logger().is_debug_enabled()) logger().debug()
#define LAZY_LOG_FINE if(logger().is_fine_enabled()) logger().fine()
}
#endif