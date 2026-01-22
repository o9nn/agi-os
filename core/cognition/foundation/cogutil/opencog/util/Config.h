#ifndef _OPENCOG_CONFIG_H
#define _OPENCOG_CONFIG_H
#include <fstream>
#include <map>
#include <string>
#include <vector>
namespace opencog
{
class Config
{
protected:
std::map<std::string, std::string> _table;
bool _no_config_loaded;
bool _had_to_search;
std::string _path_where_found;
std::string _abs_path;
std::string _cfg_filename;
void check_for_file(std::ifstream&, const char *, const char *);
void setup_logger();
public:
~Config();
Config();
static Config* createInstance(void);
virtual void reset();
void load(const char* config_file, bool resetFirst = true);
const std::string& path_where_found() const { return _path_where_found; }
const std::vector<std::string> search_paths() const;
const std::string& search_file() const { return _cfg_filename; }
const bool has(const std::string &parameter_name) const;
void set(const std::string &parameter_name, const std::string &parameter_value);
const std::string& get(const std::string &, const std::string& = "") const;
const std::string& operator[](const std::string &) const;
int get_int(const std::string &, int = 0) const;
long get_long(const std::string &, long = 0) const;
double get_double(const std::string &, double = 0.0) const;
bool get_bool(const std::string &, bool = false) const;
std::string to_string() const;
};
typedef Config* ConfigFactory(void);
Config& config(ConfigFactory* = Config::createInstance,
bool overwrite = false);
}
#endif