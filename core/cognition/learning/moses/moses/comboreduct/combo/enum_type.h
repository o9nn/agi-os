#ifndef _COMBO_ENUM_TYPE_H
#define _COMBO_ENUM_TYPE_H
#include <map>
#include <ostream>
#include <string>
#include <boost/thread.hpp>
#include <boost/operators.hpp>
namespace opencog { namespace combo {
class enum_t
: boost::less_than_comparable<enum_t>,
boost::equality_comparable<enum_t>
{
private:
unsigned id;
std::string _content;
static unsigned enum_issued;
static std::map<std::string, unsigned> enum_map;
static boost::shared_mutex id_mutex;
protected:
unsigned get_id(const std::string& token);
enum_t(const std::string &m, unsigned i)
{
_content = m;
id = i;
}
public:
enum_t(const std::string &m)
{
_content = m;
id = get_id (_content);
}
unsigned getId() const {
return id;
}
std::string getContent() const {
return _content;
}
bool operator==(const enum_t& m) const {
return id == m.id;
}
bool operator<(const enum_t& m) const {
return _content < m.getContent();
}
static std::string prefix() {
return "enum:";
}
static enum_t get_random_enum();
static size_t size();
static const enum_t& invalid_enum() {
static enum_t bad("", -1);
return bad;
}
};
std::ostream& operator<<(std::ostream&, const opencog::combo::enum_t&);
}
}
#endif