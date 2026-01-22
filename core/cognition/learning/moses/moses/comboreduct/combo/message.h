#ifndef _COMBO_MESSAGE_H
#define _COMBO_MESSAGE_H
#include <ostream>
#include <set>
#include <string>
#include <boost/operators.hpp>
namespace opencog { namespace combo {
class message
: boost::less_than_comparable<message>,
boost::equality_comparable<message>
{
private:
std::string _content;
public:
message(const std::string& m) {
_content = m;
}
std::string getContent() const {
return _content;
}
bool operator==(const message& m) const {
return _content == m.getContent();
}
bool operator<(const message& m) const {
return _content < m.getContent();
}
static std::string prefix() {
return "message:";
}
};
typedef std::set<message> message_set;
typedef message_set::iterator message_set_it;
typedef message_set::const_iterator message_set_const_it;
std::ostream& operator<<(std::ostream&, const opencog::combo::message&);
}
}
#endif