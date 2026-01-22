#pragma once
#include <map>
#include <string>
#define MAX_VARIABLE_NAME_SIZE ((unsigned int) 100)
#define MAX_NUMBER_OF_VARIABLES_IN_QUERY ((unsigned int) 100)
using namespace std;
namespace commons {
class Assignment {
public:
Assignment(bool unique_assignment_flag = false);
~Assignment();
bool assign(const string& label, const string& value);
const string& get(const string& label);
bool is_compatible(const Assignment& other);
void copy_from(const Assignment& other);
void add_assignments(const Assignment& other);
unsigned int variable_count();
string to_string();
bool operator==(const Assignment& other) const;
Assignment& operator=(const Assignment& other);
void clear();
map<string, string> table;
private:
static string EMPTY_VALUE;
bool unique_assignment_flag;
};
}