#ifndef _OPENCOG_ODOMETER_H
#define _OPENCOG_ODOMETER_H
#include <opencog/atomspace/AtomSpace.h>
namespace opencog
{
struct OdoFrame
{
HandleSet _open_points;
HandleSet _open_sections;
HandleSet _linkage;
size_t _nodo;
size_t _wheel;
void clear(void);
void print(void) const;
static void print_section(const Handle&);
};
struct Odometer
{
size_t _size;
HandleSeq _sections;
std::vector<size_t> _from_index;
HandleSeq _to_connectors;
size_t _step;
size_t _frame_depth;
void clear(void);
void print_odometer(const OdoFrame&) const;
void print_wheel(const OdoFrame&, size_t) const;
};
}
#endif