#ifndef OPENCOG_SERVER_LOAD_FILE_H_
#define OPENCOG_SERVER_LOAD_FILE_H_
#include <opencog/atomspace/AtomSpace.h>
namespace opencog {
#ifdef HAVE_GUILE
int load_scm_file (AtomSpace& as, const std::string& filename);
int load_scm_file_relative (AtomSpace& as, const std::string& filename,
std::vector<std::string> paths =
std::vector<std::string>());
#else
static inline int load_scm_file (AtomSpace& as, const std::string&) { return 2; }
static inline int load_scm_file_relative (AtomSpace& as, const std::string&,
std::vector<std::string> =
std::vector<std::string>()) { return 2; }
#endif
}
#endif