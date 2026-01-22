#ifndef _OPENCOG_MPI_MOSES_H
#define _OPENCOG_MPI_MOSES_H
#include <atomic>
#include <future>
#include <opencog/util/pool.h>
#include "../deme/deme_expander.h"
#include "../metapopulation/metapopulation.h"
#include "moses_params.h"
namespace opencog { namespace moses {
#ifdef HAVE_MPI
class moses_mpi_comm
{
public:
moses_mpi_comm();
~moses_mpi_comm();
bool is_mpi_root();
int num_workers();
void dispatch_deme(int target, const combo_tree&, int max_evals);
int probe_for_deme();
void recv_deme(int source, scored_combo_tree_set&, int& n_evals,
const demeID_t& demeID);
void send_finished(int target);
int recv_more_work();
void recv_exemplar(combo_tree&);
void send_deme(const metapopulation&, int);
std::atomic<size_t> sent_bytes;
std::atomic<size_t> recv_bytes;
protected:
void send_tree(const combo_tree&, int target);
void recv_tree(combo_tree&, int source);
void send_cscore(const composite_score&, int target);
void recv_cscore(composite_score&, int source);
};
void mpi_moses_worker(metapopulation&,
deme_expander&,
moses_mpi_comm&);
void mpi_moses(metapopulation&,
deme_expander&,
const moses_parameters&,
moses_statistics&);
#else
static inline void mpi_moses(metapopulation& mp,
deme_expander& dex,
const moses_parameters& pa,
moses_statistics& stats)
{
OC_ASSERT(0, "There is no MPI support in this version of moses");
};
#endif
}
}
#endif