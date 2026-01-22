#include <opencog/attentionbank/avalue/AttentionValue.h>
#include <opencog/atomspace/AtomSpace.h>
#ifndef ATTENTION_STAT_H
#define ATTENTION_STAT_H
using av_sti = opencog::AttentionValue::sti_t;
struct AVStat{
av_sti heblink_sti_gain = 0;
av_sti link_sti_gain = 0;
av_sti direct_sti_gain = 0;
av_sti spreading = 0;
av_sti rent = 0;
};
extern std::unordered_map<opencog::Handle, AVStat> atom_avstat;
#endif