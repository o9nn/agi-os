#ifndef _OPENCOG_LOAD_TABLE_H
#define _OPENCOG_LOAD_TABLE_H
namespace opencog {
namespace atomese {
Handle load_atomese_io(
const std::string& file_name,
const std::string& target_feature=std::string(),
const std::string& timestamp_feature=std::string(),
const std::vector<std::string>& ignore_features=std::vector<std::string>());
Handle load_atomese_compact(
const std::string& file_name,
const std::string& target_feature=std::string(),
const std::string& timestamp_feature=std::string(),
const std::vector<std::string>& ignore_features=std::vector<std::string>());
Handle load_atomese_similarity(
const std::string& file_name,
const std::string& target_feature=std::string(),
const std::string& timestamp_feature=std::string(),
const std::vector<std::string>& ignore_features=std::vector<std::string>());
Handle load_atomese_unfolded(
const std::string& file_name,
const std::string& target_feature=std::string(),
const std::string& timestamp_feature=std::string(),
const std::vector<std::string>& ignore_features=std::vector<std::string>(),
const bool use_eval=false);
}
}
#endif