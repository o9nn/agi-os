#ifndef _OPENCOG_MINERLOGGER_H_
#define _OPENCOG_MINERLOGGER_H_
#include <opencog/util/Logger.h>
namespace opencog
{
Logger& miner_logger();
#define LAZY_MINER_LOG_ERROR if(miner_logger().is_error_enabled()) miner_logger().error()
#define LAZY_MINER_LOG_WARN if(miner_logger().is_warn_enabled()) miner_logger().warn()
#define LAZY_MINER_LOG_INFO if(miner_logger().is_info_enabled()) miner_logger().info()
#define LAZY_MINER_LOG_DEBUG if(miner_logger().is_debug_enabled()) miner_logger().debug()
#define LAZY_MINER_LOG_FINE if(miner_logger().is_fine_enabled()) miner_logger().fine()
}
#endif