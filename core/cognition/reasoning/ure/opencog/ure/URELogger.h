#ifndef _OPENCOG_URELOGGER_H_
#define _OPENCOG_URELOGGER_H_
#include <opencog/util/Logger.h>
namespace opencog
{
Logger& ure_logger();
#define LAZY_URE_LOG_ERROR if(ure_logger().is_error_enabled()) ure_logger().error()
#define LAZY_URE_LOG_WARN if(ure_logger().is_warn_enabled()) ure_logger().warn()
#define LAZY_URE_LOG_INFO if(ure_logger().is_info_enabled()) ure_logger().info()
#define LAZY_URE_LOG_DEBUG if(ure_logger().is_debug_enabled()) ure_logger().debug()
#define LAZY_URE_LOG_FINE if(ure_logger().is_fine_enabled()) ure_logger().fine()
}
#endif