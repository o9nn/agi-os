/*
 * Cognitive-Grip: Unified Abstraction Layer for AGI-OS
 * Provides seamless integration between CogNumach, HurdCog, and OpenCog
 */

#ifndef _OPENCOG_COGNITIVE_GRIP_H
#define _OPENCOG_COGNITIVE_GRIP_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cogutil/Logger.h>
#include <memory>
#include <string>
#include <map>

namespace opencog {
namespace cognitive_grip {

/**
 * CognitiveGrip: Central coordination point for AGI-OS cognitive synergy
 * 
 * This class provides unified access to:
 * - CogNumach microkernel primitives
 * - HurdCog operating system services
 * - OpenCog cognitive framework
 * - AtomSpace knowledge representation
 * - Storage backends (RocksDB, PostgreSQL, CogServer)
 */
class CognitiveGrip {
public:
    CognitiveGrip();
    ~CognitiveGrip();

    // Initialize cognitive grip with unified configuration
    bool initialize(const std::string& config_file);

    // Get the unified AtomSpace
    AtomSpacePtr getAtomSpace() const { return _atomspace; }

    // Microkernel interface
    bool initializeMicrokernel();
    bool getMicrokernelStatus(std::map<std::string, std::string>& status);

    // Operating system interface
    bool initializeOS();
    bool getOSStatus(std::map<std::string, std::string>& status);

    // Cognitive component interface
    bool registerCognitiveComponent(const std::string& name, 
                                   const std::string& type);
    bool invokeCognitiveComponent(const std::string& name,
                                 const std::string& input);

    // Storage interface
    bool initializeStorage(const std::string& backend_type);
    bool storeKnowledgeGraph();
    bool loadKnowledgeGraph();

private:
    AtomSpacePtr _atomspace;
    std::map<std::string, void*> _components;
    bool _initialized;

    Logger _logger;
};

} // namespace cognitive_grip
} // namespace opencog

#endif // _OPENCOG_COGNITIVE_GRIP_H
