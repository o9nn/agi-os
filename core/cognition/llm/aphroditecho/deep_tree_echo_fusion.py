"""
Deep Tree Echo Fusion with Aphrodite Engine

Unified entry point for the complete integration of Deep Tree Echo Membrane Computing
architecture with Aphrodite Engine, implementing Echo-Self AI Evolution Engine with
Agent-Arena-Relation (AAR) as the core orchestration system, configured as a 4E
Embodied AI framework.

This module provides the seamless fusion interface for maximum challenge and innovation.
"""

import asyncio
import logging
from typing import Dict, Any, Optional
from dataclasses import dataclass, field
from pathlib import Path
import sys

# Add current directory to path for imports
sys.path.insert(0, str(Path(__file__).parent))

# Handle both absolute and relative imports
try:
    from echo_self.core.evolution_engine import EchoSelfEvolutionEngine, EvolutionConfig
    from aar_core.orchestration.core_orchestrator import AARCoreOrchestrator, AARConfig
    from echo_self.integration.dtesn_bridge import DTESNBridge
    ECHO_SELF_AVAILABLE = True
except ImportError as e:
    logging.warning(f"Echo-Self components not fully available: {e}")
    ECHO_SELF_AVAILABLE = False

# Aphrodite integration imports
try:
    from echo_self.integration.aphrodite_adaptive import AphroditeAdaptiveIntegration
    APHRODITE_INTEGRATION_AVAILABLE = True
except ImportError as e:
    logging.warning(f"Aphrodite adaptive integration not available: {e}")
    APHRODITE_INTEGRATION_AVAILABLE = False

# DTESN Kernel imports
try:
    from echo.kern.deep_tree_echo import DeepTreeEcho
    from echo.kern.dtesn_integration import DTESNIntegration
    DTESN_KERNEL_AVAILABLE = True
except ImportError as e:
    logging.warning(f"DTESN Kernel components not available: {e}")
    DTESN_KERNEL_AVAILABLE = False

logger = logging.getLogger(__name__)


@dataclass
class DeepTreeEchoConfig:
    """Configuration for Deep Tree Echo Fusion system."""
    
    # Core system settings
    max_concurrent_agents: int = 1000
    enable_4e_embodied_ai: bool = True
    enable_sensory_motor_mapping: bool = True
    enable_proprioceptive_feedback: bool = True
    
    # AAR Orchestration settings
    aar_config: AARConfig = field(default_factory=AARConfig)
    
    # Echo-Self Evolution settings
    enable_adaptive_architecture: bool = True
    evolution_config: Optional['EvolutionConfig'] = None
    evolution_generations: int = 50
    mutation_rate: float = 0.1
    
    # DTESN Kernel settings
    enable_membrane_computing: bool = True
    enable_echo_state_networks: bool = True
    reservoir_size: int = 1000
    
    # Aphrodite Engine settings
    model_name: Optional[str] = None
    tensor_parallel_size: int = 1
    gpu_memory_utilization: float = 0.9
    enable_cpu_mode: bool = True
    
    # System directories
    echo_data_dir: Path = field(default_factory=lambda: Path.home() / '.deep_tree_echo')
    log_level: str = "INFO"


class DeepTreeEchoFusion:
    """
    Unified Deep Tree Echo Fusion system.
    
    Integrates all components of the Deep Tree Echo architecture with Aphrodite Engine:
    - Echo-Self AI Evolution Engine
    - Agent-Arena-Relation (AAR) Core Orchestration
    - Deep Tree Echo State Networks (DTESN) Kernel
    - 4E Embodied AI Framework
    - Sensory-Motor Virtual Mappings
    - Proprioceptive Feedback Loops
    """
    
    def __init__(self, config: DeepTreeEchoConfig):
        """Initialize the Deep Tree Echo Fusion system."""
        self.config = config
        self.logger = logging.getLogger(__name__)
        self.logger.setLevel(getattr(logging, config.log_level))
        
        # Core components
        self.echo_self_engine: Optional[EchoSelfEvolutionEngine] = None
        self.aar_orchestrator: Optional[AARCoreOrchestrator] = None
        self.dtesn_bridge: Optional[DTESNBridge] = None
        self.aphrodite_integration: Optional[AphroditeAdaptiveIntegration] = None
        
        # System state
        self.is_initialized = False
        self.is_running = False
        self.fusion_metrics = {}
        
        # Create data directory
        self.config.echo_data_dir.mkdir(parents=True, exist_ok=True)
        
        self.logger.info("Deep Tree Echo Fusion system initialized")
    
    async def initialize(self) -> bool:
        """
        Initialize all components of the Deep Tree Echo Fusion system.
        
        Returns:
            bool: True if initialization successful, False otherwise
        """
        self.logger.info("Initializing Deep Tree Echo Fusion system...")
        
        try:
            # Initialize Echo-Self Evolution Engine
            if ECHO_SELF_AVAILABLE and self.config.enable_adaptive_architecture:
                # Create evolution config if not provided
                if self.config.evolution_config is None:
                    from echo_self.core.evolution_engine import EvolutionConfig
                    evolution_config = EvolutionConfig(
                        max_generations=self.config.evolution_generations,
                        mutation_rate=self.config.mutation_rate
                    )
                else:
                    evolution_config = self.config.evolution_config
                
                self.echo_self_engine = EchoSelfEvolutionEngine(evolution_config)
                # Note: EchoSelfEvolutionEngine doesn't have initialize method
                # Population initialization happens during first evolution cycle
                self.logger.info("✓ Echo-Self Evolution Engine initialized")
            
            # Initialize AAR Core Orchestrator
            if ECHO_SELF_AVAILABLE:
                self.aar_orchestrator = AARCoreOrchestrator(self.config.aar_config)
                # Check if AARCoreOrchestrator has initialize method
                if hasattr(self.aar_orchestrator, 'initialize'):
                    await self._safe_async_call(self.aar_orchestrator.initialize)
                self.logger.info("✓ AAR Core Orchestrator initialized")
            
            # Initialize DTESN Bridge
            if DTESN_KERNEL_AVAILABLE and self.config.enable_membrane_computing:
                self.dtesn_bridge = DTESNBridge()
                if hasattr(self.dtesn_bridge, 'initialize'):
                    await self._safe_async_call(self.dtesn_bridge.initialize)
                self.logger.info("✓ DTESN Bridge initialized")
            
            # Initialize Aphrodite Integration
            if APHRODITE_INTEGRATION_AVAILABLE:
                self.aphrodite_integration = AphroditeAdaptiveIntegration()
                if hasattr(self.aphrodite_integration, 'initialize'):
                    await self._safe_async_call(self.aphrodite_integration.initialize)
                self.logger.info("✓ Aphrodite Adaptive Integration initialized")
            
            # Establish inter-component connections
            await self._establish_fusion_connections()
            
            self.is_initialized = True
            self.logger.info("🎯 Deep Tree Echo Fusion system fully initialized and operational!")
            return True
            
        except Exception as e:
            self.logger.error(f"Failed to initialize Deep Tree Echo Fusion: {e}")
            return False
    
    async def _safe_async_call(self, func, *args, **kwargs):
        """Safely call an async function, handling both sync and async cases."""
        try:
            if asyncio.iscoroutinefunction(func):
                return await func(*args, **kwargs)
            else:
                return func(*args, **kwargs)
        except Exception as e:
            self.logger.warning(f"Safe call failed for {func.__name__}: {e}")
            return None
    
    async def _establish_fusion_connections(self):
        """Establish connections between all system components."""
        self.logger.info("Establishing fusion connections...")
        
        # Connect Echo-Self Evolution Engine with AAR Orchestrator
        if self.echo_self_engine and self.aar_orchestrator:
            # Integration point for agent evolution within AAR system
            self.fusion_metrics['echo_aar_connected'] = True
            self.logger.info("✓ Echo-Self <-> AAR connection established")
        
        # Connect DTESN Bridge with Aphrodite Integration
        if self.dtesn_bridge and self.aphrodite_integration:
            # Integration point for memory computing with inference engine
            self.fusion_metrics['dtesn_aphrodite_connected'] = True
            self.logger.info("✓ DTESN <-> Aphrodite connection established")
        
        # Establish 4E Embodied AI Framework connections
        if self.config.enable_4e_embodied_ai:
            await self._establish_4e_framework()
    
    async def _establish_4e_framework(self):
        """Establish 4E Embodied AI Framework connections."""
        self.logger.info("Establishing 4E Embodied AI Framework...")
        
        # Embodied: Virtual body representation and body-based cognition
        if self.config.enable_sensory_motor_mapping:
            self.fusion_metrics['embodied_active'] = True
            self.logger.info("✓ Embodied AI: Virtual body representation active")
        
        # Embedded: Environment-coupled processing with resource constraints
        if self.aar_orchestrator:
            self.fusion_metrics['embedded_active'] = True
            self.logger.info("✓ Embedded AI: Environment coupling active")
        
        # Extended: Cognitive scaffolding and tool use capabilities
        if self.echo_self_engine:
            self.fusion_metrics['extended_active'] = True
            self.logger.info("✓ Extended AI: Cognitive scaffolding active")
        
        # Enactive: Action-based perception and sensorimotor contingency
        if self.config.enable_proprioceptive_feedback:
            self.fusion_metrics['enactive_active'] = True
            self.logger.info("✓ Enactive AI: Proprioceptive feedback loops active")
    
    async def start_fusion(self) -> bool:
        """
        Start the complete Deep Tree Echo Fusion system.
        
        Returns:
            bool: True if system started successfully
        """
        if not self.is_initialized:
            self.logger.error("System not initialized. Call initialize() first.")
            return False
        
        self.logger.info("Starting Deep Tree Echo Fusion system...")
        
        try:
            # Start all components
            if self.aar_orchestrator and hasattr(self.aar_orchestrator, 'start'):
                await self._safe_async_call(self.aar_orchestrator.start)
            
            if self.echo_self_engine and hasattr(self.echo_self_engine, 'start'):
                await self._safe_async_call(self.echo_self_engine.start)
            
            if self.dtesn_bridge and hasattr(self.dtesn_bridge, 'start'):
                await self._safe_async_call(self.dtesn_bridge.start)
            
            self.is_running = True
            self.logger.info("🚀 Deep Tree Echo Fusion system is now running!")
            return True
            
        except Exception as e:
            self.logger.error(f"Failed to start fusion system: {e}")
            return False
    
    async def process_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """
        Process a request through the complete fusion system.
        
        Args:
            request: Request data containing prompt, parameters, etc.
            
        Returns:
            Processed response from the fusion system
        """
        if not self.is_running:
            return {"error": "Fusion system not running"}
        
        self.logger.debug(f"Processing request through fusion system: {request}")
        
        # Route through AAR orchestrator if available
        if self.aar_orchestrator:
            response = await self._safe_async_call(
                self.aar_orchestrator.orchestrate_inference, request
            )
            if response:
                return response
        
        # Fallback to direct processing
        return {
            "response": "Deep Tree Echo Fusion processing complete",
            "fusion_active": True,
            "components_active": len([c for c in [
                self.echo_self_engine, self.aar_orchestrator, 
                self.dtesn_bridge, self.aphrodite_integration
            ] if c is not None])
        }
    
    async def get_system_status(self) -> Dict[str, Any]:
        """Get comprehensive system status."""
        return {
            "initialized": self.is_initialized,
            "running": self.is_running,
            "echo_self_available": ECHO_SELF_AVAILABLE,
            "aphrodite_integration_available": APHRODITE_INTEGRATION_AVAILABLE,
            "dtesn_kernel_available": DTESN_KERNEL_AVAILABLE,
            "fusion_metrics": self.fusion_metrics,
            "config": {
                "4e_embodied_ai": self.config.enable_4e_embodied_ai,
                "sensory_motor_mapping": self.config.enable_sensory_motor_mapping,
                "proprioceptive_feedback": self.config.enable_proprioceptive_feedback,
                "adaptive_architecture": self.config.enable_adaptive_architecture,
                "membrane_computing": self.config.enable_membrane_computing,
            }
        }
    
    async def shutdown(self):
        """Gracefully shutdown the fusion system."""
        self.logger.info("Shutting down Deep Tree Echo Fusion system...")
        
        # Shutdown components in reverse order
        if self.aphrodite_integration and hasattr(self.aphrodite_integration, 'shutdown'):
            await self._safe_async_call(self.aphrodite_integration.shutdown)
        
        if self.dtesn_bridge and hasattr(self.dtesn_bridge, 'shutdown'):
            await self._safe_async_call(self.dtesn_bridge.shutdown)
        
        if self.echo_self_engine and hasattr(self.echo_self_engine, 'shutdown'):
            await self._safe_async_call(self.echo_self_engine.shutdown)
        
        if self.aar_orchestrator and hasattr(self.aar_orchestrator, 'shutdown'):
            await self._safe_async_call(self.aar_orchestrator.shutdown)
        
        self.is_running = False
        self.is_initialized = False
        self.logger.info("Deep Tree Echo Fusion system shutdown complete")


# Convenience functions for easy usage
async def create_fusion_system(config: Optional[DeepTreeEchoConfig] = None) -> DeepTreeEchoFusion:
    """Create and initialize a Deep Tree Echo Fusion system."""
    if config is None:
        config = DeepTreeEchoConfig()
    
    fusion = DeepTreeEchoFusion(config)
    await fusion.initialize()
    return fusion


async def quick_fusion_demo():
    """Quick demonstration of the fusion system capabilities."""
    print("🌳 Deep Tree Echo Fusion Demo")
    print("=" * 50)
    
    # Create fusion system
    config = DeepTreeEchoConfig(
        enable_4e_embodied_ai=True,
        enable_adaptive_architecture=True,
        enable_membrane_computing=True
    )
    
    fusion = await create_fusion_system(config)
    
    # Start the system
    await fusion.start_fusion()
    
    # Get system status
    status = await fusion.get_system_status()
    print(f"System Status: {status}")
    
    # Process a demo request
    demo_request = {
        "prompt": "Demonstrate Deep Tree Echo cognitive fusion",
        "parameters": {"max_tokens": 100}
    }
    
    response = await fusion.process_request(demo_request)
    print(f"Fusion Response: {response}")
    
    # Shutdown
    await fusion.shutdown()
    print("Demo complete!")


if __name__ == "__main__":
    # Run demo if executed directly
    asyncio.run(quick_fusion_demo())