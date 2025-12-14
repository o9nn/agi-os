"""
Integration Manager - Orchestrates all 2do components with Aphrodite Engine

This manager coordinates the initialization, lifecycle, and interaction of all
integrated components following the Deep Tree Echo integration strategy.
"""

import asyncio
import json
import logging
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
import time

from .aar_gateway import AARGateway, GatewayRequest, GatewayResponse
from .function_registry import FunctionRegistry
from .integrations import (
    LLMAdapter, AiChatAdapter, GalateaAdapter, SparkAdapter,
    ArgcAdapter, LLMFunctionsAdapter, PaphosAdapter
)
from echo.sys.prompt_kernel import PromptStore

logger = logging.getLogger(__name__)


@dataclass
class IntegrationStatus:
    """Status information for a component integration."""
    component_name: str
    adapter_type: str
    status: str = "inactive"
    health: Dict[str, Any] = field(default_factory=dict)
    initialization_time: float = 0.0
    last_health_check: float = 0.0
    error_message: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)


class IntegrationManager:
    """
    Central manager for all 2do component integrations with Aphrodite Engine.
    
    Implements the complete integration strategy outlined in the Deep Tree Echo
    architecture, providing unified orchestration of all components through
    the AAR Gateway system.
    
    Architecture Layers:
    1. Experience Layer: galatea-UI, aichat REPL, API clients
    2. Gateway Layer: AAR Gateway + extended OpenAI endpoints  
    3. Orchestration Layer: AAR Core (Agent Registry, Arena Sessions, Relations)
    4. Cognition Layer: echo.sys integration
    5. Capability Layer: Function/Tool Registry + Memory Subsystem
    6. Execution Layer: Aphrodite Inference Engine
    7. External Services: paphos-backend integration
    """

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        self.config = config or {}
        
        # Initialize core components
        self.function_registry = FunctionRegistry()
        self.prompt_store = PromptStore()
        self.aar_gateway = AARGateway(self.config.get("gateway", {}))
        
        # Initialize adapters
        self.adapters: Dict[str, Any] = {}
        self.status: Dict[str, IntegrationStatus] = {}
        
        # Track initialization
        self.initialized = False
        self.initialization_errors: List[str] = []

    async def initialize(self) -> bool:
        """Initialize all component integrations."""
        if self.initialized:
            logger.info("Integration manager already initialized")
            return True
        
        logger.info("Initializing 2do Components Integration Manager")
        logger.info("=" * 60)
        
        start_time = time.time()
        
        try:
            # Initialize adapters in dependency order
            await self._initialize_core_adapters()
            await self._initialize_service_adapters()
            await self._initialize_ui_adapters()
            
            # Perform cross-component integration
            await self._perform_cross_integration()
            
            # Final health check
            await self._perform_initial_health_checks()
            
            self.initialized = True
            initialization_time = time.time() - start_time
            
            logger.info("=" * 60)
            logger.info(f"✓ Integration initialization completed in {initialization_time:.2f}s")
            
            # Generate initialization report
            await self._generate_initialization_report()
            
            return True
            
        except Exception as e:
            logger.error(f"Failed to initialize integration manager: {e}")
            self.initialization_errors.append(str(e))
            return False

    async def _initialize_core_adapters(self):
        """Initialize core adapters (function registry, etc.)."""
        logger.info("Initializing core adapters...")
        
        # Argc Adapter - CLI command schema
        try:
            start_time = time.time()
            self.adapters["argc"] = ArgcAdapter(self.function_registry)
            init_time = time.time() - start_time
            
            health = self.adapters["argc"].health_check()
            self.status["argc"] = IntegrationStatus(
                component_name="argc",
                adapter_type="ArgcAdapter",
                status="active" if health["argc_available"] else "compatibility_mode",
                health=health,
                initialization_time=init_time
            )
            logger.info(f"  ✓ Argc Adapter: {health['status']} ({init_time:.2f}s)")
            
        except Exception as e:
            logger.error(f"  ✗ Argc Adapter failed: {e}")
            self.initialization_errors.append(f"Argc: {e}")
        
        # LLM Functions Adapter - Tool layer
        try:
            start_time = time.time()
            self.adapters["llm_functions"] = LLMFunctionsAdapter(self.function_registry)
            init_time = time.time() - start_time
            
            health = self.adapters["llm_functions"].health_check()
            self.status["llm_functions"] = IntegrationStatus(
                component_name="llm_functions",
                adapter_type="LLMFunctionsAdapter",
                status="active",
                health=health,
                initialization_time=init_time
            )
            logger.info(f"  ✓ LLM Functions Adapter: {health['status']} ({init_time:.2f}s)")
            
        except Exception as e:
            logger.error(f"  ✗ LLM Functions Adapter failed: {e}")
            self.initialization_errors.append(f"LLM Functions: {e}")

    async def _initialize_service_adapters(self):
        """Initialize service adapters (backend services)."""
        logger.info("Initializing service adapters...")
        
        # Paphos Backend Adapter - Persistence
        try:
            start_time = time.time()
            self.adapters["paphos"] = PaphosAdapter(self.config.get("paphos", {}))
            init_time = time.time() - start_time
            
            health = self.adapters["paphos"].health_check()
            self.status["paphos"] = IntegrationStatus(
                component_name="paphos",
                adapter_type="PaphosAdapter",
                status=health["status"],
                health=health,
                initialization_time=init_time
            )
            logger.info(f"  ✓ Paphos Adapter: {health['status']} ({init_time:.2f}s)")
            
        except Exception as e:
            logger.error(f"  ✗ Paphos Adapter failed: {e}")
            self.initialization_errors.append(f"Paphos: {e}")
        
        # LLM Adapter - Agent abstractions
        try:
            start_time = time.time()
            self.adapters["llm"] = LLMAdapter(self.function_registry)
            init_time = time.time() - start_time
            
            health = self.adapters["llm"].health_check()
            self.status["llm"] = IntegrationStatus(
                component_name="llm",
                adapter_type="LLMAdapter",
                status="active" if health["llm_available"] else "compatibility_mode",
                health=health,
                initialization_time=init_time
            )
            logger.info(f"  ✓ LLM Adapter: {health['status']} ({init_time:.2f}s)")
            
        except Exception as e:
            logger.error(f"  ✗ LLM Adapter failed: {e}")
            self.initialization_errors.append(f"LLM: {e}")
        
        # AiChat Adapter - Multi-provider chat
        try:
            start_time = time.time()
            self.adapters["aichat"] = AiChatAdapter()
            init_time = time.time() - start_time
            
            health = self.adapters["aichat"].health_check()
            self.status["aichat"] = IntegrationStatus(
                component_name="aichat",
                adapter_type="AiChatAdapter",
                status=health["status"],
                health=health,
                initialization_time=init_time
            )
            logger.info(f"  ✓ AiChat Adapter: {health['status']} ({init_time:.2f}s)")
            
        except Exception as e:
            logger.error(f"  ✗ AiChat Adapter failed: {e}")
            self.initialization_errors.append(f"AiChat: {e}")

    async def _initialize_ui_adapters(self):
        """Initialize UI and presentation adapters."""
        logger.info("Initializing UI adapters...")
        
        # Spark Adapter - Prompt management
        try:
            start_time = time.time()
            self.adapters["spark"] = SparkAdapter(self.prompt_store)
            init_time = time.time() - start_time
            
            health = self.adapters["spark"].health_check()
            self.status["spark"] = IntegrationStatus(
                component_name="spark",
                adapter_type="SparkAdapter",
                status=health["status"],
                health=health,
                initialization_time=init_time
            )
            logger.info(f"  ✓ Spark Adapter: {health['status']} ({init_time:.2f}s)")
            
        except Exception as e:
            logger.error(f"  ✗ Spark Adapter failed: {e}")
            self.initialization_errors.append(f"Spark: {e}")
        
        # Galatea Adapter - Web UI
        try:
            start_time = time.time()
            self.adapters["galatea"] = GalateaAdapter(self.config.get("galatea", {}))
            init_time = time.time() - start_time
            
            health = self.adapters["galatea"].health_check()
            self.status["galatea"] = IntegrationStatus(
                component_name="galatea",
                adapter_type="GalateaAdapter",
                status=health["status"],
                health=health,
                initialization_time=init_time
            )
            logger.info(f"  ✓ Galatea Adapter: {health['status']} ({init_time:.2f}s)")
            
        except Exception as e:
            logger.error(f"  ✗ Galatea Adapter failed: {e}")
            self.initialization_errors.append(f"Galatea: {e}")

    async def _perform_cross_integration(self):
        """Perform cross-component integration tasks."""
        logger.info("Performing cross-component integration...")
        
        try:
            # Sync prompts from Spark to echo.sys
            if "spark" in self.adapters:
                synced_prompts = await self.adapters["spark"].sync_with_echo_sys()
                logger.info(f"  ✓ Synced {synced_prompts} prompts with echo.sys")
            
            # Register LLM models with AAR Gateway
            if "llm" in self.adapters:
                models = self.adapters["llm"].get_available_models()
                logger.info(f"  ✓ Registered {len(models)} LLM models")
            
            # Register AiChat models with AAR Gateway
            if "aichat" in self.adapters:
                models = self.adapters["aichat"].get_available_models()
                logger.info(f"  ✓ Registered {len(models)} AiChat models")
            
            # Sync function registry with gateway
            functions = self.function_registry.list_functions()
            logger.info(f"  ✓ {len(functions)} functions available in registry")
            
        except Exception as e:
            logger.error(f"Cross-integration failed: {e}")

    async def _perform_initial_health_checks(self):
        """Perform initial health checks on all components."""
        logger.info("Performing initial health checks...")
        
        for component_name, adapter in self.adapters.items():
            try:
                health = adapter.health_check()
                if component_name in self.status:
                    self.status[component_name].health = health
                    self.status[component_name].last_health_check = time.time()
                
            except Exception as e:
                logger.error(f"Health check failed for {component_name}: {e}")

    async def _generate_initialization_report(self):
        """Generate and log initialization report."""
        report = {
            "initialization_status": "completed" if self.initialized else "failed",
            "total_components": len(self.adapters),
            "active_components": len([s for s in self.status.values() if s.status == "active"]),
            "compatibility_mode_components": len([s for s in self.status.values() if s.status == "compatibility_mode"]),
            "failed_components": len(self.initialization_errors),
            "total_functions": len(self.function_registry.functions),
            "gateway_components": len(self.aar_gateway.components),
            "initialization_errors": self.initialization_errors
        }
        
        logger.info("\nInitialization Report:")
        logger.info(json.dumps(report, indent=2))

    async def handle_request(self, request: GatewayRequest) -> GatewayResponse:
        """Handle a request through the integrated system."""
        if not self.initialized:
            return GatewayResponse(
                request_id=request.request_id,
                success=False,
                error="Integration manager not initialized"
            )
        
        # Route through AAR Gateway
        return await self.aar_gateway.handle_request(request)

    async def create_agent_from_component(self, component_name: str, model_id: str, 
                                        config: Dict[str, Any]) -> Optional[str]:
        """Create an agent from a specific component."""
        if component_name not in self.adapters:
            logger.error(f"Component {component_name} not available")
            return None
        
        adapter = self.adapters[component_name]
        
        if hasattr(adapter, 'create_agent_from_model'):
            agent = adapter.create_agent_from_model(model_id, config)
            if agent:
                logger.info(f"Created agent {agent.id} from {component_name}/{model_id}")
                return agent.id
        
        logger.error(f"Component {component_name} does not support agent creation")
        return None

    async def execute_function(self, function_name: str, arguments: Dict[str, Any], 
                             context: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Execute a function through the integrated system."""
        from .function_registry import FunctionInvocation
        
        invocation = FunctionInvocation(
            function_name=function_name,
            arguments=arguments,
            context=context
        )
        
        result = await self.function_registry.invoke_function(invocation)
        
        return {
            "success": result.success,
            "result": result.result,
            "error": result.error,
            "cost": result.cost_incurred,
            "execution_time": result.execution_time_ms,
            "metadata": result.metadata
        }

    def get_component_status(self, component_name: str) -> Optional[IntegrationStatus]:
        """Get status of a specific component."""
        return self.status.get(component_name)

    def list_components(self) -> List[Dict[str, Any]]:
        """List all integrated components."""
        return [
            {
                "name": status.component_name,
                "adapter": status.adapter_type,
                "status": status.status,
                "health": status.health.get("status", "unknown"),
                "initialization_time": status.initialization_time,
                "last_health_check": status.last_health_check,
                "error": status.error_message
            }
            for status in self.status.values()
        ]

    def get_available_models(self) -> Dict[str, List[Dict[str, Any]]]:
        """Get all available models from all adapters."""
        models = {}
        
        for component_name, adapter in self.adapters.items():
            if hasattr(adapter, 'get_available_models'):
                try:
                    component_models = adapter.get_available_models()
                    models[component_name] = [
                        {
                            "id": getattr(model, 'model_id', getattr(model, 'name', str(model))),
                            "name": getattr(model, 'name', str(model)),
                            "provider": getattr(model, 'provider', 'unknown'),
                            "capabilities": getattr(model, 'capabilities', [])
                        }
                        for model in component_models
                    ]
                except Exception as e:
                    logger.error(f"Failed to get models from {component_name}: {e}")
        
        return models

    def get_available_functions(self) -> List[Dict[str, Any]]:
        """Get all available functions from the registry."""
        functions = self.function_registry.list_functions()
        
        return [
            {
                "name": func.name,
                "description": func.description,
                "parameters": {
                    name: {
                        "type": param.type,
                        "description": param.description,
                        "required": param.required
                    }
                    for name, param in func.parameters.items()
                },
                "safety_class": func.safety_class.value,
                "cost_unit": func.cost_unit,
                "tags": func.tags,
                "status": func.status.value
            }
            for func in functions
        ]

    async def health_check(self) -> Dict[str, Any]:
        """Perform comprehensive health check of the entire system."""
        # Update component health
        for component_name, adapter in self.adapters.items():
            try:
                health = adapter.health_check()
                if component_name in self.status:
                    self.status[component_name].health = health
                    self.status[component_name].last_health_check = time.time()
            except Exception as e:
                if component_name in self.status:
                    self.status[component_name].error_message = str(e)
        
        # Gateway health
        gateway_health = self.aar_gateway.health_check()
        
        # Function registry health
        registry_health = self.function_registry.health_check()
        
        # Overall system health
        active_components = len([s for s in self.status.values() if s.status == "active"])
        total_components = len(self.status)
        
        system_health = {
            "status": "healthy" if active_components > 0 else "degraded",
            "initialized": self.initialized,
            "components": {
                "total": total_components,
                "active": active_components,
                "compatibility_mode": len([s for s in self.status.values() if s.status == "compatibility_mode"]),
                "failed": len([s for s in self.status.values() if s.status == "error"])
            },
            "gateway": gateway_health,
            "function_registry": registry_health,
            "component_health": {
                name: status.health for name, status in self.status.items()
            },
            "initialization_errors": self.initialization_errors
        }
        
        return system_health

    async def shutdown(self):
        """Shutdown all integrated components gracefully."""
        logger.info("Shutting down Integration Manager...")
        
        # Stop services that need explicit shutdown
        shutdown_tasks = []
        
        for component_name, adapter in self.adapters.items():
            if hasattr(adapter, 'stop_service'):
                shutdown_tasks.append(adapter.stop_service())
            elif hasattr(adapter, 'shutdown'):
                shutdown_tasks.append(adapter.shutdown())
        
        if shutdown_tasks:
            await asyncio.gather(*shutdown_tasks, return_exceptions=True)
        
        self.initialized = False
        logger.info("Integration Manager shutdown complete")