"""
AAR Gateway - Agent-Arena-Relation Gateway
Primary entry point for coordinating 2do components with the Aphrodite Engine.
"""

import asyncio
import logging
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, asdict
from enum import Enum
import uuid

from aar_core.agents import AgentManager
from aar_core.arena import SimulationEngine
from aar_core.orchestration import CoreOrchestrator
from echo.sys.prompt_kernel import PromptStore

logger = logging.getLogger(__name__)


class ComponentType(Enum):
    """Types of integrated components from 2do."""
    AICHAT = "aichat"
    ARGC = "argc"
    GALATEA_UI = "galatea-ui"
    GALATEA_FRONTEND = "galatea-frontend"
    LLM = "llm"
    LLM_FUNCTIONS = "llm-functions"
    PAPHOS_BACKEND = "paphos-backend"
    SPARK_SYS = "spark.sys"


@dataclass
class ComponentSpec:
    """Specification for a 2do component integration."""
    name: str
    component_type: ComponentType
    version: str
    capabilities: List[str]
    endpoints: Dict[str, str]
    dependencies: List[str]
    status: str = "inactive"
    metadata: Dict[str, Any] = None

    def __post_init__(self):
        if self.metadata is None:
            self.metadata = {}


@dataclass
class GatewayRequest:
    """Request to the AAR Gateway."""
    request_id: str
    component: str
    operation: str
    payload: Dict[str, Any]
    context: Optional[Dict[str, Any]] = None


@dataclass
class GatewayResponse:
    """Response from the AAR Gateway."""
    request_id: str
    success: bool
    result: Any = None
    error: Optional[str] = None
    metadata: Optional[Dict[str, Any]] = None


class AARGateway:
    """
    Central gateway orchestrating all 2do components with Aphrodite Engine.
    
    Implements the Gateway Layer from the integration strategy, providing:
    - Component registration and discovery
    - Request routing and mediation
    - Context augmentation for Aphrodite inference
    - Unified API surface
    """

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        self.config = config or {}
        self.components: Dict[str, ComponentSpec] = {}
        self.active_sessions: Dict[str, Dict[str, Any]] = {}
        
        # Initialize core AAR components
        self.agent_manager = AgentManager()
        self.simulation_engine = SimulationEngine()
        self.orchestrator = CoreOrchestrator()
        self.prompt_store = PromptStore()
        
        # Initialize component registrations
        self._register_builtin_components()

    def _register_builtin_components(self):
        """Register built-in 2do components."""
        components = [
            ComponentSpec(
                name="aichat",
                component_type=ComponentType.AICHAT,
                version="1.0.0",
                capabilities=["chat", "rag", "multi-model"],
                endpoints={
                    "chat": "/aichat/chat",
                    "models": "/aichat/models",
                    "health": "/aichat/health"
                },
                dependencies=["aphrodite-engine"]
            ),
            ComponentSpec(
                name="argc",
                component_type=ComponentType.ARGC,
                version="1.0.0",
                capabilities=["cli-parsing", "command-schema"],
                endpoints={
                    "parse": "/argc/parse",
                    "schema": "/argc/schema"
                },
                dependencies=[]
            ),
            ComponentSpec(
                name="galatea-ui",
                component_type=ComponentType.GALATEA_UI,
                version="1.0.0",
                capabilities=["web-ui", "auth", "session-management"],
                endpoints={
                    "api": "/galatea/api",
                    "auth": "/galatea/auth"
                },
                dependencies=["galatea-frontend", "paphos-backend"]
            ),
            ComponentSpec(
                name="galatea-frontend",
                component_type=ComponentType.GALATEA_FRONTEND,
                version="1.0.0",
                capabilities=["bff", "api-gateway"],
                endpoints={
                    "api": "/api/v1",
                    "health": "/health"
                },
                dependencies=["paphos-backend"]
            ),
            ComponentSpec(
                name="llm",
                component_type=ComponentType.LLM,
                version="1.0.0",
                capabilities=["agent-abstractions", "multi-model-wrappers"],
                endpoints={
                    "agents": "/llm/agents",
                    "models": "/llm/models"
                },
                dependencies=["llm-functions"]
            ),
            ComponentSpec(
                name="llm-functions",
                component_type=ComponentType.LLM_FUNCTIONS,
                version="1.0.0",
                capabilities=["function-calling", "mcp-tools"],
                endpoints={
                    "functions": "/functions",
                    "invoke": "/functions/invoke"
                },
                dependencies=[]
            ),
            ComponentSpec(
                name="paphos-backend",
                component_type=ComponentType.PAPHOS_BACKEND,
                version="1.0.0",
                capabilities=["persistence", "auth", "domain-services"],
                endpoints={
                    "api": "/paphos/api",
                    "auth": "/paphos/auth"
                },
                dependencies=[]
            ),
            ComponentSpec(
                name="spark.sys",
                component_type=ComponentType.SPARK_SYS,
                version="1.0.0",
                capabilities=["prompt-collections", "themes", "updates"],
                endpoints={
                    "prompts": "/spark/prompts",
                    "themes": "/spark/themes"
                },
                dependencies=["echo.sys"]
            ),
        ]
        
        for component in components:
            self.register_component(component)

    def register_component(self, spec: ComponentSpec) -> bool:
        """Register a component with the gateway."""
        try:
            self.components[spec.name] = spec
            logger.info(f"Registered component: {spec.name} ({spec.component_type.value})")
            return True
        except Exception as e:
            logger.error(f"Failed to register component {spec.name}: {e}")
            return False

    def get_component(self, name: str) -> Optional[ComponentSpec]:
        """Get component specification by name."""
        return self.components.get(name)

    def list_components(self) -> List[ComponentSpec]:
        """List all registered components."""
        return list(self.components.values())

    async def handle_request(self, request: GatewayRequest) -> GatewayResponse:
        """Handle a request through the gateway."""
        try:
            logger.info(f"Handling request {request.request_id} for {request.component}")
            
            component = self.get_component(request.component)
            if not component:
                return GatewayResponse(
                    request_id=request.request_id,
                    success=False,
                    error=f"Component {request.component} not found"
                )
            
            # Route based on component type and operation
            result = await self._route_request(component, request)
            
            return GatewayResponse(
                request_id=request.request_id,
                success=True,
                result=result,
                metadata={"component": component.name, "operation": request.operation}
            )
            
        except Exception as e:
            logger.error(f"Error handling request {request.request_id}: {e}")
            return GatewayResponse(
                request_id=request.request_id,
                success=False,
                error=str(e)
            )

    async def _route_request(self, component: ComponentSpec, request: GatewayRequest) -> Any:
        """Route request to appropriate handler based on component type."""
        
        if component.component_type == ComponentType.AICHAT:
            return await self._handle_aichat_request(request)
        elif component.component_type == ComponentType.ARGC:
            return await self._handle_argc_request(request)
        elif component.component_type == ComponentType.GALATEA_UI:
            return await self._handle_galatea_ui_request(request)
        elif component.component_type == ComponentType.GALATEA_FRONTEND:
            return await self._handle_galatea_frontend_request(request)
        elif component.component_type == ComponentType.LLM:
            return await self._handle_llm_request(request)
        elif component.component_type == ComponentType.LLM_FUNCTIONS:
            return await self._handle_llm_functions_request(request)
        elif component.component_type == ComponentType.PAPHOS_BACKEND:
            return await self._handle_paphos_request(request)
        elif component.component_type == ComponentType.SPARK_SYS:
            return await self._handle_spark_request(request)
        else:
            raise ValueError(f"Unknown component type: {component.component_type}")

    async def _handle_aichat_request(self, request: GatewayRequest) -> Dict[str, Any]:
        """Handle aichat component requests."""
        if request.operation == "chat":
            # Route to Aphrodite engine with AAR context
            return await self._orchestrated_inference(request)
        elif request.operation == "models":
            return {"models": ["gpt-4", "claude-3", "local-models"]}
        elif request.operation == "health":
            return {"status": "healthy", "component": "aichat"}
        else:
            raise ValueError(f"Unknown aichat operation: {request.operation}")

    async def _handle_argc_request(self, request: GatewayRequest) -> Dict[str, Any]:
        """Handle argc component requests."""
        if request.operation == "parse":
            # Parse command line arguments and convert to function calls
            return {"parsed": True, "functions": request.payload.get("args", [])}
        elif request.operation == "schema":
            return {"schema": "argc-command-schema"}
        else:
            raise ValueError(f"Unknown argc operation: {request.operation}")

    async def _handle_galatea_ui_request(self, request: GatewayRequest) -> Dict[str, Any]:
        """Handle galatea UI requests."""
        if request.operation == "auth":
            return {"authenticated": True, "session": str(uuid.uuid4())}
        else:
            return {"ui_response": f"handled {request.operation}"}

    async def _handle_galatea_frontend_request(self, request: GatewayRequest) -> Dict[str, Any]:
        """Handle galatea frontend requests."""
        return {"frontend_response": f"handled {request.operation}"}

    async def _handle_llm_request(self, request: GatewayRequest) -> Dict[str, Any]:
        """Handle llm component requests."""
        if request.operation == "agents":
            agents = self.agent_manager.list_agents()
            return {"agents": [asdict(agent) for agent in agents]}
        elif request.operation == "create_agent":
            agent_spec = request.payload.get("agent_spec")
            if agent_spec:
                agent = await self.agent_manager.create_agent(agent_spec)
                return {"agent": asdict(agent)}
            else:
                raise ValueError("Missing agent_spec in payload")
        else:
            return {"llm_response": f"handled {request.operation}"}

    async def _handle_llm_functions_request(self, request: GatewayRequest) -> Dict[str, Any]:
        """Handle llm-functions component requests."""
        if request.operation == "list":
            return {"functions": ["calculate", "web_search", "file_read"]}
        elif request.operation == "invoke":
            function_name = request.payload.get("function")
            args = request.payload.get("args", {})
            # This would invoke the actual function
            return {"function": function_name, "result": f"executed with {args}"}
        else:
            return {"functions_response": f"handled {request.operation}"}

    async def _handle_paphos_request(self, request: GatewayRequest) -> Dict[str, Any]:
        """Handle paphos backend requests."""
        return {"paphos_response": f"handled {request.operation}"}

    async def _handle_spark_request(self, request: GatewayRequest) -> Dict[str, Any]:
        """Handle spark.sys requests."""
        if request.operation == "get_prompt":
            prompt_id = request.payload.get("prompt_id")
            prompt = await self.prompt_store.get_prompt(prompt_id)
            return {"prompt": prompt}
        elif request.operation == "list_prompts":
            prompts = await self.prompt_store.list_prompts()
            return {"prompts": prompts}
        else:
            return {"spark_response": f"handled {request.operation}"}

    async def _orchestrated_inference(self, request: GatewayRequest) -> Dict[str, Any]:
        """
        Perform orchestrated inference with AAR context augmentation.
        This integrates with the Aphrodite engine while adding AAR orchestration.
        """
        try:
            # Extract context and augment with AAR information
            context = request.context or {}
            
            # Add agent context if available
            if "agent_id" in context:
                agent = self.agent_manager.get_agent(context["agent_id"])
                if agent:
                    context["agent_capabilities"] = agent.capabilities
            
            # Add memory context
            memory_refs = context.get("memory_refs", [])
            if memory_refs:
                # This would retrieve relevant memories
                context["retrieved_memories"] = []
            
            # Add function context
            allowed_functions = context.get("allowed_functions", [])
            context["available_functions"] = allowed_functions
            
            # Create orchestrated request for Aphrodite
            orchestrated_request = {
                "messages": request.payload.get("messages", []),
                "model": request.payload.get("model", "default"),
                "context": context,
                "aar_metadata": {
                    "request_id": request.request_id,
                    "orchestration_level": "basic",
                    "timestamp": asyncio.get_event_loop().time()
                }
            }
            
            # This would interface with the actual Aphrodite engine
            # For now, return a structured response
            return {
                "response": "This is an orchestrated response from Aphrodite Engine",
                "context": context,
                "metadata": orchestrated_request["aar_metadata"]
            }
            
        except Exception as e:
            logger.error(f"Error in orchestrated inference: {e}")
            raise

    async def create_arena_session(self, participants: List[str], config: Optional[Dict] = None) -> str:
        """Create a new arena session for multi-agent collaboration."""
        session_id = str(uuid.uuid4())
        session_config = config or {}
        
        self.active_sessions[session_id] = {
            "participants": participants,
            "config": session_config,
            "created_at": asyncio.get_event_loop().time(),
            "status": "active"
        }
        
        logger.info(f"Created arena session {session_id} with {len(participants)} participants")
        return session_id

    async def close_arena_session(self, session_id: str) -> bool:
        """Close an arena session."""
        if session_id in self.active_sessions:
            self.active_sessions[session_id]["status"] = "closed"
            logger.info(f"Closed arena session {session_id}")
            return True
        return False

    def get_session_status(self, session_id: str) -> Optional[Dict[str, Any]]:
        """Get status of an arena session."""
        return self.active_sessions.get(session_id)

    def health_check(self) -> Dict[str, Any]:
        """Perform health check of the gateway and all components."""
        health_status = {
            "gateway": "healthy",
            "components": {},
            "active_sessions": len(self.active_sessions),
            "total_components": len(self.components)
        }
        
        for name, component in self.components.items():
            # This would perform actual health checks
            health_status["components"][name] = {
                "status": "healthy",
                "type": component.component_type.value,
                "version": component.version
            }
        
        return health_status