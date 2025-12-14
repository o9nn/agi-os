"""
LLM Adapter - Integrates the llm component with AAR system

This adapter bridges the 2do/llm component's agent abstractions and model wrappers
with the AAR orchestration system, providing unified access to multi-model capabilities.
"""

import logging
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
import sys
from pathlib import Path

# Add the 2do/llm directory to the Python path for imports
llm_path = Path(__file__).parent.parent.parent / "2do" / "llm"
if str(llm_path) not in sys.path:
    sys.path.insert(0, str(llm_path))

try:
    import llm
    LLM_AVAILABLE = True
except ImportError:
    LLM_AVAILABLE = False
    logging.warning("llm component not available - adapter will run in compatibility mode")

from aar_core.agents import Agent, AgentCapabilities, AgentStatus
from ..function_registry import FunctionRegistry, FunctionSpec, ParameterSpec, SafetyClass

logger = logging.getLogger(__name__)


@dataclass
class LLMModelSpec:
    """Specification for an LLM model from the llm component."""
    model_id: str
    name: str
    provider: str
    capabilities: List[str] = field(default_factory=list)
    async_supported: bool = False
    embedding_supported: bool = False
    metadata: Dict[str, Any] = field(default_factory=dict)


class LLMAdapter:
    """
    Adapter for integrating 2do/llm component with AAR system.
    
    Provides:
    - Model discovery and registration
    - Agent creation from llm models
    - Tool integration with function registry
    - Conversation management
    """

    def __init__(self, function_registry: FunctionRegistry):
        self.function_registry = function_registry
        self.models: Dict[str, LLMModelSpec] = {}
        self.conversations: Dict[str, Any] = {}
        self.llm_available = LLM_AVAILABLE
        
        if self.llm_available:
            self._discover_models()
            self._register_llm_tools()
        else:
            self._setup_compatibility_mode()

    def _discover_models(self):
        """Discover available models from the llm component."""
        if not self.llm_available:
            return
            
        try:
            # Get sync models
            sync_models = llm.get_models()
            for model in sync_models:
                model_spec = LLMModelSpec(
                    model_id=model.model_id,
                    name=getattr(model, 'name', model.model_id),
                    provider=getattr(model, 'provider', 'unknown'),
                    capabilities=['text-generation'],
                    async_supported=False
                )
                self.models[model.model_id] = model_spec
            
            # Get async models
            async_models = llm.get_async_models()
            for model in async_models:
                model_id = model.model_id
                if model_id in self.models:
                    self.models[model_id].async_supported = True
                else:
                    model_spec = LLMModelSpec(
                        model_id=model_id,
                        name=getattr(model, 'name', model_id),
                        provider=getattr(model, 'provider', 'unknown'),
                        capabilities=['text-generation'],
                        async_supported=True
                    )
                    self.models[model_id] = model_spec
            
            # Get embedding models
            embedding_models = llm.get_embedding_models()
            for model in embedding_models:
                model_id = model.model_id
                if model_id in self.models:
                    self.models[model_id].embedding_supported = True
                    self.models[model_id].capabilities.append('embeddings')
                else:
                    model_spec = LLMModelSpec(
                        model_id=model_id,
                        name=getattr(model, 'name', model_id),
                        provider=getattr(model, 'provider', 'unknown'),
                        capabilities=['embeddings'],
                        embedding_supported=True
                    )
                    self.models[model_id] = model_spec
            
            logger.info(f"Discovered {len(self.models)} models from llm component")
            
        except Exception as e:
            logger.error(f"Failed to discover llm models: {e}")

    def _register_llm_tools(self):
        """Register tools from the llm component with the function registry."""
        if not self.llm_available:
            return
            
        try:
            tools = llm.get_tools()
            for tool_name, tool in tools.items():
                self._convert_llm_tool_to_function(tool_name, tool)
                
        except Exception as e:
            logger.error(f"Failed to register llm tools: {e}")

    def _convert_llm_tool_to_function(self, tool_name: str, tool: Any):
        """Convert an llm tool to a function registry entry."""
        try:
            # Extract tool metadata
            description = getattr(tool, 'description', f"Tool: {tool_name}")
            
            # Create parameters from tool signature if available
            parameters = {}
            if callable(tool):
                import inspect
                sig = inspect.signature(tool)
                for param_name, param in sig.parameters.items():
                    param_type = "string"  # Default type
                    if param.annotation != inspect.Parameter.empty:
                        param_type = str(param.annotation).replace("<class '", "").replace("'>", "")
                    
                    parameters[param_name] = ParameterSpec(
                        type=param_type,
                        description=f"Parameter: {param_name}",
                        required=param.default == inspect.Parameter.empty
                    )
            
            # Create function specification
            func_spec = FunctionSpec(
                name=f"llm_{tool_name}",
                description=description,
                parameters=parameters,
                safety_class=SafetyClass.MEDIUM,
                cost_unit=1.0,
                implementation_ref=f"llm.tool.{tool_name}",
                tags=["llm", "tool"]
            )
            
            # Register with function registry
            self.function_registry.register_function(func_spec, tool)
            logger.info(f"Registered llm tool as function: {func_spec.name}")
            
        except Exception as e:
            logger.error(f"Failed to convert tool {tool_name}: {e}")

    def _setup_compatibility_mode(self):
        """Setup compatibility mode when llm component is not available."""
        # Add some basic model specs for compatibility
        self.models = {
            "gpt-4o-mini": LLMModelSpec(
                model_id="gpt-4o-mini",
                name="GPT-4o Mini",
                provider="openai",
                capabilities=["text-generation"],
                async_supported=True
            ),
            "claude-3-haiku": LLMModelSpec(
                model_id="claude-3-haiku",
                name="Claude 3 Haiku",
                provider="anthropic",
                capabilities=["text-generation"],
                async_supported=True
            )
        }
        logger.info("Running in compatibility mode with basic model support")

    def get_available_models(self) -> List[LLMModelSpec]:
        """Get list of available models."""
        return list(self.models.values())

    def get_model(self, model_id: str) -> Optional[LLMModelSpec]:
        """Get specific model by ID."""
        return self.models.get(model_id)

    def create_agent_from_model(self, model_id: str, agent_config: Dict[str, Any]) -> Optional[Agent]:
        """Create an AAR agent from an LLM model."""
        model_spec = self.get_model(model_id)
        if not model_spec:
            logger.error(f"Model {model_id} not found")
            return None
        
        try:
            # Create agent capabilities based on model capabilities
            capabilities = AgentCapabilities(
                reasoning=True,
                multimodal="multimodal" in model_spec.capabilities,
                memory_enabled=agent_config.get("memory_enabled", True),
                learning_enabled=agent_config.get("learning_enabled", True),
                collaboration=agent_config.get("collaboration", True),
                specialized_domains=model_spec.capabilities,
                max_context_length=agent_config.get("max_context_length", 4096),
                processing_power=agent_config.get("processing_power", 1.0)
            )
            
            # Create agent
            agent = Agent(
                id=agent_config.get("id", f"llm_agent_{model_id}"),
                name=agent_config.get("name", f"Agent using {model_spec.name}"),
                capabilities=capabilities,
                status=AgentStatus.INITIALIZING,
                model_id=model_id,
                metadata={
                    "adapter": "llm",
                    "provider": model_spec.provider,
                    "model_capabilities": model_spec.capabilities
                }
            )
            
            logger.info(f"Created agent {agent.id} from model {model_id}")
            return agent
            
        except Exception as e:
            logger.error(f"Failed to create agent from model {model_id}: {e}")
            return None

    async def execute_llm_prompt(self, model_id: str, prompt: str, 
                                options: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Execute a prompt using the llm component."""
        if not self.llm_available:
            return {
                "response": f"[Compatibility Mode] Would execute prompt with {model_id}: {prompt[:100]}...",
                "model": model_id,
                "metadata": {"compatibility_mode": True}
            }
        
        try:
            model = llm.get_model(model_id)
            
            # Execute prompt
            if hasattr(model, 'prompt'):
                response = model.prompt(prompt)
                return {
                    "response": str(response),
                    "model": model_id,
                    "metadata": {
                        "provider": getattr(model, 'provider', 'unknown'),
                        "model_id": model.model_id
                    }
                }
            else:
                return {"error": f"Model {model_id} does not support prompting"}
                
        except Exception as e:
            logger.error(f"Failed to execute prompt with {model_id}: {e}")
            return {"error": str(e)}

    async def execute_async_llm_prompt(self, model_id: str, prompt: str,
                                     options: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Execute a prompt using async llm component."""
        if not self.llm_available:
            return {
                "response": f"[Compatibility Mode] Would execute async prompt with {model_id}: {prompt[:100]}...",
                "model": model_id,
                "metadata": {"compatibility_mode": True}
            }
        
        try:
            async_model = llm.get_async_model(model_id)
            
            # Execute async prompt
            if hasattr(async_model, 'prompt'):
                response = await async_model.prompt(prompt)
                return {
                    "response": str(response),
                    "model": model_id,
                    "metadata": {
                        "provider": getattr(async_model, 'provider', 'unknown'),
                        "model_id": async_model.model_id,
                        "async": True
                    }
                }
            else:
                return {"error": f"Async model {model_id} does not support prompting"}
                
        except Exception as e:
            logger.error(f"Failed to execute async prompt with {model_id}: {e}")
            return {"error": str(e)}

    def create_conversation(self, model_id: str, conversation_id: Optional[str] = None) -> Optional[str]:
        """Create a new conversation with the specified model."""
        if not self.llm_available:
            # Compatibility mode conversation
            import uuid
            conv_id = conversation_id or str(uuid.uuid4())
            self.conversations[conv_id] = {
                "model_id": model_id,
                "messages": [],
                "compatibility_mode": True
            }
            return conv_id
        
        try:
            model = llm.get_model(model_id)
            conversation = model.conversation()
            
            import uuid
            conv_id = conversation_id or str(uuid.uuid4())
            self.conversations[conv_id] = conversation
            
            logger.info(f"Created conversation {conv_id} with model {model_id}")
            return conv_id
            
        except Exception as e:
            logger.error(f"Failed to create conversation with {model_id}: {e}")
            return None

    async def continue_conversation(self, conversation_id: str, message: str) -> Dict[str, Any]:
        """Continue an existing conversation."""
        if conversation_id not in self.conversations:
            return {"error": f"Conversation {conversation_id} not found"}
        
        conversation = self.conversations[conversation_id]
        
        # Compatibility mode
        if isinstance(conversation, dict) and conversation.get("compatibility_mode"):
            conversation["messages"].append({"role": "user", "content": message})
            response = f"[Compatibility Mode] Response to: {message}"
            conversation["messages"].append({"role": "assistant", "content": response})
            return {
                "response": response,
                "conversation_id": conversation_id,
                "metadata": {"compatibility_mode": True}
            }
        
        # Real llm conversation
        try:
            if hasattr(conversation, 'prompt'):
                response = conversation.prompt(message)
                return {
                    "response": str(response),
                    "conversation_id": conversation_id,
                    "metadata": {"model_id": getattr(conversation.model, 'model_id', 'unknown')}
                }
            else:
                return {"error": "Conversation does not support prompting"}
                
        except Exception as e:
            logger.error(f"Failed to continue conversation {conversation_id}: {e}")
            return {"error": str(e)}

    def get_conversation_history(self, conversation_id: str) -> Optional[List[Dict[str, Any]]]:
        """Get the history of a conversation."""
        if conversation_id not in self.conversations:
            return None
        
        conversation = self.conversations[conversation_id]
        
        # Compatibility mode
        if isinstance(conversation, dict) and conversation.get("compatibility_mode"):
            return conversation["messages"]
        
        # Real llm conversation
        try:
            if hasattr(conversation, 'responses'):
                return [{"role": "assistant", "content": str(response)} 
                       for response in conversation.responses]
            else:
                return []
                
        except Exception as e:
            logger.error(f"Failed to get conversation history {conversation_id}: {e}")
            return []

    def health_check(self) -> Dict[str, Any]:
        """Perform health check of the LLM adapter."""
        return {
            "status": "healthy",
            "llm_available": self.llm_available,
            "models_discovered": len(self.models),
            "active_conversations": len(self.conversations),
            "compatibility_mode": not self.llm_available
        }