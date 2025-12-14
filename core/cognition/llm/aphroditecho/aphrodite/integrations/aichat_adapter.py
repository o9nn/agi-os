"""
AiChat Adapter - Integrates the aichat Rust component with AAR system

This adapter provides a bridge to the aichat CLI/server functionality,
enabling chat orchestration, RAG capabilities, and multi-model access.
"""

import asyncio
import logging
import subprocess
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
from pathlib import Path

from aar_core.agents import Agent, AgentCapabilities, AgentStatus

logger = logging.getLogger(__name__)


@dataclass
class AiChatModel:
    """Specification for an aichat model."""
    name: str
    provider: str
    config_name: str
    capabilities: List[str] = field(default_factory=lambda: ["chat", "streaming"])
    max_tokens: Optional[int] = None
    supports_tools: bool = False
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class ChatSession:
    """Represents an active chat session with aichat."""
    session_id: str
    model: str
    config: Dict[str, Any] = field(default_factory=dict)
    messages: List[Dict[str, str]] = field(default_factory=list)
    active: bool = True


class AiChatAdapter:
    """
    Adapter for integrating the aichat Rust component with AAR system.
    
    Provides:
    - Model discovery from aichat configuration
    - Chat session management
    - RAG and tool integration
    - Multi-provider model access
    """

    def __init__(self, aichat_binary_path: Optional[str] = None):
        self.aichat_binary = aichat_binary_path or self._find_aichat_binary()
        self.models: Dict[str, AiChatModel] = {}
        self.sessions: Dict[str, ChatSession] = {}
        self.available = False
        
        if self.aichat_binary:
            self._initialize_aichat()
        else:
            self._setup_compatibility_mode()

    def _find_aichat_binary(self) -> Optional[str]:
        """Find the aichat binary in the system or build it."""
        # Check if aichat is in PATH
        try:
            result = subprocess.run(
                ["which", "aichat"], 
                capture_output=True, 
                text=True, 
                timeout=5
            )
            if result.returncode == 0:
                return result.stdout.strip()
        except Exception:
            pass
        
        # Check if we can build from source
        aichat_src = Path(__file__).parent.parent.parent / "2do" / "aichat"
        if aichat_src.exists():
            try:
                # Build aichat binary
                logger.info("Building aichat from source...")
                result = subprocess.run(
                    ["cargo", "build", "--release"],
                    cwd=aichat_src,
                    capture_output=True,
                    text=True,
                    timeout=300  # 5 minute timeout
                )
                
                if result.returncode == 0:
                    binary_path = aichat_src / "target" / "release" / "aichat"
                    if binary_path.exists():
                        logger.info(f"Successfully built aichat at {binary_path}")
                        return str(binary_path)
                else:
                    logger.warning(f"Failed to build aichat: {result.stderr}")
            except Exception as e:
                logger.warning(f"Could not build aichat: {e}")
        
        return None

    def _initialize_aichat(self):
        """Initialize aichat and discover available models."""
        try:
            # Test if aichat is working
            result = subprocess.run(
                [self.aichat_binary, "--version"],
                capture_output=True,
                text=True,
                timeout=10
            )
            
            if result.returncode == 0:
                self.available = True
                self._discover_models()
                logger.info(f"AiChat adapter initialized with binary: {self.aichat_binary}")
            else:
                logger.error(f"AiChat binary test failed: {result.stderr}")
                self._setup_compatibility_mode()
                
        except Exception as e:
            logger.error(f"Failed to initialize aichat: {e}")
            self._setup_compatibility_mode()

    def _discover_models(self):
        """Discover available models from aichat."""
        try:
            # Get list of models from aichat
            result = subprocess.run(
                [self.aichat_binary, "--list-models"],
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if result.returncode == 0:
                # Parse model list (format may vary)
                lines = result.stdout.strip().split('\n')
                for line in lines:
                    if line.strip():
                        self._parse_model_line(line)
            else:
                # Load default models if list command fails
                self._load_default_models()
                
        except Exception as e:
            logger.error(f"Failed to discover aichat models: {e}")
            self._load_default_models()

    def _parse_model_line(self, line: str):
        """Parse a model line from aichat output."""
        # This would parse actual aichat model output format
        # For now, create basic model specs
        parts = line.strip().split()
        if len(parts) >= 2:
            model_name = parts[0]
            provider = parts[1] if len(parts) > 1 else "unknown"
            
            self.models[model_name] = AiChatModel(
                name=model_name,
                provider=provider,
                config_name=model_name,
                capabilities=["chat", "streaming"]
            )

    def _load_default_models(self):
        """Load default model configurations from the 2do/aichat source."""
        try:
            models_yaml = Path(__file__).parent.parent.parent / "2do" / "aichat" / "models.yaml"
            if models_yaml.exists():
                # Parse models.yaml (would need yaml parser)
                # For now, add some known models
                default_models = [
                    ("gpt-4o", "openai", ["chat", "streaming", "tools"]),
                    ("gpt-4o-mini", "openai", ["chat", "streaming", "tools"]),
                    ("claude-3-5-sonnet", "claude", ["chat", "streaming", "tools"]),
                    ("claude-3-haiku", "claude", ["chat", "streaming"]),
                    ("gemini-1.5-pro", "gemini", ["chat", "streaming", "tools"]),
                ]
                
                for name, provider, capabilities in default_models:
                    self.models[name] = AiChatModel(
                        name=name,
                        provider=provider,
                        config_name=name,
                        capabilities=capabilities,
                        supports_tools="tools" in capabilities
                    )
                    
                logger.info(f"Loaded {len(default_models)} default models")
                
        except Exception as e:
            logger.error(f"Failed to load default models: {e}")

    def _setup_compatibility_mode(self):
        """Setup compatibility mode when aichat is not available."""
        self.available = False
        
        # Add basic models for compatibility
        compatibility_models = [
            ("gpt-4o-mini", "openai"),
            ("claude-3-haiku", "anthropic"),
            ("gemini-1.5-flash", "google"),
        ]
        
        for name, provider in compatibility_models:
            self.models[name] = AiChatModel(
                name=name,
                provider=provider,
                config_name=name,
                capabilities=["chat"],
                metadata={"compatibility_mode": True}
            )
        
        logger.info("AiChat adapter running in compatibility mode")

    def get_available_models(self) -> List[AiChatModel]:
        """Get list of available models."""
        return list(self.models.values())

    def get_model(self, model_name: str) -> Optional[AiChatModel]:
        """Get specific model by name."""
        return self.models.get(model_name)

    async def create_chat_session(self, model_name: str, session_config: Optional[Dict[str, Any]] = None) -> Optional[str]:
        """Create a new chat session."""
        model = self.get_model(model_name)
        if not model:
            logger.error(f"Model {model_name} not found")
            return None
        
        import uuid
        session_id = str(uuid.uuid4())
        
        session = ChatSession(
            session_id=session_id,
            model=model_name,
            config=session_config or {},
            messages=[],
            active=True
        )
        
        self.sessions[session_id] = session
        logger.info(f"Created chat session {session_id} with model {model_name}")
        return session_id

    async def send_message(self, session_id: str, message: str, 
                          options: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Send a message in a chat session."""
        session = self.sessions.get(session_id)
        if not session:
            return {"error": f"Session {session_id} not found"}
        
        if not session.active:
            return {"error": f"Session {session_id} is not active"}
        
        # Add user message to history
        session.messages.append({"role": "user", "content": message})
        
        if not self.available:
            # Compatibility mode
            response = f"[AiChat Compatibility] Response to: {message[:100]}..."
            session.messages.append({"role": "assistant", "content": response})
            return {
                "response": response,
                "session_id": session_id,
                "model": session.model,
                "metadata": {"compatibility_mode": True}
            }
        
        try:
            # Prepare aichat command
            cmd = [
                self.aichat_binary,
                "--model", session.model,
                "--no-stream",  # For synchronous response
            ]
            
            # Add any session-specific options
            if options:
                if "temperature" in options:
                    cmd.extend(["--temperature", str(options["temperature"])])
                if "max_tokens" in options:
                    cmd.extend(["--max-tokens", str(options["max_tokens"])])
            
            # Execute aichat
            result = await asyncio.create_subprocess_exec(
                *cmd,
                stdin=asyncio.subprocess.PIPE,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            stdout, stderr = await result.communicate(message.encode())
            
            if result.returncode == 0:
                response = stdout.decode().strip()
                session.messages.append({"role": "assistant", "content": response})
                
                return {
                    "response": response,
                    "session_id": session_id,
                    "model": session.model,
                    "metadata": {
                        "provider": self.models[session.model].provider,
                        "message_count": len(session.messages)
                    }
                }
            else:
                error_msg = stderr.decode().strip()
                logger.error(f"AiChat error: {error_msg}")
                return {"error": error_msg}
                
        except Exception as e:
            logger.error(f"Failed to send message in session {session_id}: {e}")
            return {"error": str(e)}

    async def stream_message(self, session_id: str, message: str,
                           options: Optional[Dict[str, Any]] = None):
        """Stream a message response (async generator)."""
        session = self.sessions.get(session_id)
        if not session:
            yield {"error": f"Session {session_id} not found"}
            return
        
        if not self.available:
            # Compatibility mode streaming
            response = f"[AiChat Streaming] Response to: {message}"
            for i, char in enumerate(response):
                yield {"delta": char, "index": i}
                await asyncio.sleep(0.01)  # Simulate streaming
            return
        
        try:
            # Prepare streaming command
            cmd = [
                self.aichat_binary,
                "--model", session.model,
                "--stream",
            ]
            
            if options:
                if "temperature" in options:
                    cmd.extend(["--temperature", str(options["temperature"])])
                if "max_tokens" in options:
                    cmd.extend(["--max-tokens", str(options["max_tokens"])])
            
            # Execute with streaming
            process = await asyncio.create_subprocess_exec(
                *cmd,
                stdin=asyncio.subprocess.PIPE,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            # Send message
            process.stdin.write(message.encode())
            process.stdin.close()
            
            # Stream response
            full_response = ""
            async for line in process.stdout:
                chunk = line.decode().strip()
                if chunk:
                    full_response += chunk
                    yield {"delta": chunk, "session_id": session_id}
            
            # Add to session history
            session.messages.append({"role": "user", "content": message})
            session.messages.append({"role": "assistant", "content": full_response})
            
            await process.wait()
            
        except Exception as e:
            logger.error(f"Failed to stream message: {e}")
            yield {"error": str(e)}

    def get_session_history(self, session_id: str) -> Optional[List[Dict[str, str]]]:
        """Get the message history for a session."""
        session = self.sessions.get(session_id)
        return session.messages if session else None

    def close_session(self, session_id: str) -> bool:
        """Close a chat session."""
        if session_id in self.sessions:
            self.sessions[session_id].active = False
            logger.info(f"Closed session {session_id}")
            return True
        return False

    def create_agent_from_model(self, model_name: str, agent_config: Dict[str, Any]) -> Optional[Agent]:
        """Create an AAR agent using an aichat model."""
        model = self.get_model(model_name)
        if not model:
            logger.error(f"Model {model_name} not found")
            return None
        
        try:
            capabilities = AgentCapabilities(
                reasoning=True,
                multimodal=False,  # aichat primarily text-based
                memory_enabled=agent_config.get("memory_enabled", True),
                learning_enabled=agent_config.get("learning_enabled", True),
                collaboration=agent_config.get("collaboration", True),
                specialized_domains=model.capabilities,
                max_context_length=model.max_tokens or 4096,
                processing_power=agent_config.get("processing_power", 1.0)
            )
            
            agent = Agent(
                id=agent_config.get("id", f"aichat_agent_{model_name}"),
                name=agent_config.get("name", f"AiChat Agent using {model.name}"),
                capabilities=capabilities,
                status=AgentStatus.INITIALIZING,
                model_id=model_name,
                metadata={
                    "adapter": "aichat",
                    "provider": model.provider,
                    "supports_tools": model.supports_tools,
                    "aichat_available": self.available
                }
            )
            
            logger.info(f"Created agent {agent.id} from aichat model {model_name}")
            return agent
            
        except Exception as e:
            logger.error(f"Failed to create agent from model {model_name}: {e}")
            return None

    async def execute_rag_query(self, query: str, model_name: str, 
                               context_sources: Optional[List[str]] = None) -> Dict[str, Any]:
        """Execute a RAG query using aichat."""
        if not self.available:
            return {
                "response": f"[Compatibility Mode] RAG query: {query}",
                "sources": context_sources or [],
                "metadata": {"compatibility_mode": True}
            }
        
        try:
            # Prepare RAG command (this would depend on aichat's RAG implementation)
            cmd = [
                self.aichat_binary,
                "--model", model_name,
                "--rag",  # Hypothetical RAG flag
            ]
            
            # Add context sources if supported
            if context_sources:
                for source in context_sources:
                    cmd.extend(["--context", source])
            
            result = await asyncio.create_subprocess_exec(
                *cmd,
                stdin=asyncio.subprocess.PIPE,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            stdout, stderr = await result.communicate(query.encode())
            
            if result.returncode == 0:
                return {
                    "response": stdout.decode().strip(),
                    "query": query,
                    "sources": context_sources or [],
                    "model": model_name
                }
            else:
                return {"error": stderr.decode().strip()}
                
        except Exception as e:
            logger.error(f"Failed to execute RAG query: {e}")
            return {"error": str(e)}

    def health_check(self) -> Dict[str, Any]:
        """Perform health check of the AiChat adapter."""
        return {
            "status": "healthy" if self.available else "compatibility_mode",
            "aichat_available": self.available,
            "binary_path": self.aichat_binary,
            "models_discovered": len(self.models),
            "active_sessions": len([s for s in self.sessions.values() if s.active]),
            "total_sessions": len(self.sessions)
        }