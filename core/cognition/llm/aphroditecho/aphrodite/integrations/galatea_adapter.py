"""
Galatea Adapter - Integrates Galatea UI and Frontend with AAR system

This adapter bridges the Galatea web interface components (UI and frontend)
with the AAR orchestration system, providing web-based access to agents and arenas.
"""

import asyncio
import logging
import subprocess
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
from pathlib import Path

logger = logging.getLogger(__name__)


@dataclass
class GalateaService:
    """Represents a Galatea service (UI or Frontend)."""
    name: str
    service_type: str  # "ui" or "frontend"
    port: int
    process: Optional[subprocess.Popen] = None
    status: str = "stopped"
    endpoints: List[str] = field(default_factory=list)
    health_url: str = ""
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass  
class UserSession:
    """Represents a user session in Galatea."""
    session_id: str
    user_id: str
    username: str
    authenticated: bool = True
    permissions: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)


class GalateaAdapter:
    """
    Adapter for integrating Galatea UI and Frontend with AAR system.
    
    Provides:
    - Service management (start/stop UI and frontend)
    - User session management
    - Web API integration
    - Authentication and authorization
    - Arena access through web interface
    """

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        self.config = config or {}
        self.services: Dict[str, GalateaService] = {}
        self.user_sessions: Dict[str, UserSession] = {}
        
        # Set up service configurations
        self._setup_service_configs()
        
        # Initialize services if auto-start is enabled
        if self.config.get("auto_start", False):
            asyncio.create_task(self._auto_start_services())

    def _setup_service_configs(self):
        """Setup service configurations for UI and Frontend."""
        galatea_root = Path(__file__).parent.parent.parent / "2do"
        
        # Galatea UI service
        ui_config = self.config.get("ui", {})
        self.services["ui"] = GalateaService(
            name="galatea-ui",
            service_type="ui",
            port=ui_config.get("port", 3000),
            endpoints=["/", "/auth", "/chat", "/agents"],
            health_url="/health",
            metadata={
                "path": galatea_root / "galatea-UI",
                "technology": "javascript",
                "framework": "react"
            }
        )
        
        # Galatea Frontend service
        frontend_config = self.config.get("frontend", {})
        self.services["frontend"] = GalateaService(
            name="galatea-frontend", 
            service_type="frontend",
            port=frontend_config.get("port", 8080),
            endpoints=["/api/v1", "/auth", "/health"],
            health_url="/health",
            metadata={
                "path": galatea_root / "galatea-frontend",
                "technology": "go",
                "framework": "gin"
            }
        )

    async def _auto_start_services(self):
        """Auto-start services if configured."""
        try:
            await self.start_service("frontend")
            await asyncio.sleep(2)  # Give frontend time to start
            await self.start_service("ui")
            logger.info("Auto-started Galatea services")
        except Exception as e:
            logger.error(f"Failed to auto-start services: {e}")

    async def start_service(self, service_name: str) -> bool:
        """Start a Galatea service."""
        service = self.services.get(service_name)
        if not service:
            logger.error(f"Service {service_name} not found")
            return False
        
        if service.status == "running":
            logger.info(f"Service {service_name} is already running")
            return True
        
        try:
            service_path = service.metadata["path"]
            
            if service.service_type == "ui":
                # Start JavaScript/React UI
                success = await self._start_ui_service(service, service_path)
            elif service.service_type == "frontend":
                # Start Go frontend
                success = await self._start_frontend_service(service, service_path)
            else:
                logger.error(f"Unknown service type: {service.service_type}")
                return False
            
            if success:
                service.status = "running"
                logger.info(f"Started service {service_name} on port {service.port}")
                
                # Wait a moment and check health
                await asyncio.sleep(2)
                healthy = await self._check_service_health(service)
                if not healthy:
                    logger.warning(f"Service {service_name} started but health check failed")
                
                return True
            else:
                logger.error(f"Failed to start service {service_name}")
                return False
                
        except Exception as e:
            logger.error(f"Error starting service {service_name}: {e}")
            service.status = "error"
            return False

    async def _start_ui_service(self, service: GalateaService, service_path: Path) -> bool:
        """Start the UI service (JavaScript/React)."""
        if not service_path.exists():
            logger.error(f"UI service path does not exist: {service_path}")
            return False
        
        package_json = service_path / "package.json"
        if not package_json.exists():
            logger.error(f"package.json not found in {service_path}")
            return False
        
        try:
            # Check if dependencies are installed
            node_modules = service_path / "node_modules"
            if not node_modules.exists():
                logger.info("Installing UI dependencies...")
                install_process = await asyncio.create_subprocess_exec(
                    "npm", "install",
                    cwd=service_path,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE
                )
                await install_process.wait()
                
                if install_process.returncode != 0:
                    logger.error("Failed to install UI dependencies")
                    return False
            
            # Set environment variables
            env = {
                **subprocess.os.environ,
                "PORT": str(service.port),
                "CORE_API_SERVER": f"http://localhost:{self.services['frontend'].port}"
            }
            
            # Start the development server
            process = await asyncio.create_subprocess_exec(
                "npm", "start",
                cwd=service_path,
                env=env,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            service.process = process
            return True
            
        except Exception as e:
            logger.error(f"Failed to start UI service: {e}")
            return False

    async def _start_frontend_service(self, service: GalateaService, service_path: Path) -> bool:
        """Start the frontend service (Go)."""
        if not service_path.exists():
            logger.error(f"Frontend service path does not exist: {service_path}")
            return False
        
        # Look for go.mod or main.go
        go_mod = service_path / "go.mod" 
        server_path = service_path / "server"
        
        if server_path.exists():
            go_mod = server_path / "go.mod"
            service_path = server_path
        
        if not go_mod.exists():
            logger.error(f"go.mod not found in {service_path}")
            return False
        
        try:
            # Set environment variables
            env = {
                **subprocess.os.environ,
                "PORT": str(service.port),
                "GIN_MODE": "debug",  # or "release" for production
            }
            
            # Build and run the Go service
            process = await asyncio.create_subprocess_exec(
                "go", "run", "main.go",
                cwd=service_path,
                env=env,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            service.process = process
            return True
            
        except Exception as e:
            logger.error(f"Failed to start frontend service: {e}")
            return False

    async def _check_service_health(self, service: GalateaService) -> bool:
        """Check if a service is healthy."""
        try:
            import aiohttp
            
            async with aiohttp.ClientSession() as session:
                health_url = f"http://localhost:{service.port}{service.health_url}"
                async with session.get(health_url, timeout=5) as response:
                    return response.status == 200
                    
        except Exception as e:
            logger.debug(f"Health check failed for {service.name}: {e}")
            return False

    async def stop_service(self, service_name: str) -> bool:
        """Stop a Galatea service."""
        service = self.services.get(service_name)
        if not service:
            logger.error(f"Service {service_name} not found")
            return False
        
        if service.status == "stopped":
            logger.info(f"Service {service_name} is already stopped")
            return True
        
        try:
            if service.process:
                service.process.terminate()
                
                # Wait for graceful shutdown
                try:
                    await asyncio.wait_for(service.process.wait(), timeout=5)
                except asyncio.TimeoutError:
                    # Force kill if doesn't stop gracefully
                    service.process.kill()
                    await service.process.wait()
                
                service.process = None
            
            service.status = "stopped"
            logger.info(f"Stopped service {service_name}")
            return True
            
        except Exception as e:
            logger.error(f"Error stopping service {service_name}: {e}")
            return False

    def get_service_status(self, service_name: str) -> Optional[Dict[str, Any]]:
        """Get status of a service."""
        service = self.services.get(service_name)
        if not service:
            return None
        
        return {
            "name": service.name,
            "type": service.service_type,
            "status": service.status,
            "port": service.port,
            "endpoints": service.endpoints,
            "process_id": service.process.pid if service.process else None,
            "metadata": service.metadata
        }

    def list_services(self) -> List[Dict[str, Any]]:
        """List all services and their status."""
        return [self.get_service_status(name) for name in self.services]

    async def create_user_session(self, username: str, user_id: Optional[str] = None,
                                 permissions: Optional[List[str]] = None) -> str:
        """Create a new user session."""
        import uuid
        
        session_id = str(uuid.uuid4())
        actual_user_id = user_id or str(uuid.uuid4())
        
        session = UserSession(
            session_id=session_id,
            user_id=actual_user_id,
            username=username,
            authenticated=True,
            permissions=permissions or ["read", "write"],
            metadata={
                "created_at": asyncio.get_event_loop().time(),
                "ip_address": "127.0.0.1",  # Would be actual IP
                "user_agent": "galatea-adapter"
            }
        )
        
        self.user_sessions[session_id] = session
        logger.info(f"Created user session {session_id} for {username}")
        return session_id

    def get_user_session(self, session_id: str) -> Optional[UserSession]:
        """Get user session by ID."""
        return self.user_sessions.get(session_id)

    def authenticate_session(self, session_id: str) -> bool:
        """Check if a session is authenticated."""
        session = self.get_user_session(session_id)
        return session.authenticated if session else False

    def check_permission(self, session_id: str, permission: str) -> bool:
        """Check if a session has a specific permission."""
        session = self.get_user_session(session_id)
        if not session:
            return False
        return permission in session.permissions

    async def proxy_api_request(self, endpoint: str, method: str = "GET", 
                               data: Optional[Dict[str, Any]] = None,
                               session_id: Optional[str] = None) -> Dict[str, Any]:
        """Proxy an API request to the appropriate Galatea service."""
        # Determine which service should handle this endpoint
        target_service = None
        
        if endpoint.startswith("/api/"):
            target_service = self.services.get("frontend")
        elif endpoint.startswith("/auth/"):
            target_service = self.services.get("frontend")
        else:
            target_service = self.services.get("ui")
        
        if not target_service or target_service.status != "running":
            return {"error": "Service not available"}
        
        # Check authentication if session provided
        if session_id and not self.authenticate_session(session_id):
            return {"error": "Authentication required"}
        
        try:
            import aiohttp
            
            url = f"http://localhost:{target_service.port}{endpoint}"
            
            async with aiohttp.ClientSession() as session:
                if method.upper() == "GET":
                    async with session.get(url) as response:
                        result = await response.json()
                        return {"status": response.status, "data": result}
                elif method.upper() == "POST":
                    async with session.post(url, json=data) as response:
                        result = await response.json()
                        return {"status": response.status, "data": result}
                # Add other HTTP methods as needed
                
        except Exception as e:
            logger.error(f"Failed to proxy request to {endpoint}: {e}")
            return {"error": str(e)}

    async def create_arena_ui_session(self, arena_id: str, user_session_id: str) -> Dict[str, Any]:
        """Create a UI session for an arena."""
        session = self.get_user_session(user_session_id)
        if not session:
            return {"error": "Invalid user session"}
        
        # This would integrate with the AAR arena system
        arena_session = {
            "arena_id": arena_id,
            "user_id": session.user_id,
            "username": session.username,
            "ui_session_id": user_session_id,
            "permissions": session.permissions,
            "created_at": asyncio.get_event_loop().time()
        }
        
        logger.info(f"Created arena UI session for {arena_id}")
        return {"arena_session": arena_session}

    async def get_agent_list_for_ui(self, session_id: str) -> Dict[str, Any]:
        """Get agent list formatted for UI consumption."""
        if not self.authenticate_session(session_id):
            return {"error": "Authentication required"}
        
        # This would integrate with the AAR agent manager
        # For now, return mock data
        agents = [
            {
                "id": "agent-1",
                "name": "GPT-4 Assistant", 
                "status": "active",
                "model": "gpt-4o",
                "capabilities": ["reasoning", "conversation"]
            },
            {
                "id": "agent-2",
                "name": "Claude Analyst",
                "status": "idle", 
                "model": "claude-3-sonnet",
                "capabilities": ["analysis", "writing"]
            }
        ]
        
        return {"agents": agents}

    def health_check(self) -> Dict[str, Any]:
        """Perform health check of all Galatea services."""
        service_health = {}
        
        for name, service in self.services.items():
            service_health[name] = {
                "status": service.status,
                "port": service.port,
                "process_running": service.process is not None and service.process.poll() is None
            }
        
        return {
            "status": "healthy",
            "services": service_health,
            "active_sessions": len(self.user_sessions),
            "ui_available": self.services["ui"].status == "running",
            "frontend_available": self.services["frontend"].status == "running"
        }