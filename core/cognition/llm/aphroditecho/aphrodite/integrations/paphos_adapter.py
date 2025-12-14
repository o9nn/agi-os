"""
Paphos Backend Adapter - Integrates the Crystal paphos-backend with AAR system

This adapter provides persistence, authentication, and domain services integration
by interfacing with the Crystal Lucky framework-based backend service.
"""

import asyncio
import logging
import subprocess
import os
import re
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
from pathlib import Path
import uuid
import time

logger = logging.getLogger(__name__)


@dataclass
class PaphosUser:
    """Represents a user in the Paphos backend."""
    id: str
    email: str
    username: str
    authenticated: bool = True
    permissions: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class PaphosModel:
    """Represents a data model in Paphos backend."""
    name: str
    table_name: str
    fields: Dict[str, str] = field(default_factory=dict)
    relationships: List[str] = field(default_factory=list)
    operations: List[str] = field(default_factory=lambda: ["create", "read", "update", "delete"])
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class PaphosService:
    """Represents a service in the Paphos backend."""
    name: str
    port: int
    process: Optional[subprocess.Popen] = None
    status: str = "stopped"
    endpoints: List[str] = field(default_factory=list)
    database_url: str = ""
    metadata: Dict[str, Any] = field(default_factory=dict)


class PaphosAdapter:
    """
    Adapter for integrating Crystal paphos-backend with the AAR system.
    
    Provides:
    - Backend service management (start/stop/monitor)
    - Database operations through Lucky framework
    - User authentication and authorization
    - Data persistence for AAR system
    - Domain-specific business logic integration
    """

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        self.config = config or {}
        self.service: Optional[PaphosService] = None
        self.users: Dict[str, PaphosUser] = {}
        self.models: Dict[str, PaphosModel] = {}
        
        # Set up service configuration
        self.paphos_root = Path(__file__).parent.parent.parent / "2do" / "paphos-backend"
        self.available = False
        
        if self.paphos_root.exists():
            self._initialize_paphos_service()
        else:
            self._setup_compatibility_mode()

    def _initialize_paphos_service(self):
        """Initialize the Paphos backend service."""
        try:
            # Check if Crystal is available
            result = subprocess.run(
                ["crystal", "--version"],
                capture_output=True,
                text=True,
                timeout=10
            )
            
            if result.returncode == 0:
                self.available = True
                self._setup_service_config()
                self._discover_models()
                logger.info("Paphos backend adapter initialized successfully")
            else:
                logger.warning("Crystal not found, using compatibility mode")
                self._setup_compatibility_mode()
                
        except Exception as e:
            logger.error(f"Failed to initialize paphos: {e}")
            self._setup_compatibility_mode()

    def _setup_service_config(self):
        """Set up the service configuration."""
        port = self.config.get("port", 5000)
        
        self.service = PaphosService(
            name="paphos-backend",
            port=port,
            endpoints=[
                "/api/v1/auth",
                "/api/v1/users",
                "/api/v1/models",
                "/api/v1/health"
            ],
            database_url=self.config.get("database_url", "postgres://localhost:5432/paphos_dev"),
            metadata={
                "framework": "lucky",
                "language": "crystal",
                "source_path": str(self.paphos_root)
            }
        )

    def _discover_models(self):
        """Discover data models from the paphos-backend source."""
        try:
            models_dir = self.paphos_root / "src" / "models"
            if models_dir.exists():
                for model_file in models_dir.glob("*.cr"):
                    self._parse_crystal_model(model_file)
                    
            logger.info(f"Discovered {len(self.models)} models")
            
        except Exception as e:
            logger.error(f"Failed to discover models: {e}")

    def _parse_crystal_model(self, model_file: Path):
        """Parse a Crystal model file to extract schema information."""
        try:
            content = model_file.read_text()
            
            # Extract class name
            class_match = re.search(r'class (\w+) < BaseModel', content)
            if not class_match:
                return
            
            class_name = class_match.group(1)
            
            # Extract table name
            table_match = re.search(r'table do\s*\n(.*?)end', content, re.DOTALL)
            fields = {}
            
            if table_match:
                table_content = table_match.group(1)
                
                # Parse field definitions
                field_matches = re.findall(r'column (\w+) : (\w+)', table_content)
                for field_name, field_type in field_matches:
                    fields[field_name] = field_type
            
            # Create model specification
            model = PaphosModel(
                name=class_name.lower(),
                table_name=class_name.lower() + "s",  # Convention
                fields=fields,
                metadata={
                    "file_path": str(model_file),
                    "crystal_class": class_name
                }
            )
            
            self.models[model.name] = model
            
        except Exception as e:
            logger.warning(f"Failed to parse model {model_file}: {e}")

    def _setup_compatibility_mode(self):
        """Setup compatibility mode with mock data structures."""
        self.available = False
        
        # Create mock service
        self.service = PaphosService(
            name="paphos-backend",
            port=5000,
            status="compatibility_mode",
            endpoints=["/api/v1/health"],
            metadata={"compatibility_mode": True}
        )
        
        # Create basic data models
        basic_models = [
            PaphosModel(
                name="user",
                table_name="users", 
                fields={"id": "UUID", "email": "String", "username": "String", "created_at": "Time"},
                operations=["create", "read", "update", "delete"]
            ),
            PaphosModel(
                name="session",
                table_name="sessions",
                fields={"id": "UUID", "user_id": "UUID", "token": "String", "expires_at": "Time"},
                operations=["create", "read", "delete"]
            ),
            PaphosModel(
                name="arena",
                table_name="arenas",
                fields={"id": "UUID", "name": "String", "config": "JSON", "created_at": "Time"},
                operations=["create", "read", "update", "delete"]
            )
        ]
        
        for model in basic_models:
            self.models[model.name] = model
        
        logger.info("Paphos adapter running in compatibility mode")

    async def start_service(self) -> bool:
        """Start the paphos backend service."""
        if not self.available:
            logger.info("Paphos service started in compatibility mode")
            if self.service:
                self.service.status = "running"
            return True
        
        if self.service and self.service.status == "running":
            logger.info("Paphos service is already running")
            return True
        
        try:
            # Check if dependencies are installed
            if not self._check_dependencies():
                logger.error("Paphos dependencies not installed")
                return False
            
            # Set up environment
            env = {
                **os.environ,
                "LUCKY_ENV": "development",
                "DATABASE_URL": self.service.database_url,
                "PORT": str(self.service.port)
            }
            
            # Start the Crystal service
            process = await asyncio.create_subprocess_exec(
                "crystal", "run", "src/paphos.cr",
                cwd=self.paphos_root,
                env=env,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            self.service.process = process
            self.service.status = "running"
            
            # Wait a moment for service to start
            await asyncio.sleep(2)
            
            # Check if service is healthy
            healthy = await self._check_service_health()
            if healthy:
                logger.info(f"Paphos service started successfully on port {self.service.port}")
                return True
            else:
                logger.error("Paphos service failed health check")
                await self.stop_service()
                return False
                
        except Exception as e:
            logger.error(f"Failed to start paphos service: {e}")
            if self.service:
                self.service.status = "error"
            return False

    def _check_dependencies(self) -> bool:
        """Check if Crystal dependencies are installed."""
        try:
            # Check if shards are installed
            result = subprocess.run(
                ["crystal", "deps", "check"],
                cwd=self.paphos_root,
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if result.returncode != 0:
                # Try to install dependencies
                logger.info("Installing Crystal dependencies...")
                install_result = subprocess.run(
                    ["shards", "install"],
                    cwd=self.paphos_root,
                    capture_output=True,
                    text=True,
                    timeout=300
                )
                
                return install_result.returncode == 0
            
            return True
            
        except Exception as e:
            logger.error(f"Failed to check dependencies: {e}")
            return False

    async def _check_service_health(self) -> bool:
        """Check if the paphos service is healthy."""
        try:
            import aiohttp
            
            async with aiohttp.ClientSession() as session:
                health_url = f"http://localhost:{self.service.port}/api/v1/health"
                async with session.get(health_url, timeout=5) as response:
                    return response.status == 200
                    
        except Exception:
            return False

    async def stop_service(self) -> bool:
        """Stop the paphos backend service."""
        if not self.service:
            return True
        
        if self.service.status == "stopped":
            return True
        
        try:
            if self.service.process:
                self.service.process.terminate()
                
                # Wait for graceful shutdown
                try:
                    await asyncio.wait_for(self.service.process.wait(), timeout=10)
                except asyncio.TimeoutError:
                    # Force kill if doesn't stop gracefully
                    self.service.process.kill()
                    await self.service.process.wait()
                
                self.service.process = None
            
            self.service.status = "stopped"
            logger.info("Paphos service stopped")
            return True
            
        except Exception as e:
            logger.error(f"Failed to stop paphos service: {e}")
            return False

    async def create_user(self, user_data: Dict[str, Any]) -> Optional[str]:
        """Create a new user in the backend."""
        if not self.available:
            # Compatibility mode
            user_id = str(uuid.uuid4())
            user = PaphosUser(
                id=user_id,
                email=user_data.get("email", ""),
                username=user_data.get("username", ""),
                permissions=user_data.get("permissions", ["read"]),
                metadata={"compatibility_mode": True}
            )
            self.users[user_id] = user
            logger.info(f"Created user {user_id} in compatibility mode")
            return user_id
        
        try:
            # Make API call to paphos service
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    f"http://localhost:{self.service.port}/api/v1/users",
                    json=user_data
                ) as response:
                    if response.status == 201:
                        result = await response.json()
                        user_id = result.get("id")
                        
                        # Cache user locally
                        user = PaphosUser(
                            id=user_id,
                            email=user_data.get("email", ""),
                            username=user_data.get("username", ""),
                            permissions=user_data.get("permissions", ["read"])
                        )
                        self.users[user_id] = user
                        
                        logger.info(f"Created user {user_id}")
                        return user_id
                    else:
                        error_data = await response.json()
                        logger.error(f"Failed to create user: {error_data}")
                        return None
                        
        except Exception as e:
            logger.error(f"Failed to create user: {e}")
            return None

    async def authenticate_user(self, email: str, password: str) -> Optional[Dict[str, Any]]:
        """Authenticate a user and return session token."""
        if not self.available:
            # Compatibility mode
            user = next((u for u in self.users.values() if u.email == email), None)
            if user:
                session_token = str(uuid.uuid4())
                return {
                    "user_id": user.id,
                    "token": session_token,
                    "expires_at": time.time() + 3600,  # 1 hour
                    "compatibility_mode": True
                }
            return None
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    f"http://localhost:{self.service.port}/api/v1/auth/login",
                    json={"email": email, "password": password}
                ) as response:
                    if response.status == 200:
                        return await response.json()
                    else:
                        return None
                        
        except Exception as e:
            logger.error(f"Authentication failed: {e}")
            return None

    async def create_record(self, model_name: str, data: Dict[str, Any]) -> Optional[str]:
        """Create a record in the specified model."""
        model = self.models.get(model_name)
        if not model:
            logger.error(f"Model {model_name} not found")
            return None
        
        if not self.available:
            # Compatibility mode
            record_id = str(uuid.uuid4())
            logger.info(f"Created {model_name} record {record_id} in compatibility mode")
            return record_id
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    f"http://localhost:{self.service.port}/api/v1/{model.table_name}",
                    json=data
                ) as response:
                    if response.status == 201:
                        result = await response.json()
                        return result.get("id")
                    else:
                        error_data = await response.json()
                        logger.error(f"Failed to create {model_name}: {error_data}")
                        return None
                        
        except Exception as e:
            logger.error(f"Failed to create record: {e}")
            return None

    async def get_record(self, model_name: str, record_id: str) -> Optional[Dict[str, Any]]:
        """Get a record by ID."""
        model = self.models.get(model_name)
        if not model:
            return None
        
        if not self.available:
            # Compatibility mode
            return {
                "id": record_id,
                "model": model_name,
                "compatibility_mode": True,
                "data": {"example": "data"}
            }
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.get(
                    f"http://localhost:{self.service.port}/api/v1/{model.table_name}/{record_id}"
                ) as response:
                    if response.status == 200:
                        return await response.json()
                    else:
                        return None
                        
        except Exception as e:
            logger.error(f"Failed to get record: {e}")
            return None

    async def update_record(self, model_name: str, record_id: str, data: Dict[str, Any]) -> bool:
        """Update a record."""
        model = self.models.get(model_name)
        if not model:
            return False
        
        if not self.available:
            # Compatibility mode
            logger.info(f"Updated {model_name} record {record_id} in compatibility mode")
            return True
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.put(
                    f"http://localhost:{self.service.port}/api/v1/{model.table_name}/{record_id}",
                    json=data
                ) as response:
                    return response.status == 200
                    
        except Exception as e:
            logger.error(f"Failed to update record: {e}")
            return False

    async def delete_record(self, model_name: str, record_id: str) -> bool:
        """Delete a record."""
        model = self.models.get(model_name)
        if not model:
            return False
        
        if not self.available:
            # Compatibility mode
            logger.info(f"Deleted {model_name} record {record_id} in compatibility mode")
            return True
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.delete(
                    f"http://localhost:{self.service.port}/api/v1/{model.table_name}/{record_id}"
                ) as response:
                    return response.status == 200
                    
        except Exception as e:
            logger.error(f"Failed to delete record: {e}")
            return False

    def get_model(self, model_name: str) -> Optional[PaphosModel]:
        """Get model specification."""
        return self.models.get(model_name)

    def list_models(self) -> List[PaphosModel]:
        """List all available models."""
        return list(self.models.values())

    def get_user(self, user_id: str) -> Optional[PaphosUser]:
        """Get user by ID."""
        return self.users.get(user_id)

    def list_users(self) -> List[PaphosUser]:
        """List all users."""
        return list(self.users.values())

    def get_service_status(self) -> Dict[str, Any]:
        """Get service status information."""
        if not self.service:
            return {"status": "not_configured"}
        
        return {
            "name": self.service.name,
            "status": self.service.status,
            "port": self.service.port,
            "process_id": self.service.process.pid if self.service.process else None,
            "endpoints": self.service.endpoints,
            "database_url": self.service.database_url.replace("postgres://", "postgres://***@") if self.service.database_url else None,
            "metadata": self.service.metadata
        }

    def health_check(self) -> Dict[str, Any]:
        """Perform health check of the Paphos adapter."""
        service_health = {
            "service_available": self.available,
            "service_status": self.service.status if self.service else "not_configured",
            "crystal_available": self.available,
            "paphos_root_exists": self.paphos_root.exists()
        }
        
        data_health = {
            "models_discovered": len(self.models),
            "users_cached": len(self.users),
            "model_list": list(self.models.keys())
        }
        
        return {
            "status": "healthy" if (self.available or self.service.status == "compatibility_mode") else "degraded",
            "service": service_health,
            "data": data_health,
            "compatibility_mode": not self.available
        }