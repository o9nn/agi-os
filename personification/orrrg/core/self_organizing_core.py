"""Self-organizing core system for ORRRG."""

import asyncio
from datetime import datetime
from typing import Dict, Optional
from .autognosis_orchestrator import AutognosisOrchestrator


class SelfOrganizingCore:
    """Core self-organizing system with autognosis capabilities."""
    
    def __init__(self, autognosis_levels: int = 5):
        self._initialized = False
        self._start_time: Optional[datetime] = None
        self.autognosis = AutognosisOrchestrator(max_levels=autognosis_levels)
        
    async def initialize(self):
        """Initialize the self-organizing core system."""
        if self._initialized:
            return
        
        self._initialized = True
        self._start_time = datetime.now()
        
        # Start autognosis system
        await self.autognosis.start(self)
        
    async def shutdown(self):
        """Shutdown the self-organizing core system."""
        if not self._initialized:
            return
        
        # Stop autognosis
        await self.autognosis.stop()
        
        self._initialized = False
    
    @property
    def _uptime(self) -> float:
        """Get system uptime in seconds."""
        if not self._start_time:
            return 0.0
        return (datetime.now() - self._start_time).total_seconds()
    
    def get_autognosis_status(self) -> Dict:
        """Get autognosis system status."""
        return self.autognosis.get_status()
    
    async def run_autognosis_cycle(self):
        """Manually trigger an autognosis cycle."""
        return await self.autognosis.run_autognosis_cycle(self)
