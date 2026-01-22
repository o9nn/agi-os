import asyncio
from datetime import datetime
from typing import Dict, Optional
from .autognosis_orchestrator import AutognosisOrchestrator
class SelfOrganizingCore:
    def __init__(self, autognosis_levels: int=5):
        self._initialized = False
        self._start_time: Optional[datetime] = None
        self.autognosis = AutognosisOrchestrator(max_levels=autognosis_levels)
    async def initialize(self):
        if self._initialized:
            return
        self._initialized = True
        self._start_time = datetime.now()
        await self.autognosis.start(self)
    async def shutdown(self):
        if not self._initialized:
            return
        await self.autognosis.stop()
        self._initialized = False
    @property
    def _uptime(self) -> float:
        if not self._start_time:
            return 0.0
        return (datetime.now() - self._start_time).total_seconds()
    def get_autognosis_status(self) -> Dict:
        return self.autognosis.get_status()
    async def run_autognosis_cycle(self):
        return await self.autognosis.run_autognosis_cycle(self)