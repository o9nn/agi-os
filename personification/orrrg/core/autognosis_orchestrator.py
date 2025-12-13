"""Autognosis orchestrator for coordinating the entire system."""

from typing import Dict, List
import asyncio
from datetime import datetime
from .models import SelfImage, MetaCognitiveInsight, OptimizationOpportunity
from .self_monitor import SelfMonitor
from .hierarchical_self_modeler import HierarchicalSelfModeler
from .meta_cognitive_processor import MetaCognitiveProcessor


class AutognosisOrchestrator:
    """Coordinates the entire autognosis system."""
    
    def __init__(self, max_levels: int = 5, cycle_interval: float = 30.0):
        self.monitor = SelfMonitor()
        self.modeler = HierarchicalSelfModeler(max_levels=max_levels)
        self.processor = MetaCognitiveProcessor()
        
        self.max_levels = max_levels
        self.cycle_interval = cycle_interval
        
        self._running = False
        self._cycle_count = 0
        self._cycle_task = None
        
        self.current_self_images: Dict[int, SelfImage] = {}
        self.current_insights: List[MetaCognitiveInsight] = []
        self.optimization_opportunities: List[OptimizationOpportunity] = []
        
    async def start(self, soc):
        """Start the autognosis system."""
        if self._running:
            return
        
        self._running = True
        self._cycle_task = asyncio.create_task(self._autognosis_loop(soc))
        
    async def stop(self):
        """Stop the autognosis system."""
        self._running = False
        if self._cycle_task:
            self._cycle_task.cancel()
            try:
                await self._cycle_task
            except asyncio.CancelledError:
                pass
    
    async def _autognosis_loop(self, soc):
        """Main autognosis loop."""
        while self._running:
            try:
                await self.run_autognosis_cycle(soc)
                await asyncio.sleep(self.cycle_interval)
            except asyncio.CancelledError:
                break
            except Exception as e:
                print(f"Error in autognosis cycle: {e}")
                await asyncio.sleep(self.cycle_interval)
    
    async def run_autognosis_cycle(self, soc) -> Dict:
        """Run a complete autognosis cycle.
        
        Returns:
            Dict containing cycle results
        """
        cycle_start = datetime.now()
        
        # Take multiple observations to build pattern detection capacity
        for _ in range(3):
            await self.monitor.observe_system(soc)
        
        # Build self-images at all levels
        new_images = {}
        for level in range(self.max_levels):
            self_image = await self.modeler.build_self_image(level, self.monitor, soc)
            new_images[level] = self_image
        
        self.current_self_images = new_images
        
        # Process insights from highest level
        highest_level = self.max_levels - 1
        if highest_level in new_images:
            insights = await self.processor.process_self_image(new_images[highest_level])
            self.current_insights = insights
        
        # Discover optimization opportunities
        opportunities = self._discover_optimizations()
        self.optimization_opportunities = opportunities
        
        self._cycle_count += 1
        
        cycle_end = datetime.now()
        duration = (cycle_end - cycle_start).total_seconds()
        
        return {
            "cycle_number": self._cycle_count,
            "self_images": new_images,
            "insights": insights if highest_level in new_images else [],
            "optimization_opportunities": opportunities,
            "duration_seconds": duration,
            "timestamp": cycle_end,
        }
    
    def _discover_optimizations(self) -> List[OptimizationOpportunity]:
        """Discover optimization opportunities from insights."""
        opportunities = []
        
        # Analyze insights for optimization potential
        for insight in self.current_insights:
            if insight.insight_type == "resource_underutilization":
                opportunities.append(OptimizationOpportunity(
                    opportunity_type="resource_allocation",
                    description="Optimize resource allocation to increase utilization",
                    priority="medium",
                    risk_level="low",
                    estimated_impact=0.25,
                    proposed_actions=[
                        "Analyze component workload distribution",
                        "Redistribute processing capacity",
                        "Enable dynamic resource scaling",
                    ],
                ))
        
        return opportunities
    
    def get_status(self) -> Dict:
        """Get current autognosis status."""
        return {
            "running": self._running,
            "cycle_count": self._cycle_count,
            "max_levels": self.max_levels,
            "current_self_images": len(self.current_self_images),
            "total_insights": len(self.current_insights),
            "pending_optimizations": len(self.optimization_opportunities),
        }
