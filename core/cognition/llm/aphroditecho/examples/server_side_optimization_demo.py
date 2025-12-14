#!/usr/bin/env python3
"""
Server-Side Model Optimization Demo

Demonstrates the complete server-side model optimization system including
compilation, dynamic tuning, ensemble serving, and performance monitoring.

This script showcases Phase 8 SSR-focused MLOps capabilities for optimal
performance under varying server conditions.
"""

import asyncio
import logging
import time
import json
from typing import Dict, Any, List
from pathlib import Path

# Configure logging for demo
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

# Import optimization components
try:
    from aphrodite.engine.server_side_optimizer import (
        ServerSideOptimizer,
        OptimizationConfig,
        ServerLoadMetrics
    )
    from aphrodite.endpoints.openai.serving_optimization import (
        OptimizationServingMixin,
        create_optimized_request_context,
        OptimizationMiddleware
    )
    OPTIMIZATION_AVAILABLE = True
except ImportError as e:
    logger.warning(f"Optimization components not available: {e}")
    OPTIMIZATION_AVAILABLE = False


class MockModel:
    """Mock model for demonstration purposes."""
    
    def __init__(self, model_id: str, base_latency: float = 100.0, error_rate: float = 0.01):
        self.model_id = model_id
        self.base_latency = base_latency
        self.error_rate = error_rate
        self.request_count = 0
        
    def process(self, input_text: str) -> Dict[str, Any]:
        """Simulate model processing."""
        self.request_count += 1
        
        # Simulate variable latency based on input length
        latency = self.base_latency * (1 + len(input_text) / 1000)
        
        # Simulate occasional errors
        import random
        success = random.random() > self.error_rate
        
        if not success:
            raise Exception(f"Processing error in {self.model_id}")
        
        return {
            "model_id": self.model_id,
            "output": f"Processed: {input_text[:50]}..." if len(input_text) > 50 else f"Processed: {input_text}",
            "latency_ms": latency,
            "tokens_generated": len(input_text.split()) * 2  # Rough estimate
        }


class MockEngineClient:
    """Mock engine client for demonstration."""
    
    def __init__(self):
        self.models = {
            "fast_model": MockModel("fast_model", base_latency=80.0, error_rate=0.02),
            "balanced_model": MockModel("balanced_model", base_latency=120.0, error_rate=0.01),
            "accurate_model": MockModel("accurate_model", base_latency=180.0, error_rate=0.005)
        }
        self.default_model = self.models["balanced_model"]
    
    async def process_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """Process a request with the specified model."""
        model_id = request.get("model", "balanced_model")
        model = self.models.get(model_id, self.default_model)
        
        return model.process(request["input"])


class MockModelConfig:
    """Mock model configuration."""
    
    def __init__(self):
        self.model = "demo-model"
        self.tokenizer = "demo-tokenizer"
        self.dtype = "float16"


class OptimizationDemo:
    """
    Comprehensive demonstration of server-side model optimization.
    
    Shows all major features including compilation, dynamic tuning,
    ensemble serving, and performance monitoring.
    """
    
    def __init__(self):
        self.engine_client = MockEngineClient()
        self.model_config = MockModelConfig()
        self.optimizer = None
        self.demo_results = {}
        
    async def setup_optimizer(self):
        """Initialize the optimization system."""
        logger.info("🚀 Setting up server-side optimization system...")
        
        if not OPTIMIZATION_AVAILABLE:
            logger.error("Optimization components not available - running in demo mode only")
            return
        
        # Configure optimization with demo-friendly settings
        config = OptimizationConfig(
            enable_torch_compile=False,  # Disable for demo to avoid torch.compile
            enable_dynamic_tuning=True,
            tuning_interval_sec=5.0,     # Fast intervals for demo
            load_threshold_high=0.8,
            load_threshold_low=0.3,
            enable_ensemble=True,
            max_ensemble_size=3,
            ensemble_strategy="weighted_voting",
            enable_performance_tracking=True,
            metrics_history_size=100
        )
        
        # Initialize optimizer
        self.optimizer = ServerSideOptimizer(
            engine_client=self.engine_client,
            model_config=self.model_config,
            lora_config=None,
            optimization_config=config
        )
        
        # Start optimization
        await self.optimizer.start_optimization()
        logger.info("✅ Optimization system initialized and started")
    
    async def demonstrate_compilation(self):
        """Demonstrate model compilation features."""
        logger.info("\n📦 Demonstrating Model Compilation...")
        
        if not self.optimizer:
            logger.info("⚠️  Optimization not available - skipping compilation demo")
            return
        
        # Mock model compilation demonstration
        models_to_compile = ["fast_model", "balanced_model", "accurate_model"]
        
        for model_id in models_to_compile:
            mock_model = self.engine_client.models[model_id]
            
            # Simulate compilation process
            logger.info(f"🔧 Compiling model: {model_id}")
            
            # In a real scenario, this would call torch.compile
            compiled_model = await self.optimizer.optimize_model_for_request(
                mock_model,
                {"model_id": model_id, "compilation": True}
            )
            
            logger.info(f"✅ Model {model_id} compilation completed")
        
        # Show compilation statistics
        status = self.optimizer.get_optimization_status()
        compilation_stats = status.get("compilation_stats", {})
        
        logger.info(f"📊 Compilation Statistics:")
        for model_id, stats in compilation_stats.items():
            logger.info(f"  • {model_id}: {stats.get('compilation_time', 0):.2f}s")
    
    async def demonstrate_dynamic_tuning(self):
        """Demonstrate dynamic parameter tuning based on load."""
        logger.info("\n⚡ Demonstrating Dynamic Parameter Tuning...")
        
        if not self.optimizer:
            logger.info("⚠️  Optimization not available - simulating tuning demo")
            await self._simulate_tuning_demo()
            return
        
        # Simulate different load conditions
        load_scenarios = [
            ("Low Load", ServerLoadMetrics(
                timestamp=time.time(),
                cpu_usage_percent=15.0,
                memory_usage_percent=20.0,
                gpu_utilization=25.0,
                active_requests=2,
                queue_depth=0,
                throughput_tokens_per_sec=200.0,
                avg_latency_ms=60.0,
                error_rate=0.005
            )),
            ("Medium Load", ServerLoadMetrics(
                timestamp=time.time(),
                cpu_usage_percent=55.0,
                memory_usage_percent=60.0,
                gpu_utilization=65.0,
                active_requests=15,
                queue_depth=3,
                throughput_tokens_per_sec=120.0,
                avg_latency_ms=150.0,
                error_rate=0.02
            )),
            ("High Load", ServerLoadMetrics(
                timestamp=time.time(),
                cpu_usage_percent=90.0,
                memory_usage_percent=85.0,
                gpu_utilization=95.0,
                active_requests=50,
                queue_depth=15,
                throughput_tokens_per_sec=80.0,
                avg_latency_ms=300.0,
                error_rate=0.05
            ))
        ]
        
        for scenario_name, load_metrics in load_scenarios:
            logger.info(f"🔍 Testing {scenario_name} (load score: {load_metrics.overall_load_score:.2f})")
            
            # Determine optimization strategy
            strategy = self.optimizer.parameter_tuner.determine_optimization_strategy(load_metrics)
            
            # Apply optimization
            await self.optimizer.parameter_tuner.apply_parameter_adjustments(strategy)
            
            # Log strategy details
            logger.info(f"  Strategy: {strategy['strategy']}")
            params = strategy["parameters"]
            logger.info(f"  Max Tokens: {params.get('max_tokens', 'N/A')}")
            logger.info(f"  Temperature: {params.get('temperature', 'N/A'):.2f}")
            logger.info(f"  Top-K: {params.get('top_k', 'N/A')}")
            
            # Brief pause between scenarios
            await asyncio.sleep(1.0)
    
    async def _simulate_tuning_demo(self):
        """Simulate tuning demo when optimization not available."""
        scenarios = [
            ("Low Load (15% CPU)", "quality", {"max_tokens": 2048, "temperature": 0.8}),
            ("Medium Load (55% CPU)", "balanced", {"max_tokens": 1024, "temperature": 0.75}),
            ("High Load (90% CPU)", "performance", {"max_tokens": 512, "temperature": 0.7})
        ]
        
        for load_desc, strategy, params in scenarios:
            logger.info(f"🔍 Testing {load_desc}")
            logger.info(f"  Strategy: {strategy}")
            logger.info(f"  Max Tokens: {params['max_tokens']}")
            logger.info(f"  Temperature: {params['temperature']}")
            await asyncio.sleep(0.5)
    
    async def demonstrate_ensemble_serving(self):
        """Demonstrate ensemble model serving."""
        logger.info("\n🎯 Demonstrating Ensemble Model Serving...")
        
        if not self.optimizer:
            logger.info("⚠️  Optimization not available - simulating ensemble demo")
            await self._simulate_ensemble_demo()
            return
        
        # Add models to ensemble
        ensemble_manager = self.optimizer.ensemble_manager
        
        for model_id, model in self.engine_client.models.items():
            logger.info(f"➕ Adding {model_id} to ensemble")
            ensemble_manager.add_model_to_ensemble(
                model_id=model_id,
                model_instance=model,
                initial_weight=1.0,
                performance_metric=1.0
            )
        
        # Simulate requests with different characteristics
        test_requests = [
            {"priority": "speed", "complexity": "low"},
            {"priority": "accuracy", "complexity": "high"},
            {"priority": "balanced", "complexity": "medium"},
            {"priority": "speed", "complexity": "medium"},
            {"priority": "accuracy", "complexity": "low"}
        ]
        
        selection_stats = {}
        
        for i, request_context in enumerate(test_requests):
            logger.info(f"🔄 Request {i+1}: {request_context}")
            
            # Select optimal model
            selected_info = ensemble_manager.select_model_for_request(request_context)
            selected_model_id = selected_info["model_id"]
            
            # Update selection statistics
            selection_stats[selected_model_id] = selection_stats.get(selected_model_id, 0) + 1
            
            # Simulate request processing and performance update
            latency = 100.0 + (i * 20)  # Simulate variable latency
            success = True
            
            ensemble_manager.update_model_performance(selected_model_id, latency, success)
            
            logger.info(f"  Selected: {selected_model_id}, Latency: {latency:.0f}ms")
        
        # Show ensemble status
        ensemble_status = ensemble_manager.get_ensemble_status()
        logger.info(f"\n📊 Ensemble Statistics:")
        logger.info(f"  Total Models: {ensemble_status['model_count']}")
        logger.info(f"  Strategy: {ensemble_status['strategy']}")
        
        for model_info in ensemble_status["models"]:
            model_id = model_info["model_id"]
            selections = selection_stats.get(model_id, 0)
            logger.info(f"  • {model_id}: {selections} selections, "
                       f"avg latency: {model_info['avg_latency_ms']:.0f}ms")
    
    async def _simulate_ensemble_demo(self):
        """Simulate ensemble demo when optimization not available."""
        models = ["fast_model", "balanced_model", "accurate_model"]
        logger.info("➕ Simulating ensemble with models: " + ", ".join(models))
        
        selections = {"fast_model": 2, "balanced_model": 2, "accurate_model": 1}
        logger.info("\n📊 Simulated Selection Results:")
        for model, count in selections.items():
            latency = {"fast_model": 80, "balanced_model": 120, "accurate_model": 180}[model]
            logger.info(f"  • {model}: {count} selections, avg latency: {latency}ms")
    
    async def demonstrate_performance_monitoring(self):
        """Demonstrate performance monitoring and reporting."""
        logger.info("\n📈 Demonstrating Performance Monitoring...")
        
        if not self.optimizer:
            logger.info("⚠️  Optimization not available - simulating monitoring demo")
            await self._simulate_monitoring_demo()
            return
        
        # Simulate a series of requests to generate performance data
        test_inputs = [
            "Simple query about weather",
            "Complex analysis of machine learning algorithms and their applications in natural language processing",
            "Medium length request for code generation",
            "Short question",
            "Extended discussion about the philosophical implications of artificial intelligence and consciousness"
        ]
        
        logger.info("🔄 Processing test requests for performance monitoring...")
        
        for i, input_text in enumerate(test_inputs):
            request_id = f"demo_req_{i+1}"
            
            # Create request context
            context = create_optimized_request_context(
                model_name="balanced_model",
                request_id=request_id,
                user_preferences={"quality": "medium"},
                load_info={"current_requests": i + 1}
            )
            
            # Simulate request processing
            start_time = time.time()
            try:
                result = self.engine_client.models["balanced_model"].process(input_text)
                success = True
                latency = result["latency_ms"]
            except Exception as e:
                success = False
                latency = 200.0
                logger.warning(f"Request {request_id} failed: {e}")
            
            end_time = time.time()
            
            # Record performance metrics
            self.optimizer.record_request_performance(
                model_id="balanced_model",
                latency_ms=latency,
                success=success,
                additional_metrics={
                    "request_id": request_id,
                    "input_length": len(input_text),
                    "complexity": "high" if len(input_text) > 100 else "low"
                }
            )
            
            logger.info(f"  Request {i+1}: {'✅' if success else '❌'} "
                       f"{latency:.0f}ms ({len(input_text)} chars)")
        
        # Generate performance report
        logger.info("\n📊 Performance Report:")
        status = self.optimizer.get_optimization_status()
        
        if "recent_performance" in status:
            perf = status["recent_performance"]
            logger.info(f"  Average Latency: {perf.get('avg_latency_ms', 0):.0f}ms")
            logger.info(f"  Success Rate: {perf.get('success_rate', 0):.1%}")
        
        # Export detailed report
        report_path = "/tmp/optimization_demo_report.json"
        report = self.optimizer.export_performance_report(report_path)
        logger.info(f"  📄 Detailed report exported to: {report_path}")
    
    async def _simulate_monitoring_demo(self):
        """Simulate monitoring demo when optimization not available."""
        logger.info("🔄 Simulating request processing for monitoring...")
        
        simulated_metrics = [
            ("Request 1", True, 85),
            ("Request 2", True, 120),  
            ("Request 3", False, 200),
            ("Request 4", True, 95),
            ("Request 5", True, 110)
        ]
        
        for req_name, success, latency in simulated_metrics:
            status = "✅" if success else "❌"
            logger.info(f"  {req_name}: {status} {latency}ms")
        
        # Simulated performance summary
        success_rate = sum(1 for _, success, _ in simulated_metrics if success) / len(simulated_metrics)
        avg_latency = sum(latency for _, success, latency in simulated_metrics if success) / sum(1 for _, success, _ in simulated_metrics if success)
        
        logger.info(f"\n📊 Performance Summary:")
        logger.info(f"  Success Rate: {success_rate:.1%}")
        logger.info(f"  Average Latency: {avg_latency:.0f}ms")
    
    async def demonstrate_api_endpoints(self):
        """Demonstrate API endpoints (simulation)."""
        logger.info("\n🌐 API Endpoints Available:")
        
        endpoints = [
            ("POST", "/v1/optimization/configure", "Configure optimization settings"),
            ("POST", "/v1/optimization/start", "Start optimization system"),
            ("POST", "/v1/optimization/stop", "Stop optimization system"),
            ("GET", "/v1/optimization/status", "Get optimization status"),
            ("GET", "/v1/optimization/metrics", "Get server load metrics"),
            ("GET", "/v1/optimization/ensemble", "Get ensemble status"),
            ("POST", "/v1/optimization/ensemble/add_model", "Add model to ensemble"),
            ("GET", "/v1/optimization/report", "Get performance report"),
            ("POST", "/v1/optimization/force_recompile", "Force model recompilation")
        ]
        
        for method, path, description in endpoints:
            logger.info(f"  {method:4} {path:<35} - {description}")
        
        logger.info("\n📋 Example API Usage:")
        logger.info("  # Get current optimization status")
        logger.info("  curl -X GET http://localhost:2242/v1/optimization/status")
        logger.info("")
        logger.info("  # Configure optimization")
        logger.info('  curl -X POST http://localhost:2242/v1/optimization/configure \\')
        logger.info('    -H "Content-Type: application/json" \\')
        logger.info('    -d \'{"enable_torch_compile": true, "ensemble_strategy": "adaptive"}\'')
    
    async def run_complete_demo(self):
        """Run the complete optimization demonstration."""
        logger.info("🎭 Starting Server-Side Model Optimization Demo")
        logger.info("=" * 60)
        
        try:
            # Setup
            await self.setup_optimizer()
            
            # Run demonstrations
            await self.demonstrate_compilation()
            await self.demonstrate_dynamic_tuning()
            await self.demonstrate_ensemble_serving()
            await self.demonstrate_performance_monitoring()
            await self.demonstrate_api_endpoints()
            
            # Final status
            if self.optimizer:
                logger.info("\n🏁 Final Optimization Status:")
                status = self.optimizer.get_optimization_status()
                logger.info(f"  System Running: {status.get('running', False)}")
                
                config = status.get('configuration', {})
                logger.info(f"  Compilation: {config.get('torch_compile_enabled', False)}")
                logger.info(f"  Dynamic Tuning: {config.get('dynamic_tuning_enabled', False)}")
                logger.info(f"  Ensemble: {config.get('ensemble_enabled', False)}")
                
                ensemble_status = status.get('ensemble_status', {})
                logger.info(f"  Ensemble Models: {ensemble_status.get('model_count', 0)}")
            
            logger.info("\n✨ Demo completed successfully!")
            logger.info("The server-side optimization system is now ready for production use.")
            
        except Exception as e:
            logger.error(f"Demo failed: {e}")
            raise
        
        finally:
            # Cleanup
            if self.optimizer:
                await self.optimizer.stop_optimization()
                logger.info("🛑 Optimization system stopped")


async def main():
    """Main demo function."""
    demo = OptimizationDemo()
    await demo.run_complete_demo()


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        logger.info("\n🛑 Demo interrupted by user")
    except Exception as e:
        logger.error(f"Demo error: {e}")
        import traceback
        traceback.print_exc()