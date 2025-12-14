#!/usr/bin/env python3
"""
A/B Testing Framework Demo for Aphrodite Engine
Phase 8 - SSR-Focused MLOps & Production Observability

Demonstrates the A/B testing framework capabilities including:
- Traffic splitting and performance comparison
- Automated rollback mechanisms
- Model variant management

This demo shows the framework functionality without requiring full Aphrodite setup.
"""
import asyncio
import json
import random
import time
from datetime import datetime, timezone
from typing import Dict, Any, List

# Mock classes to demonstrate functionality without full dependencies
class MockRequest:
    """Mock request for demonstration purposes"""
    def __init__(self, user_id: str, prompt: str):
        self.headers = {"x-session-id": user_id}
        self.client = type('Client', (), {'host': '127.0.0.1'})()
        self.url = type('URL', (), {'path': '/v1/chat/completions'})()
        self.prompt = prompt

class MockEngine:
    """Mock engine for demonstration purposes"""
    def __init__(self, name: str, base_latency: float, error_rate: float):
        self.name = name
        self.base_latency = base_latency
        self.error_rate = error_rate
    
    async def generate(self, prompt: str):
        """Simulate model generation with realistic performance characteristics"""
        await asyncio.sleep(0.01)  # Simulate processing time
        
        # Simulate realistic latency variation
        latency = self.base_latency + random.gauss(0, self.base_latency * 0.1)
        
        # Simulate errors based on error rate
        success = random.random() > self.error_rate
        
        return {
            'content': f'Response from {self.name}: {prompt[:50]}...' if success else None,
            'latency_ms': latency,
            'success': success,
            'model': self.name
        }

class ABTestingDemo:
    """Demonstration of A/B testing framework capabilities"""
    
    def __init__(self):
        print("🚀 Initializing A/B Testing Framework Demo")
        print("=" * 60)
        
        # Import the actual framework components
        try:
            import sys
            import os
            sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
            
            from aphrodite.endpoints.middleware.ab_testing_middleware import ABTestingManager
            from aphrodite.endpoints.ab_testing_monitor import ABTestMonitor, AlertLevel
            
            self.ab_manager = ABTestingManager()
            self.monitor = ABTestMonitor(self.ab_manager, check_interval_seconds=2)
            
            # Setup mock engines
            self.model_a = MockEngine("stable-model-v1", base_latency=120.0, error_rate=0.02)
            self.model_b = MockEngine("optimized-model-v2", base_latency=100.0, error_rate=0.015)
            
            self.request_count = 0
            self.alerts = []
            
            print("✅ Framework components loaded successfully")
            
        except ImportError as e:
            print(f"⚠️  Could not import framework components: {e}")
            print("📝 This demo shows the intended functionality")
            self.ab_manager = None
    
    async def run_demo(self):
        """Run the complete A/B testing demonstration"""
        print("\n🎯 Starting A/B Testing Demo")
        
        if self.ab_manager:
            await self._run_full_demo()
        else:
            await self._run_mock_demo()
    
    async def _run_full_demo(self):
        """Run demo with actual framework components"""
        
        # 1. Configure and start A/B test
        print("\n📋 Step 1: Starting A/B Test")
        print("-" * 40)
        
        test_id = await self.ab_manager.start_ab_test("stable-model-v1", "optimized-model-v2")
        print(f"✅ Started A/B test: {test_id}")
        print(f"📊 Traffic split: {self.ab_manager.config.traffic_split_percent}%")
        
        # 2. Setup monitoring with alert callback
        def alert_handler(alert):
            self.alerts.append(alert)
            print(f"🚨 {alert.level.value.upper()}: {alert.message}")
        
        self.monitor.alert_callback = alert_handler
        await self.monitor.start_monitoring()
        print("✅ Automated monitoring started")
        
        # 3. Simulate traffic and collect metrics
        print("\n🚦 Step 2: Simulating Traffic")
        print("-" * 40)
        
        await self._simulate_traffic(duration_seconds=30, requests_per_second=20)
        
        # 4. Show real-time metrics
        print("\n📈 Step 3: Current Test Status")
        print("-" * 40)
        
        status = self.ab_manager.get_test_status()
        self._display_test_status(status)
        
        # 5. Demonstrate rollback scenario
        print("\n⚠️  Step 4: Simulating Performance Degradation")
        print("-" * 40)
        
        # Temporarily worsen model B performance
        original_error_rate = self.model_b.error_rate
        self.model_b.error_rate = 0.15  # 15% error rate - should trigger rollback
        
        await self._simulate_traffic(duration_seconds=10, requests_per_second=30)
        
        # Wait for monitoring to detect the issue
        await asyncio.sleep(3)
        
        # 6. Stop monitoring and get final results
        await self.monitor.stop_monitoring()
        
        if self.ab_manager.active_test:
            result = await self.ab_manager.stop_ab_test("demo_complete")
            
            print("\n🏁 Step 5: Final Test Results")
            print("-" * 40)
            self._display_test_results(result)
        
        # 7. Show monitoring alerts
        print("\n🔔 Step 6: Monitoring Alerts")
        print("-" * 40)
        self._display_alerts()
        
        # Restore original performance
        self.model_b.error_rate = original_error_rate
    
    async def _run_mock_demo(self):
        """Run mock demo without framework dependencies"""
        print("📝 Running mock demonstration of A/B testing concepts...")
        
        # Simulate A/B test configuration
        config = {
            "model_a": "stable-model-v1",
            "model_b": "optimized-model-v2", 
            "traffic_split_percent": 10.0,
            "test_duration_minutes": 60,
            "auto_rollback": True
        }
        
        print(f"\n🔧 Configuration:")
        for key, value in config.items():
            print(f"   {key}: {value}")
        
        # Simulate traffic splitting and metrics collection
        print(f"\n🚦 Simulating traffic split ({config['traffic_split_percent']}% to variant B)...")
        
        metrics = {
            "variant_a": {"requests": 900, "errors": 18, "avg_latency": 125.3},
            "variant_b": {"requests": 100, "errors": 1, "avg_latency": 108.7}
        }
        
        print(f"\n📊 Collected Metrics:")
        for variant, data in metrics.items():
            error_rate = (data["errors"] / data["requests"]) * 100
            print(f"   {variant}: {data['requests']} requests, {error_rate:.1f}% errors, {data['avg_latency']:.1f}ms avg")
        
        # Simulate decision making
        variant_b_improvement = ((metrics["variant_a"]["avg_latency"] - metrics["variant_b"]["avg_latency"]) / 
                               metrics["variant_a"]["avg_latency"]) * 100
        
        decision = "promote_b" if variant_b_improvement > 5.0 else "keep_a"
        
        print(f"\n🎯 Test Decision: {decision}")
        print(f"   Reason: Variant B shows {variant_b_improvement:.1f}% latency improvement")
    
    async def _simulate_traffic(self, duration_seconds: int, requests_per_second: int):
        """Simulate realistic traffic patterns"""
        total_requests = duration_seconds * requests_per_second
        interval = 1.0 / requests_per_second
        
        print(f"📡 Generating {total_requests} requests over {duration_seconds}s...")
        
        for i in range(total_requests):
            self.request_count += 1
            
            # Create mock request
            request = MockRequest(f"user_{i % 100}", f"Test prompt {i}")
            
            # Determine which variant to use
            use_variant_b = self.ab_manager.should_use_variant_b(request)
            variant = "b" if use_variant_b else "a"
            engine = self.model_b if use_variant_b else self.model_a
            
            # Generate response
            start_time = time.time()
            result = await engine.generate(request.prompt)
            latency_ms = (time.time() - start_time) * 1000 + result['latency_ms']
            
            # Record metrics
            self.ab_manager.record_request_metrics(variant, latency_ms, result['success'])
            
            # Show progress
            if (i + 1) % (requests_per_second * 5) == 0:  # Every 5 seconds
                print(f"   📈 Processed {i + 1}/{total_requests} requests...")
            
            await asyncio.sleep(interval)
    
    def _display_test_status(self, status: Dict[str, Any]):
        """Display current test status in a readable format"""
        if not status:
            print("❌ No active test")
            return
        
        print(f"🔍 Test ID: {status['test_id']}")
        print(f"⏱️  Duration: {status['elapsed_minutes']:.1f} minutes")
        print(f"🎯 Traffic Split: {status['traffic_split_percent']}%")
        
        print(f"\n📊 Performance Metrics:")
        for variant_name, metrics in status['metrics'].items():
            print(f"   {variant_name.upper()}:")
            print(f"      Requests: {metrics['request_count']}")
            print(f"      Error Rate: {metrics['error_rate']:.2f}%")
            print(f"      Avg Latency: {metrics['avg_latency_ms']:.1f}ms")
            print(f"      Success Rate: {((metrics['request_count'] - metrics['error_count']) / max(metrics['request_count'], 1)) * 100:.1f}%")
    
    def _display_test_results(self, result):
        """Display final test results"""
        print(f"🏆 Test Result: {result.decision.upper()}")
        print(f"📝 Reason: {result.reason}")
        print(f"⏱️  Duration: {result.start_time} → {result.end_time}")
        
        print(f"\n📈 Final Metrics Comparison:")
        
        variants = [("Variant A (Stable)", result.metrics_a), ("Variant B (Canary)", result.metrics_b)]
        
        for name, metrics in variants:
            print(f"   {name}:")
            print(f"      Total Requests: {metrics.request_count}")
            print(f"      Error Rate: {metrics.error_rate:.2f}%")
            print(f"      Avg Latency: {metrics.avg_latency_ms:.1f}ms")
            print(f"      Successful Requests: {metrics.successful_requests}")
    
    def _display_alerts(self):
        """Display monitoring alerts"""
        if not self.alerts:
            print("✅ No alerts generated during test")
            return
        
        alert_counts = {}
        for alert in self.alerts:
            level = alert.level.value
            alert_counts[level] = alert_counts.get(level, 0) + 1
        
        print(f"📋 Alert Summary: {len(self.alerts)} total alerts")
        for level, count in alert_counts.items():
            print(f"   {level.upper()}: {count}")
        
        print(f"\n🕐 Recent Alerts:")
        for alert in self.alerts[-5:]:  # Show last 5 alerts
            print(f"   [{alert.level.value.upper()}] {alert.message}")
    
    def print_summary(self):
        """Print demo summary and next steps"""
        print("\n" + "=" * 60)
        print("🎉 A/B Testing Framework Demo Complete!")
        print("=" * 60)
        
        print("\n✨ Key Features Demonstrated:")
        print("   🔄 Intelligent traffic splitting between model variants")
        print("   📊 Real-time performance metrics collection")
        print("   🛡️ Automated rollback on performance degradation")
        print("   📈 Continuous monitoring with alerting")
        print("   🎯 Automated decision making based on success criteria")
        
        print("\n🚀 Next Steps:")
        print("   1. Deploy Aphrodite Engine with A/B testing enabled")
        print("   2. Configure your model variants and success criteria")
        print("   3. Start with conservative traffic splits (1-5%)")
        print("   4. Monitor metrics closely during initial tests")
        print("   5. Gradually increase confidence and traffic splits")
        
        print("\n📚 Documentation:")
        print("   • Framework Documentation: docs/AB_TESTING_FRAMEWORK.md")
        print("   • Configuration Guide: configs/ab_testing_config.yaml")
        print("   • API Reference: /v1/ab-testing/* endpoints")
        
        print("\n🔗 Integration Points:")
        print("   • DTESN Cache: Automatic optimization for winning variants")
        print("   • AAR Orchestration: Multi-agent coordination")
        print("   • Echo.Self: Automated model evolution")


async def main():
    """Main demo execution"""
    demo = ABTestingDemo()
    
    try:
        await demo.run_demo()
    except KeyboardInterrupt:
        print("\n⚠️ Demo interrupted by user")
    except Exception as e:
        print(f"\n❌ Demo error: {e}")
    finally:
        demo.print_summary()


if __name__ == "__main__":
    print("🧪 Aphrodite Engine A/B Testing Framework Demo")
    print("Phase 8 - SSR-Focused MLOps & Production Observability")
    print("\nPress Ctrl+C at any time to stop the demo\n")
    
    asyncio.run(main())