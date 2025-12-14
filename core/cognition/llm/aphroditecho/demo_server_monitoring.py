#!/usr/bin/env python3
"""
Demo script for comprehensive server-side monitoring system.
Phase 8.3.1 - Complete visibility into server performance and capacity

This script demonstrates the monitoring and autoscaling functionality
without requiring the full Aphrodite engine to be running.
"""

import asyncio
import json
import time
import threading
from datetime import datetime, timedelta
from typing import Dict, Any
import sys
import os

# Add the project root to path
sys.path.insert(0, os.path.dirname(__file__))

# Import monitoring components directly
try:
    import psutil
    PSUTIL_AVAILABLE = True
except ImportError:
    PSUTIL_AVAILABLE = False
    print("Warning: psutil not available, using mock data")


class MockRequest:
    """Mock request for demo purposes."""
    def __init__(self, method: str, path: str):
        self.method = method
        self.url = type('url', (), {'path': path})()


def demo_request_tracing():
    """Demonstrate request tracing functionality."""
    print("\n🔍 === REQUEST TRACING DEMO ===")
    
    # Import the monitoring components
    from aphrodite.endpoints.monitoring import RequestTracer
    
    tracer = RequestTracer(max_traces=100)
    
    # Simulate some requests
    print("Simulating request traces...")
    for i in range(5):
        mock_request = MockRequest("POST", f"/v1/chat/completions?req={i}")
        
        # Start trace
        trace_id = tracer.start_trace(mock_request)
        print(f"  Started trace {trace_id[:8]}... for {mock_request.method} {mock_request.url.path}")
        
        # Simulate processing time
        processing_time = 0.1 + (i * 0.05)  # 100-300ms
        time.sleep(processing_time)
        
        # Update with model info
        tracer.update_trace(trace_id, 
                          model="gpt-3.5-turbo",
                          tokens_processed=50 + i * 20)
        
        # End trace
        status = 200 if i < 4 else 500  # Last one is an error
        tracer.end_trace(trace_id, status)
        
        completed_trace = list(tracer.completed_traces)[-1]
        print(f"  Completed in {completed_trace.duration_ms:.1f}ms, "
              f"{completed_trace.tokens_per_second:.0f} tokens/sec, "
              f"status: {status}")
    
    print(f"\n📊 Total traces: {len(tracer.completed_traces)}")
    print(f"📊 Active traces: {len(tracer.active_traces)}")
    
    return tracer


def demo_resource_monitoring():
    """Demonstrate resource monitoring functionality."""
    print("\n💻 === RESOURCE MONITORING DEMO ===")
    
    if not PSUTIL_AVAILABLE:
        print("⚠️  psutil not available, skipping resource monitoring demo")
        return None
    
    from aphrodite.endpoints.monitoring import ResourceMonitor
    
    monitor = ResourceMonitor(collection_interval=0.5)
    
    print("Starting resource monitoring...")
    monitor.start_monitoring()
    
    # Let it collect data for a few seconds
    print("Collecting metrics for 3 seconds...")
    time.sleep(3)
    
    # Get current metrics
    current_metrics = monitor.get_current_metrics()
    if current_metrics:
        print(f"📊 Current System Metrics:")
        print(f"   CPU: {current_metrics.cpu_percent:.1f}% ({current_metrics.cpu_count} cores)")
        print(f"   Memory: {current_metrics.memory_percent:.1f}% "
              f"({current_metrics.memory_used // (1024**3)}GB / "
              f"{current_metrics.memory_total // (1024**3)}GB)")
        print(f"   Process CPU: {current_metrics.process_cpu_percent:.1f}%")
        print(f"   Process Memory: {current_metrics.process_memory_rss // (1024**2)}MB RSS")
        print(f"   Threads: {current_metrics.process_num_threads}")
    
    print(f"📊 Metrics history: {len(monitor.metrics_history)} data points")
    
    monitor.stop_monitoring()
    return monitor


def demo_performance_analysis(tracer, monitor):
    """Demonstrate performance analysis functionality."""
    print("\n📈 === PERFORMANCE ANALYSIS DEMO ===")
    
    from aphrodite.endpoints.monitoring import PerformanceAnalyzer
    
    analyzer = PerformanceAnalyzer(tracer, monitor)
    
    # Analyze performance over the last few minutes
    print("Analyzing performance metrics...")
    performance_metrics = analyzer.analyze_performance(window_minutes=5)
    
    print(f"📊 Performance Analysis Results:")
    print(f"   Requests/sec: {performance_metrics.requests_per_second:.2f}")
    print(f"   Avg response time: {performance_metrics.avg_response_time_ms:.1f}ms")
    print(f"   Error rate: {performance_metrics.error_rate:.1%}")
    print(f"   Tokens/sec: {performance_metrics.tokens_per_second:.0f}")
    print(f"   Concurrent requests: {performance_metrics.concurrent_requests}")
    
    if monitor and monitor.get_current_metrics():
        print(f"   CPU efficiency: {performance_metrics.cpu_efficiency:.1f} tokens/sec/core")
        print(f"   Memory efficiency: {performance_metrics.memory_efficiency:.1f} tokens/sec/GB")
    
    # Test capacity prediction
    print("\n🔮 Capacity Prediction:")
    target_rps = performance_metrics.requests_per_second * 2  # 2x current load
    if target_rps > 0:
        prediction = analyzer.predict_capacity_needs(target_rps, performance_metrics)
        print(f"   For {target_rps:.0f} RPS (2x current):")
        print(f"   Predicted CPU: {prediction.get('predicted_cpu_percent', 0):.1f}%")
        print(f"   Predicted Memory: {prediction.get('predicted_memory_percent', 0):.1f}%")
        print(f"   Recommended instances: {prediction.get('recommended_instances', 1)}")
        print(f"   Confidence: {prediction.get('confidence', 0):.1%}")
        
        bottlenecks = prediction.get('bottleneck_prediction', [])
        if bottlenecks:
            print(f"   ⚠️  Potential bottlenecks: {', '.join(bottlenecks)}")
    
    return analyzer


def demo_autoscaling():
    """Demonstrate autoscaling functionality."""
    print("\n⚡ === AUTOSCALING DEMO ===")
    
    from aphrodite.endpoints.autoscaling import LoadPredictor, AutoscalingEngine, CapacityPlanner
    
    # Create autoscaling components
    predictor = LoadPredictor()
    engine = AutoscalingEngine(predictor)
    planner = CapacityPlanner(predictor)
    
    print("Loading historical data for prediction...")
    # Add some historical load data
    base_time = time.time() - 900  # 15 minutes ago
    for i in range(15):
        rps = 50 + i * 2 + (i % 3) * 5  # Increasing with some variation
        cpu = 40 + i * 1.5
        memory = 50 + i * 0.8
        predictor.record_load(rps, cpu, memory)
    
    # Test load prediction
    print("\n🔮 Load Prediction:")
    prediction = predictor.predict_load(15)  # 15 minutes ahead
    print(f"   Predicted RPS in 15min: {prediction.predicted_rps:.1f}")
    print(f"   Confidence: {prediction.confidence:.1%}")
    print(f"   CPU cores needed: {prediction.cpu_requirement:.1f}")
    print(f"   Memory needed: {prediction.memory_requirement:.1f}GB")
    print(f"   Instances needed: {prediction.instances_required}")
    
    # Test scaling recommendations
    print("\n⚖️  Scaling Analysis:")
    current_metrics = {
        'cpu_percent': 85.0,  # High CPU
        'memory_percent': 70.0,
        'requests_per_second': 120.0,
        'avg_response_time_ms': 800.0,  # Slow responses
        'error_rate': 0.03,
        'availability_percent': 99.2,
        'instances': 3
    }
    
    recommendations = engine.analyze_and_recommend(current_metrics)
    
    if recommendations:
        print(f"   Found {len(recommendations)} scaling recommendations:")
        for i, rec in enumerate(recommendations):
            action_emoji = {"scale_up": "📈", "scale_down": "📉", "emergency_scale": "🚨"}.get(rec.action.value, "⚖️")
            print(f"   {i+1}. {action_emoji} {rec.action.value.upper()} {rec.resource_type.value}")
            print(f"      Reason: {rec.reason}")
            print(f"      Current: {rec.current_value:.1f} → Target: {rec.target_value:.1f}")
            print(f"      Urgency: {rec.urgency:.1%}, Confidence: {rec.confidence:.1%}")
            print(f"      Est. cost impact: ${rec.estimated_cost_impact:.0f}/hour")
    else:
        print("   ✅ No scaling actions needed - system is well-balanced")
    
    # Test capacity planning
    print("\n📊 Capacity Planning:")
    capacity_plan = planner.generate_capacity_plan(
        planning_horizon_days=30,
        growth_scenarios=[1.0, 1.5, 2.0, 3.0]
    )
    
    print(f"   Planning horizon: {capacity_plan['planning_horizon_days']} days")
    print(f"   Scenarios analyzed: {len(capacity_plan['scenarios'])}")
    
    for scenario in capacity_plan['scenarios']:
        growth_pct = scenario['growth_percentage']
        risk_level = scenario['risk_assessment']['level']
        cost = scenario['estimated_monthly_cost']
        instances = scenario['resource_requirements']['instances']
        
        risk_emoji = {"low": "🟢", "medium": "🟡", "high": "🔴"}.get(risk_level, "⚪")
        print(f"   {risk_emoji} {growth_pct:+.0f}% growth: {instances} instances, ${cost:,.0f}/month, {risk_level} risk")
    
    print(f"\n💡 Key Recommendations:")
    for rec in capacity_plan['recommendations'][:3]:  # Show first 3
        print(f"   • {rec}")


def demo_monitoring_dashboard():
    """Demonstrate monitoring dashboard functionality."""
    print("\n🖥️  === MONITORING DASHBOARD DEMO ===")
    
    # This would normally integrate with a running server
    print("In a real deployment, the monitoring dashboard would be available at:")
    print("   📊 Main Dashboard: http://localhost:2242/monitoring/")
    print("   📈 Metrics API: http://localhost:2242/monitoring/api/metrics")
    print("   🔍 Request Traces: http://localhost:2242/monitoring/api/traces")
    print("   ⚡ Autoscaling API: http://localhost:2242/autoscaling/recommendations")
    print("   🎯 Capacity Planning: http://localhost:2242/autoscaling/capacity-plan")
    
    print("\n🌐 Dashboard Features:")
    print("   • Real-time performance metrics")
    print("   • System resource utilization")
    print("   • Active request traces")
    print("   • Response time distributions")
    print("   • Auto-refresh every 30 seconds")
    print("   • Server-side rendered (no client JavaScript required)")


def main():
    """Run the comprehensive monitoring demo."""
    print("🚀 APHRODITE ENGINE - COMPREHENSIVE SERVER-SIDE MONITORING DEMO")
    print("=" * 65)
    print("Phase 8.3.1 - Complete visibility into server performance and capacity")
    print()
    
    global PSUTIL_AVAILABLE
    if not PSUTIL_AVAILABLE:
        print("⚠️  Warning: psutil not available. Installing...")
        os.system("pip install psutil")
        try:
            import psutil
            PSUTIL_AVAILABLE = True
            print("✅ psutil installed successfully")
        except ImportError:
            print("❌ Could not install psutil, continuing with limited functionality")
    
    # Run demos
    tracer = demo_request_tracing()
    monitor = demo_resource_monitoring() 
    analyzer = demo_performance_analysis(tracer, monitor) if tracer else None
    demo_autoscaling()
    demo_monitoring_dashboard()
    
    print("\n" + "=" * 65)
    print("🎉 MONITORING DEMO COMPLETE!")
    print()
    print("Key Benefits of the Implementation:")
    print("✅ End-to-end request tracing and profiling")
    print("✅ Real-time resource utilization monitoring")
    print("✅ Predictive autoscaling and capacity planning")
    print("✅ Server-side rendered monitoring dashboards") 
    print("✅ Comprehensive performance analysis")
    print("✅ Integration with existing Aphrodite Engine")
    print()
    print("The monitoring system provides complete visibility into:")
    print("📊 Server performance and capacity")
    print("🔍 Request-level tracing and profiling")
    print("💻 System resource utilization")
    print("⚡ Autoscaling recommendations")
    print("🎯 Long-term capacity planning")


if __name__ == "__main__":
    main()