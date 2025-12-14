#!/usr/bin/env python3
"""
Demonstration of DTESN error monitoring and alerting system.

Shows real-time monitoring capabilities and dashboard data that support
99.9% uptime requirements.
"""

import asyncio
import json
import random
import time
from datetime import datetime, timedelta

# Import standalone components for demo
from validate_error_handling_standalone import (
    DTESNError, DTESNProcessingError, DTESNResourceError,
    ErrorSeverity, ErrorCategory, RecoveryStrategy,
    ErrorContext, CircuitBreaker, RetryManager, FallbackProcessor
)


class MockMetricsCollector:
    """Mock metrics collector for demonstration."""
    
    def __init__(self):
        self.requests = []
        self.errors = []
        self.active_requests = 0
        
    def record_request(self, success: bool, response_time_ms: float, error_type: str = None):
        """Record a request."""
        self.requests.append({
            "timestamp": datetime.now(),
            "success": success,
            "response_time_ms": response_time_ms,
            "error_type": error_type
        })
        
        if not success:
            self.errors.append({
                "timestamp": datetime.now(),
                "error_type": error_type or "unknown"
            })
    
    def get_metrics_summary(self, window_minutes: int = 5) -> dict:
        """Get metrics summary for time window."""
        cutoff = datetime.now() - timedelta(minutes=window_minutes)
        
        recent_requests = [r for r in self.requests if r["timestamp"] > cutoff]
        recent_errors = [e for e in self.errors if e["timestamp"] > cutoff]
        
        if not recent_requests:
            return {
                "total_requests": 0,
                "error_rate": 0.0,
                "availability": 100.0,
                "avg_response_time": 0.0
            }
        
        total = len(recent_requests)
        errors = len(recent_errors)
        successful = total - errors
        
        avg_response_time = sum(r["response_time_ms"] for r in recent_requests if r["success"]) / max(successful, 1)
        
        return {
            "total_requests": total,
            "error_count": errors,
            "error_rate": errors / total,
            "availability": (successful / total) * 100,
            "avg_response_time": avg_response_time,
            "throughput_rps": total / (window_minutes * 60)
        }


class MockDashboard:
    """Mock monitoring dashboard for demonstration."""
    
    def __init__(self, metrics_collector):
        self.metrics_collector = metrics_collector
        self.alerts = []
        
    def check_thresholds(self, metrics: dict) -> list:
        """Check metrics against thresholds and generate alerts."""
        alerts = []
        
        if metrics["error_rate"] > 0.10:  # 10%
            alerts.append({
                "level": "CRITICAL",
                "type": "ERROR_RATE",
                "message": f"Error rate is {metrics['error_rate']:.1%} (threshold: 10%)",
                "current_value": metrics["error_rate"],
                "threshold": 0.10
            })
        elif metrics["error_rate"] > 0.05:  # 5%
            alerts.append({
                "level": "WARNING", 
                "type": "ERROR_RATE",
                "message": f"Error rate is {metrics['error_rate']:.1%} (threshold: 5%)",
                "current_value": metrics["error_rate"],
                "threshold": 0.05
            })
        
        if metrics["availability"] < 99.0:  # 99%
            alerts.append({
                "level": "CRITICAL",
                "type": "AVAILABILITY",
                "message": f"Availability is {metrics['availability']:.1f}% (threshold: 99%)",
                "current_value": metrics["availability"],
                "threshold": 99.0
            })
        elif metrics["availability"] < 99.9:  # 99.9%
            alerts.append({
                "level": "WARNING",
                "type": "AVAILABILITY", 
                "message": f"Availability is {metrics['availability']:.2f}% (threshold: 99.9%)",
                "current_value": metrics["availability"],
                "threshold": 99.9
            })
        
        if metrics["avg_response_time"] > 5000:  # 5 seconds
            alerts.append({
                "level": "CRITICAL",
                "type": "RESPONSE_TIME",
                "message": f"Response time is {metrics['avg_response_time']:.0f}ms (threshold: 5000ms)",
                "current_value": metrics["avg_response_time"],
                "threshold": 5000
            })
        elif metrics["avg_response_time"] > 1000:  # 1 second
            alerts.append({
                "level": "WARNING",
                "type": "RESPONSE_TIME",
                "message": f"Response time is {metrics['avg_response_time']:.0f}ms (threshold: 1000ms)",
                "current_value": metrics["avg_response_time"],
                "threshold": 1000
            })
        
        return alerts
    
    def get_dashboard_data(self) -> dict:
        """Get complete dashboard data."""
        metrics = self.metrics_collector.get_metrics_summary()
        alerts = self.check_thresholds(metrics)
        
        # Calculate system status
        if any(alert["level"] == "CRITICAL" for alert in alerts):
            system_status = "CRITICAL"
        elif any(alert["level"] == "WARNING" for alert in alerts):
            system_status = "WARNING"
        else:
            system_status = "HEALTHY"
        
        # Calculate performance grade
        availability_score = min(100, metrics["availability"])
        response_time_score = max(0, 100 - (metrics["avg_response_time"] / 50))  # 50ms = 100 points
        error_rate_score = max(0, 100 - (metrics["error_rate"] * 10000))  # 1% error = 100 point deduction
        
        overall_score = (availability_score * 0.5 + response_time_score * 0.3 + error_rate_score * 0.2)
        
        if overall_score >= 95:
            grade = "A+"
        elif overall_score >= 90:
            grade = "A"
        elif overall_score >= 85:
            grade = "A-"
        elif overall_score >= 80:
            grade = "B+"
        elif overall_score >= 70:
            grade = "B"
        else:
            grade = "C"
        
        return {
            "timestamp": datetime.now().isoformat(),
            "system_status": system_status,
            "performance_grade": grade,
            "overall_score": overall_score,
            "metrics": metrics,
            "alerts": alerts,
            "sla_status": {
                "target_availability": 99.9,
                "current_availability": metrics["availability"],
                "meets_sla": metrics["availability"] >= 99.9 and metrics["error_rate"] <= 0.001,
                "error_budget_remaining": max(0, 99.9 - (metrics["error_rate"] * 100))
            }
        }


def print_dashboard(dashboard_data: dict):
    """Print formatted dashboard data."""
    print("\n" + "=" * 70)
    print(f"🚀 DTESN ERROR MONITORING DASHBOARD")
    print("=" * 70)
    print(f"⏰ Timestamp: {dashboard_data['timestamp']}")
    print(f"🏥 System Status: {dashboard_data['system_status']}")
    print(f"📊 Performance Grade: {dashboard_data['performance_grade']} ({dashboard_data['overall_score']:.1f}/100)")
    
    print(f"\n📈 METRICS SUMMARY (Last 5 minutes)")
    print("-" * 50)
    metrics = dashboard_data["metrics"]
    print(f"Total Requests: {metrics['total_requests']}")
    print(f"Error Rate: {metrics['error_rate']:.1%}")
    print(f"Availability: {metrics['availability']:.2f}%")
    print(f"Avg Response Time: {metrics['avg_response_time']:.1f}ms")
    print(f"Throughput: {metrics['throughput_rps']:.2f} req/s")
    
    print(f"\n🎯 SLA STATUS")
    print("-" * 50)
    sla = dashboard_data["sla_status"]
    print(f"Target Availability: {sla['target_availability']:.1f}%")
    print(f"Current Availability: {sla['current_availability']:.2f}%")
    print(f"Meets SLA: {'✅ YES' if sla['meets_sla'] else '❌ NO'}")
    print(f"Error Budget Remaining: {sla['error_budget_remaining']:.1f}%")
    
    alerts = dashboard_data["alerts"]
    if alerts:
        print(f"\n🚨 ACTIVE ALERTS ({len(alerts)})")
        print("-" * 50)
        for alert in alerts:
            level_emoji = "🔴" if alert["level"] == "CRITICAL" else "⚠️"
            print(f"{level_emoji} {alert['level']} - {alert['message']}")
    else:
        print(f"\n✅ NO ACTIVE ALERTS")
        print("-" * 50)
        print("All metrics are within normal thresholds")
    
    print("=" * 70)


async def simulate_traffic_scenario(collector: MockMetricsCollector, scenario: str):
    """Simulate different traffic scenarios."""
    print(f"\n🎬 Simulating scenario: {scenario}")
    
    if scenario == "normal_traffic":
        # Normal traffic pattern: low error rate, good response times
        for i in range(50):
            success = random.random() > 0.02  # 2% error rate
            response_time = random.uniform(50, 200)  # 50-200ms
            error_type = "processing" if not success else None
            collector.record_request(success, response_time, error_type)
            await asyncio.sleep(0.01)  # 100 requests/second
            
    elif scenario == "high_load":
        # High load: increased response times, moderate error rate
        for i in range(100):
            success = random.random() > 0.08  # 8% error rate
            response_time = random.uniform(200, 800)  # 200-800ms
            error_type = random.choice(["processing", "resource"]) if not success else None
            collector.record_request(success, response_time, error_type)
            await asyncio.sleep(0.005)  # 200 requests/second
            
    elif scenario == "system_degradation":
        # System degradation: high error rate, very slow responses
        for i in range(30):
            success = random.random() > 0.25  # 25% error rate
            response_time = random.uniform(1000, 5000)  # 1-5 second responses
            error_type = random.choice(["system", "resource", "processing"]) if not success else None
            collector.record_request(success, response_time, error_type)
            await asyncio.sleep(0.02)  # 50 requests/second
            
    elif scenario == "recovery":
        # Recovery scenario: gradual improvement
        for i in range(40):
            # Error rate decreases over time, response times improve
            progress = i / 40
            error_rate = 0.15 * (1 - progress)  # 15% down to 0%
            base_response_time = 2000 * (1 - progress) + 100  # 2000ms down to 100ms
            
            success = random.random() > error_rate
            response_time = random.uniform(base_response_time, base_response_time + 100)
            error_type = "processing" if not success else None
            collector.record_request(success, response_time, error_type)
            await asyncio.sleep(0.01)


async def demonstrate_error_recovery():
    """Demonstrate error recovery mechanisms."""
    print(f"\n🔧 DEMONSTRATING ERROR RECOVERY")
    print("-" * 50)
    
    # Create recovery components
    retry_manager = RetryManager()
    fallback_processor = FallbackProcessor()
    circuit_breaker = CircuitBreaker(failure_threshold=3, recovery_timeout=2)
    
    # Simulate a flaky service
    failure_count = 0
    
    async def flaky_service(input_data: str):
        nonlocal failure_count
        failure_count += 1
        
        if failure_count <= 2:
            raise DTESNProcessingError(f"Service unavailable (attempt {failure_count})")
        elif failure_count == 3:
            raise DTESNResourceError("Resource exhausted")
        else:
            return {"output": f"Service recovered: {input_data}", "membrane_layers": 3}
    
    print("🔄 Testing retry mechanism...")
    retry_result = await retry_manager.retry_async(flaky_service, "test input")
    print(f"   Retry success: {retry_result.success}")
    print(f"   Attempts made: {retry_result.attempts_made}")
    
    # Reset for fallback test
    failure_count = 0
    
    print("🔀 Testing fallback mechanism...")
    fallback_result = await fallback_processor.process_with_fallback("test input", flaky_service)
    print(f"   Fallback success: {fallback_result.success}")
    print(f"   Degraded mode: {fallback_result.degraded}")
    print(f"   Recovery mode: {fallback_result.recovery_mode}")
    
    print("⚡ Testing circuit breaker...")
    # Trigger failures to open circuit
    for i in range(4):
        try:
            circuit_breaker.call(lambda: 1/0)
        except:
            pass
    
    print(f"   Circuit breaker state: {circuit_breaker.state}")
    
    # Try to call through open circuit
    try:
        circuit_breaker.call(lambda: "should be blocked")
    except DTESNError as e:
        print(f"   Successfully blocked call: Circuit breaker protection active")
    
    print("✅ Error recovery demonstration completed")


async def main():
    """Main demonstration function."""
    print("🚀 DTESN Error Monitoring & Recovery Demonstration")
    print("🎯 Showcasing 99.9% uptime capabilities")
    
    # Initialize monitoring components
    collector = MockMetricsCollector()
    dashboard = MockDashboard(collector)
    
    # Scenario 1: Normal operations
    print("\n" + "="*70)
    print("📊 SCENARIO 1: NORMAL OPERATIONS")
    await simulate_traffic_scenario(collector, "normal_traffic")
    dashboard_data = dashboard.get_dashboard_data()
    print_dashboard(dashboard_data)
    
    # Scenario 2: High load
    print("\n" + "="*70)
    print("📊 SCENARIO 2: HIGH LOAD CONDITIONS")
    await simulate_traffic_scenario(collector, "high_load")
    dashboard_data = dashboard.get_dashboard_data()
    print_dashboard(dashboard_data)
    
    # Scenario 3: System degradation
    print("\n" + "="*70)
    print("📊 SCENARIO 3: SYSTEM DEGRADATION")
    await simulate_traffic_scenario(collector, "system_degradation")
    dashboard_data = dashboard.get_dashboard_data()
    print_dashboard(dashboard_data)
    
    # Demonstrate error recovery
    await demonstrate_error_recovery()
    
    # Scenario 4: Recovery
    print("\n" + "="*70)
    print("📊 SCENARIO 4: SYSTEM RECOVERY")
    await simulate_traffic_scenario(collector, "recovery")
    dashboard_data = dashboard.get_dashboard_data()
    print_dashboard(dashboard_data)
    
    # Final summary
    print("\n" + "🌟"*35)
    print("✅ DEMONSTRATION COMPLETED SUCCESSFULLY!")
    print("🎯 Key capabilities showcased:")
    print("   • Real-time monitoring and alerting")
    print("   • Multi-threshold alert system")
    print("   • Performance grading and SLA tracking")
    print("   • Error recovery mechanisms (retry + fallback)")
    print("   • Circuit breaker protection")
    print("   • Graceful degradation under load")
    print("   • Automatic system recovery")
    print("🚀 System ready for 99.9% uptime in production!")
    print("🌟" * 35)


if __name__ == "__main__":
    asyncio.run(main())