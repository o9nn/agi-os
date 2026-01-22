import time
import sys
import os
import json
sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'echo.kern'))
sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'echo.dash'))
try:
    from phase_3_3_3_self_monitoring import create_self_monitoring_system, MonitoringLevel, PerformanceMetrics, BodyStateMetrics, DTESNSelfMonitoringIntegration
    from cognitive_architecture import CognitiveArchitecture
    DEMO_READY = True
except ImportError as e:
    print(f'Demo dependencies not available: {e}')
    DEMO_READY = False
def demo_basic_self_monitoring():
    print('=' * 60)
    print('Phase 3.3.3 Self-Monitoring System Demo')
    print('=' * 60)
    agent_id = 'demo_agent_001'
    monitor = create_self_monitoring_system(agent_id=agent_id, monitoring_level=MonitoringLevel.COMPREHENSIVE)
    print(f'\n1. Created self-monitoring system for agent: {agent_id}')
    monitor.start_monitoring(monitoring_interval=0.5)
    print('2. Started continuous monitoring (0.5s intervals)')
    try:
        print('\n3. Collecting baseline performance data...')
        time.sleep(3)
        status = monitor.get_monitoring_status()
        print(f"   - Performance metrics collected: {status['performance_metrics_count']}")
        print(f"   - Body state metrics collected: {status['body_state_metrics_count']}")
        print(f"   - Errors detected: {status['detected_errors_count']}")
        print('\n4. Simulating performance degradation...')
        poor_metrics = [PerformanceMetrics(accuracy_score=0.9, response_time_ms=100), PerformanceMetrics(accuracy_score=0.8, response_time_ms=200), PerformanceMetrics(accuracy_score=0.7, response_time_ms=300), PerformanceMetrics(accuracy_score=0.6, response_time_ms=400), PerformanceMetrics(accuracy_score=0.5, response_time_ms=500)]
        for metrics in poor_metrics:
            monitor.performance_history.append(metrics)
        time.sleep(2)
        monitor._detect_errors()
        final_status = monitor.get_monitoring_status()
        print(f"   - Errors detected after simulation: {final_status['detected_errors_count']}")
        if monitor.detected_errors:
            for error in monitor.detected_errors:
                print(f'   - Error: {error.error_type} - {error.description}')
                if error.correction_applied:
                    print(f'     Correction applied: {error.correction_applied}')
        if monitor.error_patterns:
            print('\n5. Error patterns learned:')
            for error_type, count in monitor.error_patterns.items():
                print(f'   - {error_type}: {count} occurrences')
        output_file = '/tmp/demo_monitoring_data.json'
        monitor.export_monitoring_data(output_file)
        print(f'\n6. Monitoring data exported to: {output_file}')
        if os.path.exists(output_file):
            with open(output_file, 'r') as f:
                data = json.load(f)
            print(f"   - Export contains {len(data['performance_history'])} performance records")
            print(f"   - Export contains {len(data['detected_errors'])} error records")
    finally:
        monitor.stop_monitoring()
        print('\n7. Monitoring stopped')
    return monitor
def demo_cognitive_architecture_integration():
    print('\n' + '=' * 60)
    print('Cognitive Architecture Integration Demo')
    print('=' * 60)
    try:
        print('\n1. Creating cognitive architecture with integrated self-monitoring...')
        ca = CognitiveArchitecture()
        time.sleep(1)
        print(f'2. Self-monitoring available: {ca.has_self_monitoring()}')
        if ca.has_self_monitoring():
            status = ca.get_self_monitoring_status()
            print('3. Monitoring status:')
            print(f"   - Agent ID: {status.get('agent_id', 'unknown')}")
            print(f"   - Monitoring level: {status.get('monitoring_level', 'unknown')}")
            print(f"   - Performance metrics: {status.get('performance_metrics_count', 0)}")
            print('\n4. Simulating cognitive operations...')
            operations = [('memory_retrieval', 50, 0.95, True), ('goal_generation', 120, 0.88, True), ('decision_making', 200, 0.92, True), ('learning_update', 80, 0.85, False)]
            for op, time_ms, accuracy, success in operations:
                ca.record_cognitive_performance(op, time_ms, accuracy, success)
                print(f"   - {op}: {time_ms}ms, {accuracy:.2f} accuracy, {('success' if success else 'failed')}")
                time.sleep(0.5)
            print('\n5. Performance recommendations:')
            recommendations = ca.get_performance_recommendations()
            for i, rec in enumerate(recommendations[:3], 1):
                print(f'   {i}. {rec}')
            output_file = '/tmp/cognitive_monitoring_data.json'
            if ca.export_self_monitoring_data(output_file):
                print(f'\n6. Enhanced cognitive monitoring data exported to: {output_file}')
                with open(output_file, 'r') as f:
                    data = json.load(f)
                if 'cognitive_context' in data:
                    context = data['cognitive_context']
                    print(f"   - Memories: {context.get('memories_count', 0)}")
                    print(f"   - Goals: {context.get('goals_count', 0)}")
                    print(f"   - Activities: {context.get('activities_count', 0)}")
        ca.cleanup_self_monitoring()
        print('\n7. Cognitive architecture monitoring cleaned up')
    except Exception as e:
        print(f'Error in cognitive architecture demo: {e}')
def demo_multi_agent_monitoring():
    print('\n' + '=' * 60)
    print('Multi-Agent Monitoring with DTESN Integration')
    print('=' * 60)
    dtesn_integration = DTESNSelfMonitoringIntegration()
    agents = ['agent_alpha', 'agent_beta', 'agent_gamma']
    monitors = {}
    print('\n1. Creating monitoring for multiple agents...')
    for agent_id in agents:
        monitor = create_self_monitoring_system(agent_id, MonitoringLevel.DETAILED)
        monitors[agent_id] = monitor
        dtesn_integration.register_agent_monitor(agent_id, monitor)
        monitor.start_monitoring(monitoring_interval=0.3)
        print(f'   - Started monitoring for {agent_id}')
    try:
        print('\n2. Collecting multi-agent performance data...')
        time.sleep(4)
        system_status = dtesn_integration.get_system_wide_status()
        print('\n3. System-wide monitoring status:')
        for agent_id, status in system_status.items():
            print(f'   - {agent_id}:')
            print(f"     Performance metrics: {status.get('performance_metrics_count', 0)}")
            print(f"     Errors detected: {status.get('detected_errors_count', 0)}")
            if 'recent_performance' in status:
                perf = status['recent_performance']
                print(f"     Avg response time: {perf.get('avg_response_time_ms', 0):.1f}ms")
                print(f"     Avg accuracy: {perf.get('avg_accuracy_score', 0):.3f}")
        print('\n4. Simulating agent performance variations...')
        for agent_id, monitor in monitors.items():
            if agent_id == 'agent_alpha':
                for _ in range(3):
                    monitor.performance_history.append(PerformanceMetrics(accuracy_score=0.95, response_time_ms=80))
            elif agent_id == 'agent_beta':
                declining = [0.9, 0.8, 0.7, 0.6, 0.5]
                for acc in declining:
                    monitor.performance_history.append(PerformanceMetrics(accuracy_score=acc, response_time_ms=100 + (0.9 - acc) * 400))
        time.sleep(2)
        final_status = dtesn_integration.get_system_wide_status()
        print('\n5. Final system status after performance simulation:')
        total_errors = sum((status.get('detected_errors_count', 0) for status in final_status.values()))
        print(f'   - Total errors detected across all agents: {total_errors}')
        for agent_id, status in final_status.items():
            if status.get('detected_errors_count', 0) > 0:
                print(f"   - {agent_id}: {status['detected_errors_count']} errors detected")
    finally:
        for agent_id, monitor in monitors.items():
            monitor.stop_monitoring()
        print('\n6. All agent monitoring stopped')
def run_full_demo():
    if not DEMO_READY:
        print('Demo cannot run - missing dependencies')
        return
    print('Phase 3.3.3: Self-Monitoring Systems Demo')
    print('Deep Tree Echo - Sensory-Motor Integration')
    demo_basic_self_monitoring()
    demo_cognitive_architecture_integration()
    demo_multi_agent_monitoring()
    print('\n' + '=' * 60)
    print('Demo Summary')
    print('=' * 60)
    print('\nThe Phase 3.3.3 self-monitoring system demonstrates:')
    print('✓ Performance self-assessment with real-time metrics')
    print('✓ Error detection and automatic correction')
    print('✓ Metacognitive awareness of body state')
    print('✓ Integration with cognitive architecture')
    print('✓ Multi-agent monitoring capabilities')
    print('✓ DTESN integration support')
    print('✓ Data export for analysis and learning')
    print('\nKey Features Implemented:')
    print('- Real-time performance monitoring')
    print('- Automatic error detection and correction')
    print('- Learning and adaptation from errors')
    print('- Baseline updating based on experience')
    print('- Performance recommendations')
    print('- Body state awareness for embodied agents')
    print('- Integration with existing Deep Tree Echo components')
    print('\nSelf-monitoring system is ready for Phase 3 integration!')
if __name__ == '__main__':
    run_full_demo()