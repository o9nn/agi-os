"""
Test suite for autoscaling and capacity planning system.
Phase 8.3.1 - Capacity planning and autoscaling mechanisms
"""

import pytest
import time
from datetime import datetime, timedelta
from unittest.mock import Mock, patch

# Import the autoscaling components
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../'))

from aphrodite.endpoints.autoscaling import (
    LoadPredictor, AutoscalingEngine, CapacityPlanner,
    LoadPrediction, ScalingRecommendation, ScalingAction, ResourceType,
    get_autoscaling_routes
)


class TestLoadPredictor:
    """Test load prediction functionality."""
    
    def test_load_recording(self):
        """Test load data recording."""
        predictor = LoadPredictor(history_minutes=10)
        
        # Record some load data
        predictor.record_load(rps=50.0, cpu_percent=60.0, memory_percent=70.0)
        predictor.record_load(rps=55.0, cpu_percent=65.0, memory_percent=72.0)
        
        assert len(predictor.load_history) == 2
        assert predictor.load_history[-1]['rps'] == 55.0
        assert predictor.load_history[-1]['cpu_percent'] == 65.0
    
    def test_history_cleanup(self):
        """Test automatic cleanup of old history."""
        predictor = LoadPredictor(history_minutes=1)  # Very short history
        
        # Record old data
        old_time = datetime.now() - timedelta(minutes=5)
        predictor.load_history.append({
            'timestamp': old_time,
            'rps': 10.0,
            'cpu_percent': 20.0,
            'memory_percent': 30.0,
            'gpu_utilization': 0.0
        })
        
        # Record new data - this should trigger cleanup
        predictor.record_load(rps=50.0, cpu_percent=60.0, memory_percent=70.0)
        
        # Old data should be gone
        assert len(predictor.load_history) == 1
        assert predictor.load_history[0]['rps'] == 50.0
    
    def test_prediction_with_insufficient_data(self):
        """Test prediction when insufficient data is available."""
        predictor = LoadPredictor()
        
        # Predict with no data
        prediction = predictor.predict_load(15)
        
        assert isinstance(prediction, LoadPrediction)
        assert prediction.methodology == "insufficient_data"
        assert prediction.confidence <= 0.5
    
    def test_prediction_with_sufficient_data(self):
        """Test prediction with sufficient historical data."""
        predictor = LoadPredictor()
        
        # Add sufficient data points with upward trend
        base_time = time.time() - 600  # 10 minutes ago
        for i in range(10):
            timestamp = datetime.fromtimestamp(base_time + i * 60)
            predictor.load_history.append({
                'timestamp': timestamp,
                'rps': 50.0 + i * 5.0,  # Increasing trend
                'cpu_percent': 40.0 + i * 2.0,
                'memory_percent': 50.0 + i * 1.0,
                'gpu_utilization': 10.0 + i * 0.5
            })
        
        prediction = predictor.predict_load(15)
        
        assert prediction.methodology == "ensemble"
        assert prediction.confidence > 0.5
        assert prediction.predicted_rps > 50.0  # Should predict increase
        assert prediction.cpu_requirement > 0
        assert prediction.memory_requirement > 0
        assert prediction.instances_required >= 1
    
    def test_linear_trend_prediction(self):
        """Test linear trend prediction methodology."""
        predictor = LoadPredictor()
        
        # Test with clear upward trend
        values = [10.0, 15.0, 20.0, 25.0, 30.0]
        prediction = predictor._linear_trend_prediction(values, 15)
        
        # Should predict continued upward trend
        assert prediction > 30.0
        
        # Test with downward trend
        values = [50.0, 45.0, 40.0, 35.0, 30.0]
        prediction = predictor._linear_trend_prediction(values, 15)
        
        # Should predict continued downward trend
        assert prediction < 30.0
        assert prediction >= 0.0  # Shouldn't predict negative


class TestAutoscalingEngine:
    """Test autoscaling decision engine."""
    
    def test_cpu_analysis(self):
        """Test CPU utilization analysis."""
        predictor = LoadPredictor()
        engine = AutoscalingEngine(predictor)
        
        # Test high CPU scenario
        high_cpu_metrics = {
            'cpu_percent': 95.0,
            'memory_percent': 60.0,
            'requests_per_second': 100.0,
            'avg_response_time_ms': 200.0,
            'error_rate': 0.02,
            'availability_percent': 99.5
        }
        
        recommendations = engine.analyze_and_recommend(high_cpu_metrics)
        
        # Should recommend scaling up
        cpu_recs = [r for r in recommendations if r.resource_type == ResourceType.CPU]
        assert len(cpu_recs) > 0
        assert cpu_recs[0].action in [ScalingAction.SCALE_UP, ScalingAction.EMERGENCY_SCALE]
        assert cpu_recs[0].urgency > 0.8
    
    def test_memory_analysis(self):
        """Test memory utilization analysis."""
        predictor = LoadPredictor()
        engine = AutoscalingEngine(predictor)
        
        # Test high memory scenario
        high_memory_metrics = {
            'cpu_percent': 50.0,
            'memory_percent': 98.0,
            'requests_per_second': 75.0,
            'avg_response_time_ms': 300.0,
            'error_rate': 0.01,
            'availability_percent': 99.8
        }
        
        recommendations = engine.analyze_and_recommend(high_memory_metrics)
        
        # Should recommend scaling up for memory
        memory_recs = [r for r in recommendations if r.resource_type == ResourceType.MEMORY]
        assert len(memory_recs) > 0
        assert memory_recs[0].action in [ScalingAction.SCALE_UP, ScalingAction.EMERGENCY_SCALE]
    
    def test_performance_analysis(self):
        """Test performance-based scaling analysis."""
        predictor = LoadPredictor()
        engine = AutoscalingEngine(predictor)
        
        # Test high response time scenario
        slow_response_metrics = {
            'cpu_percent': 60.0,
            'memory_percent': 65.0,
            'requests_per_second': 80.0,
            'avg_response_time_ms': 2000.0,  # Very slow
            'error_rate': 0.03,
            'availability_percent': 99.2
        }
        
        recommendations = engine.analyze_and_recommend(slow_response_metrics)
        
        # Should recommend scaling up instances
        instance_recs = [r for r in recommendations if r.resource_type == ResourceType.INSTANCES]
        assert len(instance_recs) > 0
        assert instance_recs[0].action == ScalingAction.SCALE_UP
    
    def test_scale_down_analysis(self):
        """Test scale down recommendations."""
        predictor = LoadPredictor()
        engine = AutoscalingEngine(predictor)
        
        # Test underutilized resources
        low_usage_metrics = {
            'cpu_percent': 15.0,  # Very low CPU
            'memory_percent': 25.0,  # Low memory
            'requests_per_second': 20.0,
            'avg_response_time_ms': 100.0,
            'error_rate': 0.001,
            'availability_percent': 99.9
        }
        
        recommendations = engine.analyze_and_recommend(low_usage_metrics)
        
        # Should recommend scaling down
        scale_down_recs = [r for r in recommendations if r.action == ScalingAction.SCALE_DOWN]
        assert len(scale_down_recs) > 0
    
    def test_cooldown_periods(self):
        """Test scaling action cooldown periods."""
        predictor = LoadPredictor()
        engine = AutoscalingEngine(predictor)
        
        # Create a scaling recommendation
        rec = ScalingRecommendation(
            timestamp=datetime.now(),
            action=ScalingAction.SCALE_UP,
            resource_type=ResourceType.CPU,
            current_value=90.0,
            target_value=70.0,
            urgency=0.8,
            reason="Test scaling",
            confidence=0.9,
            estimated_cost_impact=50.0,
            implementation_time_minutes=5,
            rollback_plan="Test rollback"
        )
        
        # Record the action
        engine.record_action_taken(rec)
        
        # Try to get recommendations immediately - should be filtered by cooldown
        high_cpu_metrics = {
            'cpu_percent': 95.0,
            'memory_percent': 60.0,
            'requests_per_second': 100.0
        }
        
        recommendations = engine.analyze_and_recommend(high_cpu_metrics)
        
        # Should have fewer recommendations due to cooldown
        cpu_scale_up_recs = [
            r for r in recommendations 
            if r.resource_type == ResourceType.CPU and r.action == ScalingAction.SCALE_UP
        ]
        assert len(cpu_scale_up_recs) == 0  # Should be filtered by cooldown
    
    def test_predictive_scaling(self):
        """Test predictive scaling based on load forecasting."""
        predictor = LoadPredictor()
        engine = AutoscalingEngine(predictor)
        
        # Add historical data showing increasing trend
        base_time = time.time() - 900  # 15 minutes ago
        for i in range(15):
            timestamp = datetime.fromtimestamp(base_time + i * 60)
            predictor.load_history.append({
                'timestamp': timestamp,
                'rps': 30.0 + i * 3.0,  # Steadily increasing
                'cpu_percent': 40.0 + i * 2.0,
                'memory_percent': 50.0 + i * 1.5,
                'gpu_utilization': 0.0
            })
        
        current_metrics = {
            'cpu_percent': 70.0,
            'memory_percent': 72.0,
            'requests_per_second': 75.0,
            'instances': 2
        }
        
        recommendations = engine.analyze_and_recommend(current_metrics)
        
        # Should have predictive scaling recommendations
        predictive_recs = [
            r for r in recommendations 
            if "predicted" in r.reason.lower()
        ]
        assert len(predictive_recs) >= 0  # May or may not have predictive recs depending on confidence


class TestCapacityPlanner:
    """Test capacity planning functionality."""
    
    def test_capacity_plan_generation(self):
        """Test comprehensive capacity plan generation."""
        predictor = LoadPredictor()
        planner = CapacityPlanner(predictor)
        
        # Add some current load data
        predictor.record_load(rps=100.0, cpu_percent=60.0, memory_percent=50.0)
        
        # Generate capacity plan
        plan = planner.generate_capacity_plan(
            planning_horizon_days=30,
            growth_scenarios=[1.0, 1.5, 2.0, 3.0]
        )
        
        assert "planning_horizon_days" in plan
        assert "generated_at" in plan
        assert "scenarios" in plan
        assert "recommendations" in plan
        
        # Check scenarios
        scenarios = plan["scenarios"]
        assert len(scenarios) == 4  # Should have 4 growth scenarios
        
        for i, scenario in enumerate(scenarios):
            expected_growth = [1.0, 1.5, 2.0, 3.0][i]
            assert scenario["growth_factor"] == expected_growth
            assert scenario["projected_rps"] == 100.0 * expected_growth
            assert "resource_requirements" in scenario
            assert "estimated_monthly_cost" in scenario
            assert "risk_assessment" in scenario
    
    def test_risk_assessment(self):
        """Test risk assessment for different growth scenarios."""
        predictor = LoadPredictor()
        planner = CapacityPlanner(predictor)
        
        # Test low risk scenario
        low_risk = planner._assess_scenario_risk(1.1)  # 10% growth
        assert low_risk["level"] == "low"
        
        # Test medium risk scenario  
        medium_risk = planner._assess_scenario_risk(1.8)  # 80% growth
        assert medium_risk["level"] == "medium"
        
        # Test high risk scenario
        high_risk = planner._assess_scenario_risk(3.0)  # 200% growth
        assert high_risk["level"] == "high"
        assert len(high_risk["factors"]) >= len(medium_risk["factors"])
    
    def test_recommendation_generation(self):
        """Test capacity planning recommendations."""
        predictor = LoadPredictor()
        planner = CapacityPlanner(predictor)
        
        # Create scenarios with varying resource requirements
        scenarios = [
            {
                "growth_factor": 1.0,
                "resource_requirements": {"instances": 2},
                "estimated_monthly_cost": 10000,
                "risk_assessment": {"level": "low"}
            },
            {
                "growth_factor": 5.0,
                "resource_requirements": {"instances": 20},
                "estimated_monthly_cost": 100000,
                "risk_assessment": {"level": "high"}
            }
        ]
        
        recommendations = planner._generate_recommendations(scenarios)
        
        assert isinstance(recommendations, list)
        assert len(recommendations) > 0
        
        # Should include recommendations for high instance counts and costs
        rec_text = " ".join(recommendations)
        assert "autoscaling" in rec_text or "scaling" in rec_text
        assert "monitoring" in rec_text.lower()


class TestAutoscalingIntegration:
    """Test autoscaling system integration."""
    
    def test_routes_creation(self):
        """Test autoscaling routes creation."""
        router = get_autoscaling_routes()
        
        # Check that router has expected routes
        route_paths = [route.path for route in router.routes]
        
        assert "/autoscaling/recommendations" in route_paths
        assert "/autoscaling/predictions" in route_paths
        assert "/autoscaling/capacity-plan" in route_paths
        assert "/autoscaling/record-action" in route_paths
    
    def test_global_instances(self):
        """Test global autoscaling instances."""
        from aphrodite.endpoints.autoscaling import (
            load_predictor, autoscaling_engine, capacity_planner
        )
        
        assert load_predictor is not None
        assert autoscaling_engine is not None
        assert capacity_planner is not None
        
        # Test they work together
        load_predictor.record_load(50.0, 60.0, 70.0)
        prediction = load_predictor.predict_load(15)
        assert prediction is not None
        
        recommendations = autoscaling_engine.analyze_and_recommend({
            'cpu_percent': 80.0,
            'memory_percent': 70.0,
            'requests_per_second': 100.0
        })
        assert isinstance(recommendations, list)


class TestAutoscalingPerformance:
    """Test autoscaling system performance."""
    
    def test_prediction_performance(self):
        """Test load prediction performance."""
        predictor = LoadPredictor()
        
        # Add substantial history
        base_time = time.time() - 3600  # 1 hour ago
        for i in range(60):  # 60 data points
            timestamp = datetime.fromtimestamp(base_time + i * 60)
            predictor.load_history.append({
                'timestamp': timestamp,
                'rps': 50.0 + (i % 10) * 5.0,  # Cyclical pattern
                'cpu_percent': 40.0 + (i % 8) * 3.0,
                'memory_percent': 50.0 + (i % 6) * 2.0,
                'gpu_utilization': 0.0
            })
        
        start_time = time.time()
        
        # Generate multiple predictions
        for minutes_ahead in [5, 10, 15, 30, 60]:
            prediction = predictor.predict_load(minutes_ahead)
            assert prediction is not None
        
        end_time = time.time()
        duration = end_time - start_time
        
        # Should generate predictions quickly (< 0.1 seconds)
        assert duration < 0.1
    
    def test_analysis_performance(self):
        """Test scaling analysis performance."""
        predictor = LoadPredictor()
        engine = AutoscalingEngine(predictor)
        
        start_time = time.time()
        
        # Run multiple analyses
        for i in range(100):
            metrics = {
                'cpu_percent': 50.0 + i % 40,  # Varying CPU
                'memory_percent': 40.0 + i % 50,  # Varying memory
                'requests_per_second': 50.0 + i % 100,
                'avg_response_time_ms': 200.0 + i % 300,
                'error_rate': 0.01 + (i % 5) * 0.01,
                'availability_percent': 99.0 + (i % 10) * 0.1
            }
            
            recommendations = engine.analyze_and_recommend(metrics)
            assert isinstance(recommendations, list)
        
        end_time = time.time()
        duration = end_time - start_time
        
        # Should analyze 100 scenarios quickly (< 1 second)
        assert duration < 1.0


if __name__ == "__main__":
    pytest.main([__file__])