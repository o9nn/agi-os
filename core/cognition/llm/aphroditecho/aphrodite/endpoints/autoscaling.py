"""
Autoscaling and Capacity Planning for Aphrodite Engine
Phase 8.3.1 - Capacity planning and autoscaling mechanisms

Features:
- Real-time load analysis and prediction
- Horizontal scaling recommendations
- Resource allocation optimization
- Performance-based scaling decisions
- Integration with monitoring system
"""

import asyncio
import json
import logging
import time
from dataclasses import dataclass, asdict
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
from enum import Enum
import statistics
import threading

logger = logging.getLogger(__name__)


class ScalingAction(Enum):
    """Types of scaling actions that can be recommended."""
    SCALE_UP = "scale_up"
    SCALE_DOWN = "scale_down" 
    MAINTAIN = "maintain"
    EMERGENCY_SCALE = "emergency_scale"


class ResourceType(Enum):
    """Types of resources to consider for scaling."""
    CPU = "cpu"
    MEMORY = "memory"
    GPU = "gpu"
    NETWORK = "network"
    INSTANCES = "instances"


@dataclass
class LoadPrediction:
    """Load prediction for capacity planning."""
    timestamp: datetime
    predicted_rps: float
    confidence: float
    time_horizon_minutes: int
    methodology: str
    
    # Resource requirements prediction
    cpu_requirement: float  # CPU cores needed
    memory_requirement: float  # GB RAM needed
    gpu_requirement: float  # GPU memory needed
    instances_required: int  # Number of instances
    
    def to_dict(self) -> Dict[str, Any]:
        data = asdict(self)
        data['timestamp'] = self.timestamp.isoformat()
        return data


@dataclass
class ScalingRecommendation:
    """Scaling recommendation based on analysis."""
    timestamp: datetime
    action: ScalingAction
    resource_type: ResourceType
    current_value: float
    target_value: float
    urgency: float  # 0.0 to 1.0
    reason: str
    confidence: float
    
    # Implementation details
    estimated_cost_impact: float
    implementation_time_minutes: int
    rollback_plan: str
    
    def to_dict(self) -> Dict[str, Any]:
        data = asdict(self)
        data['timestamp'] = self.timestamp.isoformat()
        data['action'] = self.action.value
        data['resource_type'] = self.resource_type.value
        return data


class LoadPredictor:
    """Predicts future load based on historical patterns."""
    
    def __init__(self, history_minutes: int = 60):
        self.history_minutes = history_minutes
        self.load_history = []
        self.predictions_cache = {}
        self.cache_ttl = 60  # seconds
    
    def record_load(self, rps: float, cpu_percent: float, memory_percent: float, gpu_utilization: float = 0.0):
        """Record current load metrics."""
        load_point = {
            'timestamp': datetime.now(),
            'rps': rps,
            'cpu_percent': cpu_percent,
            'memory_percent': memory_percent,
            'gpu_utilization': gpu_utilization
        }
        
        # Keep only recent history
        cutoff_time = datetime.now() - timedelta(minutes=self.history_minutes)
        self.load_history = [
            point for point in self.load_history 
            if point['timestamp'] > cutoff_time
        ]
        
        self.load_history.append(load_point)
    
    def predict_load(self, minutes_ahead: int = 15) -> LoadPrediction:
        """Predict load for the specified time horizon."""
        cache_key = f"predict_{minutes_ahead}"
        
        # Check cache first
        if cache_key in self.predictions_cache:
            cached_prediction, cache_time = self.predictions_cache[cache_key]
            if time.time() - cache_time < self.cache_ttl:
                return cached_prediction
        
        if len(self.load_history) < 5:
            # Not enough data for prediction
            current_load = self.load_history[-1] if self.load_history else {
                'rps': 0.0, 'cpu_percent': 0.0, 'memory_percent': 0.0, 'gpu_utilization': 0.0
            }
            prediction = LoadPrediction(
                timestamp=datetime.now(),
                predicted_rps=current_load['rps'],
                confidence=0.3,
                time_horizon_minutes=minutes_ahead,
                methodology="insufficient_data",
                cpu_requirement=1.0,
                memory_requirement=2.0,
                gpu_requirement=0.0,
                instances_required=1
            )
        else:
            prediction = self._calculate_prediction(minutes_ahead)
        
        # Cache the prediction
        self.predictions_cache[cache_key] = (prediction, time.time())
        return prediction
    
    def _calculate_prediction(self, minutes_ahead: int) -> LoadPrediction:
        """Calculate load prediction using multiple methodologies."""
        recent_loads = self.load_history[-10:]  # Last 10 data points
        
        # Method 1: Linear trend analysis
        rps_values = [point['rps'] for point in recent_loads]
        trend_prediction = self._linear_trend_prediction(rps_values, minutes_ahead)
        
        # Method 2: Moving average
        avg_prediction = statistics.mean(rps_values)
        
        # Method 3: Seasonal pattern detection (simplified)
        seasonal_prediction = self._detect_seasonal_pattern(minutes_ahead)
        
        # Combine predictions with weighted average
        predicted_rps = (
            trend_prediction * 0.5 + 
            avg_prediction * 0.3 + 
            seasonal_prediction * 0.2
        )
        
        # Calculate confidence based on prediction variance
        predictions = [trend_prediction, avg_prediction, seasonal_prediction]
        variance = statistics.variance(predictions) if len(predictions) > 1 else 0
        confidence = max(0.1, min(0.95, 1.0 - (variance / max(predicted_rps, 1))))
        
        # Predict resource requirements
        cpu_requirement, memory_requirement, gpu_requirement, instances_required = self._predict_resources(predicted_rps)
        
        return LoadPrediction(
            timestamp=datetime.now(),
            predicted_rps=predicted_rps,
            confidence=confidence,
            time_horizon_minutes=minutes_ahead,
            methodology="ensemble",
            cpu_requirement=cpu_requirement,
            memory_requirement=memory_requirement,
            gpu_requirement=gpu_requirement,
            instances_required=instances_required
        )
    
    def _linear_trend_prediction(self, values: List[float], minutes_ahead: int) -> float:
        """Predict using linear trend extrapolation."""
        if len(values) < 2:
            return values[0] if values else 0.0
        
        # Simple linear regression
        n = len(values)
        x_values = list(range(n))
        
        sum_x = sum(x_values)
        sum_y = sum(values)
        sum_xy = sum(x * y for x, y in zip(x_values, values))
        sum_x2 = sum(x * x for x in x_values)
        
        # Calculate slope and intercept
        slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
        intercept = (sum_y - slope * sum_x) / n
        
        # Extrapolate to future point
        future_x = n + (minutes_ahead / 5)  # Assuming 5-minute intervals
        predicted_value = slope * future_x + intercept
        
        return max(0, predicted_value)  # Don't predict negative load
    
    def _detect_seasonal_pattern(self, minutes_ahead: int) -> float:
        """Detect seasonal patterns in load (simplified implementation)."""
        if len(self.load_history) < 12:  # Need at least 1 hour of data
            return self.load_history[-1]['rps'] if self.load_history else 0.0
        
        # Look for hourly patterns
        current_hour = datetime.now().hour
        hourly_loads = []
        
        for point in self.load_history:
            if point['timestamp'].hour == current_hour:
                hourly_loads.append(point['rps'])
        
        if hourly_loads:
            return statistics.mean(hourly_loads)
        else:
            return self.load_history[-1]['rps']
    
    def _predict_resources(self, predicted_rps: float) -> Tuple[float, float, float, int]:
        """Predict resource requirements for target RPS."""
        # Baseline resource requirements (can be configured based on benchmarks)
        cpu_per_rps = 0.05  # CPU cores per RPS
        memory_per_rps = 0.1  # GB RAM per RPS
        gpu_memory_per_rps = 0.2  # GB GPU memory per RPS
        
        cpu_requirement = predicted_rps * cpu_per_rps
        memory_requirement = predicted_rps * memory_per_rps
        gpu_requirement = predicted_rps * gpu_memory_per_rps
        
        # Calculate instances needed (assuming 8 CPU cores per instance)
        instances_required = max(1, int((cpu_requirement / 8) + 0.5))
        
        return cpu_requirement, memory_requirement, gpu_requirement, instances_required


class AutoscalingEngine:
    """Main autoscaling decision engine."""
    
    def __init__(self, predictor: LoadPredictor):
        self.predictor = predictor
        self.scaling_history = []
        self.cooldown_periods = {
            ScalingAction.SCALE_UP: timedelta(minutes=5),
            ScalingAction.SCALE_DOWN: timedelta(minutes=15),
            ScalingAction.EMERGENCY_SCALE: timedelta(minutes=1)
        }
        self.last_action_time = {}
    
    def analyze_and_recommend(
        self, 
        current_metrics: Dict[str, Any],
        target_performance: Dict[str, float] = None
    ) -> List[ScalingRecommendation]:
        """Analyze current state and recommend scaling actions."""
        if target_performance is None:
            target_performance = {
                'max_cpu_percent': 80.0,
                'max_memory_percent': 85.0,
                'max_response_time_ms': 1000.0,
                'min_availability_percent': 99.0
            }
        
        recommendations = []
        
        # Record current load for prediction
        self.predictor.record_load(
            current_metrics.get('requests_per_second', 0.0),
            current_metrics.get('cpu_percent', 0.0),
            current_metrics.get('memory_percent', 0.0),
            current_metrics.get('gpu_utilization', 0.0)
        )
        
        # Analyze current resource utilization
        cpu_recommendation = self._analyze_cpu_usage(current_metrics, target_performance)
        if cpu_recommendation:
            recommendations.append(cpu_recommendation)
        
        memory_recommendation = self._analyze_memory_usage(current_metrics, target_performance)
        if memory_recommendation:
            recommendations.append(memory_recommendation)
        
        # Analyze performance metrics
        performance_recommendation = self._analyze_performance(current_metrics, target_performance)
        if performance_recommendation:
            recommendations.append(performance_recommendation)
        
        # Predictive scaling based on load forecasting
        predictive_recommendation = self._predictive_scaling_analysis(current_metrics)
        if predictive_recommendation:
            recommendations.append(predictive_recommendation)
        
        # Filter recommendations based on cooldown periods
        recommendations = self._filter_by_cooldown(recommendations)
        
        return recommendations
    
    def _analyze_cpu_usage(self, metrics: Dict[str, Any], targets: Dict[str, float]) -> Optional[ScalingRecommendation]:
        """Analyze CPU usage and recommend scaling."""
        cpu_percent = metrics.get('cpu_percent', 0.0)
        max_cpu = targets['max_cpu_percent']
        
        if cpu_percent > max_cpu * 1.1:  # 10% over target
            return ScalingRecommendation(
                timestamp=datetime.now(),
                action=ScalingAction.EMERGENCY_SCALE if cpu_percent > 95 else ScalingAction.SCALE_UP,
                resource_type=ResourceType.CPU,
                current_value=cpu_percent,
                target_value=max_cpu * 0.8,  # Target 80% of max
                urgency=min(1.0, cpu_percent / 100.0),
                reason=f"CPU utilization {cpu_percent:.1f}% exceeds target {max_cpu:.1f}%",
                confidence=0.9,
                estimated_cost_impact=50.0,  # Estimated additional cost per hour
                implementation_time_minutes=5,
                rollback_plan="Scale down if utilization drops below 50%"
            )
        elif cpu_percent < max_cpu * 0.3:  # Much below target
            return ScalingRecommendation(
                timestamp=datetime.now(),
                action=ScalingAction.SCALE_DOWN,
                resource_type=ResourceType.CPU,
                current_value=cpu_percent,
                target_value=max_cpu * 0.6,  # Target 60% of max
                urgency=0.3,
                reason=f"CPU utilization {cpu_percent:.1f}% significantly below target, cost optimization opportunity",
                confidence=0.7,
                estimated_cost_impact=-25.0,  # Cost savings
                implementation_time_minutes=10,
                rollback_plan="Scale back up if utilization exceeds 70%"
            )
        
        return None
    
    def _analyze_memory_usage(self, metrics: Dict[str, Any], targets: Dict[str, float]) -> Optional[ScalingRecommendation]:
        """Analyze memory usage and recommend scaling."""
        memory_percent = metrics.get('memory_percent', 0.0)
        max_memory = targets['max_memory_percent']
        
        if memory_percent > max_memory:
            return ScalingRecommendation(
                timestamp=datetime.now(),
                action=ScalingAction.EMERGENCY_SCALE if memory_percent > 95 else ScalingAction.SCALE_UP,
                resource_type=ResourceType.MEMORY,
                current_value=memory_percent,
                target_value=max_memory * 0.8,
                urgency=min(1.0, memory_percent / 100.0),
                reason=f"Memory utilization {memory_percent:.1f}% exceeds target {max_memory:.1f}%",
                confidence=0.95,
                estimated_cost_impact=40.0,
                implementation_time_minutes=3,
                rollback_plan="Scale down if memory usage drops below 60%"
            )
        
        return None
    
    def _analyze_performance(self, metrics: Dict[str, Any], targets: Dict[str, float]) -> Optional[ScalingRecommendation]:
        """Analyze performance metrics and recommend scaling."""
        response_time = metrics.get('avg_response_time_ms', 0.0)
        error_rate = metrics.get('error_rate', 0.0)
        availability = metrics.get('availability_percent', 100.0)
        
        max_response_time = targets['max_response_time_ms']
        min_availability = targets['min_availability_percent']
        
        # Check response time
        if response_time > max_response_time:
            urgency = min(1.0, response_time / (max_response_time * 2))
            return ScalingRecommendation(
                timestamp=datetime.now(),
                action=ScalingAction.SCALE_UP,
                resource_type=ResourceType.INSTANCES,
                current_value=response_time,
                target_value=max_response_time * 0.8,
                urgency=urgency,
                reason=f"Response time {response_time:.0f}ms exceeds target {max_response_time:.0f}ms",
                confidence=0.8,
                estimated_cost_impact=60.0,
                implementation_time_minutes=8,
                rollback_plan="Scale down if response time improves and stays below target"
            )
        
        # Check availability
        if availability < min_availability:
            return ScalingRecommendation(
                timestamp=datetime.now(),
                action=ScalingAction.EMERGENCY_SCALE,
                resource_type=ResourceType.INSTANCES,
                current_value=availability,
                target_value=min_availability + 1.0,
                urgency=1.0,
                reason=f"Availability {availability:.2f}% below target {min_availability:.1f}%",
                confidence=0.9,
                estimated_cost_impact=100.0,
                implementation_time_minutes=2,
                rollback_plan="Scale down gradually once availability stabilizes"
            )
        
        return None
    
    def _predictive_scaling_analysis(self, current_metrics: Dict[str, Any]) -> Optional[ScalingRecommendation]:
        """Perform predictive scaling based on load forecasting."""
        prediction = self.predictor.predict_load(15)  # 15 minutes ahead
        
        current_instances = current_metrics.get('instances', 1)
        
        if prediction.instances_required > current_instances and prediction.confidence > 0.6:
            return ScalingRecommendation(
                timestamp=datetime.now(),
                action=ScalingAction.SCALE_UP,
                resource_type=ResourceType.INSTANCES,
                current_value=current_instances,
                target_value=prediction.instances_required,
                urgency=0.6,
                reason=f"Predicted load increase requires {prediction.instances_required} instances (confidence: {prediction.confidence:.1%})",
                confidence=prediction.confidence,
                estimated_cost_impact=30.0 * (prediction.instances_required - current_instances),
                implementation_time_minutes=10,
                rollback_plan="Scale down if predicted load does not materialize"
            )
        elif prediction.instances_required < current_instances * 0.7 and prediction.confidence > 0.7:
            return ScalingRecommendation(
                timestamp=datetime.now(),
                action=ScalingAction.SCALE_DOWN,
                resource_type=ResourceType.INSTANCES,
                current_value=current_instances,
                target_value=prediction.instances_required,
                urgency=0.4,
                reason=f"Predicted load decrease allows scaling down to {prediction.instances_required} instances",
                confidence=prediction.confidence,
                estimated_cost_impact=-20.0 * (current_instances - prediction.instances_required),
                implementation_time_minutes=15,
                rollback_plan="Scale back up if load increases beyond prediction"
            )
        
        return None
    
    def _filter_by_cooldown(self, recommendations: List[ScalingRecommendation]) -> List[ScalingRecommendation]:
        """Filter recommendations based on cooldown periods."""
        filtered = []
        now = datetime.now()
        
        for rec in recommendations:
            action_key = f"{rec.action.value}_{rec.resource_type.value}"
            
            if action_key in self.last_action_time:
                last_time = self.last_action_time[action_key]
                cooldown = self.cooldown_periods.get(rec.action, timedelta(minutes=5))
                
                if now - last_time < cooldown:
                    continue  # Still in cooldown period
            
            filtered.append(rec)
        
        return filtered
    
    def record_action_taken(self, recommendation: ScalingRecommendation):
        """Record that a scaling action was taken."""
        action_key = f"{recommendation.action.value}_{recommendation.resource_type.value}"
        self.last_action_time[action_key] = datetime.now()
        self.scaling_history.append(recommendation)
        
        # Keep only last 100 actions in history
        if len(self.scaling_history) > 100:
            self.scaling_history = self.scaling_history[-100:]


class CapacityPlanner:
    """Long-term capacity planning system."""
    
    def __init__(self, predictor: LoadPredictor):
        self.predictor = predictor
        
    def generate_capacity_plan(
        self, 
        planning_horizon_days: int = 30,
        growth_scenarios: List[float] = None
    ) -> Dict[str, Any]:
        """Generate comprehensive capacity plan."""
        if growth_scenarios is None:
            growth_scenarios = [1.0, 1.5, 2.0, 3.0]  # 0%, 50%, 100%, 200% growth
        
        plan = {
            "planning_horizon_days": planning_horizon_days,
            "generated_at": datetime.now().isoformat(),
            "scenarios": []
        }
        
        current_load = self.predictor.load_history[-1] if self.predictor.load_history else {
            'rps': 10.0, 'cpu_percent': 30.0, 'memory_percent': 40.0
        }
        
        for growth_factor in growth_scenarios:
            scenario = self._generate_scenario(
                current_load, 
                growth_factor, 
                planning_horizon_days
            )
            plan["scenarios"].append(scenario)
        
        plan["recommendations"] = self._generate_recommendations(plan["scenarios"])
        
        return plan
    
    def _generate_scenario(self, current_load: Dict, growth_factor: float, days: int) -> Dict[str, Any]:
        """Generate capacity scenario for given growth factor."""
        projected_rps = current_load['rps'] * growth_factor
        
        # Predict resource requirements
        cpu_req, memory_req, gpu_req, instances_req = self.predictor._predict_resources(projected_rps)
        
        return {
            "growth_factor": growth_factor,
            "growth_percentage": (growth_factor - 1.0) * 100,
            "projected_rps": projected_rps,
            "resource_requirements": {
                "cpu_cores": cpu_req,
                "memory_gb": memory_req,
                "gpu_memory_gb": gpu_req,
                "instances": instances_req
            },
            "estimated_monthly_cost": instances_req * 100 * 24 * 30,  # $100/instance/hour
            "risk_assessment": self._assess_scenario_risk(growth_factor),
            "implementation_timeline_weeks": max(2, int(growth_factor * 4))
        }
    
    def _assess_scenario_risk(self, growth_factor: float) -> Dict[str, Any]:
        """Assess risks associated with growth scenario."""
        if growth_factor <= 1.2:
            risk_level = "low"
            risks = ["Minor resource constraints possible"]
        elif growth_factor <= 2.0:
            risk_level = "medium" 
            risks = ["Significant scaling required", "Potential performance degradation during scaling"]
        else:
            risk_level = "high"
            risks = [
                "Major infrastructure changes required",
                "High probability of service disruption during scaling",
                "Significant cost implications"
            ]
        
        return {
            "level": risk_level,
            "factors": risks,
            "mitigation_strategies": self._get_mitigation_strategies(risk_level)
        }
    
    def _get_mitigation_strategies(self, risk_level: str) -> List[str]:
        """Get mitigation strategies for risk level."""
        strategies = {
            "low": [
                "Monitor resource utilization closely",
                "Prepare scaling procedures"
            ],
            "medium": [
                "Implement gradual scaling approach",
                "Set up automated scaling triggers",
                "Prepare rollback procedures"
            ],
            "high": [
                "Develop comprehensive scaling plan",
                "Implement blue-green deployment strategy",
                "Consider multi-region deployment",
                "Establish dedicated scaling team"
            ]
        }
        return strategies.get(risk_level, [])
    
    def _generate_recommendations(self, scenarios: List[Dict]) -> List[str]:
        """Generate actionable recommendations based on scenarios."""
        recommendations = []
        
        # Analyze all scenarios
        max_instances = max(s["resource_requirements"]["instances"] for s in scenarios)
        max_cost = max(s["estimated_monthly_cost"] for s in scenarios)
        
        if max_instances > 10:
            recommendations.append(
                "Consider implementing horizontal pod autoscaling for instance counts > 10"
            )
        
        if max_cost > 50000:  # $50k/month
            recommendations.append(
                "High cost scenarios detected - evaluate reserved instance pricing"
            )
        
        # Check for high-risk scenarios
        high_risk_scenarios = [s for s in scenarios if s["risk_assessment"]["level"] == "high"]
        if high_risk_scenarios:
            recommendations.append(
                "High-risk growth scenarios identified - develop contingency plans"
            )
        
        recommendations.extend([
            "Implement comprehensive monitoring and alerting",
            "Regular capacity planning reviews (monthly)",
            "Establish scaling playbooks and procedures"
        ])
        
        return recommendations


# Global instances for integration with monitoring system
load_predictor = LoadPredictor()
autoscaling_engine = AutoscalingEngine(load_predictor)
capacity_planner = CapacityPlanner(load_predictor)


def get_autoscaling_routes():
    """Get autoscaling API routes for FastAPI integration."""
    from fastapi import APIRouter
    
    router = APIRouter(prefix="/autoscaling", tags=["autoscaling"])
    
    @router.get("/recommendations")
    async def get_scaling_recommendations(
        current_cpu: float = 50.0,
        current_memory: float = 60.0,
        current_rps: float = 100.0,
        current_response_time: float = 200.0
    ):
        """Get current scaling recommendations."""
        metrics = {
            'cpu_percent': current_cpu,
            'memory_percent': current_memory,
            'requests_per_second': current_rps,
            'avg_response_time_ms': current_response_time,
            'error_rate': 0.02,
            'availability_percent': 99.5,
            'instances': 3
        }
        
        recommendations = autoscaling_engine.analyze_and_recommend(metrics)
        return {
            "recommendations": [rec.to_dict() for rec in recommendations],
            "timestamp": datetime.now().isoformat(),
            "metrics_analyzed": metrics
        }
    
    @router.get("/predictions")
    async def get_load_predictions(minutes_ahead: int = 15):
        """Get load predictions for capacity planning."""
        prediction = load_predictor.predict_load(minutes_ahead)
        return {
            "prediction": prediction.to_dict(),
            "data_points_used": len(load_predictor.load_history),
            "prediction_quality": "good" if prediction.confidence > 0.7 else "moderate" if prediction.confidence > 0.4 else "poor"
        }
    
    @router.get("/capacity-plan")
    async def get_capacity_plan(
        days_ahead: int = 30,
        growth_scenarios: str = "1.0,1.5,2.0,3.0"
    ):
        """Get comprehensive capacity planning analysis."""
        scenarios = [float(x.strip()) for x in growth_scenarios.split(',')]
        plan = capacity_planner.generate_capacity_plan(days_ahead, scenarios)
        return plan
    
    @router.post("/record-action")
    async def record_scaling_action(action_data: dict):
        """Record that a scaling action was taken."""
        # Convert action_data to ScalingRecommendation object
        rec = ScalingRecommendation(
            timestamp=datetime.fromisoformat(action_data['timestamp']),
            action=ScalingAction(action_data['action']),
            resource_type=ResourceType(action_data['resource_type']),
            current_value=action_data['current_value'],
            target_value=action_data['target_value'],
            urgency=action_data['urgency'],
            reason=action_data['reason'],
            confidence=action_data['confidence'],
            estimated_cost_impact=action_data['estimated_cost_impact'],
            implementation_time_minutes=action_data['implementation_time_minutes'],
            rollback_plan=action_data['rollback_plan']
        )
        
        autoscaling_engine.record_action_taken(rec)
        return {"status": "recorded", "timestamp": datetime.now().isoformat()}
    
    return router