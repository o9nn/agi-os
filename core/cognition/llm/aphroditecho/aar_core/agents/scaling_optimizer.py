#!/usr/bin/env python3
"""
Scaling Optimizer for Agent Manager

Enhances the existing Agent Manager with advanced scaling capabilities,
cost optimization, and performance-driven scaling decisions.

This module extends the existing agent management with:
- Predictive scaling based on historical patterns
- Cost-effective resource allocation
- Performance-driven scaling thresholds
- Integration with the central ScalabilityManager
"""

import time
import logging
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass, field
from enum import Enum
import statistics
from collections import deque

logger = logging.getLogger(__name__)


class ScalingTrigger(Enum):
    """Triggers for scaling decisions"""
    UTILIZATION_HIGH = "utilization_high"
    UTILIZATION_LOW = "utilization_low"
    RESPONSE_TIME_HIGH = "response_time_high"
    ERROR_RATE_HIGH = "error_rate_high"
    QUEUE_LENGTH_HIGH = "queue_length_high"
    COST_OPTIMIZATION = "cost_optimization"
    PREDICTIVE = "predictive"
    MANUAL = "manual"


class PredictionModel(Enum):
    """Prediction models for scaling"""
    LINEAR_REGRESSION = "linear_regression"
    MOVING_AVERAGE = "moving_average"
    EXPONENTIAL_SMOOTHING = "exponential_smoothing"
    SEASONAL_DECOMPOSITION = "seasonal_decomposition"


@dataclass
class ScalingMetrics:
    """Metrics for scaling decisions"""
    timestamp: float
    agent_count: int
    utilization: float
    avg_response_time_ms: float
    error_rate: float
    queue_length: int
    throughput: float
    cost_per_hour: float
    efficiency_score: float
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            'timestamp': self.timestamp,
            'agent_count': self.agent_count,
            'utilization': self.utilization,
            'avg_response_time_ms': self.avg_response_time_ms,
            'error_rate': self.error_rate,
            'queue_length': self.queue_length,
            'throughput': self.throughput,
            'cost_per_hour': self.cost_per_hour,
            'efficiency_score': self.efficiency_score
        }


@dataclass
class ScalingPrediction:
    """Scaling prediction result"""
    predicted_demand: float
    recommended_agents: int
    confidence: float
    time_horizon_minutes: int
    model_used: PredictionModel
    factors: Dict[str, float] = field(default_factory=dict)


@dataclass
class CostBenefit:
    """Cost-benefit analysis for scaling"""
    current_cost: float
    projected_cost: float
    performance_improvement: float
    efficiency_gain: float
    roi: float  # Return on investment
    recommendation: str


class ScalingOptimizer:
    """
    Advanced scaling optimizer for the Agent Manager system
    """
    
    def __init__(self,
                 min_agents: int = 1,
                 max_agents: int = 100,
                 target_utilization: float = 0.7,
                 response_time_threshold_ms: float = 500.0,
                 error_rate_threshold: float = 0.05,
                 cost_per_agent_hour: float = 0.10,
                 prediction_window_minutes: int = 30,
                 history_retention_hours: int = 24):
        
        self.min_agents = min_agents
        self.max_agents = max_agents
        self.target_utilization = target_utilization
        self.response_time_threshold = response_time_threshold_ms
        self.error_rate_threshold = error_rate_threshold
        self.cost_per_agent_hour = cost_per_agent_hour
        self.prediction_window = prediction_window_minutes
        self.history_retention = history_retention_hours
        
        # Historical data
        self.metrics_history: deque = deque(maxlen=history_retention_hours * 60)  # One per minute
        self.scaling_history: List[Dict[str, Any]] = []
        
        # Prediction models
        self.prediction_models = {
            PredictionModel.LINEAR_REGRESSION: self._linear_regression_predict,
            PredictionModel.MOVING_AVERAGE: self._moving_average_predict,
            PredictionModel.EXPONENTIAL_SMOOTHING: self._exponential_smoothing_predict,
            PredictionModel.SEASONAL_DECOMPOSITION: self._seasonal_predict
        }
        
        # Current model weights (ensemble)
        self.model_weights = {
            PredictionModel.LINEAR_REGRESSION: 0.3,
            PredictionModel.MOVING_AVERAGE: 0.2,
            PredictionModel.EXPONENTIAL_SMOOTHING: 0.3,
            PredictionModel.SEASONAL_DECOMPOSITION: 0.2
        }
        
        # Scaling thresholds
        self.scale_up_conditions = {
            'utilization_threshold': 0.8,
            'response_time_threshold': response_time_threshold_ms,
            'error_rate_threshold': error_rate_threshold,
            'queue_length_threshold': 50
        }
        
        self.scale_down_conditions = {
            'utilization_threshold': 0.3,
            'response_time_threshold': response_time_threshold_ms * 0.5,
            'error_rate_threshold': error_rate_threshold * 0.5,
            'queue_length_threshold': 5,
            'min_idle_time_minutes': 10
        }
        
        # Cooldown periods
        self.scale_up_cooldown = 300  # 5 minutes
        self.scale_down_cooldown = 600  # 10 minutes
        self.last_scaling_time = 0
        self.last_scaling_action = None
        
        # Performance tracking
        self.prediction_accuracy_history = deque(maxlen=100)
        
        # Cost optimization settings
        self.cost_optimization_enabled = True
        self.performance_cost_weight = 0.7  # Weight performance vs cost (0.7 means 70% performance, 30% cost)

    def record_metrics(self, metrics: ScalingMetrics):
        """Record metrics for analysis"""
        self.metrics_history.append(metrics)
        
        # Evaluate prediction accuracy if we have predictions
        self._evaluate_prediction_accuracy(metrics)

    def should_scale(self, current_metrics: ScalingMetrics) -> Tuple[bool, ScalingTrigger, int]:
        """
        Determine if scaling is needed and return action details
        Returns: (should_scale, trigger_reason, target_agent_count)
        """
        current_time = time.time()
        
        # Check cooldown
        if current_time - self.last_scaling_time < self._get_cooldown_time():
            return False, None, current_metrics.agent_count
        
        # Check scale-up conditions
        scale_up_triggers = self._check_scale_up_triggers(current_metrics)
        if scale_up_triggers:
            target_count = self._calculate_scale_up_target(current_metrics, scale_up_triggers)
            return True, scale_up_triggers[0], target_count
        
        # Check scale-down conditions
        scale_down_triggers = self._check_scale_down_triggers(current_metrics)
        if scale_down_triggers:
            target_count = self._calculate_scale_down_target(current_metrics, scale_down_triggers)
            return True, scale_down_triggers[0], target_count
        
        # Check predictive scaling
        if len(self.metrics_history) >= 10:  # Need some history
            prediction = self._get_predictive_scaling_recommendation(current_metrics)
            if prediction and abs(prediction.recommended_agents - current_metrics.agent_count) >= 2:
                return True, ScalingTrigger.PREDICTIVE, prediction.recommended_agents
        
        return False, None, current_metrics.agent_count

    def _check_scale_up_triggers(self, metrics: ScalingMetrics) -> List[ScalingTrigger]:
        """Check conditions that trigger scale-up"""
        triggers = []
        
        if metrics.utilization > self.scale_up_conditions['utilization_threshold']:
            triggers.append(ScalingTrigger.UTILIZATION_HIGH)
        
        if metrics.avg_response_time_ms > self.scale_up_conditions['response_time_threshold']:
            triggers.append(ScalingTrigger.RESPONSE_TIME_HIGH)
        
        if metrics.error_rate > self.scale_up_conditions['error_rate_threshold']:
            triggers.append(ScalingTrigger.ERROR_RATE_HIGH)
        
        if metrics.queue_length > self.scale_up_conditions['queue_length_threshold']:
            triggers.append(ScalingTrigger.QUEUE_LENGTH_HIGH)
        
        return triggers

    def _check_scale_down_triggers(self, metrics: ScalingMetrics) -> List[ScalingTrigger]:
        """Check conditions that trigger scale-down"""
        triggers = []
        
        # Only scale down if multiple conditions are met
        conditions_met = 0
        
        if metrics.utilization < self.scale_down_conditions['utilization_threshold']:
            conditions_met += 1
        
        if metrics.avg_response_time_ms < self.scale_down_conditions['response_time_threshold']:
            conditions_met += 1
        
        if metrics.error_rate < self.scale_down_conditions['error_rate_threshold']:
            conditions_met += 1
        
        if metrics.queue_length < self.scale_down_conditions['queue_length_threshold']:
            conditions_met += 1
        
        # Require at least 3 out of 4 conditions for scale-down
        if conditions_met >= 3:
            # Additional check: ensure we've been underutilized for a while
            recent_metrics = list(self.metrics_history)[-10:]  # Last 10 minutes
            if len(recent_metrics) >= 5:
                avg_recent_utilization = statistics.mean(m.utilization for m in recent_metrics)
                if avg_recent_utilization < self.scale_down_conditions['utilization_threshold']:
                    triggers.append(ScalingTrigger.UTILIZATION_LOW)
        
        return triggers

    def _calculate_scale_up_target(self, metrics: ScalingMetrics, triggers: List[ScalingTrigger]) -> int:
        """Calculate target agent count for scale-up"""
        current_count = metrics.agent_count
        
        # Base scaling factor
        scale_factor = 1.2  # 20% increase
        
        # Adjust based on triggers
        for trigger in triggers:
            if trigger == ScalingTrigger.UTILIZATION_HIGH:
                # Scale based on utilization overage
                overage = metrics.utilization - self.target_utilization
                additional_factor = 1 + (overage * 2)  # More aggressive scaling for higher utilization
                scale_factor = max(scale_factor, additional_factor)
            
            elif trigger == ScalingTrigger.RESPONSE_TIME_HIGH:
                # Scale based on response time
                response_factor = metrics.avg_response_time_ms / self.response_time_threshold
                scale_factor = max(scale_factor, response_factor)
            
            elif trigger == ScalingTrigger.ERROR_RATE_HIGH:
                # Aggressive scaling for errors
                scale_factor = max(scale_factor, 1.5)
            
            elif trigger == ScalingTrigger.QUEUE_LENGTH_HIGH:
                # Scale based on queue length
                queue_factor = 1 + (metrics.queue_length / 100.0)
                scale_factor = max(scale_factor, queue_factor)
        
        # Calculate target count
        target_count = int(current_count * scale_factor)
        
        # Apply constraints
        target_count = min(target_count, self.max_agents)
        target_count = max(target_count, current_count + 1)  # At least add 1
        
        return target_count

    def _calculate_scale_down_target(self, metrics: ScalingMetrics, triggers: List[ScalingTrigger]) -> int:
        """Calculate target agent count for scale-down"""
        current_count = metrics.agent_count
        
        # Conservative scale-down
        utilization_based_count = max(1, int(current_count * (metrics.utilization / self.target_utilization)))
        target_count = max(self.min_agents, utilization_based_count, current_count - 2)
        
        return target_count

    def _get_predictive_scaling_recommendation(self, current_metrics: ScalingMetrics) -> Optional[ScalingPrediction]:
        """Get predictive scaling recommendation using ensemble of models"""
        if len(self.metrics_history) < 10:
            return None
        
        predictions = []
        total_weight = 0
        
        # Get predictions from each model
        for model, weight in self.model_weights.items():
            try:
                prediction = self.prediction_models[model](current_metrics)
                if prediction:
                    predictions.append((prediction, weight))
                    total_weight += weight
            except Exception as e:
                logger.error(f"Prediction model {model} failed: {e}")
        
        if not predictions:
            return None
        
        # Ensemble prediction
        weighted_demand = sum(pred.predicted_demand * weight for pred, weight in predictions) / total_weight
        weighted_confidence = sum(pred.confidence * weight for pred, weight in predictions) / total_weight
        
        # Calculate recommended agent count
        recommended_agents = self._demand_to_agent_count(weighted_demand, current_metrics)
        
        return ScalingPrediction(
            predicted_demand=weighted_demand,
            recommended_agents=recommended_agents,
            confidence=weighted_confidence,
            time_horizon_minutes=self.prediction_window,
            model_used=PredictionModel.LINEAR_REGRESSION,  # Primary model
            factors={'ensemble_models': len(predictions)}
        )

    def _linear_regression_predict(self, current_metrics: ScalingMetrics) -> Optional[ScalingPrediction]:
        """Linear regression prediction"""
        if len(self.metrics_history) < 5:
            return None
        
        # Simple linear regression on utilization trend
        recent_metrics = list(self.metrics_history)[-20:]  # Last 20 minutes
        
        if len(recent_metrics) < 5:
            return None
        
        # Calculate trend
        x_values = list(range(len(recent_metrics)))
        y_values = [m.utilization for m in recent_metrics]
        
        # Simple linear regression
        n = len(x_values)
        sum_x = sum(x_values)
        sum_y = sum(y_values)
        sum_xy = sum(x * y for x, y in zip(x_values, y_values))
        sum_x2 = sum(x * x for x in x_values)
        
        # Calculate slope
        slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
        intercept = (sum_y - slope * sum_x) / n
        
        # Predict future utilization
        future_time = len(recent_metrics) + self.prediction_window
        predicted_utilization = slope * future_time + intercept
        
        # Convert to demand
        predicted_demand = max(0, predicted_utilization)
        
        # Confidence based on R²
        mean_y = statistics.mean(y_values)
        ss_res = sum((actual - (slope * x + intercept)) ** 2 for x, actual in zip(x_values, y_values))
        ss_tot = sum((actual - mean_y) ** 2 for actual in y_values)
        
        r_squared = 1 - (ss_res / ss_tot) if ss_tot != 0 else 0
        confidence = max(0.1, min(0.9, r_squared))
        
        return ScalingPrediction(
            predicted_demand=predicted_demand,
            recommended_agents=self._demand_to_agent_count(predicted_demand, current_metrics),
            confidence=confidence,
            time_horizon_minutes=self.prediction_window,
            model_used=PredictionModel.LINEAR_REGRESSION,
            factors={'slope': slope, 'r_squared': r_squared}
        )

    def _moving_average_predict(self, current_metrics: ScalingMetrics) -> Optional[ScalingPrediction]:
        """Moving average prediction"""
        if len(self.metrics_history) < 10:
            return None
        
        recent_metrics = list(self.metrics_history)[-15:]
        
        # Calculate moving averages
        short_ma = statistics.mean(m.utilization for m in recent_metrics[-5:])
        long_ma = statistics.mean(m.utilization for m in recent_metrics[-10:])
        
        # Trend prediction
        trend = short_ma - long_ma
        predicted_utilization = short_ma + (trend * (self.prediction_window / 5))
        
        predicted_demand = max(0, predicted_utilization)
        confidence = 0.6  # Moderate confidence for moving average
        
        return ScalingPrediction(
            predicted_demand=predicted_demand,
            recommended_agents=self._demand_to_agent_count(predicted_demand, current_metrics),
            confidence=confidence,
            time_horizon_minutes=self.prediction_window,
            model_used=PredictionModel.MOVING_AVERAGE,
            factors={'trend': trend, 'short_ma': short_ma, 'long_ma': long_ma}
        )

    def _exponential_smoothing_predict(self, current_metrics: ScalingMetrics) -> Optional[ScalingPrediction]:
        """Exponential smoothing prediction"""
        if len(self.metrics_history) < 10:
            return None
        
        recent_metrics = list(self.metrics_history)[-20:]
        alpha = 0.3  # Smoothing parameter
        
        # Calculate exponential smoothing
        smoothed_values = []
        smoothed_values.append(recent_metrics[0].utilization)
        
        for i in range(1, len(recent_metrics)):
            smoothed = alpha * recent_metrics[i].utilization + (1 - alpha) * smoothed_values[-1]
            smoothed_values.append(smoothed)
        
        # Simple trend extrapolation
        if len(smoothed_values) >= 2:
            trend = smoothed_values[-1] - smoothed_values[-2]
            predicted_utilization = smoothed_values[-1] + (trend * (self.prediction_window / 5))
        else:
            predicted_utilization = smoothed_values[-1]
        
        predicted_demand = max(0, predicted_utilization)
        confidence = 0.7  # Good confidence for exponential smoothing
        
        return ScalingPrediction(
            predicted_demand=predicted_demand,
            recommended_agents=self._demand_to_agent_count(predicted_demand, current_metrics),
            confidence=confidence,
            time_horizon_minutes=self.prediction_window,
            model_used=PredictionModel.EXPONENTIAL_SMOOTHING,
            factors={'alpha': alpha, 'trend': trend if 'trend' in locals() else 0}
        )

    def _seasonal_predict(self, current_metrics: ScalingMetrics) -> Optional[ScalingPrediction]:
        """Seasonal decomposition prediction"""
        if len(self.metrics_history) < 60:  # Need at least 1 hour of data
            return None
        
        # Simple seasonal pattern detection
        recent_metrics = list(self.metrics_history)
        
        # Group by time of day (assuming 1-minute intervals)
        current_minute = int(time.time() / 60) % (24 * 60)  # Minute of day
        
        # Find historical patterns for this time of day (±30 minutes)
        similar_times = []
        for i, metrics in enumerate(recent_metrics):
            metrics_minute = int(metrics.timestamp / 60) % (24 * 60)
            if abs(metrics_minute - current_minute) <= 30:
                similar_times.append(metrics.utilization)
        
        if len(similar_times) < 3:
            return None
        
        # Predict based on historical pattern
        avg_seasonal_utilization = statistics.mean(similar_times)
        seasonal_std = statistics.stdev(similar_times) if len(similar_times) > 1 else 0.1
        
        # Add some trend from recent data
        recent_trend = recent_metrics[-1].utilization - statistics.mean(m.utilization for m in recent_metrics[-5:])
        predicted_utilization = avg_seasonal_utilization + (recent_trend * 0.3)
        
        predicted_demand = max(0, predicted_utilization)
        confidence = max(0.3, 1.0 - seasonal_std)  # Higher confidence with lower variance
        
        return ScalingPrediction(
            predicted_demand=predicted_demand,
            recommended_agents=self._demand_to_agent_count(predicted_demand, current_metrics),
            confidence=confidence,
            time_horizon_minutes=self.prediction_window,
            model_used=PredictionModel.SEASONAL_DECOMPOSITION,
            factors={'seasonal_avg': avg_seasonal_utilization, 'seasonal_std': seasonal_std}
        )

    def _demand_to_agent_count(self, predicted_demand: float, current_metrics: ScalingMetrics) -> int:
        """Convert predicted demand to agent count"""
        if predicted_demand <= 0:
            return self.min_agents
        
        # Account for current efficiency
        current_efficiency = current_metrics.efficiency_score if current_metrics.efficiency_score > 0 else 0.8
        
        # Calculate required agents based on target utilization
        required_capacity = predicted_demand / self.target_utilization
        required_agents = int(required_capacity / current_efficiency)
        
        # Apply constraints
        required_agents = max(self.min_agents, min(self.max_agents, required_agents))
        
        return required_agents

    def _evaluate_prediction_accuracy(self, actual_metrics: ScalingMetrics):
        """Evaluate and track prediction accuracy"""
        # This would compare predictions made X minutes ago with current reality
        # For now, we'll simulate tracking
        
        # Update model weights based on accuracy (simplified)
        # In a full implementation, we'd track each model's performance
        pass

    def _get_cooldown_time(self) -> int:
        """Get appropriate cooldown time based on last action"""
        if self.last_scaling_action == ScalingTrigger.UTILIZATION_HIGH:
            return self.scale_up_cooldown
        else:
            return self.scale_down_cooldown

    def analyze_cost_benefit(self, 
                           current_metrics: ScalingMetrics,
                           proposed_agent_count: int) -> CostBenefit:
        """Analyze cost-benefit of a scaling decision"""
        current_cost = current_metrics.agent_count * self.cost_per_agent_hour
        proposed_cost = proposed_agent_count * self.cost_per_agent_hour
        
        # Estimate performance improvement
        if proposed_agent_count > current_metrics.agent_count:
            # Scaling up - estimate improvement
            utilization_improvement = max(0, current_metrics.utilization - self.target_utilization)
            response_time_improvement = max(0, (current_metrics.avg_response_time_ms - self.response_time_threshold) / self.response_time_threshold)
            performance_improvement = (utilization_improvement + response_time_improvement) / 2
        else:
            # Scaling down - potential performance degradation
            capacity_reduction = (current_metrics.agent_count - proposed_agent_count) / current_metrics.agent_count
            performance_improvement = -capacity_reduction * 0.5  # Negative = degradation
        
        # Calculate efficiency gain
        current_efficiency = current_metrics.efficiency_score
        # Estimate new efficiency (simplified model)
        if proposed_agent_count > current_metrics.agent_count:
            estimated_new_efficiency = min(1.0, current_efficiency + performance_improvement * 0.3)
        else:
            estimated_new_efficiency = max(0.1, current_efficiency + performance_improvement * 0.3)
        
        efficiency_gain = estimated_new_efficiency - current_efficiency
        
        # Calculate ROI
        cost_change = proposed_cost - current_cost
        if cost_change != 0:
            value_improvement = performance_improvement * 100  # Convert to dollar value
            roi = (value_improvement - abs(cost_change)) / abs(cost_change) * 100
        else:
            roi = 0
        
        # Generate recommendation
        if self.cost_optimization_enabled:
            performance_weight = self.performance_cost_weight
            cost_weight = 1 - performance_weight
            
            score = performance_improvement * performance_weight - (abs(cost_change) / 10) * cost_weight
            
            if score > 0.1:
                recommendation = "Recommended: Good cost-benefit ratio"
            elif score > -0.1:
                recommendation = "Neutral: Marginal cost-benefit"
            else:
                recommendation = "Not recommended: Poor cost-benefit ratio"
        else:
            recommendation = "Cost optimization disabled"
        
        return CostBenefit(
            current_cost=current_cost,
            projected_cost=proposed_cost,
            performance_improvement=performance_improvement,
            efficiency_gain=efficiency_gain,
            roi=roi,
            recommendation=recommendation
        )

    def get_scaling_insights(self) -> Dict[str, Any]:
        """Get insights and analytics about scaling behavior"""
        if not self.metrics_history:
            return {'error': 'No historical data available'}
        
        recent_metrics = list(self.metrics_history)[-60:]  # Last hour
        
        insights = {
            'timestamp': time.time(),
            'data_points': len(self.metrics_history),
            'avg_utilization': statistics.mean(m.utilization for m in recent_metrics),
            'avg_response_time': statistics.mean(m.avg_response_time_ms for m in recent_metrics),
            'avg_agent_count': statistics.mean(m.agent_count for m in recent_metrics),
            'scaling_events_last_24h': len([
                event for event in self.scaling_history
                if time.time() - event['timestamp'] < 86400
            ]),
            'cost_efficiency': {
                'avg_cost_per_hour': statistics.mean(m.cost_per_hour for m in recent_metrics),
                'avg_efficiency_score': statistics.mean(m.efficiency_score for m in recent_metrics)
            },
            'performance_trends': {
                'utilization_trend': self._calculate_trend([m.utilization for m in recent_metrics[-10:]]),
                'response_time_trend': self._calculate_trend([m.avg_response_time_ms for m in recent_metrics[-10:]]),
            },
            'prediction_accuracy': statistics.mean(self.prediction_accuracy_history) if self.prediction_accuracy_history else 0.0,
            'recommendations': self._generate_optimization_recommendations(recent_metrics)
        }
        
        return insights

    def _calculate_trend(self, values: List[float]) -> str:
        """Calculate trend direction for a list of values"""
        if len(values) < 2:
            return "insufficient_data"
        
        first_half = statistics.mean(values[:len(values)//2])
        second_half = statistics.mean(values[len(values)//2:])
        
        change = (second_half - first_half) / first_half if first_half != 0 else 0
        
        if change > 0.1:
            return "increasing"
        elif change < -0.1:
            return "decreasing"
        else:
            return "stable"

    def _generate_optimization_recommendations(self, recent_metrics: List[ScalingMetrics]) -> List[str]:
        """Generate optimization recommendations"""
        recommendations = []
        
        if not recent_metrics:
            return recommendations
        
        avg_utilization = statistics.mean(m.utilization for m in recent_metrics)
        avg_response_time = statistics.mean(m.avg_response_time_ms for m in recent_metrics)
        avg_error_rate = statistics.mean(m.error_rate for m in recent_metrics)
        
        # Utilization recommendations
        if avg_utilization > 0.9:
            recommendations.append("High utilization detected. Consider increasing minimum agent count.")
        elif avg_utilization < 0.3:
            recommendations.append("Low utilization detected. Consider reducing maximum agent count or optimizing workload distribution.")
        
        # Performance recommendations
        if avg_response_time > self.response_time_threshold * 1.2:
            recommendations.append("Response times are consistently high. Consider lowering scale-up thresholds.")
        
        # Error rate recommendations
        if avg_error_rate > self.error_rate_threshold:
            recommendations.append("Error rate is elevated. Consider more aggressive scale-up policies.")
        
        # Cost optimization recommendations
        if self.cost_optimization_enabled:
            avg_efficiency = statistics.mean(m.efficiency_score for m in recent_metrics)
            if avg_efficiency < 0.6:
                recommendations.append("Cost efficiency is low. Review scaling policies and consider predictive scaling.")
        
        return recommendations

    def record_scaling_action(self, 
                            trigger: ScalingTrigger,
                            before_count: int,
                            after_count: int,
                            metrics: ScalingMetrics):
        """Record a scaling action for analysis"""
        self.last_scaling_time = time.time()
        self.last_scaling_action = trigger
        
        event = {
            'timestamp': time.time(),
            'trigger': trigger.value,
            'before_count': before_count,
            'after_count': after_count,
            'metrics': metrics.to_dict()
        }
        
        self.scaling_history.append(event)
        
        # Keep only last 1000 events
        if len(self.scaling_history) > 1000:
            self.scaling_history = self.scaling_history[-1000:]


# Factory function for easy integration
def create_scaling_optimizer(**kwargs) -> ScalingOptimizer:
    """Create a scaling optimizer with configuration"""
    return ScalingOptimizer(**kwargs)