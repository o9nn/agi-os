"""
A/B Testing API Routes for Aphrodite Engine
Phase 8 - SSR-Focused MLOps & Production Observability

Server-side A/B testing API endpoints for model variant management.
"""
import json
import os
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Union

from fastapi import APIRouter, HTTPException, Request, Depends
from pydantic import BaseModel, Field

from aphrodite.endpoints.middleware.ab_testing_middleware import (
    ABTestingManager, ABTestConfig, ABTestResult, get_ab_testing_manager
)
from aphrodite.endpoints.ab_testing_monitor import (
    ABTestMonitor, get_auto_rollback_system, initialize_ab_monitoring
)
from aphrodite.endpoints.openai.protocol import ErrorResponse
from aphrodite.endpoints.logger import RequestLogger


# Request/Response Models
class StartABTestRequest(BaseModel):
    model_a: str = Field(..., description="Stable model identifier")
    model_b: str = Field(..., description="Canary model identifier to test")
    traffic_split_percent: Optional[float] = Field(10.0, ge=0, le=100, description="Percentage of traffic to variant B")
    test_duration_minutes: Optional[int] = Field(60, ge=5, le=1440, description="Maximum test duration in minutes")
    auto_rollback: Optional[bool] = Field(True, description="Enable automatic rollback on failure")
    success_criteria: Optional[Dict[str, float]] = Field(None, description="Custom success criteria")
    failure_criteria: Optional[Dict[str, float]] = Field(None, description="Custom failure criteria")


class ABTestStatusResponse(BaseModel):
    test_id: Optional[str] = Field(None, description="Active test ID")
    status: Optional[str] = Field(None, description="Test status")
    elapsed_minutes: Optional[float] = Field(None, description="Test elapsed time")
    traffic_split_percent: Optional[float] = Field(None, description="Traffic split percentage")
    metrics: Optional[Dict[str, Any]] = Field(None, description="Test metrics")
    models: Optional[Dict[str, str]] = Field(None, description="Model variants")


class ABTestResultResponse(BaseModel):
    test_id: str = Field(..., description="Test ID")
    status: str = Field(..., description="Test completion status")
    decision: str = Field(..., description="Test decision")
    reason: str = Field(..., description="Decision reason")
    metrics_summary: Dict[str, Any] = Field(..., description="Final metrics summary")
    start_time: str = Field(..., description="Test start time")
    end_time: Optional[str] = Field(None, description="Test end time")


class UpdateTrafficSplitRequest(BaseModel):
    traffic_split_percent: float = Field(..., ge=0, le=100, description="New traffic split percentage")


class ABTestHistoryResponse(BaseModel):
    tests: List[Dict[str, Any]] = Field(..., description="Historical test results")
    total_tests: int = Field(..., description="Total number of tests")


# A/B Testing Router
router = APIRouter(prefix="/v1/ab-testing", tags=["A/B Testing"])


@router.post("/start", 
             response_model=Dict[str, str],
             summary="Start A/B Test",
             description="Start a new A/B test between two model variants")
async def start_ab_test(
    request: StartABTestRequest,
    ab_manager: ABTestingManager = Depends(get_ab_testing_manager)
) -> Dict[str, str]:
    """Start a new A/B test between model variants"""
    
    # Check if there's already an active test
    if ab_manager.active_test:
        raise HTTPException(
            status_code=409, 
            detail="An A/B test is already running. Stop the current test first."
        )
    
    # Validate models exist (this would integrate with model registry in production)
    if request.model_a == request.model_b:
        raise HTTPException(
            status_code=400,
            detail="Model A and Model B must be different"
        )
    
    # Update configuration
    ab_manager.config.traffic_split_percent = request.traffic_split_percent
    ab_manager.config.test_duration_minutes = request.test_duration_minutes
    ab_manager.config.auto_rollback = request.auto_rollback
    
    if request.success_criteria:
        ab_manager.config.success_criteria.update(request.success_criteria)
    if request.failure_criteria:
        ab_manager.config.failure_criteria.update(request.failure_criteria)
    
    # Start the test
    test_id = await ab_manager.start_ab_test(request.model_a, request.model_b)
    
    # Log the test start
    await _log_ab_test_event("start", {
        "test_id": test_id,
        "model_a": request.model_a,
        "model_b": request.model_b,
        "traffic_split": request.traffic_split_percent
    })
    
    return {"test_id": test_id, "status": "started"}


@router.get("/status",
            response_model=ABTestStatusResponse,
            summary="Get A/B Test Status", 
            description="Get the current status of the active A/B test")
async def get_ab_test_status(
    ab_manager: ABTestingManager = Depends(get_ab_testing_manager)
) -> ABTestStatusResponse:
    """Get current A/B test status and metrics"""
    
    status_data = ab_manager.get_test_status()
    
    if not status_data:
        return ABTestStatusResponse()
    
    return ABTestStatusResponse(**status_data)


@router.post("/stop",
             response_model=ABTestResultResponse,
             summary="Stop A/B Test",
             description="Stop the current A/B test and get results")
async def stop_ab_test(
    ab_manager: ABTestingManager = Depends(get_ab_testing_manager)
) -> ABTestResultResponse:
    """Stop the current A/B test and return results"""
    
    if not ab_manager.active_test:
        raise HTTPException(
            status_code=404,
            detail="No active A/B test found"
        )
    
    # Stop the test
    result = await ab_manager.stop_ab_test("manual_stop")
    
    # Save results to history
    await _save_test_result(result)
    
    # Log the test completion
    await _log_ab_test_event("stop", {
        "test_id": result.test_id,
        "decision": result.decision,
        "reason": result.reason
    })
    
    return ABTestResultResponse(
        test_id=result.test_id,
        status=result.status,
        decision=result.decision,
        reason=result.reason,
        metrics_summary={
            "variant_a": {
                "error_rate": result.metrics_a.error_rate,
                "avg_latency_ms": result.metrics_a.avg_latency_ms,
                "request_count": result.metrics_a.request_count
            },
            "variant_b": {
                "error_rate": result.metrics_b.error_rate,
                "avg_latency_ms": result.metrics_b.avg_latency_ms,
                "request_count": result.metrics_b.request_count
            }
        },
        start_time=result.start_time,
        end_time=result.end_time
    )


@router.patch("/traffic-split",
              response_model=Dict[str, str],
              summary="Update Traffic Split",
              description="Update the traffic split percentage for the active test")
async def update_traffic_split(
    request: UpdateTrafficSplitRequest,
    ab_manager: ABTestingManager = Depends(get_ab_testing_manager)
) -> Dict[str, str]:
    """Update traffic split percentage for active A/B test"""
    
    if not ab_manager.active_test:
        raise HTTPException(
            status_code=404,
            detail="No active A/B test found"
        )
    
    old_split = ab_manager.config.traffic_split_percent
    ab_manager.config.traffic_split_percent = request.traffic_split_percent
    
    # Log the change
    await _log_ab_test_event("traffic_split_update", {
        "test_id": ab_manager.active_test["test_id"],
        "old_split": old_split,
        "new_split": request.traffic_split_percent
    })
    
    return {
        "status": "updated",
        "traffic_split_percent": str(request.traffic_split_percent)
    }


@router.get("/history",
            response_model=ABTestHistoryResponse,
            summary="Get A/B Test History",
            description="Get historical A/B test results")
async def get_ab_test_history(
    limit: int = 50,
    ab_manager: ABTestingManager = Depends(get_ab_testing_manager)
) -> ABTestHistoryResponse:
    """Get historical A/B test results"""
    
    history = await _load_test_history(limit)
    
    return ABTestHistoryResponse(
        tests=history,
        total_tests=len(history)
    )


@router.post("/rollback",
             response_model=Dict[str, str],
             summary="Emergency Rollback",
             description="Immediately rollback to stable model (emergency stop)")
async def emergency_rollback(
    ab_manager: ABTestingManager = Depends(get_ab_testing_manager)
) -> Dict[str, str]:
    """Emergency rollback to stable model"""
    
    if not ab_manager.active_test:
        raise HTTPException(
            status_code=404,
            detail="No active A/B test found"
        )
    
    # Force rollback
    result = await ab_manager.stop_ab_test("emergency_rollback")
    result.decision = "rollback"
    result.reason = "Emergency rollback initiated"
    
    # Save emergency rollback result
    await _save_test_result(result)
    
    # Log emergency rollback
    await _log_ab_test_event("emergency_rollback", {
        "test_id": result.test_id,
        "timestamp": datetime.now(timezone.utc).isoformat()
    })
    
    return {"status": "rolled_back", "test_id": result.test_id}


@router.get("/monitoring/status",
            response_model=Dict[str, Any],
            summary="Get Monitoring Status",
            description="Get current A/B test monitoring system status")
async def get_monitoring_status() -> Dict[str, Any]:
    """Get current monitoring system status"""
    auto_rollback = get_auto_rollback_system()
    
    if not auto_rollback.monitor:
        return {"monitoring_enabled": False, "message": "Monitoring not initialized"}
    
    status = auto_rollback.monitor.get_monitoring_status()
    status["monitoring_enabled"] = True
    return status


@router.get("/monitoring/alerts",
            response_model=Dict[str, Any],
            summary="Get Monitoring Alerts",
            description="Get recent monitoring alerts and events")
async def get_monitoring_alerts(limit: int = 50) -> Dict[str, Any]:
    """Get recent monitoring alerts"""
    auto_rollback = get_auto_rollback_system()
    
    if not auto_rollback.monitor:
        return {"alerts": [], "monitoring_enabled": False}
    
    alerts = auto_rollback.monitor.get_alert_history(limit)
    return {
        "alerts": alerts,
        "total_alerts": len(alerts),
        "monitoring_enabled": True
    }


@router.post("/monitoring/start",
             response_model=Dict[str, str],
             summary="Start Monitoring",
             description="Start automated monitoring for the current A/B test")
async def start_monitoring(
    check_interval_seconds: int = 30,
    ab_manager: ABTestingManager = Depends(get_ab_testing_manager)
) -> Dict[str, str]:
    """Start automated monitoring for active A/B test"""
    
    if not ab_manager.active_test:
        raise HTTPException(
            status_code=404,
            detail="No active A/B test to monitor"
        )
    
    auto_rollback = get_auto_rollback_system()
    
    # Initialize monitoring if not already done
    if not auto_rollback.monitor:
        monitor = initialize_ab_monitoring(check_interval_seconds)
    else:
        monitor = auto_rollback.monitor
    
    # Start monitoring
    await monitor.start_monitoring()
    
    return {
        "status": "monitoring_started",
        "test_id": ab_manager.active_test["test_id"],
        "check_interval_seconds": str(check_interval_seconds)
    }


@router.post("/monitoring/stop",
             response_model=Dict[str, str],
             summary="Stop Monitoring",
             description="Stop automated monitoring")
async def stop_monitoring() -> Dict[str, str]:
    """Stop automated monitoring"""
    auto_rollback = get_auto_rollback_system()
    
    if auto_rollback.monitor:
        await auto_rollback.monitor.stop_monitoring()
        return {"status": "monitoring_stopped"}
    
    return {"status": "no_monitoring_active"}


@router.get("/config",
            response_model=Dict[str, Any],
            summary="Get A/B Test Configuration",
            description="Get current A/B testing configuration")
async def get_ab_test_config(
    ab_manager: ABTestingManager = Depends(get_ab_testing_manager)
) -> Dict[str, Any]:
    """Get current A/B testing configuration"""
    
    return {
        "enabled": ab_manager.config.enabled,
        "traffic_split_percent": ab_manager.config.traffic_split_percent,
        "test_duration_minutes": ab_manager.config.test_duration_minutes,
        "auto_rollback": ab_manager.config.auto_rollback,
        "success_criteria": ab_manager.config.success_criteria,
        "failure_criteria": ab_manager.config.failure_criteria,
        "metrics_window_size": ab_manager.config.metrics_window_size
    }


# Helper functions
async def _log_ab_test_event(event_type: str, data: Dict[str, Any]):
    """Log A/B test events for audit trail"""
    log_entry = {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "event_type": event_type,
        "data": data
    }
    
    # In production, this would integrate with structured logging
    from loguru import logger
    logger.info(f"AB_TEST_EVENT: {json.dumps(log_entry)}")


async def _save_test_result(result: ABTestResult):
    """Save A/B test result to persistent storage"""
    # Create results directory if it doesn't exist
    results_dir = "/tmp/ab_test_results"
    os.makedirs(results_dir, exist_ok=True)
    
    result_file = f"{results_dir}/{result.test_id}.json"
    
    # Convert dataclass to dict for serialization
    result_dict = {
        "test_id": result.test_id,
        "status": result.status,
        "decision": result.decision,
        "reason": result.reason,
        "start_time": result.start_time,
        "end_time": result.end_time,
        "metrics_a": {
            "request_count": result.metrics_a.request_count,
            "error_count": result.metrics_a.error_count,
            "successful_requests": result.metrics_a.successful_requests,
            "total_latency_ms": result.metrics_a.total_latency_ms,
            "error_rate": result.metrics_a.error_rate,
            "avg_latency_ms": result.metrics_a.avg_latency_ms,
            "last_updated": result.metrics_a.last_updated
        },
        "metrics_b": {
            "request_count": result.metrics_b.request_count,
            "error_count": result.metrics_b.error_count,
            "successful_requests": result.metrics_b.successful_requests,
            "total_latency_ms": result.metrics_b.total_latency_ms,
            "error_rate": result.metrics_b.error_rate,
            "avg_latency_ms": result.metrics_b.avg_latency_ms,
            "last_updated": result.metrics_b.last_updated
        }
    }
    
    with open(result_file, 'w') as f:
        json.dump(result_dict, f, indent=2)


async def _load_test_history(limit: int) -> List[Dict[str, Any]]:
    """Load historical A/B test results"""
    results_dir = "/tmp/ab_test_results"
    
    if not os.path.exists(results_dir):
        return []
    
    history = []
    result_files = sorted(
        [f for f in os.listdir(results_dir) if f.endswith('.json')],
        reverse=True  # Most recent first
    )
    
    for filename in result_files[:limit]:
        try:
            with open(os.path.join(results_dir, filename)) as f:
                result_data = json.load(f)
                history.append(result_data)
        except Exception as e:
            from loguru import logger
            logger.error(f"Failed to load test result {filename}: {e}")
    
    return history


# Export router for integration with main API server
__all__ = ["router"]