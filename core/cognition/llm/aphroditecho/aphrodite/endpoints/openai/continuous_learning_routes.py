from datetime import datetime
from typing import Any, Dict, Optional
from fastapi import APIRouter, BackgroundTasks, Depends, HTTPException, Request
from loguru import logger
from aphrodite.continuous_learning import InteractionData
from aphrodite.endpoints.openai.protocol import ErrorResponse
from aphrodite.endpoints.openai.serving_continuous_learning import OpenAIServingContinuousLearning, LearningInteractionRequest, LearningControlRequest, LearningResponse
from aphrodite.endpoints.utils import with_cancellation
router = APIRouter(prefix='/v1/learning', tags=['Continuous Learning'])
_learning_service: Optional[OpenAIServingContinuousLearning] = None
def get_learning_service(request: Request) -> OpenAIServingContinuousLearning:
    service = getattr(request.app.state, 'openai_serving_continuous_learning', None)
    if service is None:
        raise HTTPException(status_code=501, detail='Continuous learning service not available. Ensure it is enabled in server configuration.')
    return service
@router.get('/status', summary='Get Learning Status', description='Get comprehensive status of the continuous learning system')
@with_cancellation
async def get_learning_status(request: Request, service: OpenAIServingContinuousLearning=Depends(get_learning_service)):
    try:
        status = await service.get_learning_status()
        return status
    except Exception as e:
        logger.error(f'Failed to get learning status: {e}')
        raise HTTPException(status_code=500, detail=str(e))
@router.get('/metrics', summary='Get Learning Metrics', description='Get detailed performance metrics for the continuous learning system')
@with_cancellation
async def get_learning_metrics(request: Request, service: OpenAIServingContinuousLearning=Depends(get_learning_service)):
    try:
        metrics = await service.get_learning_metrics()
        return metrics
    except Exception as e:
        logger.error(f'Failed to get learning metrics: {e}')
        raise HTTPException(status_code=500, detail=str(e))
@router.post('/learn', summary='Learn from Interaction', description='Learn from a production interaction or manual feedback')
@with_cancellation
async def learn_from_interaction(learning_request: LearningInteractionRequest, background_tasks: BackgroundTasks, request: Request, service: OpenAIServingContinuousLearning=Depends(get_learning_service)):
    try:
        interaction = InteractionData(interaction_id=learning_request.interaction_id or f'api_{id(learning_request)}', interaction_type=learning_request.interaction_type, input_data={'prompt': learning_request.prompt}, output_data={'response': learning_request.response}, performance_feedback=learning_request.performance_feedback, timestamp=datetime.now(), context_metadata={'source': 'api_endpoint', **(learning_request.metadata or {})}, success=True)
        result = await service.learn_from_production_interaction(interaction_data=interaction, background_tasks=background_tasks)
        return LearningResponse(success=result['success'], message=result.get('message'), data=result, timestamp=result.get('queued_at', ''), error=result.get('error'))
    except Exception as e:
        logger.error(f'Failed to process learning request: {e}')
        raise HTTPException(status_code=500, detail=str(e))
@router.post('/manual', summary='Manual Learning Trigger', description='Manually trigger learning from a specific prompt/response pair')
@with_cancellation
async def manual_learning_trigger(learning_request: LearningInteractionRequest, request: Request, service: OpenAIServingContinuousLearning=Depends(get_learning_service)):
    try:
        result = await service.trigger_manual_learning(prompt=learning_request.prompt, response=learning_request.response, performance_feedback=learning_request.performance_feedback, interaction_type=learning_request.interaction_type, metadata=learning_request.metadata)
        return LearningResponse(success=result['success'], message=result.get('message'), data=result, timestamp=result['timestamp'], error=result.get('error'))
    except Exception as e:
        logger.error(f'Failed to process manual learning: {e}')
        raise HTTPException(status_code=500, detail=str(e))
@router.post('/control', summary='Control Learning System', description='Enable, disable, or reset the continuous learning system')
@with_cancellation
async def control_learning_system(control_request: LearningControlRequest, request: Request, service: OpenAIServingContinuousLearning=Depends(get_learning_service)):
    try:
        action = control_request.action.lower()
        if action == 'enable':
            result = await service.enable_learning()
        elif action == 'disable':
            result = await service.disable_learning()
        elif action == 'reset':
            result = await service.reset_learning_state()
        else:
            raise HTTPException(status_code=400, detail=f"Invalid action '{action}'. Supported actions: enable, disable, reset")
        return LearningResponse(success=result['success'], message=result.get('message'), data=result, timestamp=result['timestamp'], error=result.get('error'))
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f'Failed to control learning system: {e}')
        raise HTTPException(status_code=500, detail=str(e))
@router.get('/health', summary='Learning Health Check', description='Health check endpoint for continuous learning system')
@with_cancellation
async def learning_health_check(request: Request, service: OpenAIServingContinuousLearning=Depends(get_learning_service)):
    try:
        status = await service.get_learning_status()
        learning_enabled = status['service_info']['learning_enabled']
        system_stats = status['learning_statistics']['system_stats']
        health_score = 1.0
        health_issues = []
        if not learning_enabled:
            health_score *= 0.5
            health_issues.append('Learning is disabled')
        interaction_count = system_stats.get('interaction_count', 0)
        if interaction_count > 0:
            success_rate = system_stats.get('metrics', {}).get('successful_adaptations', 0) / interaction_count
            if success_rate < 0.8:
                health_score *= 0.7
                health_issues.append(f'Low success rate: {success_rate:.2%}')
        if health_score >= 0.9:
            health_status = 'healthy'
        elif health_score >= 0.6:
            health_status = 'degraded'
        else:
            health_status = 'unhealthy'
        return {'status': health_status, 'health_score': health_score, 'learning_enabled': learning_enabled, 'issues': health_issues, 'summary': {'total_interactions': interaction_count, 'successful_adaptations': system_stats.get('metrics', {}).get('successful_adaptations', 0), 'current_learning_rate': system_stats.get('current_learning_rate', 0.0)}, 'timestamp': status.get('timestamp')}
    except Exception as e:
        logger.error(f'Learning health check failed: {e}')
        return {'status': 'unhealthy', 'health_score': 0.0, 'learning_enabled': False, 'issues': [f'Health check failed: {str(e)}'], 'timestamp': None}
@router.get('/config', summary='Get Learning Configuration', description='Get current continuous learning configuration')
@with_cancellation
async def get_learning_config(request: Request, service: OpenAIServingContinuousLearning=Depends(get_learning_service)):
    try:
        status = await service.get_learning_status()
        return {'server_config': status['service_info']['server_config'], 'learning_config': {'max_experiences': service.continuous_learning_system.config.max_experiences, 'replay_frequency': service.continuous_learning_system.config.replay_frequency, 'learning_rate_base': service.continuous_learning_system.config.learning_rate_base, 'enable_ewc': service.continuous_learning_system.config.enable_ewc, 'ewc_lambda': service.continuous_learning_system.config.ewc_lambda, 'current_learning_rate': service.continuous_learning_system.current_learning_rate}, 'model_info': status['model_info'], 'timestamp': status['timestamp']}
    except Exception as e:
        logger.error(f'Failed to get learning configuration: {e}')
        raise HTTPException(status_code=500, detail=str(e))
def setup_continuous_learning_routes(app, engine_client, model_config, models, request_logger=None, server_config=None):
    try:
        learning_service = OpenAIServingContinuousLearning(engine_client=engine_client, model_config=model_config, models=models, request_logger=request_logger, server_config=server_config)
        app.state.openai_serving_continuous_learning = learning_service
        app.include_router(router)
        logger.info('Continuous learning routes configured successfully')
        return learning_service
    except Exception as e:
        logger.error(f'Failed to setup continuous learning routes: {e}')
        raise
__all__ = ['router', 'setup_continuous_learning_routes']