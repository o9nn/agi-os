"""
Integration Example: Server-Side Continuous Learning with Aphrodite Engine.

Shows how to integrate the continuous learning system with the existing
OpenAI-compatible API server for production deployment.
"""

from fastapi import FastAPI
from loguru import logger

# Import continuous learning components  
from aphrodite.continuous_learning import (
    ContinuousLearningSystem,
    ContinuousLearningConfig,
    ServerSideConfig
)
from aphrodite.endpoints.middleware.continuous_learning_middleware import (
    setup_continuous_learning_middleware
)
from aphrodite.endpoints.openai.continuous_learning_routes import (
    setup_continuous_learning_routes
)


def integrate_continuous_learning_with_aphrodite_server(
    app: FastAPI,
    engine_client,
    model_config,
    serving_models,
    request_logger=None,
    enable_continuous_learning: bool = True
):
    """
    Integrate continuous learning with existing Aphrodite OpenAI server.
    
    This function demonstrates how to add continuous learning capabilities
    to the production Aphrodite server with minimal changes.
    
    Args:
        app: FastAPI application instance
        engine_client: Aphrodite engine client
        model_config: Model configuration
        serving_models: OpenAI serving models instance
        request_logger: Optional request logger
        enable_continuous_learning: Whether to enable continuous learning
    """
    
    if not enable_continuous_learning:
        logger.info("Continuous learning disabled - skipping integration")
        return None
    
    try:
        logger.info("Integrating server-side continuous learning with Aphrodite Engine...")
        
        # 1. Configure continuous learning system
        learning_config = ContinuousLearningConfig(
            max_experiences=50000,  # Large buffer for production
            replay_batch_size=16,   # Conservative batch size
            replay_frequency=20,    # Less frequent replay for stability
            learning_rate_base=0.0001,  # Conservative production learning rate
            learning_rate_decay=0.995,  # Slow decay for stability
            enable_ewc=True,            # Always enable forgetting prevention
            ewc_lambda=2000.0,          # Strong forgetting prevention
            consolidation_frequency=100, # Regular consolidation
            performance_threshold=0.7    # High threshold for production
        )
        
        # 2. Configure server-side learning
        server_config = ServerSideConfig(
            enable_request_monitoring=True,
            enable_response_feedback=True,
            background_learning_interval=60.0,  # 1 minute intervals for production
            max_concurrent_learning_tasks=2,
            enable_hot_swapping=True,
            max_learning_rate_production=0.0001,
            learning_rate_decay_production=0.995,
            enable_rollback_on_failure=True,
            min_interactions_for_learning=10,
            interaction_quality_threshold=0.6,
            enable_performance_feedback=True
        )
        
        # 3. Initialize continuous learning system
        # Note: In production, these would be properly initialized with real components
        continuous_learning_system = create_production_learning_system(
            engine_client=engine_client,
            model_config=model_config,
            learning_config=learning_config
        )
        
        # 4. Setup continuous learning middleware
        middleware = setup_continuous_learning_middleware(
            app=app,
            continuous_learning_system=continuous_learning_system,
            config=server_config
        )
        
        # 5. Setup learning API routes
        learning_service = setup_continuous_learning_routes(
            app=app,
            engine_client=engine_client,
            model_config=model_config,
            models=serving_models,
            request_logger=request_logger,
            server_config=server_config
        )
        
        # 6. Add learning status to existing health endpoints
        @app.get("/health/learning")
        async def learning_health():
            """Extended health check including learning system status."""
            return await learning_service.get_learning_status()
        
        logger.info("✅ Server-side continuous learning integration complete")
        logger.info("Available endpoints:")
        logger.info("  - GET  /v1/learning/status     - Learning system status")
        logger.info("  - GET  /v1/learning/metrics    - Detailed learning metrics")
        logger.info("  - POST /v1/learning/learn      - Learn from interaction")
        logger.info("  - POST /v1/learning/manual     - Manual learning trigger")
        logger.info("  - POST /v1/learning/control    - Control learning system")
        logger.info("  - GET  /health/learning        - Learning health check")
        
        return {
            "middleware": middleware,
            "learning_service": learning_service,
            "continuous_learning_system": continuous_learning_system
        }
        
    except Exception as e:
        logger.error(f"Failed to integrate continuous learning: {e}")
        return None


def create_production_learning_system(engine_client, model_config, learning_config):
    """
    Create production-ready continuous learning system.
    
    In a real deployment, this would initialize actual DTESN integration
    and dynamic model manager components.
    """
    
    # Import production components
    try:
        from aphrodite.dtesn_integration import DTESNDynamicIntegration
        from aphrodite.dynamic_model_manager import DynamicModelManager
        
        # Initialize with production configuration
        dtesn_config = {
            "learning_algorithms": ["stdp", "bcm", "hebbian"],
            "adaptation_strength": 0.05,  # Conservative for production
            "stability_threshold": 0.95
        }
        
        dynamic_config = {
            "update_batch_size": 16,
            "version_retention": 10,
            "rollback_enabled": True
        }
        
        dtesn_integration = DTESNDynamicIntegration(
            engine_client=engine_client,
            model_config=model_config,
            config=dtesn_config
        )
        
        dynamic_manager = DynamicModelManager(
            engine_client=engine_client,
            model_config=model_config,
            config=dynamic_config
        )
        
    except ImportError:
        # Fallback to mock components for demonstration
        logger.warning("Production components not available, using mock implementations")
        
        dtesn_integration = create_mock_dtesn_integration()
        dynamic_manager = create_mock_dynamic_manager()
    
    # Create continuous learning system
    return ContinuousLearningSystem(
        dynamic_manager=dynamic_manager,
        dtesn_integration=dtesn_integration,
        config=learning_config
    )


def create_mock_dtesn_integration():
    """Create mock DTESN integration for demonstration."""
    
    class MockDTESNIntegration:
        async def adaptive_parameter_update(self, parameter_name, current_params,
                                          target_gradient, performance_feedback):
            # Mock implementation for demonstration
            import torch
            
            # Simulate conservative production adaptation
            adaptation_rate = min(0.001, abs(performance_feedback) * 0.0005)
            updated_params = current_params + target_gradient * adaptation_rate
            
            metrics = {
                "learning_type": "production_adaptive",
                "learning_rate": adaptation_rate,
                "adaptation_strength": abs(performance_feedback) * 0.05,
                "stability_score": 0.95,
                "production_mode": True
            }
            
            return updated_params, metrics
    
    return MockDTESNIntegration()


def create_mock_dynamic_manager():
    """Create mock dynamic model manager for demonstration."""
    
    class MockDynamicModelManager:
        def __init__(self):
            self.version_count = 0
            self.update_history = []
        
        async def apply_incremental_update(self, request):
            self.version_count += 1
            update_id = f"prod_update_{self.version_count}"
            
            # Simulate production update tracking
            update_record = {
                "update_id": update_id,
                "parameter_name": request.parameter_name,
                "update_type": request.update_type,
                "learning_rate": request.learning_rate,
                "timestamp": request.metadata.get("timestamp"),
                "interaction_id": request.metadata.get("interaction_id")
            }
            
            self.update_history.append(update_record)
            
            return {
                "success": True,
                "update_id": update_id,
                "version": self.version_count,
                "production_mode": True,
                "rollback_available": len(self.update_history) > 1
            }
    
    return MockDynamicModelManager()


# Example integration with existing Aphrodite server setup
def example_server_setup_with_continuous_learning():
    """
    Example showing how to modify existing Aphrodite server setup
    to include continuous learning capabilities.
    """
    
    # This would be part of the existing api_server.py setup
    
    async def init_app_state_with_learning(
        engine_client,
        aphrodite_config, 
        state,
        args
    ):
        """
        Enhanced version of init_app_state that includes continuous learning.
        
        This shows how the existing server initialization would be modified
        to include continuous learning capabilities.
        """
        
        # ... existing server initialization code ...
        
        # Add continuous learning integration
        if getattr(args, 'enable_continuous_learning', False):
            logger.info("Initializing continuous learning integration...")
            
            # Get FastAPI app reference
            app = state.app  # Assuming app is stored in state
            
            # Setup continuous learning
            learning_components = integrate_continuous_learning_with_aphrodite_server(
                app=app,
                engine_client=engine_client,
                model_config=aphrodite_config.model_config,
                serving_models=state.openai_serving_models,
                request_logger=state.request_logger if hasattr(state, 'request_logger') else None,
                enable_continuous_learning=True
            )
            
            if learning_components:
                # Store learning components in app state
                state.continuous_learning_middleware = learning_components["middleware"]
                state.continuous_learning_service = learning_components["learning_service"] 
                state.continuous_learning_system = learning_components["continuous_learning_system"]
                
                logger.info("✅ Continuous learning integration successful")
            else:
                logger.warning("⚠️ Continuous learning integration failed")
        
        # ... rest of existing initialization ...


# Configuration example for production deployment
PRODUCTION_CONTINUOUS_LEARNING_CONFIG = {
    "enable_continuous_learning": True,
    "learning_config": {
        "max_experiences": 100000,
        "replay_batch_size": 8,
        "replay_frequency": 30,
        "learning_rate_base": 0.00005,
        "enable_ewc": True,
        "ewc_lambda": 5000.0
    },
    "server_config": {
        "background_learning_interval": 120.0,  # 2 minutes
        "max_learning_rate_production": 0.00005,
        "enable_rollback_on_failure": True,
        "min_interactions_for_learning": 20,
        "interaction_quality_threshold": 0.7
    }
}

# Command line arguments that would be added to Aphrodite server
CONTINUOUS_LEARNING_CLI_ARGS = [
    "--enable-continuous-learning",
    "--continuous-learning-config", "production_learning_config.json",
    "--learning-background-interval", "120",
    "--learning-max-rate", "0.00005",
    "--learning-quality-threshold", "0.7"
]


if __name__ == "__main__":
    print("Server-Side Continuous Learning Integration Example")
    print("=" * 60)
    print()
    print("This example shows how to integrate continuous learning")
    print("with the existing Aphrodite OpenAI-compatible server.")
    print()
    print("Key integration points:")
    print("  1. FastAPI middleware for request/response monitoring")
    print("  2. Background learning processes with safety constraints")
    print("  3. OpenAI-compatible API endpoints for learning management")
    print("  4. Production-ready configuration and monitoring")
    print()
    print("To enable in production:")
    print("  - Add continuous learning arguments to server CLI")
    print("  - Modify init_app_state to include learning setup")
    print("  - Configure production safety parameters")
    print("  - Enable monitoring and observability endpoints")
    print()
    print("Result: Zero-downtime continuous model improvement from production data!")