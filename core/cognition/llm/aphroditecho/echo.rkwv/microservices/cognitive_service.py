#!/usr/bin/env python3
"""
Cognitive Processing Microservice for Deep Tree Echo Architecture

Provides distributed cognitive processing with horizontal scaling,
integrating DTESN membrane computing, Agent-Arena-Relation (AAR),
and Echo-Self evolution capabilities.

Features:
- Parallel membrane processing (Memory, Reasoning, Grammar)
- Session state management across instances  
- Performance metrics and monitoring
- Integration with cache service
- DTESN reservoir dynamics processing
- Agent capability evaluation and routing
"""

import asyncio
import time
import logging
import json
import os
import sys
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
from enum import Enum
from aiohttp import web, ClientSession
import aioredis
import psutil
import hashlib

# Add echo.kern to path for DTESN integration
sys.path.append('/home/runner/work/aphroditecho/aphroditecho/echo.kern')
sys.path.append('/home/runner/work/aphroditecho/aphroditecho')

logger = logging.getLogger(__name__)


class ProcessingType(Enum):
    """Types of cognitive processing"""
    MEMORY_RETRIEVAL = "memory_retrieval"
    REASONING = "reasoning"
    GRAMMAR_ANALYSIS = "grammar_analysis"
    MEMBRANE_EVOLUTION = "membrane_evolution"
    AGENT_EVALUATION = "agent_evaluation"
    MULTI_MODAL = "multi_modal"


class SessionState(Enum):
    """Session state tracking"""
    ACTIVE = "active"
    IDLE = "idle"
    PROCESSING = "processing"
    ERROR = "error"
    EXPIRED = "expired"


@dataclass
class CognitiveSession:
    """Cognitive processing session"""
    session_id: str
    created_at: float
    last_activity: float
    state: SessionState = SessionState.ACTIVE
    context: Dict[str, Any] = field(default_factory=dict)
    memory_state: Dict[str, Any] = field(default_factory=dict)
    processing_history: List[Dict[str, Any]] = field(default_factory=list)
    performance_metrics: Dict[str, float] = field(default_factory=dict)
    
    @property
    def age_seconds(self) -> float:
        return time.time() - self.created_at
    
    @property
    def idle_seconds(self) -> float:
        return time.time() - self.last_activity
    
    def touch(self):
        """Update last activity timestamp"""
        self.last_activity = time.time()


@dataclass
class ProcessingRequest:
    """Cognitive processing request"""
    request_id: str
    session_id: str
    processing_type: ProcessingType
    input_data: Dict[str, Any]
    context: Dict[str, Any] = field(default_factory=dict)
    priority: int = 0  # Higher values = higher priority
    timeout_seconds: int = 30


@dataclass
class ProcessingResult:
    """Cognitive processing result"""
    request_id: str
    session_id: str
    processing_type: ProcessingType
    success: bool
    result_data: Dict[str, Any] = field(default_factory=dict)
    error_message: Optional[str] = None
    processing_time_ms: float = 0.0
    memory_usage_mb: float = 0.0


class CognitiveService:
    """
    Distributed cognitive processing service with DTESN integration
    """
    
    def __init__(self,
                 port: int = 8001,
                 max_concurrent_sessions: int = 50,
                 session_timeout_seconds: int = 3600,  # 1 hour
                 enable_caching: bool = True,
                 cache_ttl_seconds: int = 300):
        
        self.port = port
        self.max_concurrent_sessions = max_concurrent_sessions
        self.session_timeout = session_timeout_seconds
        self.enable_caching = enable_caching
        self.cache_ttl = cache_ttl_seconds
        
        # Session management
        self.sessions: Dict[str, CognitiveSession] = {}
        self.processing_queue = asyncio.Queue()
        
        # Service connections
        self.redis: Optional[aioredis.Redis] = None
        self.cache_service_url: Optional[str] = None
        self.load_balancer_url: Optional[str] = None
        
        # DTESN Integration components
        self.dtesn_system = None
        self.performance_monitor = None
        self.agent_manager = None
        
        # Processing workers
        self.worker_tasks: List[asyncio.Task] = []
        self.num_workers = min(4, max(1, psutil.cpu_count() or 1))
        
        # Background tasks
        self.session_cleanup_task: Optional[asyncio.Task] = None
        self.metrics_task: Optional[asyncio.Task] = None
        self.registration_task: Optional[asyncio.Task] = None
        
        # Metrics
        self.metrics = {
            'total_requests': 0,
            'successful_requests': 0,
            'failed_requests': 0,
            'average_processing_time': 0.0,
            'active_sessions': 0,
            'cache_hits': 0,
            'cache_misses': 0
        }
        self.processing_times = []

    async def initialize(self):
        """Initialize the cognitive service"""
        logger.info("Initializing Cognitive Processing Service...")
        
        # Connect to Redis for session state
        redis_url = os.getenv('REDIS_URL', 'redis://localhost:6379')
        try:
            self.redis = aioredis.from_url(redis_url, decode_responses=True)
            await self.redis.ping()
            logger.info("✅ Connected to Redis")
        except Exception as e:
            logger.warning(f"Redis connection failed: {e}, continuing without distributed sessions")
        
        # Service URLs from environment
        self.cache_service_url = os.getenv('CACHE_SERVICE_URL', 'http://cache-service:8002')
        self.load_balancer_url = os.getenv('LOAD_BALANCER_URL', 'http://load-balancer:8000')
        
        # Initialize DTESN components
        await self._initialize_dtesn_components()
        
        # Start processing workers
        for i in range(self.num_workers):
            task = asyncio.create_task(self._processing_worker(f"worker-{i}"))
            self.worker_tasks.append(task)
        
        # Start background tasks
        self.session_cleanup_task = asyncio.create_task(self._session_cleanup_loop())
        self.metrics_task = asyncio.create_task(self._metrics_collection_loop())
        self.registration_task = asyncio.create_task(self._service_registration_loop())
        
        logger.info(f"Cognitive Processing Service initialized with {self.num_workers} workers")

    async def shutdown(self):
        """Shutdown the cognitive service"""
        logger.info("Shutting down Cognitive Processing Service...")
        
        # Cancel all tasks
        all_tasks = (self.worker_tasks + 
                    [self.session_cleanup_task, self.metrics_task, self.registration_task])
        
        for task in all_tasks:
            if task:
                task.cancel()
        
        # Wait for all tasks to complete
        await asyncio.gather(*all_tasks, return_exceptions=True)
        
        # Close connections
        if self.redis:
            await self.redis.close()
        
        logger.info("Cognitive Processing Service shut down")

    async def _initialize_dtesn_components(self):
        """Initialize DTESN integration components"""
        try:
            # Import DTESN components
            from dtesn_integration import DTESNIntegratedSystem, DTESNConfiguration, DTESNIntegrationMode
            from performance_monitor import PerformanceMonitor
            
            # Create DTESN system for cognitive processing
            config = DTESNConfiguration(
                reservoir_size=100,
                max_membrane_depth=3,
                membranes_per_level=[1, 2, 4],
                integration_mode=DTESNIntegrationMode.MEMBRANE_COUPLED,
                coupling_strength=0.3
            )
            
            self.dtesn_system = DTESNIntegratedSystem(config)
            logger.info("✅ DTESN system initialized")
            
            # Initialize performance monitor
            self.performance_monitor = PerformanceMonitor()
            logger.info("✅ Performance monitor initialized")
            
        except ImportError as e:
            logger.warning(f"DTESN components not available: {e}")
        except Exception as e:
            logger.error(f"Failed to initialize DTESN components: {e}")

    async def create_session(self, session_id: Optional[str] = None) -> str:
        """Create a new cognitive processing session"""
        if session_id is None:
            session_id = hashlib.md5(f"{time.time()}_{os.urandom(8)}".encode()).hexdigest()
        
        if len(self.sessions) >= self.max_concurrent_sessions:
            # Clean up expired sessions first
            await self._cleanup_expired_sessions()
            
            if len(self.sessions) >= self.max_concurrent_sessions:
                raise ValueError("Maximum concurrent sessions reached")
        
        session = CognitiveSession(
            session_id=session_id,
            created_at=time.time(),
            last_activity=time.time()
        )
        
        self.sessions[session_id] = session
        self.metrics['active_sessions'] = len(self.sessions)
        
        # Store in Redis if available
        if self.redis:
            try:
                session_data = {
                    'created_at': session.created_at,
                    'last_activity': session.last_activity,
                    'state': session.state.value
                }
                await self.redis.setex(
                    f"cognitive_session:{session_id}",
                    self.session_timeout,
                    json.dumps(session_data)
                )
            except Exception as e:
                logger.error(f"Failed to store session in Redis: {e}")
        
        logger.info(f"Created cognitive session: {session_id}")
        return session_id

    async def get_session(self, session_id: str) -> Optional[CognitiveSession]:
        """Get cognitive processing session"""
        session = self.sessions.get(session_id)
        
        if session and session.state != SessionState.EXPIRED:
            session.touch()
            return session
        
        # Try to restore from Redis
        if self.redis and session_id not in self.sessions:
            try:
                session_data = await self.redis.get(f"cognitive_session:{session_id}")
                if session_data:
                    data = json.loads(session_data)
                    session = CognitiveSession(
                        session_id=session_id,
                        created_at=data['created_at'],
                        last_activity=time.time(),
                        state=SessionState(data['state'])
                    )
                    self.sessions[session_id] = session
                    return session
            except Exception as e:
                logger.error(f"Failed to restore session from Redis: {e}")
        
        return None

    async def process_request(self, request: ProcessingRequest) -> ProcessingResult:
        """Queue a cognitive processing request"""
        self.metrics['total_requests'] += 1
        
        # Get or create session
        session = await self.get_session(request.session_id)
        if not session:
            session_id = await self.create_session(request.session_id)
            session = await self.get_session(session_id)
        
        if not session:
            self.metrics['failed_requests'] += 1
            return ProcessingResult(
                request_id=request.request_id,
                session_id=request.session_id,
                processing_type=request.processing_type,
                success=False,
                error_message="Failed to create or retrieve session"
            )
        
        session.state = SessionState.PROCESSING
        
        # Check cache first if enabled
        if self.enable_caching:
            cached_result = await self._check_cache(request)
            if cached_result:
                self.metrics['cache_hits'] += 1
                self.metrics['successful_requests'] += 1
                session.state = SessionState.ACTIVE
                return cached_result
            else:
                self.metrics['cache_misses'] += 1
        
        # Queue for processing
        result_future = asyncio.Future()
        await self.processing_queue.put((request, session, result_future))
        
        # Wait for result with timeout
        try:
            result = await asyncio.wait_for(result_future, timeout=request.timeout_seconds)
            
            # Cache result if successful
            if result.success and self.enable_caching:
                await self._cache_result(request, result)
            
            if result.success:
                self.metrics['successful_requests'] += 1
            else:
                self.metrics['failed_requests'] += 1
            
            session.state = SessionState.ACTIVE
            return result
            
        except asyncio.TimeoutError:
            self.metrics['failed_requests'] += 1
            session.state = SessionState.ERROR
            return ProcessingResult(
                request_id=request.request_id,
                session_id=request.session_id,
                processing_type=request.processing_type,
                success=False,
                error_message="Processing timeout"
            )

    async def _processing_worker(self, worker_id: str):
        """Cognitive processing worker"""
        logger.info(f"Started processing worker: {worker_id}")
        
        while True:
            try:
                # Get next request from queue
                request, session, result_future = await self.processing_queue.get()
                
                if result_future.done():
                    continue
                
                start_time = time.time()
                memory_before = psutil.Process().memory_info().rss / 1024 / 1024
                
                try:
                    # Process the request
                    result_data = await self._execute_cognitive_processing(request, session)
                    
                    processing_time = (time.time() - start_time) * 1000  # Convert to ms
                    memory_after = psutil.Process().memory_info().rss / 1024 / 1024
                    memory_used = memory_after - memory_before
                    
                    # Update metrics
                    self.processing_times.append(processing_time)
                    if len(self.processing_times) > 1000:
                        self.processing_times = self.processing_times[-1000:]
                    
                    self.metrics['average_processing_time'] = sum(self.processing_times) / len(self.processing_times)
                    
                    # Create successful result
                    result = ProcessingResult(
                        request_id=request.request_id,
                        session_id=request.session_id,
                        processing_type=request.processing_type,
                        success=True,
                        result_data=result_data,
                        processing_time_ms=processing_time,
                        memory_usage_mb=memory_used
                    )
                    
                    # Update session
                    session.processing_history.append({
                        'request_id': request.request_id,
                        'processing_type': request.processing_type.value,
                        'processing_time_ms': processing_time,
                        'timestamp': time.time()
                    })
                    
                    session.performance_metrics['last_processing_time'] = processing_time
                    session.performance_metrics['average_processing_time'] = sum(
                        h.get('processing_time_ms', 0) for h in session.processing_history[-10:]
                    ) / min(10, len(session.processing_history))
                    
                except Exception as e:
                    logger.error(f"Processing error in {worker_id}: {e}")
                    
                    result = ProcessingResult(
                        request_id=request.request_id,
                        session_id=request.session_id,
                        processing_type=request.processing_type,
                        success=False,
                        error_message=str(e),
                        processing_time_ms=(time.time() - start_time) * 1000
                    )
                    
                    session.state = SessionState.ERROR
                
                # Return result
                if not result_future.done():
                    result_future.set_result(result)
                
            except asyncio.CancelledError:
                logger.info(f"Processing worker {worker_id} cancelled")
                break
            except Exception as e:
                logger.error(f"Unexpected error in worker {worker_id}: {e}")
                await asyncio.sleep(1)

    async def _execute_cognitive_processing(self, request: ProcessingRequest, session: CognitiveSession) -> Dict[str, Any]:
        """Execute cognitive processing based on request type"""
        processing_type = request.processing_type
        input_data = request.input_data
        context = {**session.context, **request.context}
        
        if processing_type == ProcessingType.MEMORY_RETRIEVAL:
            return await self._process_memory_retrieval(input_data, context, session)
        
        elif processing_type == ProcessingType.REASONING:
            return await self._process_reasoning(input_data, context, session)
        
        elif processing_type == ProcessingType.GRAMMAR_ANALYSIS:
            return await self._process_grammar_analysis(input_data, context, session)
        
        elif processing_type == ProcessingType.MEMBRANE_EVOLUTION:
            return await self._process_membrane_evolution(input_data, context, session)
        
        elif processing_type == ProcessingType.AGENT_EVALUATION:
            return await self._process_agent_evaluation(input_data, context, session)
        
        elif processing_type == ProcessingType.MULTI_MODAL:
            return await self._process_multi_modal(input_data, context, session)
        
        else:
            raise ValueError(f"Unsupported processing type: {processing_type}")

    async def _process_memory_retrieval(self, input_data: Dict[str, Any], context: Dict[str, Any], session: CognitiveSession) -> Dict[str, Any]:
        """Process memory retrieval request"""
        query = input_data.get('query', '')
        memory_type = input_data.get('memory_type', 'semantic')
        
        # Simulate memory retrieval with session state
        retrieved_memories = []
        
        # Check session memory first
        session_memories = session.memory_state.get(memory_type, [])
        for memory in session_memories:
            if query.lower() in memory.get('content', '').lower():
                retrieved_memories.append(memory)
        
        # DTESN integration if available
        if self.dtesn_system:
            try:
                # Use DTESN for enhanced memory retrieval
                import numpy as np
                query_vector = np.array([hash(query) % 256 for _ in range(10)], dtype=np.float32)
                query_vector = query_vector / np.linalg.norm(query_vector)
                
                dtesn_result = self.dtesn_system.process_input(query_vector)
                
                # Convert DTESN output to memory format
                if dtesn_result and 'membrane_outputs' in dtesn_result:
                    for membrane_id, output in dtesn_result['membrane_outputs'].items():
                        if np.any(output > 0.5):  # Threshold for relevance
                            retrieved_memories.append({
                                'source': 'dtesn_membrane',
                                'membrane_id': membrane_id,
                                'content': f"DTESN retrieval: {membrane_id}",
                                'relevance_score': float(np.max(output)),
                                'vector_output': output.tolist()
                            })
                
            except Exception as e:
                logger.error(f"DTESN memory retrieval error: {e}")
        
        return {
            'query': query,
            'memory_type': memory_type,
            'retrieved_memories': retrieved_memories,
            'total_found': len(retrieved_memories)
        }

    async def _process_reasoning(self, input_data: Dict[str, Any], context: Dict[str, Any], session: CognitiveSession) -> Dict[str, Any]:
        """Process reasoning request"""
        problem = input_data.get('problem', '')
        reasoning_type = input_data.get('reasoning_type', 'logical')
        
        # Simulate reasoning process
        reasoning_steps = []
        confidence_score = 0.8
        
        # Add reasoning steps based on type
        if reasoning_type == 'logical':
            reasoning_steps = [
                "Analyze problem premises",
                "Identify logical relationships", 
                "Apply deductive reasoning",
                "Validate conclusion"
            ]
        elif reasoning_type == 'causal':
            reasoning_steps = [
                "Identify cause-effect relationships",
                "Analyze temporal sequences",
                "Evaluate causal strength",
                "Generate causal model"
            ]
        
        # DTESN integration for enhanced reasoning
        if self.dtesn_system:
            try:
                import numpy as np
                problem_vector = np.array([hash(problem) % 256 for _ in range(20)], dtype=np.float32)
                problem_vector = problem_vector / np.linalg.norm(problem_vector)
                
                dtesn_result = self.dtesn_system.process_input(problem_vector)
                
                if dtesn_result and 'cognitive_output' in dtesn_result:
                    cognitive_output = dtesn_result['cognitive_output']
                    confidence_score = min(1.0, float(np.mean(cognitive_output)) * 1.2)
                    
                    reasoning_steps.append({
                        'step': 'DTESN cognitive processing',
                        'confidence': confidence_score,
                        'membrane_activations': dtesn_result.get('membrane_outputs', {})
                    })
            
            except Exception as e:
                logger.error(f"DTESN reasoning error: {e}")
        
        return {
            'problem': problem,
            'reasoning_type': reasoning_type,
            'reasoning_steps': reasoning_steps,
            'confidence_score': confidence_score,
            'solution': f"Processed solution for: {problem[:50]}..."
        }

    async def _process_grammar_analysis(self, input_data: Dict[str, Any], context: Dict[str, Any], session: CognitiveSession) -> Dict[str, Any]:
        """Process grammar analysis request"""
        text = input_data.get('text', '')
        analysis_type = input_data.get('analysis_type', 'syntax')
        
        # Simple grammar analysis simulation
        analysis_result = {
            'text': text,
            'analysis_type': analysis_type,
            'word_count': len(text.split()),
            'sentence_count': len([s for s in text.split('.') if s.strip()]),
            'grammar_score': 0.85,
            'detected_issues': []
        }
        
        # Add some basic grammar checks
        words = text.split()
        if len(words) > 0:
            if words[0].islower():
                analysis_result['detected_issues'].append("First word should be capitalized")
            
            if not text.strip().endswith(('.', '!', '?')):
                analysis_result['detected_issues'].append("Missing sentence ending punctuation")
        
        return analysis_result

    async def _process_membrane_evolution(self, input_data: Dict[str, Any], context: Dict[str, Any], session: CognitiveSession) -> Dict[str, Any]:
        """Process membrane evolution request using DTESN"""
        evolution_input = input_data.get('evolution_input', {})
        membrane_id = input_data.get('membrane_id', 'default')
        
        result = {
            'membrane_id': membrane_id,
            'evolution_successful': False,
            'evolution_metrics': {}
        }
        
        if self.dtesn_system:
            try:
                import numpy as np
                
                # Create evolution input vector
                if isinstance(evolution_input, dict):
                    input_values = list(evolution_input.values())
                else:
                    input_values = [evolution_input] if not isinstance(evolution_input, list) else evolution_input
                
                # Convert to numpy array
                input_vector = np.array([float(v) if isinstance(v, (int, float)) else hash(str(v)) % 256 
                                       for v in input_values[:20]], dtype=np.float32)
                
                if len(input_vector) < 20:
                    input_vector = np.pad(input_vector, (0, 20 - len(input_vector)))
                
                input_vector = input_vector / (np.linalg.norm(input_vector) + 1e-8)
                
                # Process through DTESN
                dtesn_result = self.dtesn_system.process_input(input_vector)
                
                if dtesn_result:
                    result.update({
                        'evolution_successful': True,
                        'evolution_metrics': {
                            'membrane_outputs': {k: v.tolist() if hasattr(v, 'tolist') else v 
                                               for k, v in dtesn_result.get('membrane_outputs', {}).items()},
                            'reservoir_states': {k: v.tolist() if hasattr(v, 'tolist') else v 
                                               for k, v in dtesn_result.get('reservoir_states', {}).items()},
                            'oeis_compliant': dtesn_result.get('oeis_compliant', False),
                            'processing_time_ms': dtesn_result.get('processing_time_ms', 0)
                        }
                    })
            
            except Exception as e:
                logger.error(f"Membrane evolution error: {e}")
                result['error'] = str(e)
        
        return result

    async def _process_agent_evaluation(self, input_data: Dict[str, Any], context: Dict[str, Any], session: CognitiveSession) -> Dict[str, Any]:
        """Process agent evaluation request"""
        agent_data = input_data.get('agent_data', {})
        evaluation_criteria = input_data.get('criteria', ['performance', 'efficiency', 'collaboration'])
        
        # Simulate agent evaluation
        evaluation_scores = {}
        for criterion in evaluation_criteria:
            evaluation_scores[criterion] = min(1.0, max(0.0, 0.5 + (hash(str(agent_data) + criterion) % 100) / 200))
        
        overall_score = sum(evaluation_scores.values()) / len(evaluation_scores)
        
        return {
            'agent_data': agent_data,
            'evaluation_criteria': evaluation_criteria,
            'individual_scores': evaluation_scores,
            'overall_score': overall_score,
            'recommendation': 'suitable' if overall_score > 0.7 else 'needs_improvement'
        }

    async def _process_multi_modal(self, input_data: Dict[str, Any], context: Dict[str, Any], session: CognitiveSession) -> Dict[str, Any]:
        """Process multi-modal request"""
        modalities = input_data.get('modalities', [])
        fusion_method = input_data.get('fusion_method', 'average')
        
        # Simulate multi-modal processing
        modal_results = {}
        for modality in modalities:
            modal_data = input_data.get(f'{modality}_data', {})
            modal_results[modality] = {
                'processed': True,
                'confidence': 0.7 + (hash(str(modal_data)) % 30) / 100,
                'features': f"Features extracted from {modality}"
            }
        
        # Fusion simulation
        if fusion_method == 'average':
            fused_confidence = sum(result['confidence'] for result in modal_results.values()) / len(modal_results)
        else:
            fused_confidence = max(result['confidence'] for result in modal_results.values()) if modal_results else 0.0
        
        return {
            'modalities': modalities,
            'fusion_method': fusion_method,
            'modal_results': modal_results,
            'fused_confidence': fused_confidence,
            'multi_modal_output': f"Fused result with {len(modalities)} modalities"
        }

    async def _check_cache(self, request: ProcessingRequest) -> Optional[ProcessingResult]:
        """Check cache for existing result"""
        if not self.cache_service_url:
            return None
        
        # Create cache key
        cache_key = hashlib.md5(
            f"{request.processing_type.value}:{json.dumps(request.input_data, sort_keys=True)}"
            .encode()
        ).hexdigest()
        
        try:
            async with ClientSession() as session:
                async with session.post(
                    f"{self.cache_service_url}/get",
                    json={'key': cache_key}
                ) as response:
                    if response.status == 200:
                        data = await response.json()
                        if data.get('found'):
                            cached_data = data['value']
                            return ProcessingResult(
                                request_id=request.request_id,
                                session_id=request.session_id,
                                processing_type=request.processing_type,
                                success=True,
                                result_data=cached_data,
                                processing_time_ms=0.0  # Cached result
                            )
        except Exception as e:
            logger.error(f"Cache check error: {e}")
        
        return None

    async def _cache_result(self, request: ProcessingRequest, result: ProcessingResult):
        """Cache processing result"""
        if not self.cache_service_url or not result.success:
            return
        
        # Create cache key
        cache_key = hashlib.md5(
            f"{request.processing_type.value}:{json.dumps(request.input_data, sort_keys=True)}"
            .encode()
        ).hexdigest()
        
        try:
            async with ClientSession() as session:
                async with session.post(
                    f"{self.cache_service_url}/put",
                    json={
                        'key': cache_key,
                        'value': result.result_data,
                        'ttl_seconds': self.cache_ttl,
                        'tags': [request.processing_type.value, 'cognitive_result']
                    }
                ) as response:
                    if response.status != 200:
                        logger.warning(f"Failed to cache result: HTTP {response.status}")
        except Exception as e:
            logger.error(f"Cache put error: {e}")

    async def _session_cleanup_loop(self):
        """Background session cleanup loop"""
        while True:
            try:
                await self._cleanup_expired_sessions()
                await asyncio.sleep(300)  # Clean up every 5 minutes
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Session cleanup error: {e}")
                await asyncio.sleep(300)

    async def _cleanup_expired_sessions(self):
        """Clean up expired sessions"""
        current_time = time.time()
        expired_sessions = []
        
        for session_id, session in self.sessions.items():
            if (current_time - session.last_activity > self.session_timeout or
                session.state == SessionState.EXPIRED):
                expired_sessions.append(session_id)
        
        for session_id in expired_sessions:
            del self.sessions[session_id]
            logger.info(f"Cleaned up expired session: {session_id}")
        
        self.metrics['active_sessions'] = len(self.sessions)

    async def _metrics_collection_loop(self):
        """Background metrics collection loop"""
        while True:
            try:
                await self._collect_system_metrics()
                await asyncio.sleep(30)  # Collect metrics every 30 seconds
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Metrics collection error: {e}")
                await asyncio.sleep(30)

    async def _collect_system_metrics(self):
        """Collect system metrics"""
        # System metrics
        cpu_percent = psutil.cpu_percent()
        memory = psutil.virtual_memory()
        
        metrics = {
            'timestamp': time.time(),
            'cognitive_service': {
                'cpu_usage': cpu_percent,
                'memory_usage': memory.percent,
                **self.metrics,
                'processing_queue_size': self.processing_queue.qsize()
            }
        }
        
        if self.redis:
            try:
                await self.redis.setex(
                    f'cognitive_metrics:{self.port}',
                    300,  # 5 minute expiry
                    json.dumps(metrics)
                )
            except Exception as e:
                logger.error(f"Failed to store metrics: {e}")

    async def _service_registration_loop(self):
        """Background service registration with load balancer"""
        while True:
            try:
                await self._register_with_load_balancer()
                await asyncio.sleep(60)  # Register every minute
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Service registration error: {e}")
                await asyncio.sleep(60)

    async def _register_with_load_balancer(self):
        """Register this service instance with the load balancer"""
        if not self.load_balancer_url:
            return
        
        # This would send registration data to the load balancer
        # In the actual load balancer, we'd have a registration endpoint
        logger.debug("Service registration would happen here")

    # HTTP API endpoints
    async def health_handler(self, request):
        """Health check endpoint"""
        cpu_percent = psutil.cpu_percent()
        memory = psutil.virtual_memory()
        
        return web.json_response({
            'status': 'healthy',
            'timestamp': time.time(),
            'cpu_usage': cpu_percent,
            'memory_usage': memory.percent,
            'active_sessions': len(self.sessions),
            'queue_size': self.processing_queue.qsize(),
            'dtesn_available': self.dtesn_system is not None
        })

    async def metrics_handler(self, request):
        """Metrics endpoint"""
        return web.json_response({
            'timestamp': time.time(),
            'metrics': self.metrics,
            'sessions': {
                'total': len(self.sessions),
                'by_state': {
                    state.value: sum(1 for s in self.sessions.values() if s.state == state)
                    for state in SessionState
                }
            },
            'system': {
                'cpu_usage': psutil.cpu_percent(),
                'memory_usage': psutil.virtual_memory().percent,
                'queue_size': self.processing_queue.qsize()
            }
        })

    async def process_handler(self, request):
        """Main processing endpoint"""
        try:
            data = await request.json()
            
            # Create processing request
            processing_request = ProcessingRequest(
                request_id=data.get('request_id', f"req_{int(time.time() * 1000)}"),
                session_id=data.get('session_id', 'default'),
                processing_type=ProcessingType(data.get('processing_type', 'reasoning')),
                input_data=data.get('input_data', {}),
                context=data.get('context', {}),
                priority=data.get('priority', 0),
                timeout_seconds=data.get('timeout_seconds', 30)
            )
            
            # Process the request
            result = await self.process_request(processing_request)
            
            # Return result
            return web.json_response({
                'request_id': result.request_id,
                'session_id': result.session_id,
                'processing_type': result.processing_type.value,
                'success': result.success,
                'result_data': result.result_data,
                'error_message': result.error_message,
                'processing_time_ms': result.processing_time_ms,
                'memory_usage_mb': result.memory_usage_mb
            })
        
        except Exception as e:
            logger.error(f"Process handler error: {e}")
            return web.json_response({'error': 'Internal server error'}, status=500)

    async def session_handler(self, request):
        """Session management endpoint"""
        try:
            if request.method == 'POST':
                # Create session
                data = await request.json() if request.content_length else {}
                session_id = await self.create_session(data.get('session_id'))
                return web.json_response({'session_id': session_id})
            
            elif request.method == 'GET':
                # Get session info
                session_id = request.query.get('session_id')
                if not session_id:
                    return web.json_response({'error': 'session_id required'}, status=400)
                
                session = await self.get_session(session_id)
                if not session:
                    return web.json_response({'error': 'Session not found'}, status=404)
                
                return web.json_response({
                    'session_id': session.session_id,
                    'created_at': session.created_at,
                    'last_activity': session.last_activity,
                    'state': session.state.value,
                    'age_seconds': session.age_seconds,
                    'processing_history_count': len(session.processing_history),
                    'performance_metrics': session.performance_metrics
                })
        
        except Exception as e:
            logger.error(f"Session handler error: {e}")
            return web.json_response({'error': 'Internal server error'}, status=500)

    def create_app(self):
        """Create the web application"""
        app = web.Application()
        app.router.add_get('/health', self.health_handler)
        app.router.add_get('/metrics', self.metrics_handler)
        app.router.add_post('/process', self.process_handler)
        app.router.add_post('/session', self.session_handler)
        app.router.add_get('/session', self.session_handler)
        return app

    async def run(self):
        """Run the cognitive service"""
        await self.initialize()
        
        app = self.create_app()
        runner = web.AppRunner(app)
        await runner.setup()
        
        site = web.TCPSite(runner, '0.0.0.0', self.port)
        await site.start()
        
        logger.info(f"🚀 Cognitive Processing Service running on port {self.port}")
        logger.info(f"Max concurrent sessions: {self.max_concurrent_sessions}")
        logger.info(f"Workers: {self.num_workers}")
        logger.info(f"DTESN integration: {'enabled' if self.dtesn_system else 'disabled'}")
        logger.info(f"Caching: {'enabled' if self.enable_caching else 'disabled'}")
        
        try:
            await asyncio.Future()  # Run forever
        finally:
            await self.shutdown()


async def main():
    """Main entry point"""
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    # Configuration from environment
    port = int(os.getenv('COGNITIVE_SERVICE_PORT', 8001))
    max_sessions = int(os.getenv('MAX_CONCURRENT_SESSIONS', 50))
    enable_caching = os.getenv('ENABLE_CACHING', 'true').lower() == 'true'
    cache_ttl = int(os.getenv('CACHE_TTL_SECONDS', 300))
    
    # Create and run cognitive service
    cognitive_service = CognitiveService(
        port=port,
        max_concurrent_sessions=max_sessions,
        enable_caching=enable_caching,
        cache_ttl_seconds=cache_ttl
    )
    
    await cognitive_service.run()


if __name__ == '__main__':
    asyncio.run(main())