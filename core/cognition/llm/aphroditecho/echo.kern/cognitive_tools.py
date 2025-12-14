#!/usr/bin/env python3
"""
Cognitive Tools Implementation - Basic Tools for Extended Mind System

This module provides concrete implementations of cognitive tools that integrate
with the Extended Mind System for cognitive scaffolding.
"""

import time
import asyncio
import numpy as np
from typing import Dict, List, Any, Tuple

from extended_mind_system import (
    CognitiveTool, ToolInterface, CognitiveTask, ToolType
)

class MemoryStoreTool(ToolInterface):
    """
    External memory storage tool for cognitive offloading.
    
    Provides persistent storage and retrieval of cognitive artifacts.
    """
    
    def __init__(self, storage_capacity: int = 10000):
        """
        Initialize memory store tool.
        
        Args:
            storage_capacity: Maximum number of items to store
        """
        self.storage: Dict[str, Any] = {}
        self.storage_capacity = storage_capacity
        self.access_count: Dict[str, int] = {}
        self.creation_time: Dict[str, float] = {}
    
    async def execute(self, task: CognitiveTask, parameters: Dict[str, Any]) -> Any:
        """Execute memory store operation."""
        operation = parameters.get('operation', 'retrieve')
        
        if operation == 'store':
            return await self._store_item(
                parameters.get('key'), 
                parameters.get('value'),
                parameters.get('metadata', {})
            )
        elif operation == 'retrieve':
            return await self._retrieve_item(
                parameters.get('key'),
                parameters.get('query', {})
            )
        elif operation == 'search':
            return await self._search_items(
                parameters.get('query', {}),
                parameters.get('max_results', 10)
            )
        else:
            raise ValueError(f"Unknown memory operation: {operation}")
    
    async def _store_item(self, key: str, value: Any, metadata: Dict[str, Any]) -> Dict[str, Any]:
        """Store item in external memory."""
        # Check capacity
        if len(self.storage) >= self.storage_capacity:
            await self._evict_least_used()
        
        # Store item with metadata
        item = {
            'value': value,
            'metadata': metadata,
            'stored_at': time.time()
        }
        
        self.storage[key] = item
        self.access_count[key] = 0
        self.creation_time[key] = time.time()
        
        # Simulate storage latency
        await asyncio.sleep(0.01)
        
        return {'status': 'stored', 'key': key, 'size': len(str(value))}
    
    async def _retrieve_item(self, key: str, query: Dict[str, Any]) -> Any:
        """Retrieve item from external memory."""
        if key in self.storage:
            self.access_count[key] += 1
            
            # Simulate retrieval latency based on age
            age = time.time() - self.creation_time[key]
            latency = 0.001 + min(age / 1000, 0.05)  # Max 50ms latency
            await asyncio.sleep(latency)
            
            return {
                'status': 'found',
                'value': self.storage[key]['value'],
                'metadata': self.storage[key]['metadata'],
                'access_count': self.access_count[key]
            }
        else:
            return {'status': 'not_found', 'key': key}
    
    async def _search_items(self, query: Dict[str, Any], max_results: int) -> List[Dict[str, Any]]:
        """Search items based on query."""
        results = []
        query_terms = query.get('terms', [])
        
        for key, item in self.storage.items():
            # Simple text-based search
            item_text = str(item['value']).lower()
            metadata_text = str(item['metadata']).lower()
            
            if any(term.lower() in item_text or term.lower() in metadata_text 
                   for term in query_terms):
                results.append({
                    'key': key,
                    'value': item['value'],
                    'metadata': item['metadata'],
                    'relevance': self._calculate_relevance(item, query_terms) + 
                               self.access_count.get(key, 0) * 0.1
                })
        
        # Sort by relevance and limit results
        results.sort(key=lambda x: x['relevance'], reverse=True)
        
        # Simulate search latency
        await asyncio.sleep(0.02)
        
        return results[:max_results]
    
    def _calculate_relevance(self, item: Dict[str, Any], query_terms: List[str]) -> float:
        """Calculate relevance score for search result."""
        item_text = str(item['value']).lower()
        metadata_text = str(item['metadata']).lower()
        
        score = 0.0
        for term in query_terms:
            term_lower = term.lower()
            if term_lower in item_text:
                score += 1.0
            if term_lower in metadata_text:
                score += 0.5
        
        # Return the base score (access count boost handled elsewhere)
        return score
    
    async def _evict_least_used(self):
        """Evict least recently used item to make space."""
        if not self.storage:
            return
        
        # Find least accessed item
        least_used_key = min(self.access_count, key=self.access_count.get)
        
        # Remove from all storage structures
        del self.storage[least_used_key]
        del self.access_count[least_used_key]
        del self.creation_time[least_used_key]
    
    def get_capabilities(self) -> List[str]:
        """Get tool capabilities."""
        return ['memory_storage', 'memory_retrieval', 'memory_search', 'cognitive_offloading']
    
    def estimate_cost(self, task: CognitiveTask) -> float:
        """Estimate resource cost for task."""
        # Cost based on operation complexity
        operation = task.parameters.get('operation', 'retrieve')
        
        cost_map = {
            'store': 0.1,      # Low cost for storage
            'retrieve': 0.05,  # Very low cost for retrieval
            'search': 0.2      # Higher cost for search
        }
        
        return cost_map.get(operation, 0.1)

class ComputationTool(ToolInterface):
    """
    External computation tool for cognitive processing enhancement.
    
    Provides mathematical, analytical, and simulation capabilities.
    """
    
    def __init__(self):
        """Initialize computation tool."""
        self.computation_cache: Dict[str, Any] = {}
    
    async def execute(self, task: CognitiveTask, parameters: Dict[str, Any]) -> Any:
        """Execute computation operation."""
        computation_type = parameters.get('type', 'calculate')
        
        if computation_type == 'calculate':
            return await self._calculate(parameters.get('expression'))
        elif computation_type == 'analyze':
            return await self._analyze_data(parameters.get('data'))
        elif computation_type == 'simulate':
            return await self._simulate(parameters.get('model'), parameters.get('parameters', {}))
        elif computation_type == 'optimize':
            return await self._optimize(parameters.get('objective'), parameters.get('constraints', {}))
        else:
            raise ValueError(f"Unknown computation type: {computation_type}")
    
    async def _calculate(self, expression: str) -> Dict[str, Any]:
        """Perform mathematical calculation."""
        # Create cache key
        cache_key = f"calc_{hash(expression)}"
        
        if cache_key in self.computation_cache:
            return self.computation_cache[cache_key]
        
        try:
            # Simple expression evaluation (in practice, would be more sophisticated)
            # For safety, we'll only handle basic arithmetic
            if any(char in expression for char in ['import', '__', 'exec', 'eval']):
                raise ValueError("Invalid expression")
            
            # Simulate computation time
            await asyncio.sleep(0.1)
            
            # Basic arithmetic evaluation
            allowed_chars = set('0123456789+-*/.() ')
            if all(c in allowed_chars for c in expression):
                result = eval(expression)  # In production, use a safe evaluator
                
                response = {
                    'status': 'success',
                    'result': result,
                    'expression': expression,
                    'computation_time': 0.1
                }
            else:
                response = {
                    'status': 'error',
                    'error': 'Invalid characters in expression',
                    'expression': expression
                }
            
            self.computation_cache[cache_key] = response
            return response
            
        except Exception as e:
            return {
                'status': 'error',
                'error': str(e),
                'expression': expression
            }
    
    async def _analyze_data(self, data: List[float]) -> Dict[str, Any]:
        """Analyze numerical data."""
        if not data:
            return {'status': 'error', 'error': 'No data provided'}
        
        try:
            # Simulate analysis time
            await asyncio.sleep(0.05)
            
            data_array = np.array(data)
            
            analysis = {
                'status': 'success',
                'statistics': {
                    'count': len(data),
                    'mean': float(np.mean(data_array)),
                    'std': float(np.std(data_array)),
                    'min': float(np.min(data_array)),
                    'max': float(np.max(data_array)),
                    'median': float(np.median(data_array))
                },
                'trends': {
                    'is_increasing': bool(np.all(np.diff(data_array) >= 0)),
                    'is_decreasing': bool(np.all(np.diff(data_array) <= 0)),
                    'has_trend': bool(abs(np.corrcoef(range(len(data)), data)[0, 1]) > 0.5)
                }
            }
            
            return analysis
            
        except Exception as e:
            return {
                'status': 'error',
                'error': str(e)
            }
    
    async def _simulate(self, model: str, parameters: Dict[str, Any]) -> Dict[str, Any]:
        """Run simulation model."""
        # Simulate computation time
        await asyncio.sleep(0.2)
        
        if model == 'random_walk':
            steps = parameters.get('steps', 100)
            initial_value = parameters.get('initial_value', 0.0)
            
            values = [initial_value]
            current = initial_value
            
            for _ in range(steps):
                step = np.random.normal(0, 1)
                current += step
                values.append(current)
            
            return {
                'status': 'success',
                'model': model,
                'results': {
                    'values': values,
                    'final_value': current,
                    'path_length': len(values)
                },
                'parameters': parameters
            }
        
        elif model == 'linear_growth':
            steps = parameters.get('steps', 100)
            growth_rate = parameters.get('growth_rate', 0.1)
            initial_value = parameters.get('initial_value', 1.0)
            
            values = []
            for i in range(steps):
                value = initial_value * (1 + growth_rate) ** i
                values.append(value)
            
            return {
                'status': 'success',
                'model': model,
                'results': {
                    'values': values,
                    'final_value': values[-1] if values else initial_value,
                    'growth_factor': (1 + growth_rate) ** steps
                },
                'parameters': parameters
            }
        
        else:
            return {
                'status': 'error',
                'error': f'Unknown simulation model: {model}'
            }
    
    async def _optimize(self, objective: str, constraints: Dict[str, Any]) -> Dict[str, Any]:
        """Perform optimization."""
        # Simulate optimization time
        await asyncio.sleep(0.3)
        
        # Simple optimization simulation
        if objective == 'minimize_quadratic':
            # Minimize f(x) = x^2 + constraints['linear_term'] * x + constraints['constant']
            linear_term = constraints.get('linear_term', 0)
            constant = constraints.get('constant', 0)
            
            # Analytical solution for quadratic
            optimal_x = -linear_term / 2
            optimal_value = optimal_x ** 2 + linear_term * optimal_x + constant
            
            return {
                'status': 'success',
                'objective': objective,
                'optimal_point': optimal_x,
                'optimal_value': optimal_value,
                'iterations': 1,  # Analytical solution
                'constraints': constraints
            }
        
        elif objective == 'maximize_entropy':
            # Simple entropy maximization
            n_variables = constraints.get('n_variables', 5)
            
            # Uniform distribution maximizes entropy
            optimal_distribution = [1.0 / n_variables] * n_variables
            entropy = -sum(p * np.log(p) for p in optimal_distribution)
            
            return {
                'status': 'success',
                'objective': objective,
                'optimal_distribution': optimal_distribution,
                'optimal_value': entropy,
                'iterations': 10,
                'constraints': constraints
            }
        
        else:
            return {
                'status': 'error',
                'error': f'Unknown optimization objective: {objective}'
            }
    
    def get_capabilities(self) -> List[str]:
        """Get tool capabilities."""
        return [
            'mathematical_calculation', 'data_analysis', 'simulation', 
            'optimization', 'statistical_analysis', 'numerical_processing'
        ]
    
    def estimate_cost(self, task: CognitiveTask) -> float:
        """Estimate resource cost for task."""
        computation_type = task.parameters.get('type', 'calculate')
        
        cost_map = {
            'calculate': 0.05,
            'analyze': 0.1,
            'simulate': 0.3,
            'optimize': 0.5
        }
        
        return cost_map.get(computation_type, 0.2)

class KnowledgeBaseTool(ToolInterface):
    """
    Knowledge base tool for accessing structured information.
    
    Provides query interface to stored knowledge and facts.
    """
    
    def __init__(self):
        """Initialize knowledge base tool."""
        self.knowledge_graph = self._initialize_knowledge_graph()
        self.query_cache: Dict[str, Any] = {}
    
    def _initialize_knowledge_graph(self) -> Dict[str, Any]:
        """Initialize basic knowledge graph."""
        return {
            'concepts': {
                'cognition': {
                    'definition': 'Mental processes of acquiring knowledge and understanding',
                    'related': ['memory', 'attention', 'reasoning', 'problem_solving'],
                    'properties': ['active', 'constructive', 'contextual']
                },
                'memory': {
                    'definition': 'Cognitive process of encoding, storing, and retrieving information',
                    'types': ['working_memory', 'long_term_memory', 'episodic_memory', 'semantic_memory'],
                    'related': ['cognition', 'learning', 'attention']
                },
                'embodied_cognition': {
                    'definition': 'Cognitive processes grounded in bodily experience',
                    'principles': ['embodied', 'embedded', 'extended', 'enactive'],
                    'related': ['perception', 'action', 'environmental_coupling']
                }
            },
            'relations': {
                ('cognition', 'memory'): 'includes',
                ('embodied_cognition', 'cognition'): 'specializes',
                ('memory', 'learning'): 'enables'
            },
            'facts': [
                'Working memory capacity is approximately 7±2 items',
                'Embodied cognition emphasizes the role of the body in cognitive processes',
                'Extended mind theory suggests cognition extends beyond individual boundaries',
                'Tool use enhances cognitive capabilities through external resources'
            ]
        }
    
    async def execute(self, task: CognitiveTask, parameters: Dict[str, Any]) -> Any:
        """Execute knowledge base query."""
        query_type = parameters.get('type', 'lookup')
        
        if query_type == 'lookup':
            return await self._lookup_concept(parameters.get('concept'))
        elif query_type == 'search':
            return await self._search_knowledge(parameters.get('query'))
        elif query_type == 'relate':
            return await self._find_relations(
                parameters.get('concept1'), parameters.get('concept2')
            )
        elif query_type == 'facts':
            return await self._get_relevant_facts(parameters.get('topic'))
        else:
            raise ValueError(f"Unknown query type: {query_type}")
    
    async def _lookup_concept(self, concept: str) -> Dict[str, Any]:
        """Look up specific concept in knowledge base."""
        if not concept:
            return {'status': 'error', 'error': 'No concept provided'}
            
        cache_key = f"lookup_{concept}"
        
        if cache_key in self.query_cache:
            return self.query_cache[cache_key]
        
        # Simulate lookup time
        await asyncio.sleep(0.02)
        
        concept_lower = concept.lower()
        
        if concept_lower in self.knowledge_graph['concepts']:
            result = {
                'status': 'found',
                'concept': concept,
                'information': self.knowledge_graph['concepts'][concept_lower]
            }
        else:
            # Try partial matching
            matches = [
                c for c in self.knowledge_graph['concepts'] 
                if concept_lower in c or c in concept_lower
            ]
            
            if matches:
                result = {
                    'status': 'partial_match',
                    'concept': concept,
                    'matches': [
                        {
                            'concept': match,
                            'information': self.knowledge_graph['concepts'][match]
                        }
                        for match in matches[:3]  # Limit to top 3 matches
                    ]
                }
            else:
                result = {
                    'status': 'not_found',
                    'concept': concept,
                    'suggestions': list(self.knowledge_graph['concepts'].keys())[:5]
                }
        
        self.query_cache[cache_key] = result
        return result
    
    async def _search_knowledge(self, query: str) -> Dict[str, Any]:
        """Search knowledge base with text query."""
        # Simulate search time
        await asyncio.sleep(0.05)
        
        query_lower = query.lower()
        results = []
        
        # Search in concepts
        for concept, info in self.knowledge_graph['concepts'].items():
            score = 0.0
            
            # Check concept name
            if query_lower in concept:
                score += 2.0
            
            # Check definition
            definition = info.get('definition', '').lower()
            if query_lower in definition:
                score += 1.0
            
            # Check related terms
            related = info.get('related', [])
            for term in related:
                if query_lower in term.lower():
                    score += 0.5
            
            if score > 0:
                results.append({
                    'type': 'concept',
                    'concept': concept,
                    'information': info,
                    'relevance': score
                })
        
        # Search in facts
        for fact in self.knowledge_graph['facts']:
            if query_lower in fact.lower():
                results.append({
                    'type': 'fact',
                    'fact': fact,
                    'relevance': 1.0
                })
        
        # Sort by relevance
        results.sort(key=lambda x: x['relevance'], reverse=True)
        
        return {
            'status': 'success',
            'query': query,
            'results': results[:10],  # Limit to top 10 results
            'total_found': len(results)
        }
    
    async def _find_relations(self, concept1: str, concept2: str) -> Dict[str, Any]:
        """Find relations between concepts."""
        # Simulate relation search time
        await asyncio.sleep(0.03)
        
        concept1_lower = concept1.lower()
        concept2_lower = concept2.lower()
        
        # Direct relations
        direct_relations = []
        for (c1, c2), relation in self.knowledge_graph['relations'].items():
            if (c1 == concept1_lower and c2 == concept2_lower) or \
               (c1 == concept2_lower and c2 == concept1_lower):
                direct_relations.append({
                    'from': c1,
                    'to': c2,
                    'relation': relation,
                    'bidirectional': False
                })
        
        # Indirect relations (through shared concepts)
        indirect_relations = []
        concept1_related = self.knowledge_graph['concepts'].get(concept1_lower, {}).get('related', [])
        concept2_related = self.knowledge_graph['concepts'].get(concept2_lower, {}).get('related', [])
        
        common_related = set(concept1_related) & set(concept2_related)
        for common in common_related:
            indirect_relations.append({
                'concept1': concept1,
                'concept2': concept2,
                'through': common,
                'relation': 'related_through'
            })
        
        return {
            'status': 'success',
            'concept1': concept1,
            'concept2': concept2,
            'direct_relations': direct_relations,
            'indirect_relations': indirect_relations,
            'total_relations': len(direct_relations) + len(indirect_relations)
        }
    
    async def _get_relevant_facts(self, topic: str) -> Dict[str, Any]:
        """Get facts relevant to a topic."""
        # Simulate fact retrieval time
        await asyncio.sleep(0.02)
        
        topic_lower = topic.lower()
        relevant_facts = []
        
        for fact in self.knowledge_graph['facts']:
            if topic_lower in fact.lower():
                relevant_facts.append({
                    'fact': fact,
                    'relevance': fact.lower().count(topic_lower)
                })
        
        # Sort by relevance
        relevant_facts.sort(key=lambda x: x['relevance'], reverse=True)
        
        return {
            'status': 'success',
            'topic': topic,
            'facts': [f['fact'] for f in relevant_facts],
            'total_found': len(relevant_facts)
        }
    
    def get_capabilities(self) -> List[str]:
        """Get tool capabilities."""
        return [
            'knowledge_lookup', 'concept_search', 'relation_discovery',
            'fact_retrieval', 'semantic_understanding', 'information_access'
        ]
    
    def estimate_cost(self, task: CognitiveTask) -> float:
        """Estimate resource cost for task."""
        query_type = task.parameters.get('type', 'lookup')
        
        cost_map = {
            'lookup': 0.02,
            'search': 0.05,
            'relate': 0.03,
            'facts': 0.02
        }
        
        return cost_map.get(query_type, 0.03)

# Factory function to create default cognitive tools
def create_default_cognitive_tools() -> List[Tuple[CognitiveTool, ToolInterface]]:
    """
    Create default set of cognitive tools for the Extended Mind System.
    
    Returns:
        List of (CognitiveTool, ToolInterface) tuples
    """
    tools = []
    
    # Memory Store Tool
    memory_store = MemoryStoreTool()
    memory_tool_spec = CognitiveTool(
        tool_id="memory_store_01",
        tool_type=ToolType.MEMORY_STORE,
        name="External Memory Store",
        description="Persistent storage and retrieval of cognitive artifacts",
        capabilities=memory_store.get_capabilities(),
        interface={
            'operations': ['store', 'retrieve', 'search'],
            'max_capacity': 10000,
            'search_types': ['text', 'metadata']
        },
        availability=1.0,
        cost=0.05,
        latency=0.02,
        reliability=0.95
    )
    tools.append((memory_tool_spec, memory_store))
    
    # Computation Tool
    computation = ComputationTool()
    computation_tool_spec = CognitiveTool(
        tool_id="computation_01",
        tool_type=ToolType.COMPUTATION,
        name="Mathematical Computation Engine",
        description="Mathematical, analytical, and simulation capabilities",
        capabilities=computation.get_capabilities(),
        interface={
            'types': ['calculate', 'analyze', 'simulate', 'optimize'],
            'models': ['random_walk', 'linear_growth'],
            'objectives': ['minimize_quadratic', 'maximize_entropy']
        },
        availability=1.0,
        cost=0.2,
        latency=0.15,
        reliability=0.98
    )
    tools.append((computation_tool_spec, computation))
    
    # Knowledge Base Tool
    knowledge_base = KnowledgeBaseTool()
    knowledge_tool_spec = CognitiveTool(
        tool_id="knowledge_base_01",
        tool_type=ToolType.KNOWLEDGE_BASE,
        name="Structured Knowledge Base",
        description="Query interface to structured information and facts",
        capabilities=knowledge_base.get_capabilities(),
        interface={
            'query_types': ['lookup', 'search', 'relate', 'facts'],
            'knowledge_domains': ['cognition', 'memory', 'embodied_cognition'],
            'relation_types': ['includes', 'specializes', 'enables']
        },
        availability=1.0,
        cost=0.03,
        latency=0.03,
        reliability=0.99
    )
    tools.append((knowledge_tool_spec, knowledge_base))
    
    return tools