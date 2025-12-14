#!/usr/bin/env python3
"""
Test Multi-Source Data Integration - Task 7.1.1

This test validates the implementation of multi-source data integration
for server-side processing from multiple engine components.
"""

import asyncio
import sys
import os
import time
from typing import Dict, Any

# Add project root to Python path
sys.path.insert(0, os.path.abspath('.'))

try:
    from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor
    from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
    DTESN_AVAILABLE = True
except ImportError as e:
    print(f"DTESN imports not available: {e}")
    DTESN_AVAILABLE = False

class MockAsyncAphrodite:
    """Mock AsyncAphrodite engine for testing multi-source integration."""
    
    def __init__(self):
        self.model_name = "test-model"
        self.max_model_len = 2048
        self.vocab_size = 50000
        self.hidden_size = 768
        
    async def get_model_config(self):
        return type('MockModelConfig', (), {
            'model': self.model_name,
            'max_model_len': self.max_model_len,
            'vocab_size': self.vocab_size,
            'hidden_size': self.hidden_size,
            'dtype': 'float16'
        })()
    
    async def get_tokenizer(self):
        return type('MockTokenizer', (), {})()
    
    def encode(self, text):
        return [1, 2, 3, 4, 5]
    
    def decode(self, tokens):
        return "decoded_text"
    
    async def generate(self, prompt, **kwargs):
        return f"Generated response for: {prompt}"
    
    async def check_health(self):
        return {"status": "healthy"}

class MultiSourceDataIntegrationTester:
    """Test framework for multi-source data integration functionality."""
    
    def __init__(self):
        self.test_results = {}
        self.mock_engine = MockAsyncAphrodite()
    
    async def run_all_tests(self) -> Dict[str, Any]:
        """Run comprehensive multi-source data integration tests."""
        print("🔄 Starting Multi-Source Data Integration Tests...")
        
        tests = [
            self.test_multi_source_data_fetching,
            self.test_data_aggregation_pipeline,
            self.test_processing_pipeline_creation,
            self.test_transformation_application,
            self.test_concurrent_source_access,
            self.test_error_handling_resilience,
            self.test_performance_with_multiple_sources
        ]
        
        for test in tests:
            try:
                result = await test()
                self.test_results[test.__name__] = result
                status = "✅ PASS" if result.get("passed", False) else "❌ FAIL"
                print(f"{status} {result.get('name', test.__name__)}")
                if not result.get("passed", False):
                    print(f"   Error: {result.get('error', 'Unknown error')}")
            except Exception as e:
                self.test_results[test.__name__] = {
                    "passed": False,
                    "error": str(e),
                    "name": test.__name__
                }
                print(f"❌ FAIL {test.__name__}: {e}")
        
        return self.test_results
    
    async def test_multi_source_data_fetching(self) -> Dict[str, Any]:
        """Test fetching data from multiple engine sources."""
        test_result = {
            "name": "Multi-Source Data Fetching",
            "passed": False,
            "details": {}
        }
        
        if not DTESN_AVAILABLE:
            test_result["error"] = "DTESN processor not available"
            return test_result
        
        try:
            config = DTESNConfig()
            processor = DTESNProcessor(config=config, engine=self.mock_engine)
            
            # Initialize processor
            await processor._initialize_engine_integration()
            
            # Test multi-source data fetching
            multi_source_data = await processor._fetch_multi_source_data()
            
            # Validate structure
            assert "sources" in multi_source_data, "Missing sources key"
            assert "aggregation" in multi_source_data, "Missing aggregation key"
            assert "processing_pipelines" in multi_source_data, "Missing processing_pipelines key"
            assert "transformation_ready" in multi_source_data, "Missing transformation_ready key"
            
            # Validate source count
            source_count = multi_source_data.get("source_count", 0)
            assert source_count > 0, f"No sources fetched: {source_count}"
            
            # Validate individual sources
            sources = multi_source_data.get("sources", {})
            expected_sources = ["model_config", "tokenizer", "performance", "processing_state", "resources"]
            
            for source_name in expected_sources:
                assert source_name in sources, f"Missing source: {source_name}"
                source_data = sources[source_name]
                assert "source_type" in source_data, f"Missing source_type in {source_name}"
                assert "fetch_timestamp" in source_data, f"Missing timestamp in {source_name}"
            
            test_result["passed"] = True
            test_result["details"] = {
                "sources_fetched": len(sources),
                "transformation_ready": multi_source_data.get("transformation_ready"),
                "source_names": list(sources.keys())
            }
            
        except Exception as e:
            test_result["error"] = str(e)
        
        return test_result
    
    async def test_data_aggregation_pipeline(self) -> Dict[str, Any]:
        """Test aggregation of data from multiple sources."""
        test_result = {
            "name": "Data Aggregation Pipeline",
            "passed": False,
            "details": {}
        }
        
        if not DTESN_AVAILABLE:
            test_result["error"] = "DTESN processor not available"
            return test_result
        
        try:
            config = DTESNConfig()
            processor = DTESNProcessor(config=config, engine=self.mock_engine)
            await processor._initialize_engine_integration()
            
            # Create mock source data
            mock_sources = {
                "model_config": {
                    "model_name": "test-model",
                    "max_model_len": 2048,
                    "source_type": "model_config",
                    "fetch_timestamp": time.time()
                },
                "performance": {
                    "total_requests": 100,
                    "engine_ready": True,
                    "source_type": "performance",
                    "fetch_timestamp": time.time()
                },
                "resources": {
                    "max_concurrent_processes": 50,
                    "source_type": "resources",
                    "fetch_timestamp": time.time()
                }
            }
            
            # Test aggregation
            aggregation = await processor._aggregate_multi_source_data(mock_sources)
            
            # Validate aggregation structure
            assert "total_sources" in aggregation, "Missing total_sources"
            assert "successful_sources" in aggregation, "Missing successful_sources"
            assert "data_quality_score" in aggregation, "Missing data_quality_score"
            assert "unified_metadata" in aggregation, "Missing unified_metadata"
            assert "processing_constraints" in aggregation, "Missing processing_constraints"
            
            # Validate aggregation logic
            assert aggregation["total_sources"] == 3, f"Wrong total sources: {aggregation['total_sources']}"
            assert aggregation["successful_sources"] == 3, f"Wrong successful sources: {aggregation['successful_sources']}"
            assert aggregation["data_quality_score"] == 1.0, f"Wrong quality score: {aggregation['data_quality_score']}"
            
            # Validate metadata aggregation
            unified_metadata = aggregation["unified_metadata"]
            assert "active_model" in unified_metadata, "Missing active_model in metadata"
            assert unified_metadata["active_model"] == "test-model", "Wrong active model"
            
            test_result["passed"] = True
            test_result["details"] = {
                "total_sources": aggregation["total_sources"],
                "quality_score": aggregation["data_quality_score"],
                "metadata_keys": list(unified_metadata.keys())
            }
            
        except Exception as e:
            test_result["error"] = str(e)
        
        return test_result
    
    async def test_processing_pipeline_creation(self) -> Dict[str, Any]:
        """Test creation of data processing pipelines."""
        test_result = {
            "name": "Processing Pipeline Creation",
            "passed": False,
            "details": {}
        }
        
        if not DTESN_AVAILABLE:
            test_result["error"] = "DTESN processor not available"
            return test_result
        
        try:
            config = DTESNConfig()
            processor = DTESNProcessor(config=config, engine=self.mock_engine)
            
            # Create mock data
            mock_sources = {
                "model_config": {"model_name": "test", "source_type": "model_config"},
                "performance": {"engine_ready": True, "source_type": "performance"},
                "resources": {"max_concurrent_processes": 50, "source_type": "resources"}
            }
            
            mock_aggregation = {
                "data_quality_score": 0.8,
                "optimization_hints": {"engine_optimizations_available": True},
                "processing_constraints": {"max_concurrent": 50}
            }
            
            # Test pipeline creation
            pipelines = await processor._create_data_processing_pipelines(
                mock_sources, mock_aggregation
            )
            
            # Validate pipeline structure
            assert "transformation_pipelines" in pipelines, "Missing transformation_pipelines"
            assert "data_flow_config" in pipelines, "Missing data_flow_config"
            assert "optimization_config" in pipelines, "Missing optimization_config"
            assert "pipeline_ready" in pipelines, "Missing pipeline_ready"
            
            # Validate pipelines were created
            transformation_pipelines = pipelines["transformation_pipelines"]
            assert len(transformation_pipelines) > 0, "No transformation pipelines created"
            
            # Validate pipeline structure
            for pipeline in transformation_pipelines:
                assert "name" in pipeline, "Pipeline missing name"
                assert "source" in pipeline, "Pipeline missing source"
                assert "transformations" in pipeline, "Pipeline missing transformations"
                assert "priority" in pipeline, "Pipeline missing priority"
            
            # Validate data flow config
            data_flow = pipelines["data_flow_config"]
            assert "pipeline_order" in data_flow, "Missing pipeline_order"
            assert "parallel_execution" in data_flow, "Missing parallel_execution"
            
            test_result["passed"] = True
            test_result["details"] = {
                "pipeline_count": len(transformation_pipelines),
                "pipeline_names": [p["name"] for p in transformation_pipelines],
                "parallel_execution": data_flow.get("parallel_execution", False)
            }
            
        except Exception as e:
            test_result["error"] = str(e)
        
        return test_result
    
    async def test_transformation_application(self) -> Dict[str, Any]:
        """Test application of multi-source transformations."""
        test_result = {
            "name": "Transformation Application",
            "passed": False,
            "details": {}
        }
        
        if not DTESN_AVAILABLE:
            test_result["error"] = "DTESN processor not available"
            return test_result
        
        try:
            import numpy as np
            config = DTESNConfig()
            processor = DTESNProcessor(config=config, engine=self.mock_engine)
            await processor._initialize_engine_integration()
            
            # Create comprehensive engine context with multi-source data
            engine_context = await processor._fetch_comprehensive_engine_context()
            
            # Test preprocessing with multi-source integration
            input_data = "test input data"
            input_vector = await processor._preprocess_with_engine(input_data, engine_context)
            
            # Validate transformation was applied
            assert isinstance(input_vector, np.ndarray), "Input vector should be numpy array"
            assert input_vector.size > 0, "Input vector should not be empty"
            assert input_vector.shape[1] == 1, "Input vector should be column vector"
            
            # Test individual transformation methods
            multi_source_data = engine_context.get("multi_source_data", {})
            if multi_source_data.get("transformation_ready"):
                # Test model-aware preprocessing
                model_transformed = await processor._apply_model_aware_preprocessing(
                    input_vector, multi_source_data
                )
                assert isinstance(model_transformed, np.ndarray), "Model transformation failed"
                
                # Test performance optimization
                perf_transformed = await processor._apply_performance_optimization(
                    input_vector, multi_source_data
                )
                assert isinstance(perf_transformed, np.ndarray), "Performance optimization failed"
                
                # Test resource scaling
                resource_transformed = await processor._apply_resource_scaling(
                    input_vector, multi_source_data
                )
                assert isinstance(resource_transformed, np.ndarray), "Resource scaling failed"
            
            test_result["passed"] = True
            test_result["details"] = {
                "input_vector_shape": input_vector.shape,
                "transformation_ready": multi_source_data.get("transformation_ready", False),
                "source_count": multi_source_data.get("source_count", 0)
            }
            
        except Exception as e:
            test_result["error"] = str(e)
        
        return test_result
    
    async def test_concurrent_source_access(self) -> Dict[str, Any]:
        """Test concurrent access to multiple data sources."""
        test_result = {
            "name": "Concurrent Source Access",
            "passed": False,
            "details": {}
        }
        
        if not DTESN_AVAILABLE:
            test_result["error"] = "DTESN processor not available"
            return test_result
        
        try:
            config = DTESNConfig()
            processor = DTESNProcessor(config=config, engine=self.mock_engine)
            
            # Test concurrent access by calling multiple times
            start_time = time.time()
            
            tasks = []
            for i in range(5):
                task = processor._fetch_multi_source_data()
                tasks.append(task)
            
            results = await asyncio.gather(*tasks, return_exceptions=True)
            
            end_time = time.time()
            elapsed_time = end_time - start_time
            
            # Validate all results are successful
            successful_calls = 0
            for result in results:
                if isinstance(result, dict) and not isinstance(result, Exception):
                    if result.get("source_count", 0) > 0:
                        successful_calls += 1
            
            assert successful_calls == 5, f"Only {successful_calls}/5 concurrent calls succeeded"
            assert elapsed_time < 2.0, f"Concurrent calls took too long: {elapsed_time:.2f}s"
            
            test_result["passed"] = True
            test_result["details"] = {
                "concurrent_calls": 5,
                "successful_calls": successful_calls,
                "elapsed_time": elapsed_time
            }
            
        except Exception as e:
            test_result["error"] = str(e)
        
        return test_result
    
    async def test_error_handling_resilience(self) -> Dict[str, Any]:
        """Test error handling in multi-source data integration."""
        test_result = {
            "name": "Error Handling Resilience",
            "passed": False,
            "details": {}
        }
        
        if not DTESN_AVAILABLE:
            test_result["error"] = "DTESN processor not available"
            return test_result
        
        try:
            # Create faulty engine that raises exceptions
            class FaultyEngine:
                def __getattr__(self, name):
                    raise RuntimeError(f"Simulated error in {name}")
            
            config = DTESNConfig()
            processor = DTESNProcessor(config=config, engine=FaultyEngine())
            
            # Test that multi-source fetching handles errors gracefully
            multi_source_data = await processor._fetch_multi_source_data()
            
            # Should still return valid structure even with errors
            assert "sources" in multi_source_data, "Missing sources despite errors"
            assert "aggregation" in multi_source_data, "Missing aggregation despite errors"
            assert "transformation_ready" in multi_source_data, "Missing transformation_ready despite errors"
            
            # Sources should contain error information
            sources = multi_source_data.get("sources", {})
            error_count = sum(1 for source in sources.values() if "error" in source)
            
            assert error_count > 0, "Expected some errors but found none"
            assert multi_source_data.get("source_count", 0) >= 0, "Source count should be non-negative"
            
            test_result["passed"] = True
            test_result["details"] = {
                "total_sources": len(sources),
                "error_sources": error_count,
                "graceful_degradation": True
            }
            
        except Exception as e:
            test_result["error"] = str(e)
        
        return test_result
    
    async def test_performance_with_multiple_sources(self) -> Dict[str, Any]:
        """Test performance characteristics with multiple data sources."""
        test_result = {
            "name": "Performance with Multiple Sources",
            "passed": False,
            "details": {}
        }
        
        if not DTESN_AVAILABLE:
            test_result["error"] = "DTESN processor not available"
            return test_result
        
        try:
            config = DTESNConfig()
            processor = DTESNProcessor(config=config, engine=self.mock_engine)
            await processor._initialize_engine_integration()
            
            # Measure performance of multi-source operations
            iterations = 10
            total_time = 0
            
            for i in range(iterations):
                start_time = time.time()
                
                # Full multi-source integration cycle
                engine_context = await processor._fetch_comprehensive_engine_context()
                input_vector = await processor._preprocess_with_engine("test input", engine_context)
                
                end_time = time.time()
                total_time += (end_time - start_time)
            
            avg_time = total_time / iterations
            
            # Performance should be reasonable (under 0.1 seconds per operation)
            assert avg_time < 0.1, f"Average operation time too high: {avg_time:.3f}s"
            
            # Validate multi-source data structure
            multi_source_data = engine_context.get("multi_source_data", {})
            assert multi_source_data.get("source_count", 0) > 0, "No sources in performance test"
            
            test_result["passed"] = True
            test_result["details"] = {
                "iterations": iterations,
                "avg_time_per_operation": avg_time,
                "total_time": total_time,
                "sources_processed": multi_source_data.get("source_count", 0)
            }
            
        except Exception as e:
            test_result["error"] = str(e)
        
        return test_result

async def main():
    """Run multi-source data integration tests."""
    print("=" * 70)
    print("🚀 Multi-Source Data Integration Test Suite - Task 7.1.1")
    print("=" * 70)
    
    tester = MultiSourceDataIntegrationTester()
    results = await tester.run_all_tests()
    
    # Summary
    total_tests = len(results)
    passed_tests = sum(1 for result in results.values() if result.get("passed", False))
    
    print("\n" + "=" * 70)
    print(f"📊 Test Summary: {passed_tests}/{total_tests} tests passed")
    
    if passed_tests == total_tests:
        print("✅ All tests passed! Multi-source data integration is working correctly.")
        print("\n🎯 Acceptance Criteria Met:")
        print("   ✓ Server efficiently processes data from multiple sources")
        print("   ✓ Data aggregation and processing pipelines implemented")
        print("   ✓ Efficient data transformation for DTESN operations created")
        return 0
    else:
        print(f"❌ {total_tests - passed_tests} tests failed. See details above.")
        return 1

if __name__ == "__main__":
    exit_code = asyncio.run(main())
    sys.exit(exit_code)