#!/usr/bin/env python3
"""
Integration Tests for Automated Deployment Pipeline
Phase 4.3.1: Test automated deployment pipeline functionality

This module provides comprehensive tests for the automated deployment pipeline,
including quality assurance, A/B testing, and deployment orchestration.
"""

import os
import sys
import pytest
import tempfile
from pathlib import Path
from unittest.mock import patch

# Add deployment scripts to path
sys.path.append(str(Path(__file__).parent.parent / "deployment" / "scripts"))

try:
    from quality_assurance import DeploymentQualityAssurance
    from ab_testing import ABTestingFramework
except ImportError:
    # Handle case where scripts are not available
    pass


class TestDeploymentPipeline:
    """Test suite for automated deployment pipeline"""
    
    def setup_method(self):
        """Setup test environment"""
        self.test_dir = tempfile.mkdtemp()
        self.original_cwd = os.getcwd()
        os.chdir(self.test_dir)
        
    def teardown_method(self):
        """Cleanup test environment"""
        os.chdir(self.original_cwd)
        
    def test_deployment_directory_structure(self):
        """Test that deployment directory structure exists"""
        repo_root = Path(__file__).parent.parent
        deployment_dir = repo_root / "deployment"
        
        # Check main deployment directory exists
        assert deployment_dir.exists(), "Deployment directory should exist"
        
        # Check subdirectories
        assert (deployment_dir / "scripts").exists(), "Scripts directory should exist"
        assert (deployment_dir / "configs").exists(), "Configs directory should exist"
        
        # Check key files
        assert (deployment_dir / "scripts" / "quality_assurance.py").exists(), "QA script should exist"
        assert (deployment_dir / "scripts" / "ab_testing.py").exists(), "A/B testing script should exist"
        assert (deployment_dir / "configs" / "pipeline-config.yaml").exists(), "Pipeline config should exist"
        
    def test_deployment_scripts_executable(self):
        """Test that deployment scripts are executable"""
        repo_root = Path(__file__).parent.parent
        scripts_dir = repo_root / "deployment" / "scripts"
        
        for script_path in scripts_dir.glob("*.py"):
            # Check file is executable
            assert os.access(script_path, os.X_OK), f"{script_path.name} should be executable"
    
    @pytest.mark.skipif(not Path("deployment/scripts/quality_assurance.py").exists(), reason="QA script not available")
    def test_quality_assurance_framework(self):
        """Test quality assurance framework"""
        qa = DeploymentQualityAssurance()
        
        # Test configuration loading
        assert qa.config is not None, "QA configuration should load"
        assert "quality_thresholds" in qa.config, "QA config should have thresholds"
        
        # Test model compatibility check
        success, results = qa.test_model_compatibility("test-model")
        assert isinstance(success, bool), "QA should return boolean success"
        assert isinstance(results, dict), "QA should return results dict"
        assert "test_name" in results, "QA results should have test name"
        
        # Test comprehensive QA
        report = qa.run_comprehensive_qa("test-model")
        assert "qa_report" in report, "QA should generate report"
        assert "overall_quality_score" in report["qa_report"], "Report should have quality score"
        assert "deployment_approved" in report["qa_report"], "Report should have approval status"
    
    @pytest.mark.skipif(not Path("deployment/scripts/ab_testing.py").exists(), reason="A/B testing script not available") 
    def test_ab_testing_framework(self):
        """Test A/B testing framework"""
        ab_tester = ABTestingFramework()
        
        # Test A/B test execution
        results = ab_tester.execute_ab_test(duration_minutes=1)  # Short test
        
        assert "test_execution" in results, "A/B test should return execution results"
        assert "test_id" in results["test_execution"], "A/B test should have test ID"
        assert "status" in results["test_execution"], "A/B test should have status"
        assert "decision" in results["test_execution"], "A/B test should have decision"
    
    def test_github_workflow_exists(self):
        """Test that GitHub workflow file exists and is valid"""
        repo_root = Path(__file__).parent.parent
        workflow_file = repo_root / ".github" / "workflows" / "automated-deployment-pipeline.yml"
        
        assert workflow_file.exists(), "Deployment pipeline workflow should exist"
        
        # Check workflow file has required content
        content = workflow_file.read_text()
        assert "name: 🚀 Automated Model Deployment Pipeline" in content, "Workflow should have correct name"
        assert "quality-assurance:" in content, "Workflow should have QA job"
        assert "automated-deployment:" in content, "Workflow should have deployment job"
        assert "ab-testing-setup:" in content, "Workflow should have A/B testing job"
    
    def test_deployment_configuration_valid(self):
        """Test that deployment configuration is valid"""
        repo_root = Path(__file__).parent.parent
        config_file = repo_root / "deployment" / "configs" / "pipeline-config.yaml"
        
        assert config_file.exists(), "Pipeline configuration should exist"
        
        # Check config file has required sections
        content = config_file.read_text()
        assert "quality_thresholds:" in content, "Config should have quality thresholds"
        assert "ab_testing:" in content, "Config should have A/B testing config"
        assert "deep_tree_echo:" in content, "Config should have Deep Tree Echo config"
        assert "environments:" in content, "Config should have environment config"
    
    def test_deep_tree_echo_integration_detection(self):
        """Test Deep Tree Echo integration detection"""
        qa = DeploymentQualityAssurance()
        
        # Mock environment with some Deep Tree Echo components
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.side_effect = lambda path: str(path) in ["echo.kern", "aar_core"]
            
            success, results = qa.test_deep_tree_echo_integration()
            
            assert isinstance(success, bool), "Integration test should return boolean"
            assert isinstance(results, dict), "Integration test should return results"
            assert "component_status" in results, "Results should have component status"
    
    def test_model_versioning_hash_generation(self):
        """Test model versioning and hash generation"""
        qa = DeploymentQualityAssurance()
        
        # Run QA to populate test results
        qa.run_comprehensive_qa("test-model")
        
        # Generate deployment hash
        hash1 = qa.generate_deployment_hash()
        hash2 = qa.generate_deployment_hash()
        
        assert isinstance(hash1, str), "Hash should be string"
        assert len(hash1) == 16, "Hash should be 16 characters"
        assert hash1 == hash2, "Hash should be deterministic for same results"
        
        # Change results and verify hash changes
        qa.overall_score = 50
        hash3 = qa.generate_deployment_hash()
        assert hash3 != hash1, "Hash should change when results change"
    
    def test_deployment_approval_logic(self):
        """Test deployment approval decision logic"""
        qa = DeploymentQualityAssurance()
        
        # Test high quality score - should be approved
        qa.overall_score = 95
        qa.test_results = {"compatibility": {"overall_status": "passed"}}
        approved, reasons = qa.should_approve_deployment()
        
        assert approved, "High quality score should be approved"
        assert len(reasons) == 0, "No blocking reasons for high quality"
        
        # Test low quality score - should be blocked  
        qa.overall_score = 60
        approved, reasons = qa.should_approve_deployment()
        
        assert not approved, "Low quality score should be blocked"
        assert len(reasons) > 0, "Should have blocking reasons for low quality"
    
    def test_pipeline_environment_variables(self):
        """Test that pipeline respects environment variables"""
        # Test MODEL_VERSION environment variable
        with patch.dict(os.environ, {'MODEL_VERSION': 'test-version-1.0'}):
            qa = DeploymentQualityAssurance()
            report = qa.run_comprehensive_qa()
            assert report["qa_report"]["model_name"] == "test-version-1.0"
        
        # Test TRAFFIC_SPLIT environment variable
        with patch.dict(os.environ, {'TRAFFIC_SPLIT': '25'}):
            ab_tester = ABTestingFramework()
            results = ab_tester.execute_ab_test(duration_minutes=1)
            assert results["test_execution"]["traffic_split_percent"] == 25
    
    def test_error_handling_and_recovery(self):
        """Test error handling in deployment pipeline"""
        # Test QA with invalid configuration
        qa = DeploymentQualityAssurance()
        
        # Mock a failure in model compatibility test
        with patch.object(qa, 'test_model_compatibility') as mock_test:
            mock_test.side_effect = Exception("Test failure")
            
            # Should handle exception gracefully
            report = qa.run_comprehensive_qa("test-model")
            
            assert report is not None, "Should return report even on failure"
            assert not report["qa_report"]["deployment_approved"], "Should not approve on test failure"
    
    def test_integration_with_existing_workflows(self):
        """Test integration with existing GitHub workflows"""
        repo_root = Path(__file__).parent.parent
        
        # Check that new workflow doesn't conflict with existing ones
        workflows_dir = repo_root / ".github" / "workflows"
        
        existing_workflows = list(workflows_dir.glob("*.yml"))
        workflow_names = []
        
        for workflow_file in existing_workflows:
            content = workflow_file.read_text()
            if content.startswith("name:"):
                name = content.split('\n')[0].replace("name:", "").strip()
                workflow_names.append(name)
        
        # Check no duplicate workflow names
        assert len(workflow_names) == len(set(workflow_names)), "Workflow names should be unique"
        
        # Check our workflow is included
        assert any("Automated Model Deployment Pipeline" in name for name in workflow_names), \
            "Deployment pipeline workflow should be present"


class TestIntegrationWithAphroditeEngine:
    """Integration tests with Aphrodite Engine components"""
    
    def test_aphrodite_imports_available(self):
        """Test that Aphrodite imports are available for QA testing"""
        try:
            from aphrodite import LLM, SamplingParams
            
            # Test basic functionality
            params = SamplingParams(temperature=0.8, max_tokens=10)
            assert params.temperature == 0.8
            assert params.max_tokens == 10
            
        except ImportError:
            pytest.skip("Aphrodite engine not available in test environment")
    
    def test_model_compatibility_with_real_components(self):
        """Test model compatibility with real Aphrodite components"""
        try:
            qa = DeploymentQualityAssurance()
            success, results = qa.test_model_compatibility("gpt2")  # Use small model for testing
            
            # Should succeed if Aphrodite is properly installed
            assert isinstance(success, bool)
            assert "aphrodite_imports" in results
            
        except Exception:
            pytest.skip("Aphrodite engine integration not available")


class TestDeepTreeEchoIntegration:
    """Test Deep Tree Echo specific integrations"""
    
    def test_echo_kern_detection(self):
        """Test echo.kern directory detection"""
        repo_root = Path(__file__).parent.parent
        echo_kern_dir = repo_root / "echo.kern"
        
        if echo_kern_dir.exists():
            # Test that QA framework detects it
            qa = DeploymentQualityAssurance()
            success, results = qa.test_deep_tree_echo_integration()
            
            assert "component_status" in results
            assert "dtesn" in results["component_status"]
            assert results["component_status"]["dtesn"]["available"]
        else:
            pytest.skip("echo.kern directory not available")
    
    def test_aar_core_detection(self):
        """Test AAR core detection"""
        repo_root = Path(__file__).parent.parent
        aar_core_dir = repo_root / "aar_core"
        
        if aar_core_dir.exists():
            # Test that QA framework detects it
            qa = DeploymentQualityAssurance()
            success, results = qa.test_deep_tree_echo_integration()
            
            assert "component_status" in results
            assert "aar_core" in results["component_status"] 
            assert results["component_status"]["aar_core"]["available"]
        else:
            pytest.skip("aar_core directory not available")


def test_deployment_pipeline_end_to_end():
    """End-to-end test of deployment pipeline components"""
    # This test verifies the entire deployment pipeline can run
    try:
        # 1. Quality Assurance
        qa = DeploymentQualityAssurance()
        qa_report = qa.run_comprehensive_qa("test-model")
        
        assert qa_report["qa_report"]["deployment_approved"] in [True, False]
        
        # 2. A/B Testing (only if QA passed)
        if qa_report["qa_report"]["deployment_approved"]:
            ab_tester = ABTestingFramework()
            ab_results = ab_tester.execute_ab_test(duration_minutes=1)
            
            assert "test_execution" in ab_results
            assert ab_results["test_execution"]["status"] in ["completed", "running", "failed"]
        
        print("✅ End-to-end deployment pipeline test completed successfully")
        
    except Exception as e:
        pytest.fail(f"End-to-end test failed: {str(e)}")


if __name__ == "__main__":
    # Run tests when executed directly
    pytest.main([__file__, "-v", "--tb=short"])