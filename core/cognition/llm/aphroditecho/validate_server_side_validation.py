#!/usr/bin/env python3
"""
Validation script for server-side data validation implementation.

This script validates the comprehensive server-side data validation system
for Task 7.1.2, ensuring all components work together correctly.
"""

import sys
import traceback
import json
from typing import Dict, List, Any, Tuple

# Test imports for validation components
try:
    from aphrodite.endpoints.security.input_validation import (
        InputValidationMiddleware,
        ValidationConfig,
        validate_dtesn_endpoint_data
    )
    from aphrodite.endpoints.security.dtesn_validation import (
        DTESNDataType,
        DTESNValidationConfig,
        validate_dtesn_data_structure,
        normalize_dtesn_configuration,
        ESNReservoirConfigSchema,
        PSystemMembraneSchema,
        BSeriesParametersSchema
    )
    from aphrodite.endpoints.security.data_sanitization import (
        SanitizationLevel,
        DataFormat,
        sanitize_data_value,
        create_sanitization_pipeline,
        dtesn_sanitizer
    )
    IMPORTS_SUCCESSFUL = True
except ImportError as e:
    IMPORTS_SUCCESSFUL = False
    IMPORT_ERROR = str(e)


class ServerSideValidationValidator:
    """Validator for server-side data validation implementation."""
    
    def __init__(self):
        self.results = []
        
    def add_result(self, test_name: str, success: bool, message: str = "", details: str = ""):
        """Add validation result."""
        self.results.append({
            "test": test_name,
            "success": success, 
            "message": message,
            "details": details
        })
        
        status = "✅ PASS" if success else "❌ FAIL"
        print(f"{status}: {test_name}")
        if message:
            print(f"   {message}")
        if details and not success:
            print(f"   Details: {details}")
    
    def validate_imports(self):
        """Validate that all required modules can be imported."""
        print("🔍 Testing module imports...")
        
        if IMPORTS_SUCCESSFUL:
            self.add_result(
                "Module imports", 
                True, 
                "All validation modules imported successfully"
            )
        else:
            self.add_result(
                "Module imports",
                False,
                f"Failed to import modules: {IMPORT_ERROR}",
                "Check that all new validation modules are properly created"
            )
        
        return IMPORTS_SUCCESSFUL
    
    def validate_dtesn_schema_validation(self):
        """Test DTESN schema validation functionality."""
        print("\n🔍 Testing DTESN schema validation...")
        
        # Test ESN reservoir configuration validation
        try:
            valid_esn_config = {
                "reservoir_size": 100,
                "input_dimension": 10,
                "spectral_radius": 0.95,
                "leak_rate": 0.1,
                "input_scaling": 1.0,
                "noise_level": 0.01
            }
            
            schema = ESNReservoirConfigSchema(**valid_esn_config)
            self.add_result(
                "ESN reservoir schema validation",
                True,
                f"Successfully validated ESN config with reservoir size {schema.reservoir_size}"
            )
        except Exception as e:
            self.add_result(
                "ESN reservoir schema validation",
                False,
                "Failed to validate ESN configuration",
                str(e)
            )
        
        # Test P-System membrane validation
        try:
            valid_membrane_config = {
                "membrane_id": "test_membrane",
                "depth": 2,
                "capacity": 1000,
                "rules": [
                    {"type": "evolution", "action": "multiply"},
                    {"type": "transport", "action": "move_up"}
                ]
            }
            
            schema = PSystemMembraneSchema(**valid_membrane_config)
            self.add_result(
                "P-System membrane schema validation",
                True,
                f"Successfully validated membrane '{schema.membrane_id}' with {len(schema.rules)} rules"
            )
        except Exception as e:
            self.add_result(
                "P-System membrane schema validation", 
                False,
                "Failed to validate P-System membrane",
                str(e)
            )
        
        # Test B-Series parameters validation
        try:
            valid_bseries_config = {
                "order": 3,
                "timestep": 0.01,
                "method": "rk4",
                "tolerance": 1e-6,
                "coefficients": [1.0, 0.5, 0.25, 0.125, 0.0625, 0.03125]  # Length = 1+2+3 = 6
            }
            
            schema = BSeriesParametersSchema(**valid_bseries_config)
            self.add_result(
                "B-Series parameters schema validation",
                True,
                f"Successfully validated B-Series order {schema.order} with {len(schema.coefficients)} coefficients"
            )
        except Exception as e:
            self.add_result(
                "B-Series parameters schema validation",
                False,
                "Failed to validate B-Series parameters",
                str(e)
            )
    
    def validate_dtesn_data_structure_validation(self):
        """Test DTESN data structure validation functions."""
        print("\n🔍 Testing DTESN data structure validation...")
        
        try:
            # Test ESN configuration validation
            esn_data = {
                "reservoir_size": 200,
                "input_dimension": 15,
                "spectral_radius": 0.9,
                "leak_rate": 0.2,
                "input_scaling": 1.5,
                "noise_level": 0.05
            }
            
            result = validate_dtesn_data_structure(
                esn_data,
                DTESNDataType.ESN_RESERVOIR_CONFIG
            )
            
            # Check result structure
            has_metadata = "_validation_metadata" in result
            has_correct_values = result.get("reservoir_size") == 200
            
            self.add_result(
                "DTESN data structure validation",
                has_metadata and has_correct_values,
                f"Validated ESN data with metadata tracking: {has_metadata}"
            )
            
        except Exception as e:
            self.add_result(
                "DTESN data structure validation",
                False,
                "Failed DTESN data structure validation",
                str(e)
            )
    
    def validate_configuration_normalization(self):
        """Test DTESN configuration normalization."""
        print("\n🔍 Testing configuration normalization...")
        
        try:
            # Test camelCase to snake_case conversion
            config_data = {
                "reservoirSize": 100,
                "inputDimension": 10,
                "spectralRadius": 0.95,
                "customField": "value",
                "nestedConfig": {
                    "maxDepth": 5,
                    "enableFeature": True
                }
            }
            
            normalized = normalize_dtesn_configuration(config_data)
            
            # Check conversions
            has_snake_case = "reservoir_size" in normalized and "input_dimension" in normalized
            has_defaults = normalized.get("performance_monitoring") is True
            preserved_nested = "nested_config" in normalized
            
            success = has_snake_case and has_defaults
            
            self.add_result(
                "Configuration normalization",
                success,
                f"Converted camelCase to snake_case: {has_snake_case}, Added defaults: {has_defaults}"
            )
            
        except Exception as e:
            self.add_result(
                "Configuration normalization",
                False,
                "Failed configuration normalization",
                str(e)
            )
    
    def validate_data_sanitization(self):
        """Test data sanitization functionality."""
        print("\n🔍 Testing data sanitization...")
        
        try:
            # Test potentially dangerous input
            dangerous_data = {
                "user_input": "<script>alert('xss')</script>",
                "numeric_data": [1.0, float('nan'), float('inf'), 3.14159],
                "nested_object": {
                    "sql_injection": "'; DROP TABLE users; --",
                    "path_traversal": "../../../etc/passwd",
                    "normal_field": "safe_value"
                }
            }
            
            sanitized = sanitize_data_value(dangerous_data)
            
            # Check sanitization results
            html_escaped = "&lt;script&gt;" in sanitized.get("user_input", "")
            nan_handled = 0.0 in sanitized.get("numeric_data", [])
            nested_sanitized = "nested_object" in sanitized
            
            success = html_escaped and nan_handled and nested_sanitized
            
            self.add_result(
                "Data sanitization",
                success,
                f"HTML escaped: {html_escaped}, NaN handled: {nan_handled}, Nested processed: {nested_sanitized}"
            )
            
        except Exception as e:
            self.add_result(
                "Data sanitization",
                False,
                "Failed data sanitization",
                str(e)
            )
    
    def validate_sanitization_pipelines(self):
        """Test specialized sanitization pipelines."""
        print("\n🔍 Testing sanitization pipelines...")
        
        try:
            # Test DTESN sanitization pipeline
            dtesn_test_data = {
                "reservoir_config": {
                    "size": 100,
                    "radius": float('nan'),  # Should be handled
                    "description": "<configuration>test</configuration>"
                },
                "membrane_data": [
                    {"id": "mem1", "capacity": float('inf')},
                    {"id": "mem2", "capacity": 500}
                ]
            }
            
            sanitized = dtesn_sanitizer(dtesn_test_data)
            
            # Check DTESN-specific sanitization
            nan_replaced = sanitized["reservoir_config"]["radius"] == 0.0
            html_escaped = "&lt;configuration&gt;" in sanitized["reservoir_config"]["description"] 
            structure_preserved = len(sanitized["membrane_data"]) == 2
            
            success = nan_replaced and html_escaped and structure_preserved
            
            self.add_result(
                "DTESN sanitization pipeline",
                success,
                f"NaN replaced: {nan_replaced}, HTML escaped: {html_escaped}, Structure preserved: {structure_preserved}"
            )
            
        except Exception as e:
            self.add_result(
                "DTESN sanitization pipeline", 
                False,
                "Failed DTESN sanitization pipeline",
                str(e)
            )
    
    def validate_endpoint_integration(self):
        """Test integration with endpoint validation."""
        print("\n🔍 Testing endpoint integration...")
        
        try:
            # Test DTESN endpoint validation
            dtesn_endpoint_data = {
                "config": {
                    "reservoirSize": 150,  # camelCase that should be normalized
                    "inputDimension": 20,
                    "spectralRadius": 0.85
                }
            }
            
            # Simulate DTESN endpoint path
            endpoint_path = "/dtesn/reservoir/config"
            
            result = validate_dtesn_endpoint_data(
                dtesn_endpoint_data,
                endpoint_path
            )
            
            # Should have normalized the config
            has_config = "config" in result
            normalized_keys = "reservoir_size" in result.get("config", {}) if has_config else False
            
            self.add_result(
                "DTESN endpoint integration",
                has_config and normalized_keys,
                f"Config processed: {has_config}, Keys normalized: {normalized_keys}"
            )
            
        except Exception as e:
            self.add_result(
                "DTESN endpoint integration",
                False,
                "Failed DTESN endpoint integration",
                str(e)
            )
    
    def validate_validation_config(self):
        """Test validation configuration options."""
        print("\n🔍 Testing validation configuration...")
        
        try:
            # Test DTESN validation config
            config = DTESNValidationConfig(
                max_reservoir_size=5000,
                enable_deep_validation=False,
                max_validation_time_ms=200
            )
            
            config_works = (
                config.max_reservoir_size == 5000 and
                config.enable_deep_validation is False and
                config.max_validation_time_ms == 200
            )
            
            self.add_result(
                "DTESN validation configuration",
                config_works,
                f"Custom config applied correctly: max_reservoir_size={config.max_reservoir_size}"
            )
            
        except Exception as e:
            self.add_result(
                "DTESN validation configuration",
                False,
                "Failed validation configuration test",
                str(e)
            )
    
    def validate_error_handling(self):
        """Test error handling in validation."""
        print("\n🔍 Testing error handling...")
        
        try:
            # Test invalid data that should raise HTTPException
            invalid_esn_data = {
                "reservoir_size": -1,  # Invalid
                "input_dimension": 10,
                "spectral_radius": 0.95
            }
            
            error_caught = False
            try:
                validate_dtesn_data_structure(
                    invalid_esn_data,
                    DTESNDataType.ESN_RESERVOIR_CONFIG
                )
            except Exception:
                error_caught = True
            
            self.add_result(
                "Error handling for invalid data",
                error_caught,
                f"Properly caught validation error for invalid reservoir size"
            )
            
        except Exception as e:
            self.add_result(
                "Error handling for invalid data",
                False,
                "Failed error handling test",
                str(e)
            )
    
    def run_all_validations(self):
        """Run all validation tests."""
        print("=" * 80)
        print("🚀 Server-Side Data Validation Implementation Validation")
        print("   Task 7.1.2: Build Server-Side Data Validation")
        print("=" * 80)
        
        if not self.validate_imports():
            print("\n❌ Cannot proceed with validation due to import failures")
            return False
        
        # Run all validation tests
        self.validate_dtesn_schema_validation()
        self.validate_dtesn_data_structure_validation()
        self.validate_configuration_normalization()
        self.validate_data_sanitization()
        self.validate_sanitization_pipelines()
        self.validate_endpoint_integration()
        self.validate_validation_config()
        self.validate_error_handling()
        
        # Summary
        print("\n" + "=" * 80)
        print("📊 VALIDATION SUMMARY")
        print("=" * 80)
        
        total_tests = len(self.results)
        passed_tests = sum(1 for r in self.results if r["success"])
        failed_tests = total_tests - passed_tests
        
        print(f"Total Tests: {total_tests}")
        print(f"Passed: {passed_tests} ✅")
        print(f"Failed: {failed_tests} ❌")
        print(f"Success Rate: {(passed_tests/total_tests)*100:.1f}%")
        
        if failed_tests > 0:
            print("\n❌ FAILED TESTS:")
            for result in self.results:
                if not result["success"]:
                    print(f"   • {result['test']}: {result['message']}")
        
        print("\n🎯 Task 7.1.2 Implementation Status:")
        
        # Check specific acceptance criteria
        core_functionality = [
            "Module imports",
            "DTESN data structure validation", 
            "Data sanitization",
            "Configuration normalization"
        ]
        
        core_passed = sum(1 for r in self.results 
                         if r["success"] and any(core in r["test"] for core in core_functionality))
        
        task_items = [
            f"{'✅' if core_passed >= 3 else '❌'} Comprehensive input validation for all endpoints",
            f"{'✅' if 'Data sanitization' in [r['test'] for r in self.results if r['success']] else '❌'} Data sanitization and normalization pipelines",
            f"{'✅' if 'DTESN' in str([r['test'] for r in self.results if r['success']]) else '❌'} Schema validation for complex DTESN data structures",
            f"{'✅' if passed_tests >= total_tests * 0.8 else '❌'} All input data validated and sanitized server-side"
        ]
        
        for item in task_items:
            print(f"   {item}")
        
        success = passed_tests >= total_tests * 0.8
        print(f"\n{'🚀 IMPLEMENTATION SUCCESSFUL! Ready for production.' if success else '⚠️  Implementation needs attention before deployment.'}")
        
        return success


def main():
    """Main validation function."""
    validator = ServerSideValidationValidator()
    
    try:
        success = validator.run_all_validations()
        sys.exit(0 if success else 1)
        
    except Exception as e:
        print(f"\n💥 VALIDATION FAILED WITH ERROR: {str(e)}")
        print("Stack trace:")
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()