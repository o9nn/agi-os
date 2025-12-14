"""
DTESN Bridge for Echo-Self AI Evolution Engine.

Integrates the evolution engine with the Deep Tree Echo System Network (DTESN)
components from echo.kern for membrane computing and reservoir dynamics.
"""

import logging
import numpy as np
from typing import Dict, Any

# Handle both absolute and relative imports
try:
    from core.interfaces import Individual
except ImportError:
    from ..core.interfaces import Individual

logger = logging.getLogger(__name__)


class DTESNBridge:
    """Bridge between Echo-Self Evolution Engine and DTESN components."""
    
    def __init__(self):
        self.dtesn_kernel = None
        self.membrane_system = None
        self.reservoir = None
        self.b_series_calculator = None
        self._initialized = False
    
    def initialize(self) -> bool:
        """Initialize DTESN components."""
        try:
            # Import DTESN components
            self._import_dtesn_components()
            
            # Initialize components
            self._initialize_components()
            
            self._initialized = True
            logger.info("DTESN Bridge initialized successfully")
            return True
            
        except ImportError as e:
            logger.warning(f"DTESN components not available: {e}")
            return False
        except Exception as e:
            logger.error(f"Failed to initialize DTESN Bridge: {e}")
            return False
    
    def _import_dtesn_components(self):
        """Import DTESN components from echo.kern."""
        try:
            # Prefer absolute imports for installed package
            from echo.kern.psystem_membranes import PSystemMembranes
            self.PSystemMembranes = PSystemMembranes

            from echo.kern.esn_reservoir import ESNReservoir
            self.ESNReservoir = ESNReservoir

            from echo.kern.bseries_differential_calculator import BSeriesCalculator
            self.BSeriesCalculator = BSeriesCalculator

            # Import DTESN compiler if available
            try:
                from echo.kern.dtesn_compiler import DTESNCompiler
                self.DTESNCompiler = DTESNCompiler
            except ImportError:
                logger.warning("DTESN Compiler not available")
                self.DTESNCompiler = None

        except ImportError as e:
            logger.warning(f"Some DTESN components not available: {e}")
            # Try alternative import paths
            self._try_alternative_imports()
    
    def _try_alternative_imports(self):
        """Try alternative import paths for DTESN components."""
        try:
            import sys
            import os
            
            # Add echo.kern to path
            echo_kern_path = os.path.join(os.path.dirname(__file__), '..', '..', 'echo.kern')
            if echo_kern_path not in sys.path:
                sys.path.insert(0, echo_kern_path)
            
            # Retry imports
            import psystem_membranes
            import esn_reservoir
            import bseries_differential_calculator
            
            self.PSystemMembranes = psystem_membranes.PSystemMembraneHierarchy
            self.ESNReservoir = esn_reservoir.ESNReservoir
            self.BSeriesCalculator = (
                bseries_differential_calculator.BSeriesDifferentialCalculator
            )
            
        except ImportError as e:
            logger.error(f"Could not import DTESN components: {e}")
            raise
    
    def _initialize_components(self):
        """Initialize DTESN components."""
        if not hasattr(self, 'PSystemMembranes'):
            raise ImportError("DTESN components not properly imported")
        
        # Initialize P-System membranes
        self.membrane_system = self.PSystemMembranes()
        
        # Initialize ESN reservoir with proper configuration
        import esn_reservoir
        reservoir_config = esn_reservoir.ESNConfiguration(
            reservoir_size=512,
            input_dimension=64,
            output_dimension=32,
            spectral_radius=0.9,
            leak_rate=0.1,
            sparsity_level=0.1
        )
        self.reservoir = self.ESNReservoir(reservoir_config)
        
        # Initialize B-Series calculator
        self.b_series_calculator = self.BSeriesCalculator()
        
        logger.info("DTESN components initialized")
    
    def is_initialized(self) -> bool:
        """Check if the bridge is initialized."""
        return self._initialized
    
    def process_individual_through_dtesn(self, individual: Individual) -> Dict[str, Any]:
        """Process an individual through DTESN components."""
        if not self._initialized:
            logger.warning("DTESN Bridge not initialized, returning empty results")
            return {}
        
        try:
            results = {}
            
            # Process through membrane system
            if self.membrane_system:
                membrane_state = self._process_through_membranes(individual)
                results['membrane_state'] = membrane_state
            
            # Process through reservoir
            if self.reservoir:
                reservoir_state = self._process_through_reservoir(individual)
                results['reservoir_state'] = reservoir_state
            
            # Calculate B-Series dynamics
            if self.b_series_calculator:
                dynamics = self._calculate_dynamics(individual)
                results['dynamics'] = dynamics
            
            return results
            
        except Exception as e:
            logger.error(f"Error processing individual through DTESN: {e}")
            return {}
    
    def _process_through_membranes(self, individual: Individual) -> Dict[str, Any]:
        """Process individual through P-System membranes."""
        try:
            # Convert individual genome to membrane input format
            genome_data = self._genome_to_membrane_format(individual.genome)
            
            # Get current system stats as a proxy for membrane processing
            system_stats = self.membrane_system.get_system_stats()
            
            return {
                'state': genome_data.tolist(),
                'active_membranes': system_stats.get('total_membranes', 1),
                'membrane_depth': system_stats.get('max_depth', 1),
                'processing_successful': True
            }
            
        except Exception as e:
            logger.error(f"Error in membrane processing: {e}")
            return {
                'state': [0.0] * 10,  # Default state
                'active_membranes': 1,
                'membrane_depth': 1,
                'processing_successful': False
            }
    
    def _process_through_reservoir(self, individual: Individual) -> Dict[str, Any]:
        """Process individual through ESN reservoir."""
        try:
            # Convert individual to reservoir input
            reservoir_input = self._genome_to_reservoir_format(individual.genome)
            
            # Update reservoir state with input
            self.reservoir.update_state(reservoir_input)
            
            # Get current reservoir state
            hidden_state = self.reservoir.reservoir_state
            
            return {
                'hidden_state': hidden_state.tolist() if hasattr(hidden_state, 'tolist') else list(hidden_state),
                'input_data': reservoir_input.tolist(),
                'reservoir_size': len(hidden_state),
                'processing_successful': True
            }
            
        except Exception as e:
            logger.error(f"Error in reservoir processing: {e}")
            # Return reasonable defaults
            return {
                'hidden_state': [0.0] * 512,  # Default reservoir size
                'input_data': [0.0] * 64,     # Default input size 
                'reservoir_size': 512,
                'processing_successful': False
            }
    
    def _calculate_dynamics(self, individual: Individual) -> Dict[str, Any]:
        """Calculate B-Series dynamics for individual."""
        try:
            # Create simple differential function for testing
            from bseries_differential_calculator import DifferentialFunction
            
            # Simple test function f(y) = y and f'(y) = 1
            test_function = lambda y: y
            test_derivative = lambda y: 1.0
            
            df = DifferentialFunction(test_function, test_derivative, name="test")
            
            # Use tree_id=1 (single node) and sample y value
            y_val = float(np.mean(individual.genome))
            result = self.b_series_calculator.evaluate_elementary_differential(1, df, y_val)
            
            return {
                'differential_result': result,
                'tree_id': 1,
                'y_value': y_val,
                'processing_successful': True
            }
            
        except Exception as e:
            logger.error(f"Error in dynamics calculation: {e}")
            return {
                'differential_result': 0.0,
                'tree_id': 1,
                'y_value': 0.0,
                'processing_successful': False
            }
    
    def _genome_to_membrane_format(self, genome) -> np.ndarray:
        """Convert individual genome to membrane-compatible format."""
        if hasattr(genome, '__iter__') and not isinstance(genome, str):
            return np.array(list(genome), dtype=np.float32)
        else:
            return np.array([float(genome)], dtype=np.float32)
    
    def _genome_to_reservoir_format(self, genome) -> np.ndarray:
        """Convert individual genome to reservoir input format."""
        genome_array = self._genome_to_membrane_format(genome)
        
        # Pad or truncate to expected input dimension (64)
        expected_size = 64
        if len(genome_array) < expected_size:
            # Pad with zeros
            padded = np.zeros(expected_size, dtype=np.float32)
            padded[:len(genome_array)] = genome_array
            return padded
        else:
            # Truncate
            return genome_array[:expected_size].astype(np.float32)
    
    def update_individual_with_dtesn_feedback(
        self, 
        individual: Individual, 
        dtesn_results: Dict[str, Any]
    ) -> Individual:
        """Update individual based on DTESN processing feedback."""
        if not dtesn_results:
            return individual
        
        try:
            # Update individual metadata with DTESN results
            if not hasattr(individual, 'dtesn_data'):
                individual.dtesn_data = {}
            
            individual.dtesn_data.update(dtesn_results)
            
            # Optionally modify fitness based on DTESN results
            if 'membrane_state' in dtesn_results:
                membrane_complexity = dtesn_results['membrane_state'].get('active_membranes', 1)
                # Reward moderate complexity
                complexity_bonus = 1.0 - abs(membrane_complexity - 5) / 10.0
                individual.fitness *= max(0.5, complexity_bonus)
            
            return individual
            
        except Exception as e:
            logger.error(f"Error updating individual with DTESN feedback: {e}")
            return individual
    
    def get_dtesn_statistics(self) -> Dict[str, Any]:
        """Get statistics about DTESN integration."""
        if not self._initialized:
            return {'status': 'not_initialized'}
        
        stats = {
            'status': 'initialized',
            'components': {
                'membrane_system': self.membrane_system is not None,
                'reservoir': self.reservoir is not None,
                'b_series_calculator': self.b_series_calculator is not None
            }
        }
        
        # Add component-specific stats if available
        if self.reservoir and hasattr(self.reservoir, 'get_stats'):
            stats['reservoir_stats'] = self.reservoir.get_stats()
        
        return stats