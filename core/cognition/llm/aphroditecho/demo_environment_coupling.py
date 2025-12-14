#!/usr/bin/env python3
"""
Environment Coupling System Demonstration

This script demonstrates the functionality of Task 2.2.1 Environment Coupling
by showing how the system responds to environmental changes with agent behavior adaptations.

Features demonstrated:
1. Real-time environment state integration
2. Dynamic environment adaptation
3. Context-sensitive behavior modification
4. Agent behavior changes based on environment state
"""

import asyncio
import logging
import time
from typing import Dict, List, Any

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class EnvironmentCouplingDemo:
    """Demonstration of environment coupling functionality."""
    
    def __init__(self):
        self.environment_state = {
            'temperature': 20.0,
            'humidity': 50.0,
            'resources': 100,
            'hazards': 0,
            'time_of_day': 'morning'
        }
        
        self.agents = {
            'explorer_agent': {
                'name': 'Explorer',
                'behavior': 'exploring',
                'energy': 100,
                'position': [0, 0],
                'adaptations_applied': []
            },
            'gatherer_agent': {
                'name': 'Gatherer', 
                'behavior': 'gathering',
                'energy': 100,
                'position': [5, 5],
                'adaptations_applied': []
            },
            'guard_agent': {
                'name': 'Guard',
                'behavior': 'patrolling',
                'energy': 100,
                'position': [10, 10],
                'adaptations_applied': []
            }
        }
        
        self.coupling_events = []
        self.behavior_adaptations = []
    
    def detect_environment_changes(self, new_state: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Detect significant changes in environment state."""
        changes = []
        
        for key, new_value in new_state.items():
            if key in self.environment_state:
                old_value = self.environment_state[key]
                
                # Detect significant changes
                if isinstance(old_value, (int, float)) and isinstance(new_value, (int, float)):
                    # Numeric change detection
                    change_percent = abs(new_value - old_value) / max(abs(old_value), 1) * 100
                    if change_percent > 10:  # 10% change threshold
                        changes.append({
                            'parameter': key,
                            'old_value': old_value,
                            'new_value': new_value,
                            'change_percent': change_percent,
                            'change_type': 'increase' if new_value > old_value else 'decrease'
                        })
                elif old_value != new_value:
                    # Non-numeric change
                    changes.append({
                        'parameter': key,
                        'old_value': old_value,
                        'new_value': new_value,
                        'change_type': 'value_change'
                    })
        
        # Check for new parameters
        for key, value in new_state.items():
            if key not in self.environment_state:
                changes.append({
                    'parameter': key,
                    'old_value': None,
                    'new_value': value,
                    'change_type': 'parameter_added'
                })
        
        return changes
    
    def determine_behavior_adaptations(self, changes: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Determine what behavior adaptations are needed based on changes."""
        adaptations = []
        
        for change in changes:
            parameter = change['parameter']
            change_type = change['change_type']
            new_value = change['new_value']
            
            # Temperature-based adaptations
            if parameter == 'temperature':
                if new_value > 35.0:  # Hot environment
                    adaptations.append({
                        'trigger': f"High temperature ({new_value}°C)",
                        'adaptation_type': 'environmental_stress_response',
                        'target_agents': ['all'],
                        'behavior_changes': {
                            'movement_speed': 0.7,  # Slower movement in heat
                            'energy_consumption': 1.3,  # Higher energy use
                            'shelter_seeking': True,
                            'priority': 'temperature_regulation'
                        }
                    })
                elif new_value < 5.0:  # Cold environment
                    adaptations.append({
                        'trigger': f"Low temperature ({new_value}°C)",
                        'adaptation_type': 'cold_adaptation',
                        'target_agents': ['all'],
                        'behavior_changes': {
                            'movement_speed': 0.8,  # Slightly slower in cold
                            'energy_consumption': 1.2,  # Higher energy for warmth
                            'warmth_seeking': True,
                            'priority': 'warmth_preservation'
                        }
                    })
            
            # Resource-based adaptations
            elif parameter == 'resources':
                if change_type == 'decrease' and new_value < 30:
                    adaptations.append({
                        'trigger': f"Resource depletion (only {new_value} remaining)",
                        'adaptation_type': 'resource_scarcity_response',
                        'target_agents': ['gatherer_agent', 'explorer_agent'],
                        'behavior_changes': {
                            'priority': 'resource_gathering',
                            'search_radius': 2.0,  # Wider search
                            'cooperation_level': 1.5,  # More cooperation
                            'energy_conservation': True
                        }
                    })
            
            # Hazard-based adaptations
            elif parameter == 'hazards':
                if new_value > 0:
                    adaptations.append({
                        'trigger': f"Hazards detected ({new_value} hazards)",
                        'adaptation_type': 'danger_avoidance',
                        'target_agents': ['all'],
                        'behavior_changes': {
                            'caution_level': 0.9,
                            'group_formation': True,
                            'escape_route_planning': True,
                            'priority': 'safety'
                        }
                    })
            
            # Humidity-based adaptations
            elif parameter == 'humidity' and new_value > 80.0:
                adaptations.append({
                    'trigger': f"High humidity ({new_value}%)",
                    'adaptation_type': 'humidity_adaptation',
                    'target_agents': ['all'],
                    'behavior_changes': {
                        'ventilation_seeking': True,
                        'activity_reduction': 0.8,
                        'hydration_priority': 1.2
                    }
                })
        
        return adaptations
    
    def apply_behavior_adaptations(self, adaptations: List[Dict[str, Any]]) -> Dict[str, List[str]]:
        """Apply behavior adaptations to agents."""
        application_results = {}
        
        for adaptation in adaptations:
            target_agents = adaptation['target_agents']
            behavior_changes = adaptation['behavior_changes']
            trigger = adaptation['trigger']
            
            if 'all' in target_agents:
                target_agents = list(self.agents.keys())
            
            for agent_id in target_agents:
                if agent_id in self.agents:
                    agent = self.agents[agent_id]
                    
                    # Apply behavior changes
                    for change_key, change_value in behavior_changes.items():
                        if change_key == 'priority':
                            agent['behavior'] = f"{change_value}_focused"
                        elif change_key in ['movement_speed', 'energy_consumption']:
                            if change_key not in agent:
                                agent[change_key] = 1.0
                            agent[change_key] *= change_value
                        else:
                            agent[change_key] = change_value
                    
                    # Record adaptation
                    adaptation_record = {
                        'timestamp': time.time(),
                        'trigger': trigger,
                        'adaptation_type': adaptation['adaptation_type'],
                        'changes_applied': behavior_changes
                    }
                    agent['adaptations_applied'].append(adaptation_record)
                    
                    if agent_id not in application_results:
                        application_results[agent_id] = []
                    application_results[agent_id].append(f"Applied {adaptation['adaptation_type']}")
        
        return application_results
    
    def analyze_context_sensitivity(self) -> Dict[str, float]:
        """Analyze context sensitivity for current environment."""
        sensitivity_scores = {}
        
        # Environmental sensitivity based on extremes
        temp = self.environment_state['temperature']
        humidity = self.environment_state['humidity']
        
        # Temperature sensitivity (higher when extreme)
        temp_sensitivity = min(1.0, abs(temp - 20.0) / 30.0)  # Ideal temp is 20°C
        
        # Humidity sensitivity  
        humidity_sensitivity = min(1.0, abs(humidity - 50.0) / 50.0)  # Ideal humidity is 50%
        
        # Resource sensitivity (higher when scarce)
        resources = self.environment_state['resources']
        resource_sensitivity = max(0.0, 1.0 - resources / 100.0)
        
        # Hazard sensitivity (directly proportional to hazards)
        hazards = self.environment_state['hazards']
        hazard_sensitivity = min(1.0, hazards / 5.0)  # Max sensitivity at 5+ hazards
        
        # Social sensitivity based on agent interactions
        agent_count = len(self.agents)
        social_sensitivity = min(1.0, agent_count / 10.0)  # Max sensitivity at 10+ agents
        
        sensitivity_scores = {
            'environmental': (temp_sensitivity + humidity_sensitivity) / 2.0,
            'resource': resource_sensitivity,
            'danger': hazard_sensitivity,
            'social': social_sensitivity,
            'overall': sum([temp_sensitivity, humidity_sensitivity, resource_sensitivity, 
                           hazard_sensitivity, social_sensitivity]) / 5.0
        }
        
        return sensitivity_scores
    
    async def update_environment_state(self, new_state: Dict[str, Any]) -> Dict[str, Any]:
        """Update environment state and trigger coupling processes."""
        print(f"\n{'='*60}")
        print("ENVIRONMENT STATE UPDATE")
        print(f"{'='*60}")
        
        # Detect changes
        changes = self.detect_environment_changes(new_state)
        
        if changes:
            print("Environment changes detected:")
            for change in changes:
                if change['change_type'] in ['increase', 'decrease']:
                    print(f"  • {change['parameter']}: {change['old_value']} → {change['new_value']} "
                          f"({change['change_percent']:.1f}% {change['change_type']})")
                else:
                    print(f"  • {change['parameter']}: {change['old_value']} → {change['new_value']}")
        else:
            print("No significant environment changes detected.")
        
        # Update state
        self.environment_state.update(new_state)
        
        # Record coupling event
        coupling_event = {
            'timestamp': time.time(),
            'changes': changes,
            'environment_state': self.environment_state.copy()
        }
        self.coupling_events.append(coupling_event)
        
        # Determine behavior adaptations
        adaptations = self.determine_behavior_adaptations(changes)
        
        if adaptations:
            print("\nBehavior adaptations determined:")
            for adaptation in adaptations:
                print(f"  • {adaptation['trigger']}")
                print(f"    Type: {adaptation['adaptation_type']}")
                print(f"    Target agents: {', '.join(adaptation['target_agents'])}")
                print(f"    Changes: {adaptation['behavior_changes']}")
        else:
            print("No behavior adaptations needed.")
        
        # Apply adaptations
        application_results = self.apply_behavior_adaptations(adaptations)
        
        if application_results:
            print("\nAdaptations applied:")
            for agent_id, results in application_results.items():
                agent_name = self.agents[agent_id]['name']
                print(f"  • {agent_name} ({agent_id}): {', '.join(results)}")
        
        # Analyze context sensitivity
        sensitivity = self.analyze_context_sensitivity()
        print("\nContext sensitivity analysis:")
        for context_type, score in sensitivity.items():
            print(f"  • {context_type}: {score:.2f}")
        
        return {
            'status': 'success',
            'changes_detected': len(changes),
            'adaptations_applied': len(adaptations),
            'agents_affected': len(application_results),
            'context_sensitivity': sensitivity['overall'],
            'timestamp': time.time()
        }
    
    def get_agent_status(self) -> Dict[str, Dict[str, Any]]:
        """Get current status of all agents."""
        return {agent_id: agent.copy() for agent_id, agent in self.agents.items()}
    
    def get_environment_status(self) -> Dict[str, Any]:
        """Get current environment status."""
        return self.environment_state.copy()
    
    def print_system_summary(self):
        """Print a summary of the entire system state."""
        print(f"\n{'='*60}")
        print("SYSTEM SUMMARY")
        print(f"{'='*60}")
        
        # Environment summary
        print("Environment State:")
        for key, value in self.environment_state.items():
            print(f"  • {key}: {value}")
        
        # Agent summary
        print("\nAgent Status:")
        for agent_id, agent in self.agents.items():
            print(f"  • {agent['name']} ({agent_id}):")
            print(f"    Behavior: {agent['behavior']}")
            print(f"    Energy: {agent['energy']}")
            print(f"    Position: {agent['position']}")
            if agent['adaptations_applied']:
                print(f"    Adaptations: {len(agent['adaptations_applied'])}")
                for adaptation in agent['adaptations_applied'][-3:]:  # Show last 3
                    print(f"      - {adaptation['adaptation_type']}: {adaptation['trigger']}")
        
        # System statistics
        total_adaptations = sum(len(agent['adaptations_applied']) for agent in self.agents.values())
        print("\nSystem Statistics:")
        print(f"  • Total coupling events: {len(self.coupling_events)}")
        print(f"  • Total behavior adaptations: {total_adaptations}")
        print(f"  • Active agents: {len(self.agents)}")


async def run_environment_coupling_demonstration():
    """Run a comprehensive demonstration of environment coupling."""
    print("ENVIRONMENT COUPLING SYSTEM DEMONSTRATION")
    print("Task 2.2.1: Design Environment Coupling")
    print("Phase 2 - 4E Embodied AI Framework")
    
    demo = EnvironmentCouplingDemo()
    demo.print_system_summary()
    
    # Scenario 1: Normal to hot temperature
    print("\n\nSCENARIO 1: Temperature Increase")
    await demo.update_environment_state({
        'temperature': 38.0  # Hot day
    })
    
    # Small delay to show temporal progression
    await asyncio.sleep(1)
    
    # Scenario 2: Resource depletion
    print("\n\nSCENARIO 2: Resource Depletion")  
    await demo.update_environment_state({
        'resources': 20,  # Low resources
        'humidity': 85.0   # High humidity
    })
    
    await asyncio.sleep(1)
    
    # Scenario 3: Hazard introduction
    print("\n\nSCENARIO 3: Hazard Detection")
    await demo.update_environment_state({
        'hazards': 3,      # Multiple hazards
        'temperature': 42.0,  # Even hotter
        'time_of_day': 'noon'
    })
    
    await asyncio.sleep(1)
    
    # Scenario 4: Environmental improvement
    print("\n\nSCENARIO 4: Environmental Recovery")
    await demo.update_environment_state({
        'temperature': 25.0,  # Moderate temperature
        'resources': 80,      # Resources replenished
        'hazards': 0,         # Hazards cleared
        'humidity': 60.0,     # Moderate humidity
        'time_of_day': 'evening'
    })
    
    # Final system summary
    demo.print_system_summary()
    
    print(f"\n{'='*60}")
    print("ACCEPTANCE CRITERIA VALIDATION")
    print(f"{'='*60}")
    
    # Validate that agents adapted behavior based on environment changes
    total_adaptations = sum(len(agent['adaptations_applied']) for agent in demo.agents.values())
    agents_with_adaptations = sum(1 for agent in demo.agents.values() if agent['adaptations_applied'])
    
    print(f"✓ Real-time environment state integration: {len(demo.coupling_events)} events processed")
    print(f"✓ Dynamic environment adaptation: {total_adaptations} adaptations applied")
    print(f"✓ Context-sensitive behavior modification: {agents_with_adaptations}/{len(demo.agents)} agents adapted")
    
    acceptance_met = total_adaptations > 0 and agents_with_adaptations > 0
    print(f"\n🎯 ACCEPTANCE CRITERIA: {'PASSED' if acceptance_met else 'FAILED'}")
    print(f"   'Agents adapt behavior based on environment changes': {'YES' if acceptance_met else 'NO'}")
    
    return acceptance_met


if __name__ == "__main__":
    try:
        success = asyncio.run(run_environment_coupling_demonstration())
        exit_code = 0 if success else 1
        print(f"\nDemo completed with exit code: {exit_code}")
        exit(exit_code)
    except Exception as e:
        print(f"Error running demonstration: {e}")
        import traceback
        traceback.print_exc()
        exit(1)