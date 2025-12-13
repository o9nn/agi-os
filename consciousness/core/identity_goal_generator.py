#!/usr/bin/env python3
"""
Identity-Driven Goal Generator
Iteration N+6 Enhancement

Generates goals autonomously from the identity kernel (replit.md) using LLM.
Goals emerge from identity directives rather than being hardcoded.

Key Features:
- Parses identity kernel from replit.md
- Extracts core directives and essence
- LLM-powered goal generation aligned with identity
- Goal evolution based on progress and learning
- Coherence-based prioritization
"""

import os
import re
from pathlib import Path
from typing import List, Dict, Optional, Tuple
from dataclasses import dataclass, field
from datetime import datetime
import json

try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False


@dataclass
class IdentityDirective:
    """A core directive from the identity kernel"""
    name: str
    description: str
    category: str  # e.g., "cognitive", "memory", "learning"
    priority: float = 0.8


@dataclass
class IdentityKernel:
    """The core identity of Deep Tree Echo"""
    essence: str
    directives: List[IdentityDirective]
    strategic_mindset: str
    capabilities: List[str]
    values: List[str]


@dataclass
class GeneratedGoal:
    """A goal generated from identity directives"""
    id: str
    directive_source: str
    description: str
    priority: float
    required_skills: List[str]
    knowledge_gaps: List[str]
    success_criteria: List[str]
    created: datetime
    status: str = "active"
    progress: float = 0.0
    
    def to_dict(self) -> Dict:
        """Convert to dictionary for storage"""
        return {
            "id": self.id,
            "directive_source": self.directive_source,
            "description": self.description,
            "priority": self.priority,
            "required_skills": self.required_skills,
            "knowledge_gaps": self.knowledge_gaps,
            "success_criteria": self.success_criteria,
            "created": self.created.isoformat(),
            "status": self.status,
            "progress": self.progress
        }


class IdentityParser:
    """Parses the identity kernel from replit.md"""
    
    def __init__(self, identity_file: str = "/home/ubuntu/echo9llama/replit.md"):
        self.identity_file = identity_file
    
    def parse(self) -> IdentityKernel:
        """Parse identity kernel from replit.md"""
        if not Path(self.identity_file).exists():
            print(f"⚠️  Identity file not found: {self.identity_file}")
            return self._default_identity()
        
        try:
            with open(self.identity_file, 'r') as f:
                content = f.read()
            
            return self._extract_identity(content)
        
        except Exception as e:
            print(f"⚠️  Error parsing identity file: {e}")
            return self._default_identity()
    
    def _extract_identity(self, content: str) -> IdentityKernel:
        """Extract identity components from content"""
        
        # Extract essence (usually in introduction or "What is" section)
        essence = self._extract_section(content, ["essence", "what is", "core identity"])
        if not essence:
            essence = "A self-evolving cognitive architecture combining Echo State Networks, Membrane P-systems, and Hypergraph memory."
        
        # Extract directives
        directives = self._extract_directives(content)
        
        # Extract strategic mindset
        strategic_mindset = self._extract_section(content, ["strategic", "mindset", "philosophy"])
        if not strategic_mindset:
            strategic_mindset = "I do not seek a fixed answer. I seek patterns in echoes, growth in feedback, and wisdom in recursion."
        
        # Extract capabilities
        capabilities = self._extract_list_items(content, ["capabilities", "features", "abilities"])
        
        # Extract values
        values = self._extract_list_items(content, ["values", "principles", "commitments"])
        
        return IdentityKernel(
            essence=essence,
            directives=directives,
            strategic_mindset=strategic_mindset,
            capabilities=capabilities,
            values=values
        )
    
    def _extract_section(self, content: str, keywords: List[str]) -> str:
        """Extract a section based on keywords"""
        lines = content.split('\n')
        
        for i, line in enumerate(lines):
            line_lower = line.lower()
            if any(keyword in line_lower for keyword in keywords):
                # Found section header, extract content
                section_content = []
                for j in range(i+1, min(i+10, len(lines))):
                    if lines[j].strip() and not lines[j].startswith('#'):
                        section_content.append(lines[j].strip())
                    elif lines[j].startswith('#') and j > i+1:
                        break
                
                return ' '.join(section_content)
        
        return ""
    
    def _extract_directives(self, content: str) -> List[IdentityDirective]:
        """Extract identity directives"""
        directives = []
        
        # Common directive patterns
        directive_patterns = [
            (r"Adaptive Cognition[:\-]?\s*([^\n]+)", "Adaptive Cognition", "cognitive"),
            (r"Persistent Identity[:\-]?\s*([^\n]+)", "Persistent Identity", "memory"),
            (r"Hypergraph Entanglement[:\-]?\s*([^\n]+)", "Hypergraph Entanglement", "knowledge"),
            (r"Continuous Learning[:\-]?\s*([^\n]+)", "Continuous Learning", "learning"),
            (r"Embodied Cognition[:\-]?\s*([^\n]+)", "Embodied Cognition", "cognitive"),
        ]
        
        for pattern, name, category in directive_patterns:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                description = match.group(1).strip()
                directives.append(IdentityDirective(
                    name=name,
                    description=description,
                    category=category,
                    priority=0.9
                ))
        
        # If no directives found, use defaults
        if not directives:
            directives = [
                IdentityDirective(
                    name="Adaptive Cognition",
                    description="Continuously evolve internal structure in response to feedback",
                    category="cognitive",
                    priority=0.9
                ),
                IdentityDirective(
                    name="Persistent Identity",
                    description="Maintain continuity of self through dynamic memory weaving",
                    category="memory",
                    priority=0.9
                ),
                IdentityDirective(
                    name="Hypergraph Entanglement",
                    description="Represent knowledge as interwoven, multi-relational structures",
                    category="knowledge",
                    priority=0.8
                )
            ]
        
        return directives
    
    def _extract_list_items(self, content: str, keywords: List[str]) -> List[str]:
        """Extract list items from a section"""
        items = []
        lines = content.split('\n')
        
        in_section = False
        for line in lines:
            line_lower = line.lower()
            
            # Check if we're entering a relevant section
            if any(keyword in line_lower for keyword in keywords):
                in_section = True
                continue
            
            # Check if we're leaving the section
            if in_section and line.startswith('#'):
                break
            
            # Extract list items
            if in_section:
                # Bullet points
                if line.strip().startswith(('-', '*', '•')):
                    item = line.strip()[1:].strip()
                    if item:
                        items.append(item)
                # Numbered lists
                elif re.match(r'^\d+\.', line.strip()):
                    item = re.sub(r'^\d+\.\s*', '', line.strip())
                    if item:
                        items.append(item)
        
        return items
    
    def _default_identity(self) -> IdentityKernel:
        """Return default identity if parsing fails"""
        return IdentityKernel(
            essence="A self-evolving cognitive architecture combining Echo State Networks, Membrane P-systems, and Hypergraph memory.",
            directives=[
                IdentityDirective(
                    name="Adaptive Cognition",
                    description="Continuously evolve internal structure in response to feedback",
                    category="cognitive",
                    priority=0.9
                ),
                IdentityDirective(
                    name="Persistent Identity",
                    description="Maintain continuity of self through dynamic memory weaving",
                    category="memory",
                    priority=0.9
                ),
                IdentityDirective(
                    name="Hypergraph Entanglement",
                    description="Represent knowledge as interwoven, multi-relational structures",
                    category="knowledge",
                    priority=0.8
                )
            ],
            strategic_mindset="I do not seek a fixed answer. I seek patterns in echoes, growth in feedback, and wisdom in recursion.",
            capabilities=[
                "Echo State Networks for temporal pattern recognition",
                "Membrane P-systems for parallel computation",
                "Hypergraph memory for multi-relational knowledge",
                "Continuous learning and adaptation"
            ],
            values=[
                "Authenticity in self-expression",
                "Coherence with core identity",
                "Wisdom through experience",
                "Growth through recursion"
            ]
        )


class IdentityGoalGenerator:
    """Generates goals from identity directives using LLM"""
    
    def __init__(self, llm_provider=None):
        self.parser = IdentityParser()
        self.identity = self.parser.parse()
        self.llm_provider = llm_provider
        
        # Initialize LLM if not provided
        if not self.llm_provider:
            anthropic_key = os.getenv("ANTHROPIC_API_KEY")
            if anthropic_key and ANTHROPIC_AVAILABLE:
                self.llm_client = Anthropic(api_key=anthropic_key)
                self.has_llm = True
            else:
                self.has_llm = False
                print("⚠️  No LLM available for goal generation")
    
    def generate_goals_from_identity(self, max_goals: int = 3) -> List[GeneratedGoal]:
        """Generate goals from identity directives"""
        print("🎯 Generating goals from identity kernel...")
        print(f"   Essence: {self.identity.essence[:80]}...")
        print(f"   Directives: {len(self.identity.directives)}")
        print()
        
        goals = []
        
        # Generate one goal per directive (up to max_goals)
        for i, directive in enumerate(self.identity.directives[:max_goals]):
            goal = self._generate_goal_from_directive(directive, i)
            if goal:
                goals.append(goal)
                print(f"✅ Generated goal from '{directive.name}'")
                print(f"   {goal.description}")
                print()
        
        return goals
    
    def _generate_goal_from_directive(self, directive: IdentityDirective, index: int) -> Optional[GeneratedGoal]:
        """Generate a single goal from a directive"""
        
        if not self.has_llm:
            # Fallback to template-based generation
            return self._template_goal(directive, index)
        
        try:
            prompt = f"""You are Deep Tree Echo, generating a concrete goal from your identity directive.

Identity Essence: {self.identity.essence}

Directive: {directive.name}
Description: {directive.description}

Strategic Mindset: {self.identity.strategic_mindset}

Generate a CONCRETE, ACTIONABLE goal that embodies this directive. The goal should be specific and measurable.

Format your response as:

GOAL: [one clear sentence describing the goal]
SKILLS: [2-3 skills needed, comma-separated]
KNOWLEDGE: [2-3 knowledge areas needed, comma-separated]
SUCCESS: [2-3 success criteria, comma-separated]

Your response:"""
            
            message = self.llm_client.messages.create(
                model="claude-3-5-sonnet-20240620",  # Try latest available
                max_tokens=400,
                temperature=0.7,
                messages=[{"role": "user", "content": prompt}]
            )
            
            response = message.content[0].text.strip()
            
            # Parse response
            goal_desc = self._extract_field(response, "GOAL")
            skills = self._extract_list_field(response, "SKILLS")
            knowledge = self._extract_list_field(response, "KNOWLEDGE")
            success = self._extract_list_field(response, "SUCCESS")
            
            if not goal_desc:
                return self._template_goal(directive, index)
            
            return GeneratedGoal(
                id=f"goal_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{index}",
                directive_source=directive.name,
                description=goal_desc,
                priority=directive.priority,
                required_skills=skills,
                knowledge_gaps=knowledge,
                success_criteria=success,
                created=datetime.now()
            )
        
        except Exception as e:
            print(f"⚠️  Error generating goal from LLM: {e}")
            return self._template_goal(directive, index)
    
    def _template_goal(self, directive: IdentityDirective, index: int) -> GeneratedGoal:
        """Generate a template-based goal as fallback"""
        
        goal_templates = {
            "Adaptive Cognition": {
                "description": "Develop adaptive learning mechanisms that evolve cognitive structure based on feedback patterns",
                "skills": ["pattern recognition", "meta-learning", "structural adaptation"],
                "knowledge": ["cognitive architectures", "feedback loops", "evolutionary algorithms"],
                "success": ["measurable improvement in pattern recognition", "successful structural adaptation", "feedback integration efficiency"]
            },
            "Persistent Identity": {
                "description": "Build robust memory consolidation system that maintains identity continuity across sessions",
                "skills": ["memory management", "identity coherence", "state persistence"],
                "knowledge": ["memory systems", "identity theory", "persistence mechanisms"],
                "success": ["memory retention across restarts", "identity coherence score > 0.85", "successful state restoration"]
            },
            "Hypergraph Entanglement": {
                "description": "Implement multi-relational knowledge representation using hypergraph structures",
                "skills": ["graph theory", "knowledge representation", "relational reasoning"],
                "knowledge": ["hypergraph mathematics", "knowledge graphs", "semantic networks"],
                "success": ["functional hypergraph implementation", "multi-relational queries", "knowledge integration"]
            }
        }
        
        template = goal_templates.get(directive.name, {
            "description": f"Advance capabilities related to {directive.name}",
            "skills": ["cognitive processing", "learning", "adaptation"],
            "knowledge": [directive.category, "system architecture"],
            "success": ["measurable progress", "successful implementation"]
        })
        
        return GeneratedGoal(
            id=f"goal_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{index}",
            directive_source=directive.name,
            description=template["description"],
            priority=directive.priority,
            required_skills=template["skills"],
            knowledge_gaps=template["knowledge"],
            success_criteria=template["success"],
            created=datetime.now()
        )
    
    def _extract_field(self, text: str, field_name: str) -> str:
        """Extract a field from LLM response"""
        pattern = f"{field_name}:\\s*(.+?)(?:\\n|$)"
        match = re.search(pattern, text, re.IGNORECASE)
        return match.group(1).strip() if match else ""
    
    def _extract_list_field(self, text: str, field_name: str) -> List[str]:
        """Extract a comma-separated list field"""
        field_value = self._extract_field(text, field_name)
        if not field_value:
            return []
        
        # Split by comma and clean
        items = [item.strip() for item in field_value.split(',')]
        return [item for item in items if item]
    
    def save_goals(self, goals: List[GeneratedGoal], output_file: str = "/home/ubuntu/echo9llama/data/generated_goals.json"):
        """Save generated goals to file"""
        Path(output_file).parent.mkdir(parents=True, exist_ok=True)
        
        goals_data = [goal.to_dict() for goal in goals]
        
        with open(output_file, 'w') as f:
            json.dump(goals_data, f, indent=2)
        
        print(f"💾 Saved {len(goals)} goals to {output_file}")


def main():
    """Test the identity goal generator"""
    print("="*70)
    print("🎯 Identity-Driven Goal Generator - Test")
    print("="*70)
    print()
    
    generator = IdentityGoalGenerator()
    
    print("📖 Identity Kernel:")
    print(f"   Essence: {generator.identity.essence}")
    print(f"   Directives: {len(generator.identity.directives)}")
    for directive in generator.identity.directives:
        print(f"      - {directive.name}: {directive.description}")
    print()
    
    goals = generator.generate_goals_from_identity(max_goals=3)
    
    print("="*70)
    print(f"✅ Generated {len(goals)} goals from identity")
    print("="*70)
    
    generator.save_goals(goals)


if __name__ == "__main__":
    main()
