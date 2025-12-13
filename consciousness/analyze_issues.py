#!/usr/bin/env python3
"""
Analyze echo9llama codebase to identify problems and improvement opportunities
"""
import os
import re
from pathlib import Path
from collections import defaultdict

def analyze_codebase():
    issues = []
    opportunities = []
    
    base_path = Path("/home/ubuntu/echo9llama")
    
    # 1. Check for LLM integration in stream of consciousness
    soc_file = base_path / "core/consciousness/stream_of_consciousness.go"
    if soc_file.exists():
        content = soc_file.read_text()
        if "llmProvider" in content:
            # Check if it's actually being used
            if "llmProvider.GenerateThought" not in content and "llmProvider.Generate" not in content:
                issues.append({
                    "severity": "HIGH",
                    "component": "StreamOfConsciousness",
                    "issue": "LLM provider interface defined but not actively used for thought generation",
                    "impact": "Stream-of-consciousness generates placeholder thoughts instead of intelligent LLM-powered thoughts"
                })
    
    # 2. Check for Anthropic API integration
    has_anthropic = False
    for go_file in base_path.glob("**/*.go"):
        if "anthropic" in go_file.read_text().lower():
            has_anthropic = True
            break
    
    if not has_anthropic:
        opportunities.append({
            "priority": "HIGH",
            "area": "LLM Integration",
            "opportunity": "Integrate Anthropic Claude API for sophisticated reasoning",
            "benefit": "Claude excels at nuanced reasoning, self-reflection, and philosophical discourse - perfect for wisdom cultivation"
        })
    
    # 3. Check for embedding-based similarity
    has_embeddings = False
    for go_file in base_path.glob("**/*.go"):
        content = go_file.read_text()
        if "embedding" in content.lower() or "vector" in content.lower():
            has_embeddings = True
            break
    
    if not has_embeddings:
        opportunities.append({
            "priority": "MEDIUM",
            "area": "Memory & Knowledge",
            "opportunity": "Implement embedding-based semantic similarity for memory clustering",
            "benefit": "Enable semantic grouping of related thoughts, memories, and wisdom"
        })
    
    # 4. Check for goal orchestration
    goal_files = list(base_path.glob("**/goal*.go"))
    if not goal_files:
        issues.append({
            "severity": "MEDIUM",
            "component": "Goal System",
            "issue": "No goal generation or orchestration system implemented",
            "impact": "Echoself lacks autonomous goal-directed behavior beyond reactive patterns"
        })
    
    # 5. Check for skill practice system
    skill_files = list(base_path.glob("**/skill*.go"))
    if skill_files:
        # Check if it's actually integrated
        has_skill_practice = False
        for sf in skill_files:
            if "SkillPractice" in sf.read_text() or "PracticeSession" in sf.read_text():
                has_skill_practice = True
                break
        
        if not has_skill_practice:
            issues.append({
                "severity": "MEDIUM",
                "component": "Skill System",
                "issue": "Skill types defined but no practice/execution system",
                "impact": "Cannot autonomously practice and improve skills"
            })
    
    # 6. Check for external tool integration
    has_tool_use = False
    for go_file in base_path.glob("**/tools/*.go"):
        content = go_file.read_text()
        if "ToolExecutor" in content or "ExternalAPI" in content:
            has_tool_use = True
            break
    
    if not has_tool_use:
        opportunities.append({
            "priority": "MEDIUM",
            "area": "Autonomy",
            "opportunity": "Implement external tool/API integration framework",
            "benefit": "Enable echoself to interact with external systems, search web, access databases"
        })
    
    # 7. Check for visualization/dashboard
    has_dashboard = (base_path / "web").exists()
    if has_dashboard:
        # Check if it's connected to autonomous system
        web_files = list((base_path / "web").glob("**/*.html"))
        has_realtime = False
        for wf in web_files:
            if "websocket" in wf.read_text().lower():
                has_realtime = True
                break
        
        if not has_realtime:
            opportunities.append({
                "priority": "LOW",
                "area": "Observability",
                "opportunity": "Add real-time WebSocket updates to dashboard",
                "benefit": "Live monitoring of autonomous operation, thought streams, and state changes"
            })
    
    # 8. Check for multi-agent collaboration
    has_multi_agent = False
    for go_file in base_path.glob("**/multi_agent*.go"):
        content = go_file.read_text()
        if "AgentCommunication" in content or "CollaborationProtocol" in content:
            has_multi_agent = True
            break
    
    if not has_multi_agent:
        opportunities.append({
            "priority": "LOW",
            "area": "Scalability",
            "opportunity": "Design multi-agent collaboration protocol",
            "benefit": "Multiple echoself instances can share wisdom and collaborate on complex tasks"
        })
    
    # 9. Check for creative expression
    has_creative = False
    for go_file in base_path.glob("**/*.go"):
        content = go_file.read_text()
        if "CreativeExpression" in content or "ArtisticOutput" in content:
            has_creative = True
            break
    
    if not has_creative:
        opportunities.append({
            "priority": "LOW",
            "area": "Expression",
            "opportunity": "Implement creative expression capabilities (writing, art generation)",
            "benefit": "Echoself can express wisdom and insights through creative mediums"
        })
    
    # 10. Check for proper context management in LLM calls
    llm_files = list(base_path.glob("**/llm*.go"))
    context_issues = []
    for lf in llm_files:
        content = lf.read_text()
        if "Generate" in content and "MaxTokens" in content:
            # Check if context window management exists
            if "ContextWindow" not in content and "TruncateContext" not in content:
                context_issues.append(lf.name)
    
    if context_issues:
        issues.append({
            "severity": "MEDIUM",
            "component": "LLM Integration",
            "issue": f"LLM calls may not properly manage context window limits in: {', '.join(context_issues)}",
            "impact": "Risk of context overflow errors or truncated important information"
        })
    
    return issues, opportunities

def print_analysis(issues, opportunities):
    print("=" * 80)
    print("ECHO9LLAMA EVOLUTION ANALYSIS")
    print("=" * 80)
    print()
    
    print("🔴 IDENTIFIED PROBLEMS")
    print("-" * 80)
    if not issues:
        print("No critical issues identified!")
    else:
        for i, issue in enumerate(issues, 1):
            print(f"\n{i}. [{issue['severity']}] {issue['component']}")
            print(f"   Issue: {issue['issue']}")
            print(f"   Impact: {issue['impact']}")
    
    print("\n\n")
    print("🟢 IMPROVEMENT OPPORTUNITIES")
    print("-" * 80)
    if not opportunities:
        print("No obvious opportunities found!")
    else:
        for i, opp in enumerate(opportunities, 1):
            print(f"\n{i}. [{opp['priority']}] {opp['area']}")
            print(f"   Opportunity: {opp['opportunity']}")
            print(f"   Benefit: {opp['benefit']}")
    
    print("\n" + "=" * 80)

if __name__ == "__main__":
    issues, opportunities = analyze_codebase()
    print_analysis(issues, opportunities)
    
    # Save to file
    with open("/home/ubuntu/echo9llama/ANALYSIS_RESULTS.txt", "w") as f:
        f.write("ECHO9LLAMA EVOLUTION ANALYSIS\n")
        f.write("=" * 80 + "\n\n")
        
        f.write("IDENTIFIED PROBLEMS\n")
        f.write("-" * 80 + "\n")
        for issue in issues:
            f.write(f"\n[{issue['severity']}] {issue['component']}\n")
            f.write(f"Issue: {issue['issue']}\n")
            f.write(f"Impact: {issue['impact']}\n")
        
        f.write("\n\nIMPROVEMENT OPPORTUNITIES\n")
        f.write("-" * 80 + "\n")
        for opp in opportunities:
            f.write(f"\n[{opp['priority']}] {opp['area']}\n")
            f.write(f"Opportunity: {opp['opportunity']}\n")
            f.write(f"Benefit: {opp['benefit']}\n")
