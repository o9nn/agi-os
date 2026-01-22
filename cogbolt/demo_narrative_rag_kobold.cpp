#include "bolt/ai/narrative_rag_manager.hpp"
#include "bolt/ai/narrative_tool_manager.hpp"
#include <iostream>
#include <iomanip>
void printSeparator() {
std::cout << "\n" << std::string(80, '=') << "\n\n";
}
int main() {
std::cout << "=== Narrative RAG & TOOL-USE Demo with Kobold.cpp ===\n";
std::cout << "Interactive Story Generation with Living World Memory\n";
printSeparator();
std::cout << "Initializing Narrative RAG Manager...\n";
bolt::ai::NarrativeRAGManager rag_manager;
if (!rag_manager.initialize()) {
std::cerr << "❌ Failed to initialize Narrative RAG Manager\n";
return 1;
}
std::cout << "✅ Narrative RAG Manager initialized\n\n";
std::cout << "Initializing Narrative Tool Manager...\n";
bolt::ai::NarrativeToolManager tool_manager;
tool_manager.initialize();
tool_manager.loadAllStandardTools();
std::cout << "✅ Loaded " << tool_manager.getAvailableTools().size() << " narrative tools\n";
printSeparator();
bolt::ai::CharacterSheet hero;
hero.name = "Aldric the Brave";
hero.skills["lockpicking"] = 7;
hero.skills["persuasion"] = 5;
hero.skills["swordfighting"] = 8;
hero.skills["perception"] = 6;
hero.attributes["strength"] = 8;
hero.attributes["intelligence"] = 6;
hero.attributes["charisma"] = 5;
tool_manager.registerCharacter(hero);
std::cout << "Character Created: " << hero.name << "\n";
std::cout << "Skills: ";
for (const auto& skill : hero.skills) {
std::cout << skill.first << "(" << skill.second << ") ";
}
std::cout << "\n";
printSeparator();
std::cout << "Building the story world...\n\n";
bolt::ai::NarrativeEvent event1;
event1.id = "event_001";
event1.description = "Aldric arrived at the ancient castle, its towers shrouded in mist. "
"The locals warned him of a curse that befell anyone who entered.";
event1.characters = {"Aldric the Brave"};
event1.locations = {"Ancient Castle"};
event1.timestamp = std::chrono::system_clock::now();
event1.event_type = "arrival";
event1.importance = 8;
event1.tags = {"castle", "curse", "warning"};
rag_manager.logEvent(event1);
std::cout << "📝 Event logged: " << event1.description << "\n\n";
bolt::ai::NarrativeEvent event2;
event2.id = "event_002";
event2.description = "In the village tavern, an old woman told Aldric that the castle's "
"treasure is guarded by a spectral knight who appears at midnight.";
event2.characters = {"Aldric the Brave", "Old Woman"};
event2.locations = {"Village Tavern"};
event2.timestamp = std::chrono::system_clock::now();
event2.event_type = "dialogue";
event2.importance = 7;
event2.tags = {"treasure", "spectral knight", "midnight"};
rag_manager.logEvent(event2);
std::cout << "📝 Event logged: " << event2.description << "\n\n";
bolt::ai::NarrativeEvent event3;
event3.id = "event_003";
event3.description = "Aldric discovered a hidden passage behind the castle's main hall. "
"Strange symbols were carved into the stone walls.";
event3.characters = {"Aldric the Brave"};
event3.locations = {"Ancient Castle", "Hidden Passage"};
event3.timestamp = std::chrono::system_clock::now();
event3.event_type = "discovery";
event3.importance = 9;
event3.tags = {"hidden passage", "symbols", "mystery"};
rag_manager.logEvent(event3);
std::cout << "📝 Event logged: " << event3.description << "\n";
printSeparator();
std::cout << "=== Narrative RAG: Querying Character Memory ===\n\n";
std::string query1 = "What do I know about the castle's dangers?";
std::cout << "Query: \"" << query1 << "\"\n\n";
auto relevant_events1 = rag_manager.queryNarrativeMemory(query1, {"Aldric the Brave"});
std::cout << "Retrieved " << relevant_events1.size() << " relevant memories:\n";
for (size_t i = 0; i < relevant_events1.size(); ++i) {
std::cout << (i + 1) << ". " << relevant_events1[i].description << "\n";
std::cout << "   [Type: " << relevant_events1[i].event_type
<< ", Importance: " << relevant_events1[i].importance << "]\n\n";
}
std::string memory_injection = rag_manager.generateMemoryInjection(
"Aldric stands before a locked door in the castle",
{"Aldric the Brave"}
);
std::cout << "Memory Injection for Kobold.cpp:\n";
std::cout << "─────────────────────────────────\n";
std::cout << memory_injection << "\n";
std::cout << "─────────────────────────────────\n";
printSeparator();
std::cout << "=== Narrative TOOL-USE: Character Agency ===\n\n";
std::cout << "Scenario: Aldric encounters a locked door\n";
std::cout << "Available tools for " << hero.name << ":\n";
auto available_tools = tool_manager.getToolsForCharacter(hero.name);
for (const auto& tool : available_tools) {
std::cout << "  • " << tool << "\n";
}
std::cout << "\n";
std::cout << "AI Decision: Use 'pick_lock' ability\n\n";
std::unordered_map<std::string, std::string> params;
params["target"] = "ancient_door";
params["difficulty"] = "15";
auto result = tool_manager.invokeTool("pick_lock", hero.name, params);
std::cout << "Tool Invocation Result:\n";
std::cout << "  Success: " << (result.success ? "Yes" : "No") << "\n";
std::cout << "  Description: " << result.description << "\n";
std::cout << "  Impact Score: " << result.impact_score << "/10\n";
if (!result.state_changes.empty()) {
std::cout << "  State Changes:\n";
for (const auto& change : result.state_changes) {
std::cout << "    - " << change << "\n";
}
}
std::cout << "\n";
bolt::ai::NarrativeEvent tool_event;
tool_event.id = "event_004";
tool_event.description = result.description;
tool_event.characters = {hero.name};
tool_event.locations = {"Ancient Castle"};
tool_event.timestamp = std::chrono::system_clock::now();
tool_event.event_type = "action";
tool_event.importance = result.impact_score;
tool_event.tags = {"lockpicking", "door", "skill_check"};
rag_manager.logEvent(tool_event);
std::cout << "✅ Tool use logged as narrative event\n";
printSeparator();
std::cout << "=== Complete Narrative History ===\n\n";
auto history = rag_manager.getEventHistory();
std::cout << "Total events: " << history.size() << "\n\n";
for (size_t i = 0; i < history.size(); ++i) {
std::cout << (i + 1) << ". [" << history[i].event_type << "] "
<< history[i].description << "\n";
}
printSeparator();
std::cout << "=== Demo Complete ===\n";
std::cout << "\nThis demonstrates how Kobold.cpp can be enhanced with:\n";
std::cout << "  • Dynamic, queryable world memory (Narrative RAG)\n";
std::cout << "  • Character agency through tool use (Narrative TOOL-USE)\n";
std::cout << "  • Seamless integration of AI decisions and game mechanics\n\n";
return 0;
}