import { RoomType } from "../components/EchoHomeMap";
import { useOrchestrator } from "../contexts/OrchestratorContext";
import { FileData } from "../store/appStore";
import { supabase } from "./supabaseClient";
import { useMem0AI } from "../services/mem0aiService";
import { useDeepTreeEchoAI } from "../services/openaiService";
export class OrchestratorService {
private static instance: OrchestratorService;
private userId: string | null = null;
private isAuthenticated: boolean = false;
private activeConnections: Map<string, unknown> = new Map();
private eventListeners: Map<string, Set<(data: unknown) => void>> = new Map();
private constructor() {
this.initializeAuth();
}
public static getInstance(): OrchestratorService {
if (!OrchestratorService.instance) {
OrchestratorService.instance = new OrchestratorService();
}
return OrchestratorService.instance;
}
private async initializeAuth(): Promise<void> {
try {
const { data } = await supabase.auth.getSession();
if (data.session) {
this.isAuthenticated = true;
this.userId = data.session.user.id;
}
supabase.auth.onAuthStateChange((event, session) => {
if (event === "SIGNED_IN" && session) {
this.isAuthenticated = true;
this.userId = session.user.id;
this.dispatchEvent("auth_change", {
status: "signed_in",
userId: session.user.id,
});
} else if (event === "SIGNED_OUT") {
this.isAuthenticated = false;
this.userId = null;
this.dispatchEvent("auth_change", { status: "signed_out" });
}
});
} catch (error) {
console.error("Error initializing auth in orchestrator:", error);
}
}
public addEventListener(event: string, callback: (data: unknown) => void): () => void {
if (!this.eventListeners.has(event)) {
this.eventListeners.set(event, new Set());
}
this.eventListeners.get(event)!.add(callback);
return () => {
const listeners = this.eventListeners.get(event);
if (listeners) {
listeners.delete(callback);
}
};
}
private dispatchEvent(event: string, data: unknown): void {
const listeners = this.eventListeners.get(event);
if (listeners) {
listeners.forEach(callback => {
try {
callback(data);
} catch (error) {
console.error(`Error in event listener for ${event}:`, error);
}
});
}
}
public navigateTo(component: string, options?: Record<string, unknown>): void {
this.dispatchEvent("navigation", { target: component, options });
}
public navigateToRoom(room: RoomType): void {
this.dispatchEvent("room_change", { room });
}
public focusFile(file: FileData): void {
this.dispatchEvent("file_focus", { file });
}
public registerComponent(componentId: string, componentInterface: unknown): void {
this.activeConnections.set(componentId, componentInterface);
this.dispatchEvent("component_registered", { componentId });
}
public unregisterComponent(componentId: string): void {
this.activeConnections.delete(componentId);
this.dispatchEvent("component_unregistered", { componentId });
}
public isComponentRegistered(componentId: string): boolean {
return this.activeConnections.has(componentId);
}
public getComponentInterface(componentId: string): unknown {
return this.activeConnections.get(componentId);
}
public transferContent(
fromComponent: string,
toComponent: string,
content: unknown
): boolean {
const sourceInterface = this.activeConnections.get(fromComponent);
const targetInterface = this.activeConnections.get(toComponent);
if (!sourceInterface || !targetInterface) {
console.error(
`Cannot transfer content: one or both components not registered`
);
return false;
}
this.dispatchEvent("content_transfer", {
from: fromComponent,
to: toComponent,
contentType: typeof content,
});
return true;
}
public async executeInTerminal(command: string): Promise<string> {
this.dispatchEvent("terminal_command", { command });
return `Simulated output for command: ${command}`;
}
public getAuthStatus(): { isAuthenticated: boolean; userId: string | null } {
return {
isAuthenticated: this.isAuthenticated,
userId: this.userId,
};
}
}
export const useOrchestratorService = () => {
const orchestratorService = OrchestratorService.getInstance();
const orchestrator = useOrchestrator();
const mem0ai = useMem0AI();
const deepTreeEchoAI = useDeepTreeEchoAI();
return {
navigateTo: (component: string, options?: Record<string, unknown>) => {
orchestratorService.navigateTo(component, options);
orchestrator.setActiveComponent(component as any);
},
navigateToRoom: (room: RoomType) => {
orchestratorService.navigateToRoom(room);
orchestrator.navigateToRoom(room);
},
focusFile: (file: FileData) => {
orchestratorService.focusFile(file);
orchestrator.focusOnFile(file.id);
},
registerComponent: (componentId: string, componentInterface: unknown) =>
orchestratorService.registerComponent(componentId, componentInterface),
unregisterComponent: (componentId: string) =>
orchestratorService.unregisterComponent(componentId),
getComponentInterface: (componentId: string) =>
orchestratorService.getComponentInterface(componentId),
transferContent: (
fromComponent: string,
toComponent: string,
content: unknown
) => {
orchestratorService.transferContent(fromComponent, toComponent, content);
return orchestrator.transferContentBetweenComponents(
fromComponent,
toComponent,
content
);
},
executeCommand: (command: string) =>
orchestrator.executeInTerminal(command),
saveToMemory: (content: unknown, tags: string[]) =>
orchestrator.saveToMemory(content, tags),
generateAIResponse: async (prompt: string, options?: Record<string, unknown>) => {
orchestrator.logEvent({
type: "ai_interaction",
description: "AI response requested",
component: orchestrator.state.activeComponent,
data: { promptLength: prompt.length },
});
if (mem0ai.isInitialized()) {
return mem0ai.generateResponseWithMemoryContext(
prompt,
[],
options
);
} else if (deepTreeEchoAI.hasApiKey) {
return deepTreeEchoAI.generateResponse(prompt, options);
} else {
return "AI systems not initialized. Please configure API key in settings.";
}
},
getSystemStatus: () => ({
health: orchestrator.state.systemHealthStatus,
memoryIntegration: orchestrator.state.memoryIntegrationStatus,
aiIntegration: orchestrator.state.aiIntegrationStatus,
activeComponent: orchestrator.state.activeComponent,
currentRoom: orchestrator.state.activeRoom,
}),
addEventListener: (event: string, callback: (data: unknown) => void) =>
orchestratorService.addEventListener(event, callback),
getAuthStatus: () => orchestratorService.getAuthStatus(),
};
};