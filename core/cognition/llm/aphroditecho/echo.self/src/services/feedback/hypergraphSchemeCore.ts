export interface HypergraphNode {
id: string;
type: "concept" | "procedure" | "goal" | "pattern" | "model";
content: any;
links: string[];
salience: number;
attention: number;
lastUpdated: Date;
}
export interface CognitivePattern {
id: string;
pattern: string;
strength: number;
confidence: number;
frequency: number;
context: string[];
}
export interface SalienceMetrics {
demand: number;
freshness: number;
urgency: number;
overall: number;
}
export class HypergraphSchemeCore {
private atomSpace: Map<string, HypergraphNode> = new Map();
private patterns: Map<string, CognitivePattern> = new Map();
private attentionThreshold: number = 0.5;
constructor() {
this.initializeAtomSpace();
}
private initializeAtomSpace(): void {
this.createNode(
"deep-tree-echo",
"concept",
{
description: "Core cognitive architecture",
capabilities: [
"neural-symbolic-reasoning",
"adaptive-attention",
"hypergraph-encoding",
],
},
[]
);
}
public createNode(
id: string,
type: HypergraphNode["type"],
content: any,
links: string[]
): HypergraphNode {
const node: HypergraphNode = {
id,
type,
content,
links,
salience: this.calculateSemanticSalience(id, content),
attention: 0.5,
lastUpdated: new Date(),
};
this.atomSpace.set(id, node);
return node;
}
private calculateSemanticSalience(id: string, content: any): number {
let salience = 0.5;
if (id.includes("AtomSpace") || id.includes("core")) {
salience = 0.95;
} else if (id.includes("src") || id.includes("models")) {
salience = 0.85;
} else if (id.includes("README") || id.includes("docs")) {
salience = 0.8;
}
if (typeof content === "object" && content.importance) {
salience = Math.max(salience, content.importance);
}
return Math.min(salience, 1.0);
}
public adaptiveAttention(
currentLoad: number,
recentActivity: number
): number {
return 0.5 + currentLoad * 0.3 + (0.2 - recentActivity);
}
public spreadAttention(
sourceNodeId: string,
spreadingFactor: number = 0.1
): void {
const sourceNode = this.atomSpace.get(sourceNodeId);
if (!sourceNode) return;
sourceNode.links.forEach(linkedNodeId => {
const linkedNode = this.atomSpace.get(linkedNodeId);
if (linkedNode) {
const spreadAmount = sourceNode.attention * spreadingFactor;
linkedNode.attention += spreadAmount;
linkedNode.attention = Math.min(linkedNode.attention, 1.0);
}
});
}
public getAttentionFilteredNodes(threshold?: number): HypergraphNode[] {
const effectiveThreshold = threshold || this.attentionThreshold;
return Array.from(this.atomSpace.values()).filter(
node => node.attention > effectiveThreshold
);
}
public mineCognitivePatterns(
patternThreshold: number = 0.7
): CognitivePattern[] {
const discoveredPatterns: CognitivePattern[] = [];
const nodes = Array.from(this.atomSpace.values());
const frequentSubgraphs = this.findFrequentSubgraphs(
nodes,
patternThreshold
);
frequentSubgraphs.forEach((pattern, index) => {
const cognitivePattern: CognitivePattern = {
id: `pattern-${index}`,
pattern: JSON.stringify(pattern),
strength: this.calculatePatternStrength(pattern),
confidence: 0.85,
frequency: pattern.frequency || 1,
context: pattern.context || [],
};
discoveredPatterns.push(cognitivePattern);
this.patterns.set(cognitivePattern.id, cognitivePattern);
});
return discoveredPatterns;
}
private findFrequentSubgraphs(
nodes: HypergraphNode[],
threshold: number
): any[] {
const linkPatterns = new Map<string, number>();
nodes.forEach(node => {
if (node.links.length > 0) {
const patternKey = node.links.sort().join("-");
linkPatterns.set(patternKey, (linkPatterns.get(patternKey) || 0) + 1);
}
});
return Array.from(linkPatterns.entries())
.filter(([_, frequency]) => frequency >= threshold)
.map(([pattern, frequency]) => ({
pattern,
frequency,
context: pattern.split("-"),
}));
}
private calculatePatternStrength(pattern: any): number {
const baseStrength = Math.min(pattern.frequency / 10, 1.0);
const contextBonus = pattern.context.length * 0.1;
return Math.min(baseStrength + contextBonus, 1.0);
}
public embodyPattern(pattern: CognitivePattern): HypergraphNode {
const patternNode = this.createNode(
`pattern-node-${pattern.id}`,
"pattern",
{
pattern: pattern.pattern,
strength: pattern.strength,
confidence: pattern.confidence,
},
pattern.context
);
patternNode.attention = pattern.strength;
return patternNode;
}
public updateAttentionThreshold(threshold: number): void {
this.attentionThreshold = Math.max(0.1, Math.min(threshold, 1.0));
}
public getNode(id: string): HypergraphNode | undefined {
return this.atomSpace.get(id);
}
public getAllNodes(): HypergraphNode[] {
return Array.from(this.atomSpace.values());
}
public createLink(fromNodeId: string, toNodeId: string): boolean {
const fromNode = this.atomSpace.get(fromNodeId);
const toNode = this.atomSpace.get(toNodeId);
if (fromNode && toNode) {
if (!fromNode.links.includes(toNodeId)) {
fromNode.links.push(toNodeId);
}
if (!toNode.links.includes(fromNodeId)) {
toNode.links.push(fromNodeId);
}
return true;
}
return false;
}
public calculateSalienceMetrics(
nodeId: string,
usageHistory: any[] = []
): SalienceMetrics {
const node = this.atomSpace.get(nodeId);
if (!node) {
return { demand: 0, freshness: 0, urgency: 0, overall: 0 };
}
const demand = Math.min(usageHistory.length / 10, 1.0);
const daysSinceUpdate =
(Date.now() - node.lastUpdated.getTime()) / (1000 * 60 * 60 * 24);
const freshness = Math.max(0, 1 - daysSinceUpdate / 30);
const urgency = node.attention;
const overall = demand * 0.4 + freshness * 0.3 + urgency * 0.3;
return { demand, freshness, urgency, overall };
}
}