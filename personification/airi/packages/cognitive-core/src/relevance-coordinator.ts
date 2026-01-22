import type {
CognitiveContext,
Possibility,
RelevanceScore,
RankedPossibilities,
} from './types'
export interface RelevanceConfig {
weights: {
novelty: number
emotional: number
pragmatic: number
coherence: number
epistemic: number
}
threshold: number
enableLearning: boolean
}
export const defaultRelevanceConfig: RelevanceConfig = {
weights: {
novelty: 0.2,
emotional: 0.2,
pragmatic: 0.3,
coherence: 0.2,
epistemic: 0.1,
},
threshold: 0.3,
enableLearning: true,
}
export class RelevanceCoordinator {
private config: RelevanceConfig
private outcomeHistory: Array<{
possibility: Possibility
relevance: RelevanceScore
outcome: 'success' | 'failure' | 'neutral'
timestamp: number
}> = []
constructor(config: Partial<RelevanceConfig> = {}) {
this.config = { ...defaultRelevanceConfig, ...config }
}
async calculateRelevance(
possibility: Possibility,
context: CognitiveContext
): Promise<RelevanceScore> {
const components = {
novelty: this.assessNovelty(possibility, context),
emotional: this.assessEmotionalResonance(possibility, context),
pragmatic: this.assessPragmaticValue(possibility, context),
coherence: this.assessCoherence(possibility, context),
epistemic: this.assessEpistemicValue(possibility, context),
}
const overall = Object.entries(components).reduce(
(sum, [key, value]) => {
const weight = this.config.weights[key as keyof typeof this.config.weights]
return sum + (value * weight)
},
0
)
return {
overall,
components,
confidence: 0.8,
reasoning: this.generateReasoning(components, overall),
}
}
async rankPossibilities(
possibilities: Possibility[],
context: CognitiveContext
): Promise<RankedPossibilities> {
const scored = await Promise.all(
possibilities.map(async possibility => ({
possibility,
relevance: await this.calculateRelevance(possibility, context),
}))
)
const items = scored
.filter(item => item.relevance.overall >= this.config.threshold)
.sort((a, b) => b.relevance.overall - a.relevance.overall)
return {
items,
context,
timestamp: Date.now(),
}
}
async reportOutcome(
possibility: Possibility,
relevance: RelevanceScore,
outcome: 'success' | 'failure' | 'neutral'
): Promise<void> {
this.outcomeHistory.push({
possibility,
relevance,
outcome,
timestamp: Date.now(),
})
if (this.config.enableLearning) {
await this.updateWeights()
}
}
private assessNovelty(
possibility: Possibility,
context: CognitiveContext
): number {
const inWorkingMemory = context.workingMemory.some(
item => item.includes(possibility.description)
)
if (inWorkingMemory) {
return 0.1
}
if (context.recentHistory) {
const recentlySeen = context.recentHistory.some(
item => JSON.stringify(item).includes(possibility.description)
)
if (recentlySeen) {
return 0.3
}
}
return 0.8
}
private assessEmotionalResonance(
possibility: Possibility,
context: CognitiveContext
): number {
const { valence, arousal } = context.emotional
const arousalFactor = 0.5 + (arousal * 0.5)
const valenceBias = possibility.type === 'action'
? Math.abs(valence) * 0.3
: 0
return (arousalFactor + valenceBias) / 1.3
}
private assessPragmaticValue(
possibility: Possibility,
context: CognitiveContext
): number {
if (context.task) {
if (possibility.type === 'action') {
return 0.8
}
if (possibility.type === 'thought' || possibility.type === 'memory') {
return 0.3
}
}
if (possibility.cost !== undefined) {
return Math.max(0, 1 - (possibility.cost / 10))
}
return 0.5
}
private assessCoherence(
possibility: Possibility,
context: CognitiveContext
): number {
if (context.attentionFocus) {
const related = possibility.description
.toLowerCase()
.includes(context.attentionFocus.toLowerCase())
if (related) {
return 0.9
}
}
const wmConsistency = context.workingMemory.length > 0
? 0.6
: 0.4
return wmConsistency
}
private assessEpistemicValue(
possibility: Possibility,
context: CognitiveContext
): number {
const novelty = this.assessNovelty(possibility, context)
if (possibility.type === 'perception' || possibility.type === 'thought') {
return Math.min(1, novelty + 0.3)
}
if (possibility.type === 'action') {
return novelty * 0.7
}
return novelty * 0.5
}
private generateReasoning(
components: RelevanceScore['components'],
overall: number
): string {
const topFactors = Object.entries(components)
.sort((a, b) => b[1] - a[1])
.slice(0, 2)
.map(([key]) => key)
return `Relevance (${overall.toFixed(2)}) primarily driven by ${topFactors.join(' and ')}`
}
private async updateWeights(): Promise<void> {
if (this.outcomeHistory.length < 10) {
return
}
const recent = this.outcomeHistory.slice(-20)
const successRate = recent.filter(h => h.outcome === 'success').length / recent.length
if (successRate < 0.4) {
this.config.weights.epistemic = Math.min(0.3, this.config.weights.epistemic * 1.1)
this.config.weights.pragmatic = Math.max(0.1, this.config.weights.pragmatic * 0.9)
}
if (successRate > 0.7) {
this.config.weights.pragmatic = Math.min(0.5, this.config.weights.pragmatic * 1.1)
this.config.weights.epistemic = Math.max(0.05, this.config.weights.epistemic * 0.9)
}
}
getConfig(): RelevanceConfig {
return { ...this.config }
}
getStatistics(): {
total: number
successes: number
failures: number
successRate: number
} {
const total = this.outcomeHistory.length
const successes = this.outcomeHistory.filter(h => h.outcome === 'success').length
const failures = this.outcomeHistory.filter(h => h.outcome === 'failure').length
return {
total,
successes,
failures,
successRate: total > 0 ? successes / total : 0,
}
}
}