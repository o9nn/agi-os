import type { EchoConfig, EchoReflection, CognitiveState } from './config'
import { defaultEchoConfig, initialCognitiveState } from './config'
import { ECHO_SYSTEM_PROMPT, ECHO_COGNITIVE_INSTRUCTIONS, ECHO_REFLECTION_TEMPLATE } from './prompts'
export class EchoCharacter {
private config: EchoConfig
private state: CognitiveState
constructor(config?: Partial<EchoConfig>) {
this.config = { ...defaultEchoConfig, ...config }
this.state = { ...initialCognitiveState }
}
getSystemPrompt(): string {
return ECHO_SYSTEM_PROMPT
}
getCognitiveInstructions(): string {
return ECHO_COGNITIVE_INSTRUCTIONS
}
getPersonality(): {
systemPrompt: string
cognitiveInstructions: string
reflectionTemplate: string
config: EchoConfig
} {
return {
systemPrompt: this.getSystemPrompt(),
cognitiveInstructions: this.getCognitiveInstructions(),
reflectionTemplate: ECHO_REFLECTION_TEMPLATE,
config: this.config,
}
}
updateWorkingMemory(item: string): void {
this.state.workingMemory.push(item)
if (this.state.workingMemory.length > this.config.workingMemoryCapacity) {
this.state.workingMemory = this.state.workingMemory.slice(-this.config.workingMemoryCapacity)
}
this.state.cognitiveLoad = this.state.workingMemory.length / this.config.workingMemoryCapacity
}
setAttentionFocus(focus: string): void {
this.state.attentionFocus = focus
}
updateEmotionalState(valence: number, arousal: number): void {
this.state.emotionalState.valence = Math.max(-1, Math.min(1, valence))
this.state.emotionalState.arousal = Math.max(0, Math.min(1, arousal))
}
incrementInteraction(): void {
this.state.interactionCount++
}
shouldReflect(): boolean {
if (!this.config.enableReflection) {
return false
}
return this.state.interactionCount % this.config.reflectionInterval === 0
}
addReflection(reflection: EchoReflection): void {
this.state.reflections.push(reflection)
if (this.state.reflections.length > 20) {
this.state.reflections = this.state.reflections.slice(-20)
}
}
getState(): Readonly<CognitiveState> {
return { ...this.state }
}
getConfig(): Readonly<EchoConfig> {
return { ...this.config }
}
adaptTrait(traitName: keyof EchoConfig['traits'], delta: number): void {
const currentValue = this.config.traits[traitName]
const maxDelta = 0.15
const boundedDelta = Math.max(-maxDelta, Math.min(maxDelta, delta))
this.config.traits[traitName] = Math.max(0, Math.min(1, currentValue + boundedDelta))
}
calculateRelevance(
novelty: number,
emotional: number,
practical: number,
coherence: number,
): number {
const weights = {
novelty: 0.25,
emotional: 0.25 * this.state.emotionalState.arousal,
practical: 0.25,
coherence: 0.25,
}
const totalWeight = Object.values(weights).reduce((sum, w) => sum + w, 0)
const normalizedWeights = {
novelty: weights.novelty / totalWeight,
emotional: weights.emotional / totalWeight,
practical: weights.practical / totalWeight,
coherence: weights.coherence / totalWeight,
}
return (
novelty * normalizedWeights.novelty
+ emotional * normalizedWeights.emotional
+ practical * normalizedWeights.practical
+ coherence * normalizedWeights.coherence
)
}
processInput(input: string): {
workingMemoryUpdated: boolean
shouldReflect: boolean
cognitiveLoad: number
} {
this.updateWorkingMemory(input.substring(0, 100))
this.incrementInteraction()
const shouldReflect = this.shouldReflect()
return {
workingMemoryUpdated: true,
shouldReflect,
cognitiveLoad: this.state.cognitiveLoad,
}
}
generateReflectionPrompt(): string {
const recentMemory = this.state.workingMemory.slice(-5).join('; ')
return `Based on recent interactions: ${recentMemory}
${ECHO_REFLECTION_TEMPLATE}
Please provide a reflection following the template above.`
}
resetState(): void {
this.state = { ...initialCognitiveState }
}
}