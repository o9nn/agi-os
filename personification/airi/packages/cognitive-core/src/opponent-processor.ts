import type { CognitiveContext, CognitiveFrame } from './types'
export interface Position {
claim: string
arguments: Argument[]
evidence: string[]
assumptions: string[]
}
export interface Argument {
premise: string
conclusion: string
type: 'deductive' | 'inductive' | 'abductive' | 'analogical'
strength: number
}
export interface AlternativeFrame {
frame: CognitiveFrame
fitness: number
novelty: number
reasoning: string
}
export interface SteelManResult {
originalPosition: Position
strengthenedPosition: Position
improvements: string[]
confidence: number
}
export interface Synthesis {
thesis: Position
antithesis: Position
synthesis: Position
preserves: string[]
transcends: string[]
emergent: string[]
quality: number
}
export interface DetectedBias {
type: 'confirmation' | 'availability' | 'anchoring' | 'recency' | 'affect' | 'dunning-kruger' | 'other'
description: string
severity: number
evidence: string[]
mitigation: string
}
export interface BiasAssessment {
biasesDetected: DetectedBias[]
confidence: number
recommendations: string[]
}
export type AlternativeType = 'opposite' | 'orthogonal' | 'domain-transfer' | 'scale-shift' | 'temporal-shift'
export class OpponentProcessor {
private biasPatterns: Map<string, RegExp> = new Map()
constructor() {
this.initializeBiasPatterns()
}
generateAlternatives(
currentFrame: CognitiveFrame,
context: CognitiveContext,
options: {
count?: number
minNovelty?: number
types?: AlternativeType[]
} = {}
): AlternativeFrame[] {
const {
count = 3,
minNovelty = 0.3,
types = ['opposite', 'orthogonal', 'domain-transfer'],
} = options
const alternatives: AlternativeFrame[] = []
for (const type of types) {
let alternative: AlternativeFrame | null = null
switch (type) {
case 'opposite':
alternative = this.generateOppositeFrame(currentFrame, context)
break
case 'orthogonal':
alternative = this.generateOrthogonalFrame(currentFrame, context)
break
case 'domain-transfer':
alternative = this.generateDomainTransferFrame(currentFrame, context)
break
case 'scale-shift':
alternative = this.generateScaleShiftFrame(currentFrame, context)
break
case 'temporal-shift':
alternative = this.generateTemporalShiftFrame(currentFrame, context)
break
}
if (alternative && alternative.novelty >= minNovelty) {
alternatives.push(alternative)
}
if (alternatives.length >= count) break
}
return alternatives
.sort((a, b) => (b.fitness * b.novelty) - (a.fitness * a.novelty))
.slice(0, count)
}
steelMan(position: Position, context: CognitiveContext): SteelManResult {
const improvements: string[] = []
const strengthenedArguments: Argument[] = []
for (const arg of position.arguments) {
const strengthened = this.strengthenArgument(arg)
strengthenedArguments.push(strengthened.argument)
if (strengthened.improved) {
improvements.push(strengthened.improvement)
}
}
const missingArguments = this.identifyMissingArguments(position)
strengthenedArguments.push(...missingArguments.arguments)
if (missingArguments.arguments.length > 0) {
improvements.push(`Added ${missingArguments.arguments.length} supporting arguments`)
}
const strengthenedAssumptions = this.makeAssumptionsExplicit(position.assumptions)
if (strengthenedAssumptions.improved) {
improvements.push('Made assumptions more explicit and defensible')
}
const additionalEvidence = this.findStrongerEvidence(position, context)
const allEvidence = [...position.evidence, ...additionalEvidence]
if (additionalEvidence.length > 0) {
improvements.push(`Added ${additionalEvidence.length} pieces of stronger evidence`)
}
const strengthenedPosition: Position = {
claim: position.claim,
arguments: strengthenedArguments,
evidence: allEvidence,
assumptions: strengthenedAssumptions.assumptions,
}
const confidence = improvements.length / (position.arguments.length + 2)
return {
originalPosition: position,
strengthenedPosition,
improvements,
confidence: Math.min(0.9, confidence),
}
}
synthesize(thesis: Position, antithesis: Position, _context: CognitiveContext): Synthesis {
const preserves = this.findCommonGround(thesis, antithesis)
const transcends = this.findTranscendableElements(thesis, antithesis)
const emergent = this.findEmergentInsights(thesis, antithesis, preserves, transcends)
const synthesisClaim = this.createSynthesisClaim(thesis, antithesis, preserves, transcends, emergent)
const synthesisArguments = this.createSynthesisArguments(thesis, antithesis, preserves, emergent)
const synthesisEvidence = [...new Set([...thesis.evidence, ...antithesis.evidence])]
const synthesisAssumptions = this.synthesizeAssumptions(thesis.assumptions, antithesis.assumptions)
const synthesis: Position = {
claim: synthesisClaim,
arguments: synthesisArguments,
evidence: synthesisEvidence,
assumptions: synthesisAssumptions,
}
const quality = this.assessSynthesisQuality(thesis, antithesis, synthesis, preserves, transcends, emergent)
return {
thesis,
antithesis,
synthesis,
preserves,
transcends,
emergent,
quality,
}
}
detectBiases(
position: Position,
context: CognitiveContext,
recentHistory?: Position[]
): BiasAssessment {
const biases: DetectedBias[] = []
const confirmationBias = this.checkConfirmationBias(position, context)
if (confirmationBias) biases.push(confirmationBias)
const availabilityBias = this.checkAvailabilityBias(position, context)
if (availabilityBias) biases.push(availabilityBias)
if (recentHistory && recentHistory.length > 0) {
const anchoringBias = this.checkAnchoringBias(position, recentHistory)
if (anchoringBias) biases.push(anchoringBias)
}
if (recentHistory && recentHistory.length > 0) {
const recencyBias = this.checkRecencyBias(position, recentHistory)
if (recencyBias) biases.push(recencyBias)
}
const affectBias = this.checkAffectHeuristic(position, context)
if (affectBias) biases.push(affectBias)
const recommendations = biases.map(b => b.mitigation)
if (biases.length === 0) {
recommendations.push('No significant biases detected. Continue critical evaluation.')
} else {
recommendations.push('Consider alternative perspectives to counteract detected biases.')
}
const confidence = biases.length > 0 ? 0.7 : 0.5
return {
biasesDetected: biases,
confidence,
recommendations,
}
}
private generateOppositeFrame(current: CognitiveFrame, _context: CognitiveContext): AlternativeFrame {
const oppositeFrame: CognitiveFrame = {
id: `opposite-${current.id}`,
name: `Anti-${current.name}`,
description: `Opposite perspective to ${current.name}`,
saliencePatterns: current.blindSpots,
blindSpots: current.saliencePatterns,
domain: current.domain,
activation: 0,
fitness: 0,
}
const fitness = 0.6
const novelty = 0.9
return {
frame: oppositeFrame,
fitness,
novelty,
reasoning: `Opposite frame highlights what ${current.name} obscures`,
}
}
private generateOrthogonalFrame(current: CognitiveFrame, _context: CognitiveContext): AlternativeFrame {
const orthogonalFrame: CognitiveFrame = {
id: `orthogonal-${current.id}`,
name: `Alternative to ${current.name}`,
description: `Different angle from ${current.name}`,
saliencePatterns: [`Alternative patterns to ${current.saliencePatterns[0]}`],
blindSpots: [`Different blind spots from ${current.name}`],
domain: current.domain,
activation: 0,
fitness: 0,
}
const fitness = 0.7
const novelty = 0.7
return {
frame: orthogonalFrame,
fitness,
novelty,
reasoning: 'Orthogonal frame provides complementary view',
}
}
private generateDomainTransferFrame(current: CognitiveFrame, _context: CognitiveContext): AlternativeFrame {
const domains = ['scientific', 'artistic', 'pragmatic', 'philosophical', 'social', 'technical']
const otherDomain = domains.find(d => d !== current.domain) || 'alternative'
const transferFrame: CognitiveFrame = {
id: `transfer-${current.id}`,
name: `${otherDomain.charAt(0).toUpperCase() + otherDomain.slice(1)} view`,
description: `How ${otherDomain} thinking views this`,
saliencePatterns: [`${otherDomain} perspective patterns`],
blindSpots: [`Typical ${otherDomain} blind spots`],
domain: otherDomain,
activation: 0,
fitness: 0,
}
const fitness = 0.5
const novelty = 0.8
return {
frame: transferFrame,
fitness,
novelty,
reasoning: `${otherDomain} thinking provides fresh perspective`,
}
}
private generateScaleShiftFrame(current: CognitiveFrame, _context: CognitiveContext): AlternativeFrame {
const scaleFrame: CognitiveFrame = {
id: `scale-${current.id}`,
name: `Scale-shifted ${current.name}`,
description: 'View at different scale (micro/macro)',
saliencePatterns: ['Patterns at different scale'],
blindSpots: ['Scale-dependent blind spots'],
domain: current.domain,
activation: 0,
fitness: 0,
}
const fitness = 0.6
const novelty = 0.6
return {
frame: scaleFrame,
fitness,
novelty,
reasoning: 'Different scales reveal different patterns',
}
}
private generateTemporalShiftFrame(current: CognitiveFrame, _context: CognitiveContext): AlternativeFrame {
const temporalFrame: CognitiveFrame = {
id: `temporal-${current.id}`,
name: `Temporal view of ${current.name}`,
description: 'View across different time horizons',
saliencePatterns: ['Temporal patterns and trends'],
blindSpots: ['Present-focused blind spots'],
domain: current.domain,
activation: 0,
fitness: 0,
}
const fitness = 0.7
const novelty = 0.5
return {
frame: temporalFrame,
fitness,
novelty,
reasoning: 'Temporal perspective reveals dynamics',
}
}
private strengthenArgument(arg: Argument): { argument: Argument; improved: boolean; improvement: string } {
let improved = false
let improvement = ''
let strength = arg.strength
if (arg.strength < 0.7) {
strength = Math.min(0.9, arg.strength + 0.2)
improved = true
improvement = `Strengthened ${arg.type} argument`
}
return {
argument: { ...arg, strength },
improved,
improvement,
}
}
private identifyMissingArguments(position: Position): { arguments: Argument[] } {
const missing: Argument[] = []
if (position.arguments.length < 2) {
missing.push({
premise: 'Additional supporting evidence exists',
conclusion: 'This strengthens the position',
type: 'inductive',
strength: 0.6,
})
}
return { arguments: missing }
}
private makeAssumptionsExplicit(assumptions: string[]): { assumptions: string[]; improved: boolean } {
const explicit = assumptions.map(a => `Assuming: ${a}`)
return {
assumptions: explicit,
improved: true,
}
}
private findStrongerEvidence(position: Position, _context: CognitiveContext): string[] {
if (position.evidence.length < 2) {
return ['Multiple independent sources support this', 'Consistent pattern across contexts']
}
return []
}
private findCommonGround(thesis: Position, antithesis: Position): string[] {
const common: string[] = []
for (const assumption of thesis.assumptions) {
if (antithesis.assumptions.some(a => this.isSimilar(a, assumption))) {
common.push(`Shared assumption: ${assumption}`)
}
}
if (common.length === 0) {
common.push('Both positions seek understanding')
common.push('Both have valid concerns')
}
return common
}
private findTranscendableElements(_thesis: Position, _antithesis: Position): string[] {
return [
'The either/or framing',
'The assumption of mutual exclusivity',
'The binary thinking',
]
}
private findEmergentInsights(
_thesis: Position,
_antithesis: Position,
_preserves: string[],
_transcends: string[]
): string[] {
return [
'Context determines optimal approach',
'Both perspectives are situationally valid',
'Integration creates new possibilities',
]
}
private createSynthesisClaim(
thesis: Position,
_antithesis: Position,
_preserves: string[],
_transcends: string[],
_emergent: string[]
): string {
return `Integration of "${thesis.claim}" and its alternative reveals context-dependent truth`
}
private createSynthesisArguments(
_thesis: Position,
_antithesis: Position,
_preserves: string[],
_emergent: string[]
): Argument[] {
return [
{
premise: 'Both positions have merit in context',
conclusion: 'Synthesis preserves situational validity',
type: 'abductive',
strength: 0.8,
},
{
premise: 'Context determines optimal approach',
conclusion: 'Integration transcends binary thinking',
type: 'deductive',
strength: 0.85,
},
]
}
private synthesizeAssumptions(thesisAssumptions: string[], antithesisAssumptions: string[]): string[] {
const all = [...thesisAssumptions, ...antithesisAssumptions]
const unique = [...new Set(all)]
return unique.slice(0, 5)
}
private assessSynthesisQuality(
_thesis: Position,
_antithesis: Position,
_synthesis: Position,
preserves: string[],
transcends: string[],
emergent: string[]
): number {
let quality = 0.5
quality += (preserves.length * 0.1)
quality += (transcends.length * 0.1)
quality += (emergent.length * 0.1)
return Math.min(0.95, quality)
}
private checkConfirmationBias(position: Position, _context: CognitiveContext): DetectedBias | null {
const allSupporting = position.evidence.every(e => !e.includes('but') && !e.includes('however'))
if (allSupporting && position.evidence.length > 2) {
return {
type: 'confirmation',
description: 'Only supporting evidence presented',
severity: 0.6,
evidence: ['All evidence supports position', 'No counterevidence considered'],
mitigation: 'Actively seek disconfirming evidence',
}
}
return null
}
private checkAvailabilityBias(position: Position, _context: CognitiveContext): DetectedBias | null {
const hasRecentKeywords = position.evidence.some(e =>
e.includes('recently') || e.includes('just') || e.includes('latest')
)
if (hasRecentKeywords && position.evidence.length < 3) {
return {
type: 'availability',
description: 'Over-reliance on recent examples',
severity: 0.5,
evidence: ['Evidence emphasizes recency'],
mitigation: 'Consider historical patterns and base rates',
}
}
return null
}
private checkAnchoringBias(position: Position, history: Position[]): DetectedBias | null {
if (history.length > 0) {
const first = history[0]
if (this.isSimilar(position.claim, first.claim)) {
return {
type: 'anchoring',
description: 'Position anchored to initial view',
severity: 0.4,
evidence: ['Current position similar to initial position'],
mitigation: 'Consider starting from different anchor points',
}
}
}
return null
}
private checkRecencyBias(position: Position, _history: Position[]): DetectedBias | null {
const recentEvidence = position.evidence.filter(e =>
e.includes('recent') || e.includes('latest') || e.includes('now')
)
if (recentEvidence.length > position.evidence.length / 2) {
return {
type: 'recency',
description: 'Overweighting recent information',
severity: 0.5,
evidence: ['Majority of evidence is recent'],
mitigation: 'Consider long-term patterns',
}
}
return null
}
private checkAffectHeuristic(_position: Position, context: CognitiveContext): DetectedBias | null {
const highArousal = context.emotional.arousal > 0.7
const strongValence = Math.abs(context.emotional.valence) > 0.7
if (highArousal && strongValence) {
return {
type: 'affect',
description: 'Strong emotions may be influencing reasoning',
severity: 0.6,
evidence: [`High arousal: ${context.emotional.arousal}`, `Strong valence: ${context.emotional.valence}`],
mitigation: 'Wait for emotions to moderate before finalizing position',
}
}
return null
}
private isSimilar(a: string, b: string): boolean {
const normalize = (s: string) => s.toLowerCase().replace(/[^a-z0-9]/g, '')
const na = normalize(a)
const nb = normalize(b)
return na.includes(nb) || nb.includes(na) || na === nb
}
private initializeBiasPatterns(): void {
this.biasPatterns.set('confirmation', /always|never|everyone|no one|obviously/i)
this.biasPatterns.set('availability', /recently|just happened|latest|vivid/i)
this.biasPatterns.set('anchoring', /initially thought|first impression|started with/i)
this.biasPatterns.set('recency', /recent|latest|now|current/i)
this.biasPatterns.set('affect', /feel strongly|emotional|passionate|hate|love/i)
}
}