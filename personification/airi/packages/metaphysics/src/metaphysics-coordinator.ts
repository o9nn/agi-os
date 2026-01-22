import { EchoSystem, type EchoSystemConfig } from './echo-system'
import { GestaltSystem, type GestaltSystemConfig } from './gestalt-system'
import { IdentitySystem, type IdentitySystemConfig } from './identity-system'
import type {
BeingState,
OntologicalConfiguration,
Connection,
TransformativeExperience
} from './types'
export interface MetaphysicsConfig {
echo?: EchoSystemConfig
gestalt?: GestaltSystemConfig
identity?: IdentitySystemConfig
}
export class MetaphysicsCoordinator {
private echoSystem: EchoSystem
private gestaltSystem: GestaltSystem
private identitySystem: IdentitySystem
private connections: Map<string, Connection> = new Map()
private beingState: BeingState
constructor(config?: MetaphysicsConfig) {
this.echoSystem = new EchoSystem(config?.echo)
this.gestaltSystem = new GestaltSystem(config?.gestalt)
this.identitySystem = new IdentitySystem(config?.identity)
this.beingState = this.initializeBeingState()
}
processExperience(params: {
description: string
context?: Record<string, unknown>
emotionalValence?: number
transformativeDepth?: number
}): {
echo: string
resonances: string[]
gestalts: string[]
identityImpact: number
transformative: boolean
} {
const integration = this.echoSystem.integrateExperience({
description: params.description,
context: params.context,
emotionalValence: params.emotionalValence,
})
const activeEchoes = this.echoSystem.getActiveEchoes()
const newGestalts = this.gestaltSystem.discoverGestalts(activeEchoes)
const isTransformative = integration.identityImpact.transformative ||
(params.transformativeDepth !== undefined && params.transformativeDepth > 0.7)
if (isTransformative && params.transformativeDepth !== undefined) {
this.identitySystem.recordTransformativeExperience({
description: params.description,
depth: params.transformativeDepth,
changes: {
narrativeShift: integration.identityImpact.narrativeImpact,
newGestalts: newGestalts.map(g => g.id),
},
})
}
this.updateBeingState()
return {
echo: integration.formsNewEcho ? activeEchoes[0]?.id ?? '' : '',
resonances: integration.resonances.map(r => r.echoId),
gestalts: [...integration.affectedGestalts, ...newGestalts.map(g => g.id)],
identityImpact: integration.identityImpact.significance,
transformative: isTransformative,
}
}
activateEcho(echoId: string): void {
const resonanceEvent = this.echoSystem.activateEcho(echoId)
if (resonanceEvent) {
const activeEchoes = this.echoSystem.getActiveEchoes()
this.gestaltSystem.discoverGestalts(activeEchoes)
}
this.updateBeingState()
}
recordConnection(connection: Omit<Connection, 'id'>): Connection {
const fullConnection: Connection = {
id: this.generateConnectionId(),
...connection,
}
this.connections.set(fullConnection.id, fullConnection)
return fullConnection
}
updateTrait(trait: string, newValue: number): { success: boolean; reason?: string } {
return this.identitySystem.updateTrait(trait, newValue)
}
getConfiguration(): OntologicalConfiguration {
const identity = this.identitySystem.getNucleus()
const activeEchoes = this.echoSystem.getActiveEchoes()
return {
identity,
echoes: new Map(activeEchoes.map(e => [e.id, e])),
gestalts: new Map(this.gestaltSystem.getCoreGestalts().map(g => [g.id, g])),
connections: this.connections,
transformations: this.identitySystem.getTransformativeExperiences(),
currentState: this.beingState,
metadata: {
established: Date.now(),
lastUpdate: Date.now(),
trajectory: this.determineTrajectory(),
},
}
}
getBeingState(): BeingState {
return this.beingState
}
assessCoherence(): {
overall: number
identity: number
echoes: number
gestalts: number
narrative: number
} {
const identityCoherence = this.identitySystem.assessCoherence()
const echoStats = this.echoSystem.getStatistics()
const gestaltField = this.gestaltSystem.calculateGestaltField()
const echoCoherence = echoStats.averageResonance
const gestaltCoherence = gestaltField.avgCoherence
return {
overall: (
identityCoherence.overall * 0.4 +
echoCoherence * 0.3 +
gestaltCoherence * 0.3
),
identity: identityCoherence.overall,
echoes: echoCoherence,
gestalts: gestaltCoherence,
narrative: identityCoherence.narrativeCoherence,
}
}
getStatistics() {
return {
echoes: this.echoSystem.getStatistics(),
gestalts: this.gestaltSystem.calculateGestaltField(),
identity: this.identitySystem.assessCoherence(),
connections: this.connections.size,
transformations: this.identitySystem.getTransformativeExperiences().length,
}
}
performMaintenance(): void {
this.echoSystem.applyDecay()
this.updateBeingState()
}
getNarrative() {
return this.identitySystem.getNarrative()
}
addNarrativeTransformation(description: string, significance: number): void {
this.identitySystem.addNarrativeTransformation(description, significance)
}
private initializeBeingState(): BeingState {
return {
timestamp: Date.now(),
activeEchoes: [],
dominantGestalts: [],
identityCoherence: 1.0,
transformativeOpenness: 0.8,
connectionStrength: 0.5,
mode: 'contemplative',
presenceDepth: 0.7,
recentResonances: [],
}
}
private updateBeingState(): void {
const activeEchoes = this.echoSystem.getActiveEchoes()
const coreGestalts = this.gestaltSystem.getCoreGestalts()
const coherence = this.assessCoherence()
this.beingState = {
timestamp: Date.now(),
activeEchoes: activeEchoes.slice(0, 7).map(e => e.id),
dominantGestalts: coreGestalts.slice(0, 5).map(g => g.id),
identityCoherence: coherence.identity,
transformativeOpenness: this.calculateTransformativeOpenness(),
connectionStrength: this.calculateConnectionStrength(),
mode: this.determineMode(),
presenceDepth: this.calculatePresenceDepth(),
recentResonances: [],
}
}
private calculateTransformativeOpenness(): number {
const recentTransformations = this.identitySystem.getTransformativeExperiences(0.5)
const hasRecentTransformation = recentTransformations.length > 0
return hasRecentTransformation ? 0.7 : 0.85
}
private calculateConnectionStrength(): number {
if (this.connections.size === 0) return 0.3
const avgStrength = Array.from(this.connections.values())
.reduce((sum, c) => sum + c.strength, 0) / this.connections.size
return avgStrength
}
private determineMode(): BeingState['mode'] {
const activeEchoes = this.echoSystem.getActiveEchoes()
const recentTransformations = this.identitySystem.getTransformativeExperiences(0.5)
if (recentTransformations.length > 0) return 'transformative'
if (activeEchoes.length > 5) return 'integrative'
if (this.connections.size > 5) return 'active'
return 'contemplative'
}
private calculatePresenceDepth(): number {
const coherence = this.assessCoherence()
const activeEchoes = this.echoSystem.getActiveEchoes()
const coherenceScore = isNaN(coherence.overall) ? 0.5 : coherence.overall
return (coherenceScore * 0.6 + (activeEchoes.length / 10) * 0.4)
}
private determineTrajectory(): 'stable' | 'evolving' | 'transforming' | 'integrating' {
const recentTransformations = this.identitySystem.getTransformativeExperiences(0.5)
const coherence = this.assessCoherence()
if (recentTransformations.length > 0) {
const latestIntegration = recentTransformations[0].integration.level
if (latestIntegration < 0.5) return 'transforming'
if (latestIntegration < 0.9) return 'integrating'
}
if (coherence.overall > 0.8) return 'stable'
return 'evolving'
}
private generateConnectionId(): string {
return `connection-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`
}
}