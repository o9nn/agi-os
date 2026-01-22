import type { FourWaysOfKnowing } from './types'
export type KnowingEvent = {
type: 'propositional' | 'procedural' | 'perspectival' | 'participatory'
description: string
weight?: number
timestamp: number
}
export interface BalanceConfig {
targets: FourWaysOfKnowing
tolerance: number
timeWindow: number
}
export const defaultBalanceConfig: BalanceConfig = {
targets: {
propositional: 0.25,
procedural: 0.25,
perspectival: 0.25,
participatory: 0.25,
},
tolerance: 0.10,
timeWindow: 24 * 60 * 60 * 1000,
}
export interface BalanceRecommendation {
way: keyof FourWaysOfKnowing
current: number
target: number
gap: number
recommendation: string
priority: number
}
export class FourWaysTracker {
private events: KnowingEvent[] = []
private config: BalanceConfig
constructor(config: Partial<BalanceConfig> = {}) {
this.config = { ...defaultBalanceConfig, ...config }
}
recordEvent(event: Omit<KnowingEvent, 'timestamp'>): void {
this.events.push({
...event,
weight: event.weight ?? 1,
timestamp: Date.now(),
})
this.pruneOldEvents()
}
getBalance(): FourWaysOfKnowing {
this.pruneOldEvents()
if (this.events.length === 0) {
return {
propositional: 0.25,
procedural: 0.25,
perspectival: 0.25,
participatory: 0.25,
}
}
const totals = {
propositional: 0,
procedural: 0,
perspectival: 0,
participatory: 0,
}
let totalWeight = 0
for (const event of this.events) {
const weight = event.weight ?? 1
totals[event.type] += weight
totalWeight += weight
}
return {
propositional: totals.propositional / totalWeight,
procedural: totals.procedural / totalWeight,
perspectival: totals.perspectival / totalWeight,
participatory: totals.participatory / totalWeight,
}
}
isBalanced(): boolean {
const current = this.getBalance()
const { targets, tolerance } = this.config
return Object.entries(current).every(([key, value]) => {
const target = targets[key as keyof FourWaysOfKnowing]
return Math.abs(value - target) <= tolerance
})
}
getRecommendations(): BalanceRecommendation[] {
const current = this.getBalance()
const { targets } = this.config
const recommendations: BalanceRecommendation[] = []
for (const [key, currentValue] of Object.entries(current)) {
const way = key as keyof FourWaysOfKnowing
const target = targets[way]
const gap = target - currentValue
if (Math.abs(gap) > this.config.tolerance / 2) {
recommendations.push({
way,
current: currentValue,
target,
gap,
recommendation: this.generateRecommendation(way, gap),
priority: Math.abs(gap) / target,
})
}
}
return recommendations.sort((a, b) => b.priority - a.priority)
}
private generateRecommendation(
way: keyof FourWaysOfKnowing,
gap: number
): string {
const needMore = gap > 0
const recommendations: Record<keyof FourWaysOfKnowing, { more: string; less: string }> = {
propositional: {
more: 'Engage in more fact-learning, reading, conceptual analysis, or theoretical reasoning',
less: 'Reduce passive information consumption, focus on embodied practice instead',
},
procedural: {
more: 'Practice skills, engage in hands-on activities, develop competencies through doing',
less: 'Balance skill practice with reflection and conceptual understanding',
},
perspectival: {
more: 'Explore different framings, practice reframing problems, shift attention patterns',
less: 'Commit to perspectives longer, develop deeper understanding before shifting',
},
participatory: {
more: 'Engage in identity-transforming experiences, deep relationships, existential reflection',
less: 'Balance transformation with stability, integrate changes before seeking more',
},
}
return needMore
? recommendations[way].more
: recommendations[way].less
}
private pruneOldEvents(): void {
const cutoff = Date.now() - this.config.timeWindow
this.events = this.events.filter(e => e.timestamp >= cutoff)
}
getStatistics(): {
totalEvents: number
eventsByType: Record<keyof FourWaysOfKnowing, number>
balance: FourWaysOfKnowing
isBalanced: boolean
timeWindow: number
} {
this.pruneOldEvents()
const eventsByType = {
propositional: this.events.filter(e => e.type === 'propositional').length,
procedural: this.events.filter(e => e.type === 'procedural').length,
perspectival: this.events.filter(e => e.type === 'perspectival').length,
participatory: this.events.filter(e => e.type === 'participatory').length,
}
return {
totalEvents: this.events.length,
eventsByType,
balance: this.getBalance(),
isBalanced: this.isBalanced(),
timeWindow: this.config.timeWindow,
}
}
reset(): void {
this.events = []
}
}