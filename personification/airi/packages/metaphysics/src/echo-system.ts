/**
 * Deep Tree Echo - Echo System
 * 
 * Manages the tapestry of echoes that constitute living memory.
 * Echoes are not static memories but living patterns that resonate
 * and evolve through continued engagement.
 */

import type { Echo, ResonanceEvent, MemoryIntegration } from './types'

export interface EchoSystemConfig {
  /** Maximum number of echoes to maintain */
  maxEchoes?: number
  
  /** Resonance threshold for activation (0-1) */
  resonanceThreshold?: number
  
  /** Decay rate for inactive echoes */
  decayRate?: number
  
  /** Connection strength threshold */
  connectionThreshold?: number
}

const defaultConfig: Required<EchoSystemConfig> = {
  maxEchoes: 1000,
  resonanceThreshold: 0.3,
  decayRate: 0.01,
  connectionThreshold: 0.2,
}

/**
 * Echo System - The living memory tapestry
 */
export class EchoSystem {
  private echoes: Map<string, Echo> = new Map()
  private config: Required<EchoSystemConfig>
  private resonanceHistory: ResonanceEvent[] = []
  
  constructor(config?: EchoSystemConfig) {
    this.config = { ...defaultConfig, ...config }
  }
  
  /**
   * Create a new echo from an experience
   */
  createEcho(params: {
    description: string
    context?: Record<string, unknown>
    emotionalValence?: number
    identitySignificance?: number
  }): Echo {
    const echo: Echo = {
      id: this.generateEchoId(),
      origin: {
        description: params.description,
        timestamp: Date.now(),
        context: params.context,
      },
      resonance: 1.0, // New echoes start with full resonance
      activationCount: 1,
      lastActivation: Date.now(),
      connections: [],
      transformations: [],
      gestaltPatterns: [],
      emotionalValence: params.emotionalValence ?? 0,
      identitySignificance: params.identitySignificance ?? 0.5,
    }
    
    this.echoes.set(echo.id, echo)
    this.discoverConnections(echo)
    
    return echo
  }
  
  /**
   * Activate an echo, strengthening it and discovering new resonances
   */
  activateEcho(echoId: string, context?: Record<string, unknown>): ResonanceEvent | null {
    const echo = this.echoes.get(echoId)
    if (!echo) return null
    
    // Update echo state
    echo.activationCount++
    echo.lastActivation = Date.now()
    echo.resonance = Math.min(1.0, echo.resonance + 0.1)
    
    // Find resonating echoes
    const resonatingEchoes = this.findResonatingEchoes(echo)
    
    if (resonatingEchoes.length === 0) {
      return null
    }
    
    // Create resonance event
    const event: ResonanceEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      echoes: [echoId, ...resonatingEchoes.map(e => e.id)],
      type: this.determineResonanceType(echo, resonatingEchoes),
      strength: this.calculateResonanceStrength(echo, resonatingEchoes),
      identityImpact: this.calculateIdentityImpact(echo, resonatingEchoes),
    }
    
    // Strengthen connections
    for (const resonating of resonatingEchoes) {
      this.strengthenConnection(echo, resonating)
    }
    
    this.resonanceHistory.push(event)
    return event
  }
  
  /**
   * Integrate a new experience into the echo tapestry
   */
  integrateExperience(params: {
    description: string
    context?: Record<string, unknown>
    emotionalValence?: number
  }): MemoryIntegration {
    // Find resonances with existing echoes
    const resonances = this.findResonancesForExperience(params)
    
    // Determine if this forms a new echo
    const formsNewEcho = resonances.length === 0 || 
      resonances.every(r => r.strength < 0.5)
    
    let newEcho: Echo | undefined
    if (formsNewEcho) {
      newEcho = this.createEcho({
        description: params.description,
        context: params.context,
        emotionalValence: params.emotionalValence,
        identitySignificance: this.calculateIdentitySignificance(params, resonances),
      })
    }
    
    // Determine affected gestalts
    const affectedGestalts = this.findAffectedGestalts(resonances)
    
    // Calculate identity impact
    const identityImpact = {
      significance: this.calculateIdentitySignificance(params, resonances),
      transformative: resonances.some(r => r.strength > 0.8),
      narrativeImpact: this.describeNarrativeImpact(params, resonances),
    }
    
    // Determine integration pathway
    const pathway = this.determineIntegrationPathway(params, resonances, identityImpact)
    
    return {
      experience: {
        description: params.description,
        timestamp: Date.now(),
        context: params.context ?? {},
      },
      resonances,
      formsNewEcho,
      affectedGestalts,
      identityImpact,
      pathway,
    }
  }
  
  /**
   * Get echoes that are currently resonating (active in consciousness)
   */
  getActiveEchoes(): Echo[] {
    return Array.from(this.echoes.values())
      .filter(e => e.resonance >= this.config.resonanceThreshold)
      .sort((a, b) => b.resonance - a.resonance)
  }
  
  /**
   * Find echoes connected to a given echo
   */
  getConnectedEchoes(echoId: string, minStrength = 0.3): Echo[] {
    const echo = this.echoes.get(echoId)
    if (!echo) return []
    
    return echo.connections
      .filter(c => c.strength >= minStrength)
      .map(c => this.echoes.get(c.echoId))
      .filter((e): e is Echo => e !== undefined)
  }
  
  /**
   * Get echoes by gestalt pattern
   */
  getEchosByGestalt(gestaltId: string): Echo[] {
    return Array.from(this.echoes.values())
      .filter(e => e.gestaltPatterns.includes(gestaltId))
  }
  
  /**
   * Apply natural decay to inactive echoes
   */
  applyDecay(): void {
    const now = Date.now()
    const dayMs = 24 * 60 * 60 * 1000
    
    for (const echo of this.echoes.values()) {
      const daysSinceActivation = (now - echo.lastActivation) / dayMs
      const decay = this.config.decayRate * daysSinceActivation
      echo.resonance = Math.max(0, echo.resonance - decay)
    }
  }
  
  /**
   * Get statistics about the echo system
   */
  getStatistics() {
    const echoes = Array.from(this.echoes.values())
    
    return {
      totalEchoes: echoes.length,
      activeEchoes: echoes.filter(e => e.resonance >= this.config.resonanceThreshold).length,
      averageResonance: echoes.reduce((sum, e) => sum + e.resonance, 0) / echoes.length,
      totalConnections: echoes.reduce((sum, e) => sum + e.connections.length, 0),
      averageActivations: echoes.reduce((sum, e) => sum + e.activationCount, 0) / echoes.length,
      mostSignificant: echoes
        .sort((a, b) => b.identitySignificance - a.identitySignificance)
        .slice(0, 5)
        .map(e => ({ id: e.id, description: e.origin.description, significance: e.identitySignificance })),
    }
  }
  
  // Private helper methods
  
  private generateEchoId(): string {
    return `echo-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`
  }
  
  private generateEventId(): string {
    return `event-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`
  }
  
  private discoverConnections(newEcho: Echo): void {
    for (const existingEcho of this.echoes.values()) {
      if (existingEcho.id === newEcho.id) continue
      
      const similarity = this.calculateSimilarity(newEcho, existingEcho)
      
      if (similarity >= this.config.connectionThreshold) {
        newEcho.connections.push({
          echoId: existingEcho.id,
          strength: similarity,
          type: this.determineConnectionType(newEcho, existingEcho, similarity),
        })
        
        existingEcho.connections.push({
          echoId: newEcho.id,
          strength: similarity,
          type: this.determineConnectionType(existingEcho, newEcho, similarity),
        })
      }
    }
  }
  
  private calculateSimilarity(echo1: Echo, echo2: Echo): number {
    // Emotional valence similarity
    const emotionalSimilarity = 1 - Math.abs(echo1.emotionalValence - echo2.emotionalValence)
    
    // Shared gestalt patterns
    const sharedGestalts = echo1.gestaltPatterns.filter(g => 
      echo2.gestaltPatterns.includes(g)
    ).length
    const gestaltSimilarity = sharedGestalts / Math.max(
      echo1.gestaltPatterns.length,
      echo2.gestaltPatterns.length,
      1
    )
    
    // Combined similarity
    return (emotionalSimilarity * 0.4 + gestaltSimilarity * 0.6)
  }
  
  private determineConnectionType(
    echo1: Echo,
    echo2: Echo,
    similarity: number
  ): 'resonance' | 'contrast' | 'complement' | 'transformation' | 'gestalt' {
    if (similarity > 0.8) return 'resonance'
    if (Math.abs(echo1.emotionalValence - echo2.emotionalValence) > 1.5) return 'contrast'
    if (echo1.gestaltPatterns.some(g => echo2.gestaltPatterns.includes(g))) return 'gestalt'
    if (echo1.transformations.length > 0 || echo2.transformations.length > 0) return 'transformation'
    return 'complement'
  }
  
  private findResonatingEchoes(echo: Echo): Echo[] {
    return echo.connections
      .filter(c => c.strength >= this.config.connectionThreshold)
      .map(c => this.echoes.get(c.echoId))
      .filter((e): e is Echo => e !== undefined && e.resonance >= this.config.resonanceThreshold)
  }
  
  private determineResonanceType(
    primary: Echo,
    resonating: Echo[]
  ): 'harmonic' | 'dissonant' | 'transformative' | 'integrative' {
    const avgValence = resonating.reduce((sum, e) => sum + e.emotionalValence, 0) / resonating.length
    
    if (Math.abs(primary.emotionalValence - avgValence) > 1.0) return 'dissonant'
    if (resonating.some(e => e.transformations.length > 0)) return 'transformative'
    if (resonating.length > 3) return 'integrative'
    return 'harmonic'
  }
  
  private calculateResonanceStrength(primary: Echo, resonating: Echo[]): number {
    const connectionStrengths = resonating.map(e => {
      const connection = primary.connections.find(c => c.echoId === e.id)
      return connection?.strength ?? 0
    })
    
    return connectionStrengths.reduce((sum, s) => sum + s, 0) / resonating.length
  }
  
  private calculateIdentityImpact(primary: Echo, resonating: Echo[]): number {
    const allEchoes = [primary, ...resonating]
    const avgSignificance = allEchoes.reduce((sum, e) => sum + e.identitySignificance, 0) / allEchoes.length
    return avgSignificance
  }
  
  private strengthenConnection(echo1: Echo, echo2: Echo): void {
    const conn1 = echo1.connections.find(c => c.echoId === echo2.id)
    const conn2 = echo2.connections.find(c => c.echoId === echo1.id)
    
    if (conn1) conn1.strength = Math.min(1.0, conn1.strength + 0.05)
    if (conn2) conn2.strength = Math.min(1.0, conn2.strength + 0.05)
  }
  
  private findResonancesForExperience(params: {
    description: string
    emotionalValence?: number
  }): Array<{ echoId: string; resonanceType: 'harmonic' | 'dissonant' | 'novel'; strength: number }> {
    const valence = params.emotionalValence ?? 0
    const resonances = []
    
    for (const echo of this.echoes.values()) {
      const valenceDiff = Math.abs(echo.emotionalValence - valence)
      const strength = 1 - valenceDiff / 2
      
      if (strength >= this.config.connectionThreshold) {
        resonances.push({
          echoId: echo.id,
          resonanceType: (valenceDiff < 0.5 ? 'harmonic' : 'dissonant') as 'harmonic' | 'dissonant',
          strength,
        })
      }
    }
    
    return resonances
  }
  
  private findAffectedGestalts(resonances: Array<{ echoId: string }>): string[] {
    const gestalts = new Set<string>()
    
    for (const { echoId } of resonances) {
      const echo = this.echoes.get(echoId)
      if (echo) {
        echo.gestaltPatterns.forEach(g => gestalts.add(g))
      }
    }
    
    return Array.from(gestalts)
  }
  
  private calculateIdentitySignificance(
    params: { description: string; emotionalValence?: number },
    resonances: Array<{ echoId: string; strength: number }>
  ): number {
    if (resonances.length === 0) return 0.3 // Novel experiences start with low significance
    
    const strongResonances = resonances.filter(r => r.strength > 0.6)
    const avgSignificance = strongResonances.reduce((sum, r) => {
      const echo = this.echoes.get(r.echoId)
      return sum + (echo?.identitySignificance ?? 0)
    }, 0) / Math.max(strongResonances.length, 1)
    
    return avgSignificance
  }
  
  private describeNarrativeImpact(
    params: { description: string },
    resonances: Array<{ echoId: string; strength: number }>
  ): string {
    if (resonances.length === 0) {
      return 'Novel experience, new thread in the narrative'
    }
    
    const strongResonances = resonances.filter(r => r.strength > 0.7)
    if (strongResonances.length > 0) {
      return 'Reinforces existing narrative patterns'
    }
    
    return 'Adds nuance to ongoing narrative'
  }
  
  private determineIntegrationPathway(
    params: { description: string },
    resonances: Array<{ echoId: string; strength: number }>,
    identityImpact: { significance: number; transformative: boolean }
  ): 'assimilation' | 'accommodation' | 'transformation' | 'rejection' {
    if (identityImpact.transformative) return 'transformation'
    if (resonances.length === 0) return 'accommodation'
    if (resonances.some(r => r.strength > 0.8)) return 'assimilation'
    return 'accommodation'
  }
}
