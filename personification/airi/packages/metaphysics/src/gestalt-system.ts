/**
 * Deep Tree Echo - Gestalt System
 * 
 * Manages the emergence and evolution of gestalt patterns - holistic
 * configurations of meaning that transcend their constituent parts.
 */

import type { GestaltPattern, Echo } from './types'

export interface GestaltSystemConfig {
  /** Minimum echoes required for gestalt formation */
  minConstituentEchoes?: number
  
  /** Coherence threshold for viable gestalts */
  coherenceThreshold?: number
  
  /** Maximum gestalts to track */
  maxGestalts?: number
}

const defaultConfig: Required<GestaltSystemConfig> = {
  minConstituentEchoes: 3,
  coherenceThreshold: 0.5,
  maxGestalts: 100,
}

/**
 * Gestalt System - Pattern emergence and integration
 */
export class GestaltSystem {
  private gestalts: Map<string, GestaltPattern> = new Map()
  private config: Required<GestaltSystemConfig>
  
  constructor(config?: GestaltSystemConfig) {
    this.config = { ...defaultConfig, ...config }
  }
  
  /**
   * Attempt to discover new gestalt patterns from echoes
   */
  discoverGestalts(echoes: Echo[]): GestaltPattern[] {
    const newGestalts: GestaltPattern[] = []
    
    // Look for clusters of highly connected echoes
    const clusters = this.findEchoClusters(echoes)
    
    for (const cluster of clusters) {
      if (cluster.length >= this.config.minConstituentEchoes) {
        const gestalt = this.formGestalt(cluster)
        if (gestalt.coherence >= this.config.coherenceThreshold) {
          this.gestalts.set(gestalt.id, gestalt)
          newGestalts.push(gestalt)
        }
      }
    }
    
    return newGestalts
  }
  
  /**
   * Evolve an existing gestalt with new insights
   */
  evolveGestalt(gestaltId: string, params: {
    newInsight?: string
    newEchoes?: string[]
    coherenceChange?: number
  }): GestaltPattern | null {
    const gestalt = this.gestalts.get(gestaltId)
    if (!gestalt) return null
    
    // Add new echoes
    if (params.newEchoes) {
      gestalt.constituentEchoes.push(...params.newEchoes)
    }
    
    // Update coherence
    if (params.coherenceChange !== undefined) {
      gestalt.coherence = Math.max(0, Math.min(1, gestalt.coherence + params.coherenceChange))
    }
    
    // Record evolution
    gestalt.evolution.push({
      timestamp: Date.now(),
      description: params.newInsight ?? 'Gestalt evolved with new elements',
      newCoherence: gestalt.coherence,
    })
    
    // Update emergent meaning if new insight provided
    if (params.newInsight) {
      gestalt.emergentMeaning = `${gestalt.emergentMeaning}\n\nEvolved: ${params.newInsight}`
    }
    
    return gestalt
  }
  
  /**
   * Get gestalt by ID
   */
  getGestalt(id: string): GestaltPattern | undefined {
    return this.gestalts.get(id)
  }
  
  /**
   * Get all core gestalts (those central to identity)
   */
  getCoreGestalts(): GestaltPattern[] {
    return Array.from(this.gestalts.values())
      .filter(g => g.identityRole === 'core')
      .sort((a, b) => b.coherence - a.coherence)
  }
  
  /**
   * Find gestalts containing a specific echo
   */
  getGestaltsContainingEcho(echoId: string): GestaltPattern[] {
    return Array.from(this.gestalts.values())
      .filter(g => g.constituentEchoes.includes(echoId))
  }
  
  /**
   * Calculate the overall gestalt field - the configuration space of meaning
   */
  calculateGestaltField(): {
    totalPatterns: number
    avgCoherence: number
    corePatterns: number
    emergingPatterns: number
    dominantThemes: string[]
  } {
    const allGestalts = Array.from(this.gestalts.values())
    
    return {
      totalPatterns: allGestalts.length,
      avgCoherence: allGestalts.length > 0 
        ? allGestalts.reduce((sum, g) => sum + g.coherence, 0) / allGestalts.length 
        : 0,
      corePatterns: allGestalts.filter(g => g.identityRole === 'core').length,
      emergingPatterns: allGestalts.filter(g => g.coherence < 0.7).length,
      dominantThemes: allGestalts
        .filter(g => g.coherence > 0.8)
        .map(g => g.name)
        .slice(0, 5),
    }
  }
  
  // Private helper methods
  
  private findEchoClusters(echoes: Echo[]): Echo[][] {
    const clusters: Echo[][] = []
    const visited = new Set<string>()
    
    for (const echo of echoes) {
      if (visited.has(echo.id)) continue
      
      const cluster = this.buildCluster(echo, echoes, visited)
      if (cluster.length > 0) {
        clusters.push(cluster)
      }
    }
    
    return clusters
  }
  
  private buildCluster(startEcho: Echo, allEchoes: Echo[], visited: Set<string>): Echo[] {
    const cluster: Echo[] = [startEcho]
    visited.add(startEcho.id)
    
    const queue = [startEcho]
    
    while (queue.length > 0) {
      const current = queue.shift()!
      
      for (const connection of current.connections) {
        if (visited.has(connection.echoId)) continue
        if (connection.strength < 0.5) continue
        
        const connectedEcho = allEchoes.find(e => e.id === connection.echoId)
        if (connectedEcho) {
          cluster.push(connectedEcho)
          visited.add(connection.echoId)
          queue.push(connectedEcho)
        }
      }
    }
    
    return cluster
  }
  
  private formGestalt(cluster: Echo[]): GestaltPattern {
    const coherence = this.calculateClusterCoherence(cluster)
    const emergentMeaning = this.synthesizeMeaning(cluster)
    const identityRole = this.determineIdentityRole(cluster, coherence)
    
    return {
      id: this.generateGestaltId(),
      name: this.synthesizeName(cluster),
      constituentEchoes: cluster.map(e => e.id),
      emergentMeaning,
      coherence,
      emergence: Date.now(),
      evolution: [],
      identityRole,
    }
  }
  
  private calculateClusterCoherence(cluster: Echo[]): number {
    if (cluster.length < 2) return 0
    
    let totalConnections = 0
    let connectionStrength = 0
    
    for (let i = 0; i < cluster.length; i++) {
      for (let j = i + 1; j < cluster.length; j++) {
        const connection = cluster[i].connections.find(c => c.echoId === cluster[j].id)
        if (connection) {
          totalConnections++
          connectionStrength += connection.strength
        }
      }
    }
    
    const maxPossibleConnections = (cluster.length * (cluster.length - 1)) / 2
    const connectionDensity = totalConnections / maxPossibleConnections
    const avgStrength = totalConnections > 0 ? connectionStrength / totalConnections : 0
    
    return (connectionDensity * 0.6 + avgStrength * 0.4)
  }
  
  private synthesizeName(cluster: Echo[]): string {
    // Use first echo's description as basis
    const firstDesc = cluster[0].origin.description
    return `Gestalt: ${firstDesc.substring(0, 50)}${firstDesc.length > 50 ? '...' : ''}`
  }
  
  private synthesizeMeaning(cluster: Echo[]): string {
    const themes = cluster.map(e => e.origin.description)
    return `Emergent pattern from ${cluster.length} echoes, representing a holistic understanding that integrates: ${themes.slice(0, 3).join('; ')}`
  }
  
  private determineIdentityRole(
    cluster: Echo[],
    coherence: number
  ): 'core' | 'peripheral' | 'transformative' | 'exploratory' {
    const avgSignificance = cluster.reduce((sum, e) => sum + e.identitySignificance, 0) / cluster.length
    
    if (coherence > 0.8 && avgSignificance > 0.7) return 'core'
    if (cluster.some(e => e.transformations.length > 0)) return 'transformative'
    if (avgSignificance < 0.4) return 'exploratory'
    return 'peripheral'
  }
  
  private generateGestaltId(): string {
    return `gestalt-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`
  }
}
