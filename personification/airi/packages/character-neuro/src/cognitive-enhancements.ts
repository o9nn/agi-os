/**
 * Cognitive Enhancements for Neuro Character
 * 
 * Enhanced implementations for relevance realization, belief updating,
 * emotion recognition, and confidence estimation
 */

import type { TheoryOfMindModel } from './types'
import { SimpleAtomSpace } from './atomspace'

/**
 * Enhanced relevance realization using multiple strategies
 */
export class RelevanceRealizer {
  private atomSpace: SimpleAtomSpace
  
  constructor(atomSpace: SimpleAtomSpace) {
    this.atomSpace = atomSpace
  }
  
  /**
   * Realize relevance from input using multiple strategies
   */
  realize(input: string, context?: Record<string, any>): {
    keywords: string[]
    concepts: string[]
    relationships: string[]
    relevanceScores: Map<string, number>
  } {
    const keywords = this.extractKeywords(input)
    const concepts = this.identifyConcepts(keywords)
    const relationships = this.findRelationships(concepts)
    const relevanceScores = this.scoreRelevance(concepts, context)
    
    // Spread attention to relevant concepts in AtomSpace
    for (const [concept, score] of relevanceScores.entries()) {
      const atoms = this.atomSpace.findByName(concept)
      for (const atom of atoms) {
        this.atomSpace.spreadAttention(atom.id, score * 0.2)
      }
    }
    
    return { keywords, concepts, relationships, relevanceScores }
  }
  
  /**
   * Extract keywords using TF-IDF-inspired approach
   */
  private extractKeywords(input: string): string[] {
    const words = input.toLowerCase()
      .replace(/[^\w\s]/g, ' ')
      .split(/\s+/)
      .filter(w => w.length > 3)
    
    // Remove common stop words
    const stopWords = new Set(['that', 'this', 'with', 'from', 'have', 'been', 'were', 'what', 'when', 'where', 'which', 'their', 'there', 'would', 'could', 'should'])
    const filtered = words.filter(w => !stopWords.has(w))
    
    // Count frequencies
    const freq = new Map<string, number>()
    for (const word of filtered) {
      freq.set(word, (freq.get(word) || 0) + 1)
    }
    
    // Sort by frequency and return top keywords
    return Array.from(freq.entries())
      .sort((a, b) => b[1] - a[1])
      .slice(0, 10)
      .map(([word]) => word)
  }
  
  /**
   * Identify concepts from keywords using AtomSpace
   */
  private identifyConcepts(keywords: string[]): string[] {
    const concepts = new Set<string>()
    
    for (const keyword of keywords) {
      // Find matching concepts in AtomSpace
      const atoms = this.atomSpace.findByName(keyword)
      for (const atom of atoms) {
        if (atom.type === 'ConceptNode') {
          concepts.add(atom.name)
        }
      }
      
      // Also add the keyword itself as a potential concept
      concepts.add(keyword)
    }
    
    return Array.from(concepts)
  }
  
  /**
   * Find relationships between concepts
   */
  private findRelationships(concepts: string[]): string[] {
    const relationships: string[] = []
    
    for (const concept of concepts) {
      const atoms = this.atomSpace.findByName(concept)
      for (const atom of atoms) {
        const links = this.atomSpace.findLinksTo(atom.id)
        for (const link of links) {
          if (link.type === 'InheritanceLink' || link.type === 'SimilarityLink') {
            relationships.push(`${link.name} (${link.truthValue.strength.toFixed(2)})`)
          }
        }
      }
    }
    
    return relationships.slice(0, 5)  // Top 5 relationships
  }
  
  /**
   * Score relevance of concepts based on context and attention
   */
  private scoreRelevance(concepts: string[], context?: Record<string, any>): Map<string, number> {
    const scores = new Map<string, number>()
    
    for (const concept of concepts) {
      let score = 0.5  // Base relevance
      
      // Boost based on AtomSpace attention
      const atoms = this.atomSpace.findByName(concept)
      for (const atom of atoms) {
        score += atom.attentionValue.sti * 0.3
        score += atom.attentionValue.lti * 0.2
      }
      
      // Boost based on context match
      if (context) {
        for (const [_key, value] of Object.entries(context)) {
          if (typeof value === 'string' && value.toLowerCase().includes(concept.toLowerCase())) {
            score += 0.2
          }
        }
      }
      
      scores.set(concept, Math.min(1, score))
    }
    
    return scores
  }
}

/**
 * Belief updating system for Theory of Mind
 */
export class BeliefUpdater {
  private atomSpace: SimpleAtomSpace
  
  constructor(atomSpace: SimpleAtomSpace) {
    this.atomSpace = atomSpace
  }
  
  /**
   * Update beliefs about a target based on new evidence
   */
  updateBeliefs(
    tomModel: TheoryOfMindModel,
    input: string,
    context?: Record<string, any>
  ): void {
    // Extract belief-relevant information
    const beliefs = this.extractBeliefs(input, context)
    
    // Update beliefs in ToM model
    for (const belief of beliefs.aboutSelf) {
      if (!tomModel.beliefs.about_self.includes(belief)) {
        tomModel.beliefs.about_self.push(belief)
      }
    }
    
    for (const belief of beliefs.aboutSituation) {
      if (!tomModel.beliefs.about_situation.includes(belief)) {
        tomModel.beliefs.about_situation.push(belief)
      }
    }
    
    for (const expectation of beliefs.expectations) {
      if (!tomModel.beliefs.expectations.includes(expectation)) {
        tomModel.beliefs.expectations.push(expectation)
      }
    }
    
    // Store beliefs in AtomSpace
    this.storeBeliefInAtomSpace(tomModel.targetId, beliefs)
    
    // Limit belief list sizes
    tomModel.beliefs.about_self = tomModel.beliefs.about_self.slice(-10)
    tomModel.beliefs.about_situation = tomModel.beliefs.about_situation.slice(-10)
    tomModel.beliefs.expectations = tomModel.beliefs.expectations.slice(-10)
  }
  
  /**
   * Extract beliefs from input
   */
  private extractBeliefs(input: string, _context?: Record<string, any>): {
    aboutSelf: string[]
    aboutSituation: string[]
    expectations: string[]
  } {
    const inputLower = input.toLowerCase()
    const aboutSelf: string[] = []
    const aboutSituation: string[] = []
    const expectations: string[] = []
    
    // Detect self-beliefs (I am, I think, I feel, I want)
    if (inputLower.includes('i am') || inputLower.includes("i'm")) {
      aboutSelf.push(input.substring(0, 100))
    }
    
    if (inputLower.includes('i think') || inputLower.includes('i believe')) {
      aboutSelf.push(input.substring(0, 100))
    }
    
    if (inputLower.includes('i feel') || inputLower.includes('i\'m feeling')) {
      aboutSelf.push(input.substring(0, 100))
    }
    
    // Detect situational beliefs
    if (inputLower.includes('this is') || inputLower.includes('that is')) {
      aboutSituation.push(input.substring(0, 100))
    }
    
    // Detect expectations (will, should, expect, hope)
    if (inputLower.includes('will') || inputLower.includes('should') || 
        inputLower.includes('expect') || inputLower.includes('hope')) {
      expectations.push(input.substring(0, 100))
    }
    
    return { aboutSelf, aboutSituation, expectations }
  }
  
  /**
   * Store beliefs in AtomSpace for long-term memory
   */
  private storeBeliefInAtomSpace(targetId: string, beliefs: any): void {
    // Create concept node for target if not exists
    const targetConcept = this.atomSpace.addConceptNode(
      `Person_${targetId}`,
      { strength: 0.9, confidence: 0.9 }
    )
    
    // Store beliefs as evaluation links
    for (const _belief of beliefs.aboutSelf) {
      const beliefPredicate = this.atomSpace.addPredicateNode('believes_about_self')
      this.atomSpace.addEvaluationLink(
        beliefPredicate.id,
        [targetConcept.id],
        { strength: 0.7, confidence: 0.6 }
      )
    }
  }
}

/**
 * Emotion recognition system
 */
export class EmotionRecognizer {
  /**
   * Recognize emotion from input text
   */
  recognize(input: string): {
    valence: number  // -1 (negative) to 1 (positive)
    arousal: number  // 0 (calm) to 1 (excited)
    confidence: number
    dominantEmotion: string
  } {
    const inputLower = input.toLowerCase()
    
    let valence = 0
    let arousal = 0.5
    let confidence = 0.5
    
    // Positive emotion keywords
    const positiveKeywords = ['happy', 'excited', 'love', 'great', 'awesome', 'wonderful', 'amazing', 'good', 'nice', 'fun', 'lol', 'haha', '😊', '😄', '❤️', '💖']
    const negativeKeywords = ['sad', 'angry', 'hate', 'terrible', 'awful', 'bad', 'annoyed', 'frustrated', 'upset', 'disappointed', '😢', '😠', '😤']
    const highArousalKeywords = ['excited', 'angry', 'anxious', 'energetic', 'hyper', '!!!', '!!']
    const lowArousalKeywords = ['calm', 'relaxed', 'tired', 'bored', 'sleepy', 'meh']
    
    // Count keyword matches
    let positiveCount = 0
    let negativeCount = 0
    let highArousalCount = 0
    let lowArousalCount = 0
    
    for (const keyword of positiveKeywords) {
      if (inputLower.includes(keyword)) positiveCount++
    }
    
    for (const keyword of negativeKeywords) {
      if (inputLower.includes(keyword)) negativeCount++
    }
    
    for (const keyword of highArousalKeywords) {
      if (inputLower.includes(keyword)) highArousalCount++
    }
    
    for (const keyword of lowArousalKeywords) {
      if (inputLower.includes(keyword)) lowArousalCount++
    }
    
    // Calculate valence
    if (positiveCount > 0 || negativeCount > 0) {
      valence = (positiveCount - negativeCount) / (positiveCount + negativeCount + 1)
      confidence = Math.min(0.9, 0.5 + (positiveCount + negativeCount) * 0.1)
    }
    
    // Calculate arousal
    if (highArousalCount > 0 || lowArousalCount > 0) {
      arousal = 0.5 + (highArousalCount - lowArousalCount) * 0.2
      arousal = Math.max(0, Math.min(1, arousal))
      confidence = Math.max(confidence, 0.5 + (highArousalCount + lowArousalCount) * 0.1)
    }
    
    // Detect exclamation marks and caps (high arousal indicators)
    const exclamationCount = (input.match(/!/g) || []).length
    const capsRatio = (input.match(/[A-Z]/g) || []).length / Math.max(1, input.length)
    
    if (exclamationCount > 2) {
      arousal = Math.min(1, arousal + 0.2)
    }
    
    if (capsRatio > 0.5 && input.length > 10) {
      arousal = Math.min(1, arousal + 0.3)
    }
    
    // Determine dominant emotion
    const dominantEmotion = this.classifyEmotion(valence, arousal)
    
    return { valence, arousal, confidence, dominantEmotion }
  }
  
  /**
   * Classify emotion based on valence-arousal model
   */
  private classifyEmotion(valence: number, arousal: number): string {
    if (arousal > 0.7) {
      if (valence > 0.5) return 'excited'
      if (valence < -0.5) return 'angry'
      return 'anxious'
    } else if (arousal < 0.3) {
      if (valence > 0.5) return 'content'
      if (valence < -0.5) return 'sad'
      return 'bored'
    } else {
      if (valence > 0.3) return 'happy'
      if (valence < -0.3) return 'annoyed'
      return 'neutral'
    }
  }
}

/**
 * Confidence estimation system
 */
export class ConfidenceEstimator {
  /**
   * Estimate confidence in a response based on multiple factors
   */
  estimate(factors: {
    knowledgeAvailable: boolean
    contextClarity: number  // 0-1
    optionQuality: number  // 0-1
    pastSuccessRate: number  // 0-1
    cognitiveLoad: number  // 0-1
    emotionalStability: number  // 0-1
  }): {
    confidence: number
    reasoning: string[]
  } {
    const reasoning: string[] = []
    let confidence = 0.5  // Base confidence
    
    // Knowledge availability
    if (factors.knowledgeAvailable) {
      confidence += 0.2
      reasoning.push('Relevant knowledge available')
    } else {
      confidence -= 0.2
      reasoning.push('Limited knowledge on topic')
    }
    
    // Context clarity
    confidence += factors.contextClarity * 0.15
    if (factors.contextClarity > 0.7) {
      reasoning.push('Context is clear')
    } else if (factors.contextClarity < 0.3) {
      reasoning.push('Context is ambiguous')
    }
    
    // Option quality
    confidence += factors.optionQuality * 0.15
    if (factors.optionQuality > 0.8) {
      reasoning.push('High-quality response options')
    } else if (factors.optionQuality < 0.4) {
      reasoning.push('Suboptimal response options')
    }
    
    // Past success rate
    confidence += factors.pastSuccessRate * 0.2
    if (factors.pastSuccessRate > 0.7) {
      reasoning.push('Good track record in similar situations')
    } else if (factors.pastSuccessRate < 0.4) {
      reasoning.push('Struggled with similar situations before')
    }
    
    // Cognitive load (inverse relationship)
    confidence -= factors.cognitiveLoad * 0.1
    if (factors.cognitiveLoad > 0.7) {
      reasoning.push('High cognitive load affecting judgment')
    }
    
    // Emotional stability
    confidence += factors.emotionalStability * 0.1
    if (factors.emotionalStability < 0.4) {
      reasoning.push('Emotional state affecting confidence')
    }
    
    // Bound confidence to [0, 1]
    confidence = Math.max(0, Math.min(1, confidence))
    
    return { confidence, reasoning }
  }
  
  /**
   * Estimate reasoning quality based on process metrics
   */
  estimateReasoningQuality(metrics: {
    frameStability: boolean
    relevanceScore: number
    constraintSatisfaction: number
    metacognitiveAwareness: number
  }): number {
    let quality = 0.5
    
    if (metrics.frameStability) quality += 0.15
    quality += metrics.relevanceScore * 0.25
    quality += metrics.constraintSatisfaction * 0.35
    quality += metrics.metacognitiveAwareness * 0.25
    
    return Math.max(0, Math.min(1, quality))
  }
}

/**
 * Relationship tracking system
 */
export class RelationshipTracker {
  /**
   * Update relationship metrics based on interaction
   */
  updateRelationship(
    tomModel: TheoryOfMindModel,
    interaction: {
      wasPositive: boolean
      wasEngaging: boolean
      wasRoasted: boolean
      roastWellReceived?: boolean
    }
  ): void {
    // Update trust
    if (interaction.wasPositive) {
      tomModel.relationship.trust = Math.min(1, tomModel.relationship.trust + 0.05)
    } else {
      tomModel.relationship.trust = Math.max(0, tomModel.relationship.trust - 0.03)
    }
    
    // Update familiarity
    tomModel.relationship.familiarity = Math.min(1, tomModel.relationship.familiarity + 0.02)
    
    // Update roast tolerance
    if (interaction.wasRoasted) {
      if (interaction.roastWellReceived) {
        tomModel.relationship.roast_tolerance = Math.min(1, tomModel.relationship.roast_tolerance + 0.05)
      } else {
        tomModel.relationship.roast_tolerance = Math.max(0, tomModel.relationship.roast_tolerance - 0.1)
      }
    }
    
    // Decay roast tolerance slightly over time if not roasting
    if (!interaction.wasRoasted) {
      tomModel.relationship.roast_tolerance = Math.max(0.3, tomModel.relationship.roast_tolerance - 0.01)
    }
  }
}
