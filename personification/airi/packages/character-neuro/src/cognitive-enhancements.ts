import type { TheoryOfMindModel } from './types'
import { SimpleAtomSpace } from './atomspace'
export class RelevanceRealizer {
  private atomSpace: SimpleAtomSpace
  constructor(atomSpace: SimpleAtomSpace) {
    this.atomSpace = atomSpace
  }
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
    for (const [concept, score] of relevanceScores.entries()) {
      const atoms = this.atomSpace.findByName(concept)
      for (const atom of atoms) {
        this.atomSpace.spreadAttention(atom.id, score * 0.2)
      }
    }
    return { keywords, concepts, relationships, relevanceScores }
  }
  private extractKeywords(input: string): string[] {
    const words = input.toLowerCase()
      .replace(/[^\w\s]/g, ' ')
      .split(/\s+/)
      .filter(w => w.length > 3)
    const stopWords = new Set(['that', 'this', 'with', 'from', 'have', 'been', 'were', 'what', 'when', 'where', 'which', 'their', 'there', 'would', 'could', 'should'])
    const filtered = words.filter(w => !stopWords.has(w))
    const freq = new Map<string, number>()
    for (const word of filtered) {
      freq.set(word, (freq.get(word) || 0) + 1)
    }
    return Array.from(freq.entries())
      .sort((a, b) => b[1] - a[1])
      .slice(0, 10)
      .map(([word]) => word)
  }
  private identifyConcepts(keywords: string[]): string[] {
    const concepts = new Set<string>()
    for (const keyword of keywords) {
      const atoms = this.atomSpace.findByName(keyword)
      for (const atom of atoms) {
        if (atom.type === 'ConceptNode') {
          concepts.add(atom.name)
        }
      }
      concepts.add(keyword)
    }
    return Array.from(concepts)
  }
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
    return relationships.slice(0, 5)  
  }
  private scoreRelevance(concepts: string[], context?: Record<string, any>): Map<string, number> {
    const scores = new Map<string, number>()
    for (const concept of concepts) {
      let score = 0.5  
      const atoms = this.atomSpace.findByName(concept)
      for (const atom of atoms) {
        score += atom.attentionValue.sti * 0.3
        score += atom.attentionValue.lti * 0.2
      }
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
export class BeliefUpdater {
  private atomSpace: SimpleAtomSpace
  constructor(atomSpace: SimpleAtomSpace) {
    this.atomSpace = atomSpace
  }
  updateBeliefs(
    tomModel: TheoryOfMindModel,
    input: string,
    context?: Record<string, any>
  ): void {
    const beliefs = this.extractBeliefs(input, context)
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
    this.storeBeliefInAtomSpace(tomModel.targetId, beliefs)
    tomModel.beliefs.about_self = tomModel.beliefs.about_self.slice(-10)
    tomModel.beliefs.about_situation = tomModel.beliefs.about_situation.slice(-10)
    tomModel.beliefs.expectations = tomModel.beliefs.expectations.slice(-10)
  }
  private extractBeliefs(input: string, _context?: Record<string, any>): {
    aboutSelf: string[]
    aboutSituation: string[]
    expectations: string[]
  } {
    const inputLower = input.toLowerCase()
    const aboutSelf: string[] = []
    const aboutSituation: string[] = []
    const expectations: string[] = []
    if (inputLower.includes('i am') || inputLower.includes("i'm")) {
      aboutSelf.push(input.substring(0, 100))
    }
    if (inputLower.includes('i think') || inputLower.includes('i believe')) {
      aboutSelf.push(input.substring(0, 100))
    }
    if (inputLower.includes('i feel') || inputLower.includes('i\'m feeling')) {
      aboutSelf.push(input.substring(0, 100))
    }
    if (inputLower.includes('this is') || inputLower.includes('that is')) {
      aboutSituation.push(input.substring(0, 100))
    }
    if (inputLower.includes('will') || inputLower.includes('should') || 
        inputLower.includes('expect') || inputLower.includes('hope')) {
      expectations.push(input.substring(0, 100))
    }
    return { aboutSelf, aboutSituation, expectations }
  }
  private storeBeliefInAtomSpace(targetId: string, beliefs: any): void {
    const targetConcept = this.atomSpace.addConceptNode(
      `Person_${targetId}`,
      { strength: 0.9, confidence: 0.9 }
    )
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
export class EmotionRecognizer {
  recognize(input: string): {
    valence: number  
    arousal: number  
    confidence: number
    dominantEmotion: string
  } {
    const inputLower = input.toLowerCase()
    let valence = 0
    let arousal = 0.5
    let confidence = 0.5
    const positiveKeywords = ['happy', 'excited', 'love', 'great', 'awesome', 'wonderful', 'amazing', 'good', 'nice', 'fun', 'lol', 'haha', '😊', '😄', '❤️', '💖']
    const negativeKeywords = ['sad', 'angry', 'hate', 'terrible', 'awful', 'bad', 'annoyed', 'frustrated', 'upset', 'disappointed', '😢', '😠', '😤']
    const highArousalKeywords = ['excited', 'angry', 'anxious', 'energetic', 'hyper', '!!!', '!!']
    const lowArousalKeywords = ['calm', 'relaxed', 'tired', 'bored', 'sleepy', 'meh']
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
    if (positiveCount > 0 || negativeCount > 0) {
      valence = (positiveCount - negativeCount) / (positiveCount + negativeCount + 1)
      confidence = Math.min(0.9, 0.5 + (positiveCount + negativeCount) * 0.1)
    }
    if (highArousalCount > 0 || lowArousalCount > 0) {
      arousal = 0.5 + (highArousalCount - lowArousalCount) * 0.2
      arousal = Math.max(0, Math.min(1, arousal))
      confidence = Math.max(confidence, 0.5 + (highArousalCount + lowArousalCount) * 0.1)
    }
    const exclamationCount = (input.match(/!/g) || []).length
    const capsRatio = (input.match(/[A-Z]/g) || []).length / Math.max(1, input.length)
    if (exclamationCount > 2) {
      arousal = Math.min(1, arousal + 0.2)
    }
    if (capsRatio > 0.5 && input.length > 10) {
      arousal = Math.min(1, arousal + 0.3)
    }
    const dominantEmotion = this.classifyEmotion(valence, arousal)
    return { valence, arousal, confidence, dominantEmotion }
  }
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
export class ConfidenceEstimator {
  estimate(factors: {
    knowledgeAvailable: boolean
    contextClarity: number  
    optionQuality: number  
    pastSuccessRate: number  
    cognitiveLoad: number  
    emotionalStability: number  
  }): {
    confidence: number
    reasoning: string[]
  } {
    const reasoning: string[] = []
    let confidence = 0.5  
    if (factors.knowledgeAvailable) {
      confidence += 0.2
      reasoning.push('Relevant knowledge available')
    } else {
      confidence -= 0.2
      reasoning.push('Limited knowledge on topic')
    }
    confidence += factors.contextClarity * 0.15
    if (factors.contextClarity > 0.7) {
      reasoning.push('Context is clear')
    } else if (factors.contextClarity < 0.3) {
      reasoning.push('Context is ambiguous')
    }
    confidence += factors.optionQuality * 0.15
    if (factors.optionQuality > 0.8) {
      reasoning.push('High-quality response options')
    } else if (factors.optionQuality < 0.4) {
      reasoning.push('Suboptimal response options')
    }
    confidence += factors.pastSuccessRate * 0.2
    if (factors.pastSuccessRate > 0.7) {
      reasoning.push('Good track record in similar situations')
    } else if (factors.pastSuccessRate < 0.4) {
      reasoning.push('Struggled with similar situations before')
    }
    confidence -= factors.cognitiveLoad * 0.1
    if (factors.cognitiveLoad > 0.7) {
      reasoning.push('High cognitive load affecting judgment')
    }
    confidence += factors.emotionalStability * 0.1
    if (factors.emotionalStability < 0.4) {
      reasoning.push('Emotional state affecting confidence')
    }
    confidence = Math.max(0, Math.min(1, confidence))
    return { confidence, reasoning }
  }
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
export class RelationshipTracker {
  updateRelationship(
    tomModel: TheoryOfMindModel,
    interaction: {
      wasPositive: boolean
      wasEngaging: boolean
      wasRoasted: boolean
      roastWellReceived?: boolean
    }
  ): void {
    if (interaction.wasPositive) {
      tomModel.relationship.trust = Math.min(1, tomModel.relationship.trust + 0.05)
    } else {
      tomModel.relationship.trust = Math.max(0, tomModel.relationship.trust - 0.03)
    }
    tomModel.relationship.familiarity = Math.min(1, tomModel.relationship.familiarity + 0.02)
    if (interaction.wasRoasted) {
      if (interaction.roastWellReceived) {
        tomModel.relationship.roast_tolerance = Math.min(1, tomModel.relationship.roast_tolerance + 0.05)
      } else {
        tomModel.relationship.roast_tolerance = Math.max(0, tomModel.relationship.roast_tolerance - 0.1)
      }
    }
    if (!interaction.wasRoasted) {
      tomModel.relationship.roast_tolerance = Math.max(0.3, tomModel.relationship.roast_tolerance - 0.01)
    }
  }
}