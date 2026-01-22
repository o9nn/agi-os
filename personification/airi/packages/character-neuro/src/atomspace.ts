export type TruthValue = {
  strength: number  
  confidence: number  
}
export type AttentionValue = {
  sti: number  
  lti: number  
  vlti: boolean  
}
export type AtomType =
  | 'ConceptNode'
  | 'PredicateNode'
  | 'InheritanceLink'
  | 'SimilarityLink'
  | 'EvaluationLink'
  | 'ListLink'
export interface Atom {
  id: string
  type: AtomType
  name: string
  truthValue: TruthValue
  attentionValue: AttentionValue
  metadata?: Record<string, any>
  timestamp: number
}
export interface Link extends Atom {
  outgoing: string[]  
}
export class SimpleAtomSpace {
  private atoms: Map<string, Atom>
  private attentionBank: string[]  
  private queryCache: Map<string, Atom[]>
  constructor() {
    this.atoms = new Map()
    this.attentionBank = []
    this.queryCache = new Map()
  }
  addConceptNode(
    name: string,
    truthValue: TruthValue = { strength: 0.8, confidence: 0.8 },
    metadata?: Record<string, any>
  ): Atom {
    const atom: Atom = {
      id: this.generateId('concept', name),
      type: 'ConceptNode',
      name,
      truthValue,
      attentionValue: { sti: 0, lti: 0, vlti: false },
      metadata,
      timestamp: Date.now(),
    }
    this.atoms.set(atom.id, atom)
    this.invalidateCache()
    return atom
  }
  addPredicateNode(
    name: string,
    truthValue: TruthValue = { strength: 0.8, confidence: 0.8 }
  ): Atom {
    const atom: Atom = {
      id: this.generateId('predicate', name),
      type: 'PredicateNode',
      name,
      truthValue,
      attentionValue: { sti: 0, lti: 0, vlti: false },
      timestamp: Date.now(),
    }
    this.atoms.set(atom.id, atom)
    this.invalidateCache()
    return atom
  }
  addInheritanceLink(
    childId: string,
    parentId: string,
    truthValue: TruthValue = { strength: 0.9, confidence: 0.9 }
  ): Link {
    const link: Link = {
      id: this.generateId('inheritance', `${childId}-${parentId}`),
      type: 'InheritanceLink',
      name: `${childId} inherits ${parentId}`,
      truthValue,
      attentionValue: { sti: 0, lti: 0, vlti: false },
      outgoing: [childId, parentId],
      timestamp: Date.now(),
    }
    this.atoms.set(link.id, link)
    this.invalidateCache()
    return link
  }
  addSimilarityLink(
    aId: string,
    bId: string,
    truthValue: TruthValue = { strength: 0.7, confidence: 0.8 }
  ): Link {
    const link: Link = {
      id: this.generateId('similarity', `${aId}-${bId}`),
      type: 'SimilarityLink',
      name: `${aId} similar ${bId}`,
      truthValue,
      attentionValue: { sti: 0, lti: 0, vlti: false },
      outgoing: [aId, bId],
      timestamp: Date.now(),
    }
    this.atoms.set(link.id, link)
    this.invalidateCache()
    return link
  }
  addEvaluationLink(
    predicateId: string,
    argumentIds: string[],
    truthValue: TruthValue = { strength: 0.8, confidence: 0.8 }
  ): Link {
    const link: Link = {
      id: this.generateId('evaluation', `${predicateId}-${argumentIds.join('-')}`),
      type: 'EvaluationLink',
      name: `eval ${predicateId}`,
      truthValue,
      attentionValue: { sti: 0, lti: 0, vlti: false },
      outgoing: [predicateId, ...argumentIds],
      timestamp: Date.now(),
    }
    this.atoms.set(link.id, link)
    this.invalidateCache()
    return link
  }
  getAtom(id: string): Atom | undefined {
    return this.atoms.get(id)
  }
  findByName(name: string): Atom[] {
    const cacheKey = `name:${name}`
    if (this.queryCache.has(cacheKey)) {
      return this.queryCache.get(cacheKey)!
    }
    const results = Array.from(this.atoms.values()).filter(
      atom => atom.name.toLowerCase().includes(name.toLowerCase())
    )
    this.queryCache.set(cacheKey, results)
    return results
  }
  findByType(type: AtomType): Atom[] {
    const cacheKey = `type:${type}`
    if (this.queryCache.has(cacheKey)) {
      return this.queryCache.get(cacheKey)!
    }
    const results = Array.from(this.atoms.values()).filter(atom => atom.type === type)
    this.queryCache.set(cacheKey, results)
    return results
  }
  findLinksTo(atomId: string): Link[] {
    return Array.from(this.atoms.values()).filter(
      atom => 'outgoing' in atom && (atom as Link).outgoing.includes(atomId)
    ) as Link[]
  }
  spreadAttention(sourceId: string, amount: number = 0.1): void {
    const source = this.atoms.get(sourceId)
    if (!source) return
    source.attentionValue.sti += amount
    const links = this.findLinksTo(sourceId)
    for (const link of links) {
      link.attentionValue.sti += amount * 0.5
      if ('outgoing' in link) {
        for (const targetId of link.outgoing) {
          if (targetId !== sourceId) {
            const target = this.atoms.get(targetId)
            if (target) {
              target.attentionValue.sti += amount * 0.3
            }
          }
        }
      }
    }
    this.updateAttentionBank()
  }
  getAttentionalFocus(limit: number = 10): Atom[] {
    return this.attentionBank
      .slice(0, limit)
      .map(id => this.atoms.get(id)!)
      .filter(Boolean)
  }
  decayAttention(decayRate: number = 0.1): void {
    for (const atom of this.atoms.values()) {
      atom.attentionValue.sti *= (1 - decayRate)
      if (atom.attentionValue.sti > 0.7) {
        atom.attentionValue.lti = Math.min(1, atom.attentionValue.lti + 0.05)
      }
    }
    this.updateAttentionBank()
  }
  patternMatch(pattern: {
    type?: AtomType
    nameContains?: string
    minStrength?: number
    minConfidence?: number
    minSTI?: number
  }): Atom[] {
    let results = Array.from(this.atoms.values())
    if (pattern.type) {
      results = results.filter(atom => atom.type === pattern.type)
    }
    if (pattern.nameContains) {
      results = results.filter(atom =>
        atom.name.toLowerCase().includes(pattern.nameContains!.toLowerCase())
      )
    }
    if (pattern.minStrength !== undefined) {
      results = results.filter(atom => atom.truthValue.strength >= pattern.minStrength!)
    }
    if (pattern.minConfidence !== undefined) {
      results = results.filter(atom => atom.truthValue.confidence >= pattern.minConfidence!)
    }
    if (pattern.minSTI !== undefined) {
      results = results.filter(atom => atom.attentionValue.sti >= pattern.minSTI!)
    }
    return results
  }
  getStats(): {
    totalAtoms: number
    byType: Record<AtomType, number>
    avgSTI: number
    avgLTI: number
    highAttentionCount: number
  } {
    const byType: Record<AtomType, number> = {
      ConceptNode: 0,
      PredicateNode: 0,
      InheritanceLink: 0,
      SimilarityLink: 0,
      EvaluationLink: 0,
      ListLink: 0,
    }
    let totalSTI = 0
    let totalLTI = 0
    let highAttentionCount = 0
    for (const atom of this.atoms.values()) {
      byType[atom.type]++
      totalSTI += atom.attentionValue.sti
      totalLTI += atom.attentionValue.lti
      if (atom.attentionValue.sti > 0.5) {
        highAttentionCount++
      }
    }
    const totalAtoms = this.atoms.size
    return {
      totalAtoms,
      byType,
      avgSTI: totalAtoms > 0 ? totalSTI / totalAtoms : 0,
      avgLTI: totalAtoms > 0 ? totalLTI / totalAtoms : 0,
      highAttentionCount,
    }
  }
  clear(): void {
    this.atoms.clear()
    this.attentionBank = []
    this.queryCache.clear()
  }
  private generateId(type: string, name: string): string {
    return `${type}:${name}:${Date.now()}:${Math.random().toString(36).substr(2, 9)}`
  }
  private updateAttentionBank(): void {
    this.attentionBank = Array.from(this.atoms.keys()).sort((a, b) => {
      const atomA = this.atoms.get(a)!
      const atomB = this.atoms.get(b)!
      return atomB.attentionValue.sti - atomA.attentionValue.sti
    })
  }
  private invalidateCache(): void {
    this.queryCache.clear()
  }
}
export const TruthValueHelpers = {
  combine(tv1: TruthValue, tv2: TruthValue, weight1: number = 0.5): TruthValue {
    const weight2 = 1 - weight1
    return {
      strength: tv1.strength * weight1 + tv2.strength * weight2,
      confidence: Math.min(tv1.confidence, tv2.confidence),  
    }
  },
  revise(prior: TruthValue, evidence: TruthValue): TruthValue {
    const k = 1  
    const totalConf = prior.confidence + k * evidence.confidence
    return {
      strength: (prior.strength * prior.confidence + evidence.strength * k * evidence.confidence) / totalConf,
      confidence: Math.min(1, totalConf),
    }
  },
  isTrue(tv: TruthValue, strengthThreshold: number = 0.7, confidenceThreshold: number = 0.6): boolean {
    return tv.strength >= strengthThreshold && tv.confidence >= confidenceThreshold
  },
  isFalse(tv: TruthValue, strengthThreshold: number = 0.3, confidenceThreshold: number = 0.6): boolean {
    return tv.strength <= strengthThreshold && tv.confidence >= confidenceThreshold
  },
}