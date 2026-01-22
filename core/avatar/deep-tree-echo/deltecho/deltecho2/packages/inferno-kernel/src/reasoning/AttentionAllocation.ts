import { Atom, AtomSpace, AttentionValue } from '../atomspace/AtomSpace.js'
export interface AttentionConfig {
  maxSTI: number
  minSTI: number
  forgettingRate: number
  hebbianLearningRate: number
  importanceSpreadingRate: number
}
export class AttentionAllocation {
  private atomSpace: AtomSpace
  private config: AttentionConfig
  private attentionalFocus: Set<string>
  constructor(atomSpace: AtomSpace, config: Partial<AttentionConfig> = {}) {
    this.atomSpace = atomSpace
    this.config = {
      maxSTI: config.maxSTI || 1000,
      minSTI: config.minSTI || -1000,
      forgettingRate: config.forgettingRate || 0.01,
      hebbianLearningRate: config.hebbianLearningRate || 0.1,
      importanceSpreadingRate: config.importanceSpreadingRate || 0.5,
    }
    this.attentionalFocus = new Set()
  }
  updateAttention(): void {
    const atoms = this.atomSpace.getAllAtoms()
    for (const atom of atoms) {
      this.applyForgetting(atom)
      this.spreadImportance(atom)
      this.updateAttentionalFocus(atom)
    }
    console.log(`[AttentionAllocation] Attentional focus: ${this.attentionalFocus.size} atoms`)
  }
  private applyForgetting(atom: Atom): void {
    const currentSTI = atom.attentionValue.sti
    const newSTI = currentSTI * (1 - this.config.forgettingRate)
    this.atomSpace.setAttentionValue(atom.id, {
      ...atom.attentionValue,
      sti: Math.max(this.config.minSTI, newSTI),
    })
  }
  private spreadImportance(atom: Atom): void {
    const spreadAmount = atom.attentionValue.sti * this.config.importanceSpreadingRate
    const incoming = this.atomSpace.getIncoming(atom.id)
    const outgoing = this.atomSpace.getOutgoing(atom.id)
    const connected = [...incoming, ...outgoing]
    if (connected.length === 0) return
    const perAtom = spreadAmount / connected.length
    for (const target of connected) {
      const newSTI = target.attentionValue.sti + perAtom
      this.atomSpace.setAttentionValue(target.id, {
        ...target.attentionValue,
        sti: Math.min(this.config.maxSTI, newSTI),
      })
    }
  }
  private updateAttentionalFocus(atom: Atom): void {
    const threshold = this.config.maxSTI * 0.5
    if (atom.attentionValue.sti >= threshold) {
      this.attentionalFocus.add(atom.id)
    } else {
      this.attentionalFocus.delete(atom.id)
    }
  }
  stimulate(atomId: string, amount: number): void {
    const atom = this.atomSpace.getAtom(atomId)
    if (!atom) return
    const newSTI = Math.min(
      this.config.maxSTI,
      atom.attentionValue.sti + amount
    )
    this.atomSpace.setAttentionValue(atomId, {
      ...atom.attentionValue,
      sti: newSTI,
    })
    console.log(`[AttentionAllocation] Stimulated atom ${atomId}: STI = ${newSTI}`)
  }
  getAttentionalFocus(): Atom[] {
    return Array.from(this.attentionalFocus)
      .map(id => this.atomSpace.getAtom(id))
      .filter((a): a is Atom => a !== undefined)
  }
  applyHebbianLearning(atom1Id: string, atom2Id: string): void {
    const atom1 = this.atomSpace.getAtom(atom1Id)
    const atom2 = this.atomSpace.getAtom(atom2Id)
    if (!atom1 || !atom2) return
    const links = this.atomSpace.getIncoming(atom1Id)
    let link = links.find(l => 
      l.outgoing?.includes(atom1Id) && l.outgoing?.includes(atom2Id)
    )
    if (!link) {
      link = this.atomSpace.addLink('SimilarityLink', [atom1Id, atom2Id], {
        strength: 0.1,
        confidence: 0.5,
      })
    }
    const newStrength = Math.min(
      1.0,
      link.truthValue.strength + this.config.hebbianLearningRate
    )
    this.atomSpace.setTruthValue(link.id, {
      strength: newStrength,
      confidence: link.truthValue.confidence,
    })
    console.log('[AttentionAllocation] Applied Hebbian learning')
  }
  collectRent(
    stiThreshold: number = -100,
    ltiThreshold: number = 0
  ): number {
    const atoms = this.atomSpace.getAllAtoms()
    let removed = 0
    for (const atom of atoms) {
      if (
        atom.attentionValue.sti < stiThreshold &&
        atom.attentionValue.lti < ltiThreshold
      ) {
        this.atomSpace.removeAtom(atom.id)
        removed++
      }
    }
    console.log(`[AttentionAllocation] Rent collection: removed ${removed} atoms`)
    return removed
  }
  getStats(): {
    focusSize: number
    avgSTI: number
    maxSTI: number
    minSTI: number
  } {
    const atoms = this.atomSpace.getAllAtoms()
    if (atoms.length === 0) {
      return { focusSize: 0, avgSTI: 0, maxSTI: 0, minSTI: 0 }
    }
    const stis = atoms.map(a => a.attentionValue.sti)
    const sum = stis.reduce((a, b) => a + b, 0)
    return {
      focusSize: this.attentionalFocus.size,
      avgSTI: sum / atoms.length,
      maxSTI: Math.max(...stis),
      minSTI: Math.min(...stis),
    }
  }
}