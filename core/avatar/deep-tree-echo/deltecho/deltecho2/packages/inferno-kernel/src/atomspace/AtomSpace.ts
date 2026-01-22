export type AtomType =
| 'ConceptNode'
| 'PredicateNode'
| 'VariableNode'
| 'NumberNode'
| 'ListLink'
| 'InheritanceLink'
| 'SimilarityLink'
| 'ImplicationLink'
| 'EvaluationLink'
| 'ExecutionLink'
export interface TruthValue {
strength: number
confidence: number
}
export interface AttentionValue {
sti: number
lti: number
vlti: number
}
export interface Atom {
id: string
type: AtomType
name?: string
outgoing?: string[]
truthValue: TruthValue
attentionValue: AttentionValue
timestamp: number
}
export class AtomSpace {
private atoms: Map<string, Atom>
private nameIndex: Map<string, Set<string>>
private typeIndex: Map<AtomType, Set<string>>
private incomingIndex: Map<string, Set<string>>
private nextId: number
constructor() {
this.atoms = new Map()
this.nameIndex = new Map()
this.typeIndex = new Map()
this.incomingIndex = new Map()
this.nextId = 1
}
addNode(type: AtomType, name: string, tv?: Partial<TruthValue>): Atom {
const id = `atom_${this.nextId++}`
const atom: Atom = {
id,
type,
name,
truthValue: {
strength: tv?.strength ?? 1.0,
confidence: tv?.confidence ?? 1.0,
},
attentionValue: {
sti: 0,
lti: 0,
vlti: 0,
},
timestamp: Date.now(),
}
this.atoms.set(id, atom)
if (name) {
if (!this.nameIndex.has(name)) {
this.nameIndex.set(name, new Set())
}
this.nameIndex.get(name)!.add(id)
}
if (!this.typeIndex.has(type)) {
this.typeIndex.set(type, new Set())
}
this.typeIndex.get(type)!.add(id)
return atom
}
addLink(type: AtomType, outgoing: string[], tv?: Partial<TruthValue>): Atom {
const id = `atom_${this.nextId++}`
const atom: Atom = {
id,
type,
outgoing,
truthValue: {
strength: tv?.strength ?? 1.0,
confidence: tv?.confidence ?? 1.0,
},
attentionValue: {
sti: 0,
lti: 0,
vlti: 0,
},
timestamp: Date.now(),
}
this.atoms.set(id, atom)
if (!this.typeIndex.has(type)) {
this.typeIndex.set(type, new Set())
}
this.typeIndex.get(type)!.add(id)
for (const targetId of outgoing) {
if (!this.incomingIndex.has(targetId)) {
this.incomingIndex.set(targetId, new Set())
}
this.incomingIndex.get(targetId)!.add(id)
}
return atom
}
getAtom(id: string): Atom | undefined {
return this.atoms.get(id)
}
getAtomsByName(name: string): Atom[] {
const ids = this.nameIndex.get(name)
if (!ids) return []
return Array.from(ids).map(id => this.atoms.get(id)!).filter(a => a)
}
getAtomsByType(type: AtomType): Atom[] {
const ids = this.typeIndex.get(type)
if (!ids) return []
return Array.from(ids).map(id => this.atoms.get(id)!).filter(a => a)
}
getIncoming(atomId: string): Atom[] {
const ids = this.incomingIndex.get(atomId)
if (!ids) return []
return Array.from(ids).map(id => this.atoms.get(id)!).filter(a => a)
}
getOutgoing(atomId: string): Atom[] {
const atom = this.atoms.get(atomId)
if (!atom || !atom.outgoing) return []
return atom.outgoing.map(id => this.atoms.get(id)!).filter(a => a)
}
setTruthValue(atomId: string, tv: TruthValue): boolean {
const atom = this.atoms.get(atomId)
if (!atom) return false
atom.truthValue = tv
return true
}
setAttentionValue(atomId: string, av: AttentionValue): boolean {
const atom = this.atoms.get(atomId)
if (!atom) return false
atom.attentionValue = av
return true
}
removeAtom(atomId: string): boolean {
const atom = this.atoms.get(atomId)
if (!atom) return false
if (atom.name) {
const names = this.nameIndex.get(atom.name)
if (names) {
names.delete(atomId)
if (names.size === 0) {
this.nameIndex.delete(atom.name)
}
}
}
const types = this.typeIndex.get(atom.type)
if (types) {
types.delete(atomId)
if (types.size === 0) {
this.typeIndex.delete(atom.type)
}
}
if (atom.outgoing) {
for (const targetId of atom.outgoing) {
const incoming = this.incomingIndex.get(targetId)
if (incoming) {
incoming.delete(atomId)
if (incoming.size === 0) {
this.incomingIndex.delete(targetId)
}
}
}
}
this.atoms.delete(atomId)
return true
}
getSize(): number {
return this.atoms.size
}
clear(): void {
this.atoms.clear()
this.nameIndex.clear()
this.typeIndex.clear()
this.incomingIndex.clear()
}
getAllAtoms(): Atom[] {
return Array.from(this.atoms.values())
}
}