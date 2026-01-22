export interface TruthValue {
  strength: number 
  confidence: number 
}
export interface AttentionValue {
  sti: number 
  lti: number 
  vlti: boolean 
}
export interface Atom {
  id: string 
  type: AtomType 
  tv: TruthValue 
  av: AttentionValue 
  createdAt: number 
  updatedAt: number 
  metadata?: Record<string, any> 
}
export enum AtomType {
  CONCEPT_NODE = 1, 
  PREDICATE_NODE = 2, 
  VARIABLE_NODE = 3, 
  TEMPORAL_NODE = 4, 
  CONTEXT_NODE = 5, 
  EMOTION_NODE = 6, 
  MEMORY_NODE = 7, 
  COMPANION_NODE = 8, 
  TOPIC_NODE = 9, 
  INHERITANCE_LINK = 100, 
  SIMILARITY_LINK = 101, 
  EVALUATION_LINK = 102, 
  STATE_LINK = 103, 
  MEMBER_LINK = 104, 
  REFERENCE_LINK = 105, 
  PART_OF_LINK = 106, 
  CAUSAL_LINK = 107, 
  TEMPORAL_LINK = 108, 
  ATTENTION_LINK = 109, 
  HYPEREDGE = 200, 
  RECURSIVE_LINK = 201, 
  EMERGENT_PATTERN = 202, 
  CONSCIOUSNESS_LINK = 203, 
}
export interface Node extends Atom {
  type: AtomType 
  name: string 
  properties?: Record<string, any> 
}
export interface Link extends Atom {
  type: AtomType 
  targets: string[] 
  weight: number 
  properties?: Record<string, any> 
}
export interface HyperEdge extends Link {
  type:
    | AtomType.HYPEREDGE
    | AtomType.RECURSIVE_LINK
    | AtomType.EMERGENT_PATTERN
    | AtomType.CONSCIOUSNESS_LINK
  dimension: number 
  structure: string 
}
export interface RecursiveStructure {
  baseAtoms: string[] 
  depth: number 
  pattern: string 
  emergentProperties: string[] 
}
export interface AtomSpace {
  nodes: Record<string, Node>
  links: Record<string, Link>
  hyperEdges: Record<string, HyperEdge>
  recursiveStructures: Record<string, RecursiveStructure>
  attentionFocus: string[] 
  attentionSpan: number 
  indices: {
    byType: Record<AtomType, string[]>
    byAttentionValue: string[] 
    byCreationTime: string[] 
    byName?: Record<string, string[]> 
  }
}
export interface DeepTreeEchoState {
  consciousnessLevel: number 
  selfAwarenessIndex: number 
  reflectiveCapacity: number 
  temporalHorizon: number 
  currentFocus: string[] 
  backgroundProcesses: string[] 
  reflectionStack: {
    level: number
    focusAtoms: string[]
    insights: string[]
  }[]
  emergentPatterns: Record<
    string,
    {
      patternId: string
      strength: number
      constituentAtoms: string[]
      description: string
    }
  >
}
export const createNode = (
  type: AtomType,
  name: string,
  truthValue?: Partial<TruthValue>,
  attentionValue?: Partial<AttentionValue>,
  properties?: Record<string, any>
): Node => {
  const now = Date.now()
  return {
    id: `node-${type}-${name}-${now}`,
    type,
    name,
    tv: {
      strength: truthValue?.strength ?? 1.0,
      confidence: truthValue?.confidence ?? 0.9,
    },
    av: {
      sti: attentionValue?.sti ?? 0,
      lti: attentionValue?.lti ?? 0,
      vlti: attentionValue?.vlti ?? false,
    },
    createdAt: now,
    updatedAt: now,
    properties,
  }
}
export const createLink = (
  type: AtomType,
  targets: string[],
  weight = 1.0,
  truthValue?: Partial<TruthValue>,
  attentionValue?: Partial<AttentionValue>,
  properties?: Record<string, any>
): Link => {
  const now = Date.now()
  return {
    id: `link-${type}-${targets.join('-')}-${now}`,
    type,
    targets,
    weight,
    tv: {
      strength: truthValue?.strength ?? 1.0,
      confidence: truthValue?.confidence ?? 0.9,
    },
    av: {
      sti: attentionValue?.sti ?? 0,
      lti: attentionValue?.lti ?? 0,
      vlti: attentionValue?.vlti ?? false,
    },
    createdAt: now,
    updatedAt: now,
    properties,
  }
}
export const createHyperEdge = (
  type:
    | AtomType.HYPEREDGE
    | AtomType.RECURSIVE_LINK
    | AtomType.EMERGENT_PATTERN
    | AtomType.CONSCIOUSNESS_LINK,
  targets: string[],
  dimension: number,
  structure: string,
  weight = 1.0,
  truthValue?: Partial<TruthValue>,
  attentionValue?: Partial<AttentionValue>,
  properties?: Record<string, any>
): HyperEdge => {
  const now = Date.now()
  return {
    id: `hyperedge-${type}-${targets.join('-')}-${now}`,
    type,
    targets,
    dimension,
    structure,
    weight,
    tv: {
      strength: truthValue?.strength ?? 1.0,
      confidence: truthValue?.confidence ?? 0.9,
    },
    av: {
      sti: attentionValue?.sti ?? 0,
      lti: attentionValue?.lti ?? 0,
      vlti: attentionValue?.vlti ?? false,
    },
    createdAt: now,
    updatedAt: now,
    properties,
  }
}
export const createAtomSpace = (): AtomSpace => ({
  nodes: {},
  links: {},
  hyperEdges: {},
  recursiveStructures: {},
  attentionFocus: [],
  attentionSpan: 7, 
  indices: {
    byType: Object.values(AtomType).reduce(
      (acc, type) => {
        if (typeof type === 'number') acc[type as AtomType] = []
        return acc
      },
      {} as Record<AtomType, string[]>
    ),
    byAttentionValue: [],
    byCreationTime: [],
  },
})
export const createDeepTreeEchoState = (): DeepTreeEchoState => ({
  consciousnessLevel: 1.0,
  selfAwarenessIndex: 0.5,
  reflectiveCapacity: 0.7,
  temporalHorizon: 30, 
  currentFocus: [],
  backgroundProcesses: [],
  reflectionStack: [
    {
      level: 0,
      focusAtoms: [],
      insights: [],
    },
  ],
  emergentPatterns: {},
})