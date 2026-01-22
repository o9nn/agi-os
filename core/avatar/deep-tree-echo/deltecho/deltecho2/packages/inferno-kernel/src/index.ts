export { InfernoKernel } from './core/InfernoKernel.js'
export type {
  KernelConfig,
  CognitiveProcess,
  KernelStats,
} from './core/InfernoKernel.js'
export { AtomSpace } from './atomspace/AtomSpace.js'
export type {
  Atom,
  AtomType,
  TruthValue,
  AttentionValue,
} from './atomspace/AtomSpace.js'
export { PatternMatcher } from './atomspace/PatternMatcher.js'
export type { Pattern, MatchResult } from './atomspace/PatternMatcher.js'
export { PLNEngine } from './reasoning/PLNEngine.js'
export type { InferenceRule } from './reasoning/PLNEngine.js'
export { AttentionAllocation } from './reasoning/AttentionAllocation.js'
export type { AttentionConfig } from './reasoning/AttentionAllocation.js'
export { MOSES } from './reasoning/MOSES.js'
export type {
  Program,
  MOSESConfig,
  FitnessFunction,
} from './reasoning/MOSES.js'
export { OpenPsi } from './reasoning/OpenPsi.js'
export type { Goal, Drive, Emotion } from './reasoning/OpenPsi.js'
export { DistributedCoordinator } from './distributed/DistributedCoordinator.js'
export type {
  NodeInfo,
  DistributedTask,
  CoordinatorConfig,
} from './distributed/DistributedCoordinator.js'
import { InfernoKernel } from './core/InfernoKernel.js'
export async function createAGIKernel(config?: {
  maxAtoms?: number
  distributedNodes?: string[]
  reasoningDepth?: number
}) {
  const kernel = new InfernoKernel(config)
  await kernel.boot()
  return kernel
}