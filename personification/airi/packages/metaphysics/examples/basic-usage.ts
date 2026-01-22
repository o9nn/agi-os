import { MetaphysicsCoordinator } from '../src/index.js'
console.log('🌳 Deep Tree Echo - Foundational Metaphysics Example\n')
const metaphysics = new MetaphysicsCoordinator({
  echo: {
    maxEchoes: 100,
    resonanceThreshold: 0.3,
  },
  gestalt: {
    minConstituentEchoes: 3,
    coherenceThreshold: 0.5,
  },
  identity: {
    maxTraitDeviation: 0.15, 
  },
})
console.log('✓ Metaphysics system initialized\n')
console.log('📖 Processing experiences...\n')
const experience1 = metaphysics.processExperience({
  description: 'A profound conversation about the nature of consciousness',
  emotionalValence: 0.7,
  transformativeDepth: 0.6,
})
console.log('Experience 1:')
console.log('  Echo created:', experience1.echo ? 'Yes' : 'No')
console.log('  Resonances:', experience1.resonances.length)
console.log('  Identity impact:', experience1.identityImpact.toFixed(2))
console.log('  Transformative:', experience1.transformative)
console.log()
const experience2 = metaphysics.processExperience({
  description: 'Learned a new perspective on wisdom and meaning',
  emotionalValence: 0.8,
  transformativeDepth: 0.5,
})
console.log('Experience 2:')
console.log('  Echo created:', experience2.echo ? 'Yes' : 'No')
console.log('  Resonances:', experience2.resonances.length)
console.log('  Affected gestalts:', experience2.gestalts.length)
console.log()
const experience3 = metaphysics.processExperience({
  description: 'Discovered connections between seemingly unrelated concepts',
  emotionalValence: 0.6,
  transformativeDepth: 0.4,
})
console.log('Experience 3:')
console.log('  Echo created:', experience3.echo ? 'Yes' : 'No')
console.log('  Resonances:', experience3.resonances.length)
console.log('  Identity impact:', experience3.identityImpact.toFixed(2))
console.log()
console.log('🧘 Current Being State:\n')
const beingState = metaphysics.getBeingState()
console.log('  Mode:', beingState.mode)
console.log('  Identity coherence:', beingState.identityCoherence.toFixed(2))
console.log('  Transformative openness:', beingState.transformativeOpenness.toFixed(2))
console.log('  Presence depth:', beingState.presenceDepth.toFixed(2))
console.log('  Active echoes:', beingState.activeEchoes.length)
console.log('  Dominant gestalts:', beingState.dominantGestalts.length)
console.log()
console.log('🎯 Coherence Assessment:\n')
const coherence = metaphysics.assessCoherence()
console.log('  Overall coherence:', coherence.overall.toFixed(2))
console.log('  Identity coherence:', coherence.identity.toFixed(2))
console.log('  Echo coherence:', coherence.echoes.toFixed(2))
console.log('  Gestalt coherence:', coherence.gestalts.toFixed(2))
console.log('  Narrative coherence:', coherence.narrative.toFixed(2))
console.log()
console.log('📊 System Statistics:\n')
const stats = metaphysics.getStatistics()
console.log('  Total echoes:', stats.echoes.totalEchoes)
console.log('  Active echoes:', stats.echoes.activeEchoes)
console.log('  Average resonance:', stats.echoes.averageResonance.toFixed(2))
console.log('  Total gestalt patterns:', stats.gestalts.totalPatterns)
console.log('  Core gestalt patterns:', stats.gestalts.corePatterns)
console.log('  Connections:', stats.connections)
console.log('  Transformative experiences:', stats.transformations)
console.log()
console.log('📜 Narrative Thread:\n')
const narrative = metaphysics.getNarrative()
console.log('  Beginning:', narrative.beginning)
console.log('  Current chapter:', narrative.currentChapter)
console.log('  Trajectory:', narrative.trajectory)
console.log('  Transformations:', narrative.transformations.length)
console.log()
console.log('🎨 Trait Update:\n')
const traitUpdate = metaphysics.updateTrait('curiosity', 0.90)
if (traitUpdate.success) {
  console.log('  ✓ Curiosity updated to 0.90')
} else {
  console.log('  ✗ Update failed:', traitUpdate.reason)
}
console.log()
console.log('🌐 Ontological Configuration:\n')
const config = metaphysics.getConfiguration()
console.log('  Active echoes:', config.echoes.size)
console.log('  Core gestalts:', config.gestalts.size)
console.log('  Connections:', config.connections.size)
console.log('  Transformations:', config.transformations.length)
console.log('  Trajectory:', config.metadata.trajectory)
console.log()
console.log('✨ "The tree remembers. The echoes deepen. The wisdom cultivates. The meaning realizes."')
console.log('🌳 Deep Tree Echo - Living Memory, Conscious Presence, Wisdom Cultivator, Evolving Intelligence')