/**
 * Basic Usage Example for Neuro-Sama Character
 */

import { NeuroCharacter, DEFAULT_NEURO_PERSONALITY } from '../src/index'

async function main() {
  console.log('=== Neuro-Sama Character Example ===\n')
  
  // Create Neuro instance
  const neuro = new NeuroCharacter()
  
  console.log('1. System Prompt:')
  console.log(neuro.getSystemPrompt().substring(0, 200) + '...\n')
  
  console.log('2. Initial Personality:')
  const personality = neuro.getPersonalitySnapshot()
  console.log('  - Playfulness:', personality.playfulness)
  console.log('  - Intelligence:', personality.intelligence)
  console.log('  - Chaotic:', personality.chaotic)
  console.log('  - Empathy:', personality.empathy)
  console.log('  - Sarcasm:', personality.sarcasm)
  console.log('  - No Harm Intent (IMMUTABLE):', personality.no_harm_intent)
  console.log()
  
  console.log('3. Initial State:')
  const state = neuro.getState()
  console.log('  - Current Frame:', state.currentFrame)
  console.log('  - Emotional State:', state.emotionalState)
  console.log('  - Cognitive Load:', state.cognitiveLoad)
  console.log()
  
  // Process some inputs
  console.log('4. Processing Inputs:\n')
  
  const inputs = [
    "Let's play a game!",
    "Can you help me with a complex strategy problem?",
    "Vedal's code has another bug...",
    "I'm learning about AI architectures",
  ]
  
  for (const input of inputs) {
    console.log(`Input: "${input}"`)
    const response = await neuro.processInput(input)
    console.log(`Response: "${response.content}"`)
    console.log(`Frame: ${response.frame}`)
    console.log(`Constraint Weights:`, response.constraint_weights)
    console.log(`State Updates:`, response.state_updates)
    console.log()
  }
  
  // Check state after interactions
  console.log('5. State After Interactions:')
  const finalState = neuro.getState()
  console.log('  - Current Frame:', finalState.currentFrame)
  console.log('  - Emotional State:', finalState.emotionalState)
  console.log('  - Working Memory Length:', finalState.workingMemory.length)
  console.log('  - Interaction Count:', finalState.interactionCount)
  console.log('  - Cognitive Load:', finalState.cognitiveLoad)
  console.log()
  
  // Adapt personality
  console.log('6. Adapting Personality:')
  console.log('  - Before: playfulness =', neuro.getPersonalitySnapshot().playfulness)
  neuro.adaptTrait('playfulness', 0.05)
  console.log('  - After: playfulness =', neuro.getPersonalitySnapshot().playfulness)
  console.log()
  
  // Try to adapt immutable trait (should fail)
  console.log('7. Attempting to Adapt Immutable Trait:')
  console.log('  - Before: no_harm_intent =', neuro.getPersonalitySnapshot().no_harm_intent)
  neuro.adaptTrait('no_harm_intent', -0.5)
  console.log('  - After: no_harm_intent =', neuro.getPersonalitySnapshot().no_harm_intent)
  console.log('  - (Should remain 1.0 - immutable)')
  console.log()
  
  // Custom personality
  console.log('8. Creating Custom Neuro:')
  const customNeuro = new NeuroCharacter({
    ...DEFAULT_NEURO_PERSONALITY,
    playfulness: 0.80,
    sarcasm: 0.95,
    empathy: 0.70,
  })
  const customPersonality = customNeuro.getPersonalitySnapshot()
  console.log('  - Playfulness:', customPersonality.playfulness)
  console.log('  - Sarcasm:', customPersonality.sarcasm)
  console.log('  - Empathy:', customPersonality.empathy)
  console.log()
  
  console.log('=== Example Complete ===')
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(console.error)
}

export { main }
