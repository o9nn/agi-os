/**
 * Example: Using Neuro-sama Character Card
 *
 * This example demonstrates how to use the Neuro-sama character card
 * with the Airi platform and Moeru-AI components.
 */

import type { AiriCard } from '@proj-airi/ccc'
import { createNeuroSamaInstance, neuroSama, neuroSamaAiriCard, neuroSamaConfigs } from '@proj-airi/ccc'

// Example 1: Use the basic character card
console.log('Basic Neuro-sama card:', neuroSama)

// Example 2: Use the full Airi instance with default configuration
const defaultNeuro: AiriCard = neuroSamaAiriCard
console.log('Neuro-sama with Airi extensions:', defaultNeuro)

// Example 3: Create a custom instance with specific settings
const customNeuro = createNeuroSamaInstance({
  modules: {
    consciousness: {
      model: 'gpt-4-turbo',
    },
    speech: {
      model: 'eleven_multilingual_v2',
      voice_id: 'custom-voice',
      pitch: 1.2,
      rate: 1.15,
      ssml: true,
      language: 'en-US',
    },
    vrm: {
      source: 'url',
      url: 'https://example.com/neuro-sama.vrm',
    },
  },
  agents: {
    minecraft: {
      prompt: 'Custom Minecraft agent behavior...',
    },
    // Add custom agents
    customGame: {
      prompt: 'Custom game-specific behavior...',
    },
  },
})
console.log('Custom Neuro-sama instance:', customNeuro)

// Example 4: Use preset configurations
const gamingNeuro = neuroSamaConfigs.gaming
const streamingNeuro = neuroSamaConfigs.streaming
const optimizedNeuro = neuroSamaConfigs.optimized

console.log('Gaming preset:', gamingNeuro)
console.log('Streaming preset:', streamingNeuro)
console.log('Optimized preset:', optimizedNeuro)

// Example 5: Access character properties
console.log('Character name:', neuroSama.name)
console.log('Character personality:', neuroSama.personality)
console.log('System prompt:', neuroSama.systemPrompt)
console.log('First greeting:', neuroSama.greetings?.[0])

// Example 6: Integration with Airi Card Store (conceptual)
// In a real application, you would use this with the actual store
/*
import { useAiriCardStore } from '@proj-airi/stage-ui/stores/modules/airi-card'

const airiCardStore = useAiriCardStore()

// Add Neuro-sama to the card store
const cardId = airiCardStore.addCard(neuroSamaAiriCard)

// Set as active card
airiCardStore.activeCardId = cardId

// Access the active card
const activeCard = airiCardStore.activeCard
console.log('Active card:', activeCard)
*/
