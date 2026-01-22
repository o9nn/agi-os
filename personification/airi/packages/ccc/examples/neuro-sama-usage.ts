import type { AiriCard } from '@proj-airi/ccc'
import { createNeuroSamaInstance, neuroSama, neuroSamaAiriCard, neuroSamaConfigs } from '@proj-airi/ccc'
console.log('Basic Neuro-sama card:', neuroSama)
const defaultNeuro: AiriCard = neuroSamaAiriCard
console.log('Neuro-sama with Airi extensions:', defaultNeuro)
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
    customGame: {
      prompt: 'Custom game-specific behavior...',
    },
  },
})
console.log('Custom Neuro-sama instance:', customNeuro)
const gamingNeuro = neuroSamaConfigs.gaming
const streamingNeuro = neuroSamaConfigs.streaming
const optimizedNeuro = neuroSamaConfigs.optimized
console.log('Gaming preset:', gamingNeuro)
console.log('Streaming preset:', streamingNeuro)
console.log('Optimized preset:', optimizedNeuro)
console.log('Character name:', neuroSama.name)
console.log('Character personality:', neuroSama.personality)
console.log('System prompt:', neuroSama.systemPrompt)
console.log('First greeting:', neuroSama.greetings?.[0])