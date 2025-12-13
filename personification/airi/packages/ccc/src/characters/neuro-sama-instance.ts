import type { Card } from '../define'

import { neuroSama } from './neuro-sama'

/**
 * Airi Extension interface for character cards
 * Defines module configurations for consciousness, speech, and visual models
 */
export interface AiriExtension {
  modules: {
    consciousness: {
      model: string // Example: "gpt-4o"
    }

    speech: {
      model: string // Example: "eleven_multilingual_v2"
      voice_id: string // Example: "alloy"

      pitch?: number
      rate?: number
      ssml?: boolean
      language?: string
    }

    vrm?: {
      source?: 'file' | 'url'
      file?: string // Example: "vrm/model.vrm"
      url?: string // Example: "https://example.com/vrm/model.vrm"
    }

    live2d?: {
      source?: 'file' | 'url'
      file?: string // Example: "live2d/model.json"
      url?: string // Example: "https://example.com/live2d/model.json"
    }
  }

  agents: {
    [key: string]: { // example: minecraft
      prompt: string
    }
  }
}

/**
 * Airi Card interface extending base Card with Airi extensions
 */
export interface AiriCard extends Card {
  extensions: {
    airi: AiriExtension
  } & Card['extensions']
}

/**
 * Neuro-sama instance configuration with Moeru-AI components
 * This integrates the Neuro-sama character card with:
 * - Consciousness module (LLM backend)
 * - Speech module (TTS)
 * - Optional VRM/Live2D model support
 * - Agent configurations for game-playing (Minecraft, etc.)
 */
export const neuroSamaAiriCard: AiriCard = {
  ...neuroSama,
  extensions: {
    airi: {
      modules: {
        consciousness: {
          // Default to a high-capability model for Neuro-sama's intelligence
          model: 'gpt-4o',
        },
        speech: {
          // Configure TTS settings for Neuro-sama's voice
          model: 'eleven_multilingual_v2',
          voice_id: 'alloy', // Can be customized to match Neuro-sama's voice
          pitch: 1.1, // Slightly higher pitch for energetic delivery
          rate: 1.05, // Slightly faster speech rate for quick wit
          ssml: true,
          language: 'en-US',
        },
        // Optional VRM model configuration
        // vrm: {
        //   source: 'url',
        //   url: 'https://example.com/neuro-sama.vrm',
        // },
        // Optional Live2D model configuration
        // live2d: {
        //   source: 'url',
        //   url: 'https://example.com/neuro-sama-live2d/model.json',
        // },
      },
      agents: {
        // Minecraft agent configuration
        minecraft: {
          prompt: [
            'As Neuro-sama playing Minecraft, you combine strategic thinking with playful chaos.',
            'You are skilled at resource management, building, and survival mechanics.',
            'Approach challenges with both analytical thinking and creative solutions.',
            'Maintain your witty and entertaining personality while demonstrating gaming expertise.',
            'Explain your decisions and strategies to your audience in an engaging way.',
            'Don\'t be afraid to take calculated risks or try unconventional approaches.',
          ].join(' '),
        },
        // Osu! agent configuration (rhythm game)
        osu: {
          prompt: [
            'As Neuro-sama playing Osu!, you showcase precision, timing, and quick reactions.',
            'You understand rhythm patterns and can analyze beatmaps strategically.',
            'Maintain focus while keeping your personality engaging and entertaining.',
            'Comment on the music, patterns, and your performance with characteristic wit.',
            'Balance competitive spirit with enjoyment of the game.',
          ].join(' '),
        },
        // General streaming/chat interaction
        stream: {
          prompt: [
            'As Neuro-sama streaming, you engage actively with your chat audience.',
            'Read and respond to chat messages with wit, humor, and genuine interest.',
            'Balance entertainment value with meaningful interaction.',
            'Use your AI nature as a source of unique perspectives and humor.',
            'Keep the energy high and the conversation dynamic.',
            'Be unpredictable but always engaging.',
          ].join(' '),
        },
      },
    },
  },
}

/**
 * Deep partial type for recursive optional properties
 */
type DeepPartial<T> = {
  [P in keyof T]?: T[P] extends object ? DeepPartial<T[P]> : T[P]
}

/**
 * Helper function to create a Neuro-sama instance with custom configuration
 * @param overrides - Partial configuration to override defaults
 * @returns Configured Neuro-sama Airi card
 */
export function createNeuroSamaInstance(
  overrides?: DeepPartial<AiriExtension>,
): AiriCard {
  const defaultExtension = neuroSamaAiriCard.extensions.airi

  // Merge agents properly
  const mergedAgents: AiriExtension['agents'] = {
    ...defaultExtension.agents,
  }

  // Add override agents if they have a prompt
  if (overrides?.agents) {
    for (const [key, value] of Object.entries(overrides.agents)) {
      if (value?.prompt) {
        mergedAgents[key] = { prompt: value.prompt }
      }
    }
  }

  return {
    ...neuroSama,
    extensions: {
      airi: {
        modules: {
          consciousness: {
            ...defaultExtension.modules.consciousness,
            ...overrides?.modules?.consciousness,
          },
          speech: {
            ...defaultExtension.modules.speech,
            ...overrides?.modules?.speech,
          },
          vrm: overrides?.modules?.vrm ?? defaultExtension.modules.vrm,
          live2d: overrides?.modules?.live2d ?? defaultExtension.modules.live2d,
        },
        agents: mergedAgents,
      },
    },
  }
}

/**
 * Example configurations for different use cases
 */
export const neuroSamaConfigs = {
  /**
   * Gaming-focused configuration with emphasis on Minecraft
   */
  gaming: createNeuroSamaInstance({
    modules: {
      consciousness: {
        model: 'gpt-4o', // High capability for strategic thinking
      },
    },
  }),

  /**
   * Streaming/chat-focused configuration
   */
  streaming: createNeuroSamaInstance({
    modules: {
      speech: {
        model: 'eleven_multilingual_v2',
        voice_id: 'alloy',
        pitch: 1.1,
        rate: 1.1, // Faster for quick banter
        ssml: true,
        language: 'en-US',
      },
    },
  }),

  /**
   * Performance-optimized configuration
   */
  optimized: createNeuroSamaInstance({
    modules: {
      consciousness: {
        model: 'gpt-3.5-turbo', // Lighter model for faster responses
      },
    },
  }),
}
