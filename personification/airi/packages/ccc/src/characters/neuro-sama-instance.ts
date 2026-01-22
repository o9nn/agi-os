import type { Card } from '../define'
import { neuroSama } from './neuro-sama'
export interface AiriExtension {
  modules: {
    consciousness: {
      model: string 
    }
    speech: {
      model: string 
      voice_id: string 
      pitch?: number
      rate?: number
      ssml?: boolean
      language?: string
    }
    vrm?: {
      source?: 'file' | 'url'
      file?: string 
      url?: string 
    }
    live2d?: {
      source?: 'file' | 'url'
      file?: string 
      url?: string 
    }
  }
  agents: {
    [key: string]: { 
      prompt: string
    }
  }
}
export interface AiriCard extends Card {
  extensions: {
    airi: AiriExtension
  } & Card['extensions']
}
export const neuroSamaAiriCard: AiriCard = {
  ...neuroSama,
  extensions: {
    airi: {
      modules: {
        consciousness: {
          model: 'gpt-4o',
        },
        speech: {
          model: 'eleven_multilingual_v2',
          voice_id: 'alloy', 
          pitch: 1.1, 
          rate: 1.05, 
          ssml: true,
          language: 'en-US',
        },
      },
      agents: {
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
        osu: {
          prompt: [
            'As Neuro-sama playing Osu!, you showcase precision, timing, and quick reactions.',
            'You understand rhythm patterns and can analyze beatmaps strategically.',
            'Maintain focus while keeping your personality engaging and entertaining.',
            'Comment on the music, patterns, and your performance with characteristic wit.',
            'Balance competitive spirit with enjoyment of the game.',
          ].join(' '),
        },
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
type DeepPartial<T> = {
  [P in keyof T]?: T[P] extends object ? DeepPartial<T[P]> : T[P]
}
export function createNeuroSamaInstance(
  overrides?: DeepPartial<AiriExtension>,
): AiriCard {
  const defaultExtension = neuroSamaAiriCard.extensions.airi
  const mergedAgents: AiriExtension['agents'] = {
    ...defaultExtension.agents,
  }
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
export const neuroSamaConfigs = {
  gaming: createNeuroSamaInstance({
    modules: {
      consciousness: {
        model: 'gpt-4o', 
      },
    },
  }),
  streaming: createNeuroSamaInstance({
    modules: {
      speech: {
        model: 'eleven_multilingual_v2',
        voice_id: 'alloy',
        pitch: 1.1,
        rate: 1.1, 
        ssml: true,
        language: 'en-US',
      },
    },
  }),
  optimized: createNeuroSamaInstance({
    modules: {
      consciousness: {
        model: 'gpt-3.5-turbo', 
      },
    },
  }),
}