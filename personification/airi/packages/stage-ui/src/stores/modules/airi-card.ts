import type { Card, ccv3 } from '@proj-airi/ccc'
import { useLocalStorage } from '@vueuse/core'
import { nanoid } from 'nanoid'
import { defineStore, storeToRefs } from 'pinia'
import { computed, onMounted, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import SystemPromptV2 from '../../constants/prompts/system-v2'
import { useConsciousnessStore } from './consciousness'
import { useSpeechStore } from './speech'
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
export const useAiriCardStore = defineStore('airi-card', () => {
  const cards = useLocalStorage<Map<string, AiriCard>>('airi-cards', new Map())
  const activeCardId = useLocalStorage('airi-card-active-id', 'default')
  const activeCard = computed(() => cards.value.get(activeCardId.value))
  const consciousnessStore = useConsciousnessStore()
  const speechStore = useSpeechStore()
  const {
    activeModel: activeConsciousnessModel,
  } = storeToRefs(consciousnessStore)
  const {
    activeSpeechVoiceId,
    activeSpeechModel,
  } = storeToRefs(speechStore)
  const addCard = (card: AiriCard | Card | ccv3.CharacterCardV3) => {
    const newCardId = nanoid()
    cards.value.set(newCardId, newAiriCard(card))
    return newCardId
  }
  const removeCard = (id: string) => {
    cards.value.delete(id)
  }
  const getCard = (id: string) => {
    return cards.value.get(id)
  }
  function resolveAiriExtension(card: Card | ccv3.CharacterCardV3): AiriExtension {
    const existingExtension = ('data' in card
      ? card.data?.extensions?.airi
      : card.extensions?.airi) as AiriExtension
    const defaultModules = {
      consciousness: {
        model: activeConsciousnessModel.value,
      },
      speech: {
        model: activeSpeechModel.value,
        voice_id: activeSpeechVoiceId.value,
      },
    }
    if (!existingExtension) {
      return {
        modules: defaultModules,
        agents: {},
      }
    }
    return {
      modules: {
        consciousness: {
          model: existingExtension.modules?.consciousness?.model ?? defaultModules.consciousness.model,
        },
        speech: {
          model: existingExtension.modules?.speech?.model ?? defaultModules.speech.model,
          voice_id: existingExtension.modules?.speech?.voice_id ?? defaultModules.speech.voice_id,
          pitch: existingExtension.modules?.speech?.pitch,
          rate: existingExtension.modules?.speech?.rate,
          ssml: existingExtension.modules?.speech?.ssml,
          language: existingExtension.modules?.speech?.language,
        },
        vrm: existingExtension.modules?.vrm,
        live2d: existingExtension.modules?.live2d,
      },
      agents: existingExtension.agents ?? {},
    }
  }
  function newAiriCard(card: Card | ccv3.CharacterCardV3): AiriCard {
    if ('data' in card) {
      const ccv3Card = card as ccv3.CharacterCardV3
      return {
        name: ccv3Card.data.name,
        version: ccv3Card.data.character_version ?? '1.0.0',
        description: ccv3Card.data.description ?? '',
        creator: ccv3Card.data.creator ?? '',
        notes: ccv3Card.data.creator_notes ?? '',
        notesMultilingual: ccv3Card.data.creator_notes_multilingual,
        personality: ccv3Card.data.personality ?? '',
        scenario: ccv3Card.data.scenario ?? '',
        greetings: [
          ccv3Card.data.first_mes,
          ...(ccv3Card.data.alternate_greetings ?? []),
        ],
        greetingsGroupOnly: ccv3Card.data.group_only_greetings ?? [],
        systemPrompt: ccv3Card.data.system_prompt ?? '',
        postHistoryInstructions: ccv3Card.data.post_history_instructions ?? '',
        messageExample: ccv3Card.data.mes_example
          ? ccv3Card.data.mes_example
              .split('<START>\n')
              .filter(Boolean)
              .map(example => example.split('\n')
                .map((line) => {
                  if (line.startsWith('{{char}}:') || line.startsWith('{{user}}:'))
                    return line as `{{char}}: ${string}` | `{{user}}: ${string}`
                  throw new Error(`Invalid message example format: ${line}`)
                }))
          : [],
        tags: ccv3Card.data.tags ?? [],
        extensions: {
          airi: resolveAiriExtension(ccv3Card),
          ...ccv3Card.data.extensions,
        },
      }
    }
    return {
      ...card,
      extensions: {
        airi: resolveAiriExtension(card),
        ...card.extensions,
      },
    }
  }
  onMounted(() => {
    const { t } = useI18n()
    cards.value.set('default', newAiriCard({
      name: 'ReLU',
      version: '1.0.0',
      description: SystemPromptV2(
        t('base.prompt.prefix'),
        t('base.prompt.suffix'),
      ).content,
    }))
  })
  watch(activeCard, (newCard: AiriCard | undefined) => {
    if (!newCard)
      return
    const extension = resolveAiriExtension(newCard)
    if (!extension)
      return
    activeConsciousnessModel.value = extension?.modules?.consciousness?.model
    activeSpeechModel.value = extension?.modules?.speech?.model
    activeSpeechVoiceId.value = extension?.modules?.speech?.voice_id
  })
  return {
    cards,
    activeCard,
    activeCardId,
    addCard,
    removeCard,
    getCard,
    currentModels: computed(() => {
      return {
        consciousness: {
          model: activeConsciousnessModel.value,
        },
        speech: {
          model: activeSpeechModel.value,
          voice_id: activeSpeechVoiceId.value,
        },
      } satisfies AiriExtension['modules']
    }),
    systemPrompt: computed(() => {
      const card = activeCard.value
      if (!card)
        return ''
      const components = [
        card.systemPrompt,
        card.description,
        card.personality,
      ].filter(Boolean)
      return components.join('\n')
    }),
  }
})