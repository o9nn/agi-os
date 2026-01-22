import type { EmotionIntensity, EmotionParameterMap, Live2DModelParameters, PartialLive2DParameters } from './types'
import { Emotion } from './types'
export const DEFAULT_EMOTION_MAP: EmotionParameterMap = {
  neutral: {
    leftEyeOpen: 1.0,
    rightEyeOpen: 1.0,
    leftEyeSmile: 0.0,
    rightEyeSmile: 0.0,
    leftEyebrowY: 0.0,
    rightEyebrowY: 0.0,
    leftEyebrowAngle: 0.0,
    rightEyebrowAngle: 0.0,
    mouthOpen: 0.0,
    mouthForm: 0.0,
    cheek: 0.0,
  },
  happy: {
    leftEyeOpen: 0.8,
    rightEyeOpen: 0.8,
    leftEyeSmile: 1.0,
    rightEyeSmile: 1.0,
    leftEyebrowY: 0.3,
    rightEyebrowY: 0.3,
    mouthOpen: 0.4,
    mouthForm: 1.0, 
    cheek: 0.6,
  },
  sad: {
    leftEyeOpen: 0.6,
    rightEyeOpen: 0.6,
    leftEyeSmile: 0.0,
    rightEyeSmile: 0.0,
    leftEyebrowY: -0.5,
    rightEyebrowY: -0.5,
    leftEyebrowAngle: 0.3,
    rightEyebrowAngle: -0.3,
    mouthOpen: 0.1,
    mouthForm: -0.5, 
    cheek: 0.0,
  },
  angry: {
    leftEyeOpen: 0.9,
    rightEyeOpen: 0.9,
    leftEyeSmile: 0.0,
    rightEyeSmile: 0.0,
    leftEyebrowY: -0.7,
    rightEyebrowY: -0.7,
    leftEyebrowAngle: -0.5,
    rightEyebrowAngle: 0.5,
    mouthOpen: 0.2,
    mouthForm: -0.3,
    cheek: 0.0,
  },
  surprised: {
    leftEyeOpen: 1.0,
    rightEyeOpen: 1.0,
    leftEyeSmile: 0.0,
    rightEyeSmile: 0.0,
    leftEyebrowY: 0.8,
    rightEyebrowY: 0.8,
    mouthOpen: 0.7,
    mouthForm: 0.0,
    cheek: 0.2,
  },
  disgusted: {
    leftEyeOpen: 0.5,
    rightEyeOpen: 0.5,
    leftEyeSmile: 0.0,
    rightEyeSmile: 0.0,
    leftEyebrowY: -0.4,
    rightEyebrowY: -0.4,
    leftEyebrowAngle: -0.3,
    rightEyebrowAngle: 0.3,
    mouthOpen: 0.0,
    mouthForm: -0.7,
    cheek: 0.0,
  },
  fearful: {
    leftEyeOpen: 1.0,
    rightEyeOpen: 1.0,
    leftEyeSmile: 0.0,
    rightEyeSmile: 0.0,
    leftEyebrowY: 0.5,
    rightEyebrowY: 0.5,
    leftEyebrowAngle: 0.3,
    rightEyebrowAngle: -0.3,
    mouthOpen: 0.3,
    mouthForm: -0.2,
    cheek: 0.0,
  },
  contempt: {
    leftEyeOpen: 0.7,
    rightEyeOpen: 0.7,
    leftEyeSmile: 0.0,
    rightEyeSmile: 0.3,
    leftEyebrowY: -0.2,
    rightEyebrowY: 0.2,
    mouthOpen: 0.0,
    mouthForm: 0.3,
    cheek: 0.0,
  },
  excited: {
    leftEyeOpen: 1.0,
    rightEyeOpen: 1.0,
    leftEyeSmile: 0.7,
    rightEyeSmile: 0.7,
    leftEyebrowY: 0.5,
    rightEyebrowY: 0.5,
    mouthOpen: 0.6,
    mouthForm: 1.0,
    cheek: 0.8,
  },
  confused: {
    leftEyeOpen: 0.8,
    rightEyeOpen: 0.6,
    leftEyeSmile: 0.0,
    rightEyeSmile: 0.0,
    leftEyebrowY: 0.3,
    rightEyebrowY: -0.3,
    leftEyebrowAngle: 0.2,
    rightEyebrowAngle: -0.2,
    mouthOpen: 0.1,
    mouthForm: -0.1,
    cheek: 0.0,
  },
  bored: {
    leftEyeOpen: 0.4,
    rightEyeOpen: 0.4,
    leftEyeSmile: 0.0,
    rightEyeSmile: 0.0,
    leftEyebrowY: -0.2,
    rightEyebrowY: -0.2,
    mouthOpen: 0.0,
    mouthForm: 0.0,
    cheek: 0.0,
  },
  thoughtful: {
    leftEyeOpen: 0.7,
    rightEyeOpen: 0.7,
    leftEyeSmile: 0.0,
    rightEyeSmile: 0.0,
    leftEyebrowY: 0.2,
    rightEyebrowY: 0.2,
    leftEyebrowAngle: 0.1,
    rightEyebrowAngle: -0.1,
    mouthOpen: 0.0,
    mouthForm: 0.0,
    cheek: 0.0,
  },
  amused: {
    leftEyeOpen: 0.6,
    rightEyeOpen: 0.6,
    leftEyeSmile: 0.8,
    rightEyeSmile: 0.8,
    leftEyebrowY: 0.2,
    rightEyebrowY: 0.2,
    mouthOpen: 0.2,
    mouthForm: 0.7,
    cheek: 0.5,
  },
  embarrassed: {
    leftEyeOpen: 0.5,
    rightEyeOpen: 0.5,
    leftEyeSmile: 0.3,
    rightEyeSmile: 0.3,
    leftEyebrowY: 0.1,
    rightEyebrowY: 0.1,
    leftEyebrowAngle: 0.2,
    rightEyebrowAngle: -0.2,
    mouthOpen: 0.1,
    mouthForm: 0.2,
    cheek: 0.9,
  },
}
export class EmotionMapper {
  private emotionMap: EmotionParameterMap
  constructor(customMap?: Partial<EmotionParameterMap>) {
    this.emotionMap = { ...DEFAULT_EMOTION_MAP, ...customMap }
  }
  getParametersForEmotion(
    emotion: Emotion,
    intensity: EmotionIntensity | number = 1.0,
  ): PartialLive2DParameters {
    const baseParams = this.emotionMap[emotion]
    if (intensity === 1.0) {
      return baseParams
    }
    const scaledParams: PartialLive2DParameters = {}
    for (const [key, value] of Object.entries(baseParams)) {
      if (typeof value === 'number') {
        scaledParams[key as keyof Live2DModelParameters] = value * intensity
      }
    }
    return scaledParams
  }
  blendEmotions(
    emotion1: Emotion,
    emotion2: Emotion,
    blendFactor: number, 
  ): PartialLive2DParameters {
    const params1 = this.emotionMap[emotion1]
    const params2 = this.emotionMap[emotion2]
    const blended: PartialLive2DParameters = {}
    const allKeys = new Set([
      ...Object.keys(params1),
      ...Object.keys(params2),
    ])
    for (const key of allKeys) {
      const k = key as keyof Live2DModelParameters
      const val1 = params1[k] ?? 0
      const val2 = params2[k] ?? 0
      blended[k] = val1 * (1 - blendFactor) + val2 * blendFactor
    }
    return blended
  }
  updateEmotionMap(emotion: Emotion, parameters: PartialLive2DParameters): void {
    this.emotionMap[emotion] = { ...this.emotionMap[emotion], ...parameters }
  }
  getAvailableEmotions(): Emotion[] {
    return Object.values(Emotion)
  }
}
export function createEmotionMapper(customMap?: Partial<EmotionParameterMap>): EmotionMapper {
  return new EmotionMapper(customMap)
}