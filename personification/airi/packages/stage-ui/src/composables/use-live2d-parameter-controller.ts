/**
 * Enhanced Live2D Parameter Controller
 * 
 * Provides utilities for controlling Live2D model parameters with
 * emotion mapping and smooth animations.
 */

import { ref, watch } from 'vue'
import type { Ref } from 'vue'
import type { Live2DModelParameters, PartialLive2DParameters } from '@proj-airi/live2d-core'
import {
  Emotion,
  createEmotionMapper,
  createParameterAnimator,
  Easing,
  EmotionIntensity,
  clampParameters,
} from '@proj-airi/live2d-core'

export interface Live2DParameterControllerOptions {
  /**
   * Default animation duration in milliseconds
   */
  defaultDuration?: number
  
  /**
   * Default easing function
   */
  defaultEasing?: (t: number) => number
  
  /**
   * Enable automatic parameter clamping
   */
  autoClamp?: boolean
}

/**
 * Enhanced Live2D parameter controller with emotion mapping
 */
export function useLive2DParameterController(
  options: Live2DParameterControllerOptions = {},
) {
  const {
    defaultDuration = 300,
    defaultEasing = Easing.easeOutCubic,
    autoClamp = true,
  } = options
  
  const emotionMapper = createEmotionMapper()
  const animator = createParameterAnimator()
  
  const currentEmotion = ref<Emotion | null>(null)
  const currentParameters = ref<PartialLive2DParameters>({})
  const isAnimating = ref(false)
  
  /**
   * Set emotion with animation
   */
  function setEmotion(
    emotion: Emotion,
    intensity: number | typeof EmotionIntensity[keyof typeof EmotionIntensity] = EmotionIntensity.Strong,
    duration: number = defaultDuration,
  ): void {
    currentEmotion.value = emotion
    const params = emotionMapper.getParametersForEmotion(emotion, intensity)
    const finalParams = autoClamp ? clampParameters(params) : params
    animator.animateParameters(finalParams, duration, defaultEasing)
  }
  
  /**
   * Blend between two emotions
   */
  function blendEmotions(
    emotion1: Emotion,
    emotion2: Emotion,
    blendFactor: number,
    duration: number = defaultDuration,
  ): void {
    const params = emotionMapper.blendEmotions(emotion1, emotion2, blendFactor)
    const finalParams = autoClamp ? clampParameters(params) : params
    animator.animateParameters(finalParams, duration, defaultEasing)
  }
  
  /**
   * Set individual parameter with animation
   */
  function setParameter(
    key: keyof Live2DModelParameters,
    value: number,
    duration: number = defaultDuration,
  ): void {
    const finalValue = autoClamp ? clampParameters({ [key]: value })[key]! : value
    animator.animateParameter(key, finalValue, duration, defaultEasing)
  }
  
  /**
   * Set multiple parameters with animation
   */
  function setParameters(
    params: PartialLive2DParameters,
    duration: number = defaultDuration,
  ): void {
    const finalParams = autoClamp ? clampParameters(params) : params
    animator.animateParameters(finalParams, duration, defaultEasing)
  }
  
  /**
   * Set parameter immediately without animation
   */
  function setParameterImmediate(
    key: keyof Live2DModelParameters,
    value: number,
  ): void {
    const finalValue = autoClamp ? clampParameters({ [key]: value })[key]! : value
    animator.setParameterImmediate(key, finalValue)
  }
  
  /**
   * Set multiple parameters immediately
   */
  function setParametersImmediate(params: PartialLive2DParameters): void {
    const finalParams = autoClamp ? clampParameters(params) : params
    animator.setParametersImmediate(finalParams)
  }
  
  /**
   * Reset to neutral emotion
   */
  function reset(duration: number = defaultDuration): void {
    setEmotion(Emotion.Neutral, EmotionIntensity.Strong, duration)
  }
  
  /**
   * Stop all animations
   */
  function stopAnimations(): void {
    animator.stopAllAnimations()
  }
  
  /**
   * Update animation (call in animation loop)
   */
  function update(): PartialLive2DParameters {
    const params = animator.update()
    currentParameters.value = params
    isAnimating.value = animator.hasActiveAnimations()
    return params
  }
  
  /**
   * Get current parameter values
   */
  function getCurrentParameters(): PartialLive2DParameters {
    return animator.getCurrentValues()
  }
  
  /**
   * Update custom emotion mapping
   */
  function updateEmotionMapping(
    emotion: Emotion,
    parameters: PartialLive2DParameters,
  ): void {
    emotionMapper.updateEmotionMap(emotion, parameters)
  }
  
  return {
    // State
    currentEmotion,
    currentParameters,
    isAnimating,
    
    // Methods
    setEmotion,
    blendEmotions,
    setParameter,
    setParameters,
    setParameterImmediate,
    setParametersImmediate,
    reset,
    stopAnimations,
    update,
    getCurrentParameters,
    updateEmotionMapping,
    
    // Instances (for advanced usage)
    emotionMapper,
    animator,
  }
}

/**
 * Auto-update Live2D parameters in a reactive way
 */
export function useAutoUpdateLive2DParameters(
  controller: ReturnType<typeof useLive2DParameterController>,
  targetRef: Ref<PartialLive2DParameters>,
): void {
  // Set up automatic update loop
  let animationFrameId: number | null = null
  
  const updateLoop = () => {
    const params = controller.update()
    targetRef.value = { ...targetRef.value, ...params }
    
    if (controller.isAnimating.value) {
      animationFrameId = requestAnimationFrame(updateLoop)
    }
  }
  
  // Watch for animation state changes
  watch(controller.isAnimating, (isAnimating) => {
    if (isAnimating && animationFrameId === null) {
      animationFrameId = requestAnimationFrame(updateLoop)
    } else if (!isAnimating && animationFrameId !== null) {
      cancelAnimationFrame(animationFrameId)
      animationFrameId = null
    }
  })
}
