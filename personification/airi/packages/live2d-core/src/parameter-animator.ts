/**
 * Parameter Animator
 * 
 * Provides smooth parameter transitions and animations for Live2D models.
 * Includes easing functions and interpolation utilities.
 */

import type { EasingFunction, Live2DModelParameters, PartialLive2DParameters } from './types'

/**
 * Standard easing functions
 */

// Helper function for bounce easing (defined first to avoid circular dependency)
function bounceOut(t: number): number {
  const n1 = 7.5625
  const d1 = 2.75
  
  if (t < 1 / d1) {
    return n1 * t * t
  } else if (t < 2 / d1) {
    const adjusted = t - (1.5 / d1)
    return n1 * adjusted * adjusted + 0.75
  } else if (t < 2.5 / d1) {
    const adjusted = t - (2.25 / d1)
    return n1 * adjusted * adjusted + 0.9375
  } else {
    const adjusted = t - (2.625 / d1)
    return n1 * adjusted * adjusted + 0.984375
  }
}

export const Easing = {
  linear: (t: number): number => t,
  
  easeInQuad: (t: number): number => t * t,
  easeOutQuad: (t: number): number => t * (2 - t),
  easeInOutQuad: (t: number): number => t < 0.5 ? 2 * t * t : -1 + (4 - 2 * t) * t,
  
  easeInCubic: (t: number): number => t * t * t,
  easeOutCubic: (t: number): number => {
    const adjusted = t - 1
    return adjusted * adjusted * adjusted + 1
  },
  easeInOutCubic: (t: number): number => {
    if (t < 0.5) {
      return 4 * t * t * t
    }
    const adjusted = t - 1
    return adjusted * (2 * adjusted - 2) * (2 * adjusted - 2) + 1
  },
  
  easeInQuart: (t: number): number => t * t * t * t,
  easeOutQuart: (t: number): number => {
    const adjusted = t - 1
    return 1 - adjusted * adjusted * adjusted * adjusted
  },
  easeInOutQuart: (t: number): number => {
    if (t < 0.5) {
      return 8 * t * t * t * t
    }
    const adjusted = t - 1
    return 1 - 8 * adjusted * adjusted * adjusted * adjusted
  },
  
  easeInSine: (t: number): number => 1 - Math.cos((t * Math.PI) / 2),
  easeOutSine: (t: number): number => Math.sin((t * Math.PI) / 2),
  easeInOutSine: (t: number): number => -(Math.cos(Math.PI * t) - 1) / 2,
  
  easeInExpo: (t: number): number => t === 0 ? 0 : Math.pow(2, 10 * t - 10),
  easeOutExpo: (t: number): number => t === 1 ? 1 : 1 - Math.pow(2, -10 * t),
  easeInOutExpo: (t: number): number => {
    if (t === 0) return 0
    if (t === 1) return 1
    return t < 0.5 ? Math.pow(2, 20 * t - 10) / 2 : (2 - Math.pow(2, -20 * t + 10)) / 2
  },
  
  easeInElastic: (t: number): number => {
    const c4 = (2 * Math.PI) / 3
    return t === 0 ? 0 : t === 1 ? 1 : -Math.pow(2, 10 * t - 10) * Math.sin((t * 10 - 10.75) * c4)
  },
  easeOutElastic: (t: number): number => {
    const c4 = (2 * Math.PI) / 3
    return t === 0 ? 0 : t === 1 ? 1 : Math.pow(2, -10 * t) * Math.sin((t * 10 - 0.75) * c4) + 1
  },
  
  easeInBounce: (t: number): number => 1 - bounceOut(1 - t),
  easeOutBounce: bounceOut,
} as const

/**
 * Active animation tracking
 */
interface ActiveAnimation {
  key: keyof Live2DModelParameters
  startValue: number
  targetValue: number
  startTime: number
  duration: number
  easing: EasingFunction
}

/**
 * Parameter Animator class
 * Manages smooth parameter transitions
 */
export class ParameterAnimator {
  private activeAnimations: Map<keyof Live2DModelParameters, ActiveAnimation> = new Map()
  private currentValues: Partial<Live2DModelParameters> = {}
  
  /**
   * Start animating a parameter
   */
  animateParameter(
    key: keyof Live2DModelParameters,
    targetValue: number,
    duration: number,
    easing: EasingFunction = Easing.easeOutCubic,
  ): void {
    const startValue = this.currentValues[key] ?? 0
    const startTime = performance.now()
    
    this.activeAnimations.set(key, {
      key,
      startValue,
      targetValue,
      startTime,
      duration,
      easing,
    })
  }
  
  /**
   * Animate multiple parameters at once
   */
  animateParameters(
    parameters: PartialLive2DParameters,
    duration: number,
    easing: EasingFunction = Easing.easeOutCubic,
  ): void {
    for (const [key, targetValue] of Object.entries(parameters)) {
      if (typeof targetValue === 'number') {
        this.animateParameter(key as keyof Live2DModelParameters, targetValue, duration, easing)
      }
    }
  }
  
  /**
   * Update animations and get current parameter values
   * Should be called in animation loop
   */
  update(currentTime: number = performance.now()): PartialLive2DParameters {
    const updatedValues: PartialLive2DParameters = {}
    const completedAnimations: (keyof Live2DModelParameters)[] = []
    
    for (const [key, animation] of this.activeAnimations) {
      const elapsed = currentTime - animation.startTime
      const progress = Math.min(elapsed / animation.duration, 1.0)
      
      if (progress >= 1.0) {
        // Animation complete
        this.currentValues[key] = animation.targetValue
        updatedValues[key] = animation.targetValue
        completedAnimations.push(key)
      } else {
        // Calculate interpolated value
        const easedProgress = animation.easing(progress)
        const value = animation.startValue + (animation.targetValue - animation.startValue) * easedProgress
        this.currentValues[key] = value
        updatedValues[key] = value
      }
    }
    
    // Remove completed animations
    for (const key of completedAnimations) {
      this.activeAnimations.delete(key)
    }
    
    return updatedValues
  }
  
  /**
   * Stop an active animation
   */
  stopAnimation(key: keyof Live2DModelParameters): void {
    this.activeAnimations.delete(key)
  }
  
  /**
   * Stop all animations
   */
  stopAllAnimations(): void {
    this.activeAnimations.clear()
  }
  
  /**
   * Check if any animations are active
   */
  hasActiveAnimations(): boolean {
    return this.activeAnimations.size > 0
  }
  
  /**
   * Get current parameter values
   */
  getCurrentValues(): Partial<Live2DModelParameters> {
    return { ...this.currentValues }
  }
  
  /**
   * Set current value without animation
   */
  setParameterImmediate(key: keyof Live2DModelParameters, value: number): void {
    this.currentValues[key] = value
    this.activeAnimations.delete(key)
  }
  
  /**
   * Set multiple parameters immediately
   */
  setParametersImmediate(parameters: PartialLive2DParameters): void {
    for (const [key, value] of Object.entries(parameters)) {
      if (typeof value === 'number') {
        this.setParameterImmediate(key as keyof Live2DModelParameters, value)
      }
    }
  }
}

/**
 * Interpolate between two values
 */
export function lerp(start: number, end: number, t: number): number {
  return start + (end - start) * t
}

/**
 * Clamp value between min and max
 */
export function clamp(value: number, min: number, max: number): number {
  return Math.min(Math.max(value, min), max)
}

/**
 * Normalize value from one range to another
 */
export function normalize(value: number, min: number, max: number): number {
  return (value - min) / (max - min)
}

/**
 * Map value from one range to another
 */
export function mapRange(
  value: number,
  inMin: number,
  inMax: number,
  outMin: number,
  outMax: number,
): number {
  return outMin + ((value - inMin) * (outMax - outMin)) / (inMax - inMin)
}

/**
 * Create a parameter animator instance
 */
export function createParameterAnimator(): ParameterAnimator {
  return new ParameterAnimator()
}
