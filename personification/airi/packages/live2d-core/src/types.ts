/**
 * Live2D Cubism SDK Type Definitions
 * 
 * This module provides comprehensive type definitions for Live2D model parameters
 * and utilities for working with the Cubism SDK.
 */

/**
 * Standard Live2D Cubism parameter IDs
 * These are the most common parameters used across Live2D models
 */
export const CubismParameterIds = {
  // Face angle parameters
  ParamAngleX: 'ParamAngleX',
  ParamAngleY: 'ParamAngleY',
  ParamAngleZ: 'ParamAngleZ',
  
  // Eye parameters
  ParamEyeLOpen: 'ParamEyeLOpen',
  ParamEyeROpen: 'ParamEyeROpen',
  ParamEyeLSmile: 'ParamEyeLSmile',
  ParamEyeRSmile: 'ParamEyeRSmile',
  ParamEyeBallX: 'ParamEyeBallX',
  ParamEyeBallY: 'ParamEyeBallY',
  ParamEyeBallForm: 'ParamEyeBallForm',
  
  // Eyebrow parameters
  ParamBrowLY: 'ParamBrowLY',
  ParamBrowRY: 'ParamBrowRY',
  ParamBrowLX: 'ParamBrowLX',
  ParamBrowRX: 'ParamBrowRX',
  ParamBrowLAngle: 'ParamBrowLAngle',
  ParamBrowRAngle: 'ParamBrowRAngle',
  ParamBrowLForm: 'ParamBrowLForm',
  ParamBrowRForm: 'ParamBrowRForm',
  
  // Mouth parameters
  ParamMouthOpenY: 'ParamMouthOpenY',
  ParamMouthForm: 'ParamMouthForm',
  
  // Body parameters
  ParamBodyAngleX: 'ParamBodyAngleX',
  ParamBodyAngleY: 'ParamBodyAngleY',
  ParamBodyAngleZ: 'ParamBodyAngleZ',
  
  // Other facial parameters
  ParamCheek: 'ParamCheek',
  ParamBreath: 'ParamBreath',
} as const

export type CubismParameterId = typeof CubismParameterIds[keyof typeof CubismParameterIds]

/**
 * Live2D model parameter values
 * All values are normalized floats, typically in range [-1, 1] or [0, 1]
 */
export interface Live2DModelParameters {
  // Face rotation
  angleX: number
  angleY: number
  angleZ: number
  
  // Eyes
  leftEyeOpen: number
  rightEyeOpen: number
  leftEyeSmile: number
  rightEyeSmile: number
  eyeBallX: number
  eyeBallY: number
  
  // Eyebrows
  leftEyebrowLR: number
  rightEyebrowLR: number
  leftEyebrowY: number
  rightEyebrowY: number
  leftEyebrowAngle: number
  rightEyebrowAngle: number
  leftEyebrowForm: number
  rightEyebrowForm: number
  
  // Mouth
  mouthOpen: number
  mouthForm: number
  
  // Body
  bodyAngleX: number
  bodyAngleY: number
  bodyAngleZ: number
  
  // Other
  cheek: number
  breath: number
}

/**
 * Partial model parameters for updates
 */
export type PartialLive2DParameters = Partial<Live2DModelParameters>

/**
 * Emotion definitions for facial expressions
 */
export enum Emotion {
  Neutral = 'neutral',
  Happy = 'happy',
  Sad = 'sad',
  Angry = 'angry',
  Surprised = 'surprised',
  Disgusted = 'disgusted',
  Fearful = 'fearful',
  Contempt = 'contempt',
  Excited = 'excited',
  Confused = 'confused',
  Bored = 'bored',
  Thoughtful = 'thoughtful',
  Amused = 'amused',
  Embarrassed = 'embarrassed',
}

/**
 * Emotion intensity levels
 */
export enum EmotionIntensity {
  Subtle = 0.3,
  Moderate = 0.6,
  Strong = 1.0,
}

/**
 * Emotion to parameter mapping
 * Defines how emotions translate to Live2D model parameters
 */
export type EmotionParameterMap = {
  [K in Emotion]: PartialLive2DParameters
}

/**
 * Animation easing functions
 */
export type EasingFunction = (t: number) => number

/**
 * Parameter animation configuration
 */
export interface ParameterAnimation {
  targetValue: number
  duration: number
  easing?: EasingFunction
  delay?: number
}

/**
 * Live2D motion information
 */
export interface Live2DMotionInfo {
  motionName: string
  motionIndex: number
  fileName: string
  duration?: number
  loop?: boolean
}

/**
 * Live2D model state
 */
export interface Live2DModelState {
  parameters: Live2DModelParameters
  currentEmotion: Emotion
  currentMotion?: Live2DMotionInfo
  isPlaying: boolean
  isSpeaking: boolean
}

/**
 * Model validation result
 */
export interface ModelValidationResult {
  isValid: boolean
  errors: string[]
  warnings: string[]
  availableParameters: string[]
  missingParameters: string[]
}

/**
 * Live2D SDK configuration
 */
export interface Live2DConfig {
  autoUpdate?: boolean
  autoInteract?: boolean
  enableEyeBlink?: boolean
  enableBreath?: boolean
  enableIdleMotion?: boolean
  motionFadeTime?: number
  expressionFadeTime?: number
}
