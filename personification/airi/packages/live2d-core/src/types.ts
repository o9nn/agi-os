export const CubismParameterIds = {
  ParamAngleX: 'ParamAngleX',
  ParamAngleY: 'ParamAngleY',
  ParamAngleZ: 'ParamAngleZ',
  ParamEyeLOpen: 'ParamEyeLOpen',
  ParamEyeROpen: 'ParamEyeROpen',
  ParamEyeLSmile: 'ParamEyeLSmile',
  ParamEyeRSmile: 'ParamEyeRSmile',
  ParamEyeBallX: 'ParamEyeBallX',
  ParamEyeBallY: 'ParamEyeBallY',
  ParamEyeBallForm: 'ParamEyeBallForm',
  ParamBrowLY: 'ParamBrowLY',
  ParamBrowRY: 'ParamBrowRY',
  ParamBrowLX: 'ParamBrowLX',
  ParamBrowRX: 'ParamBrowRX',
  ParamBrowLAngle: 'ParamBrowLAngle',
  ParamBrowRAngle: 'ParamBrowRAngle',
  ParamBrowLForm: 'ParamBrowLForm',
  ParamBrowRForm: 'ParamBrowRForm',
  ParamMouthOpenY: 'ParamMouthOpenY',
  ParamMouthForm: 'ParamMouthForm',
  ParamBodyAngleX: 'ParamBodyAngleX',
  ParamBodyAngleY: 'ParamBodyAngleY',
  ParamBodyAngleZ: 'ParamBodyAngleZ',
  ParamCheek: 'ParamCheek',
  ParamBreath: 'ParamBreath',
} as const
export type CubismParameterId = typeof CubismParameterIds[keyof typeof CubismParameterIds]
export interface Live2DModelParameters {
  angleX: number
  angleY: number
  angleZ: number
  leftEyeOpen: number
  rightEyeOpen: number
  leftEyeSmile: number
  rightEyeSmile: number
  eyeBallX: number
  eyeBallY: number
  leftEyebrowLR: number
  rightEyebrowLR: number
  leftEyebrowY: number
  rightEyebrowY: number
  leftEyebrowAngle: number
  rightEyebrowAngle: number
  leftEyebrowForm: number
  rightEyebrowForm: number
  mouthOpen: number
  mouthForm: number
  bodyAngleX: number
  bodyAngleY: number
  bodyAngleZ: number
  cheek: number
  breath: number
}
export type PartialLive2DParameters = Partial<Live2DModelParameters>
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
export enum EmotionIntensity {
  Subtle = 0.3,
  Moderate = 0.6,
  Strong = 1.0,
}
export type EmotionParameterMap = {
  [K in Emotion]: PartialLive2DParameters
}
export type EasingFunction = (t: number) => number
export interface ParameterAnimation {
  targetValue: number
  duration: number
  easing?: EasingFunction
  delay?: number
}
export interface Live2DMotionInfo {
  motionName: string
  motionIndex: number
  fileName: string
  duration?: number
  loop?: boolean
}
export interface Live2DModelState {
  parameters: Live2DModelParameters
  currentEmotion: Emotion
  currentMotion?: Live2DMotionInfo
  isPlaying: boolean
  isSpeaking: boolean
}
export interface ModelValidationResult {
  isValid: boolean
  errors: string[]
  warnings: string[]
  availableParameters: string[]
  missingParameters: string[]
}
export interface Live2DConfig {
  autoUpdate?: boolean
  autoInteract?: boolean
  enableEyeBlink?: boolean
  enableBreath?: boolean
  enableIdleMotion?: boolean
  motionFadeTime?: number
  expressionFadeTime?: number
}