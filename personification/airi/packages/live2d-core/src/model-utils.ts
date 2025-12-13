/**
 * Live2D Model Utilities
 * 
 * Provides utility functions for model validation, parameter management,
 * and common operations.
 */

import type { CubismParameterId, Live2DModelParameters, ModelValidationResult, PartialLive2DParameters } from './types'
import { CubismParameterIds } from './types'

/**
 * Validate a Live2D model's available parameters
 */
export function validateModel(availableParams: string[]): ModelValidationResult {
  const errors: string[] = []
  const warnings: string[] = []
  const missingParameters: string[] = []
  
  // Check for essential parameters
  const essentialParams = [
    CubismParameterIds.ParamAngleX,
    CubismParameterIds.ParamAngleY,
    CubismParameterIds.ParamAngleZ,
  ]
  
  for (const param of essentialParams) {
    if (!availableParams.includes(param)) {
      errors.push(`Missing essential parameter: ${param}`)
      missingParameters.push(param)
    }
  }
  
  // Check for recommended parameters
  const recommendedParams = [
    CubismParameterIds.ParamEyeLOpen,
    CubismParameterIds.ParamEyeROpen,
    CubismParameterIds.ParamMouthOpenY,
  ]
  
  for (const param of recommendedParams) {
    if (!availableParams.includes(param)) {
      warnings.push(`Missing recommended parameter: ${param}`)
      missingParameters.push(param)
    }
  }
  
  const isValid = errors.length === 0
  
  return {
    isValid,
    errors,
    warnings,
    availableParameters: availableParams,
    missingParameters,
  }
}

/**
 * Get default parameter values
 */
export function getDefaultParameters(): Live2DModelParameters {
  return {
    angleX: 0,
    angleY: 0,
    angleZ: 0,
    leftEyeOpen: 1,
    rightEyeOpen: 1,
    leftEyeSmile: 0,
    rightEyeSmile: 0,
    eyeBallX: 0,
    eyeBallY: 0,
    leftEyebrowLR: 0,
    rightEyebrowLR: 0,
    leftEyebrowY: 0,
    rightEyebrowY: 0,
    leftEyebrowAngle: 0,
    rightEyebrowAngle: 0,
    leftEyebrowForm: 0,
    rightEyebrowForm: 0,
    mouthOpen: 0,
    mouthForm: 0,
    bodyAngleX: 0,
    bodyAngleY: 0,
    bodyAngleZ: 0,
    cheek: 0,
    breath: 0,
  }
}

/**
 * Merge partial parameters with current parameters
 */
export function mergeParameters(
  current: Live2DModelParameters,
  updates: PartialLive2DParameters,
): Live2DModelParameters {
  return { ...current, ...updates }
}

/**
 * Parameter categories for range validation
 */
const EYE_OPEN_PARAMETERS = new Set([
  'leftEyeOpen',
  'rightEyeOpen',
])

/**
 * Clamp parameter values to valid ranges
 */
export function clampParameters(params: PartialLive2DParameters): PartialLive2DParameters {
  const clamped: PartialLive2DParameters = {}
  
  for (const [key, value] of Object.entries(params)) {
    if (typeof value === 'number') {
      const paramKey = key as keyof Live2DModelParameters
      // Eye open values should be [0, 1], other parameters are typically [-1, 1]
      if (EYE_OPEN_PARAMETERS.has(paramKey)) {
        clamped[paramKey] = Math.max(0, Math.min(1, value))
      } else {
        clamped[paramKey] = Math.max(-1, Math.min(1, value))
      }
    }
  }
  
  return clamped
}

/**
 * Map parameter name to Cubism parameter ID
 */
export function mapParameterToCubismId(paramName: keyof Live2DModelParameters): CubismParameterId {
  const mapping: Record<keyof Live2DModelParameters, CubismParameterId> = {
    angleX: CubismParameterIds.ParamAngleX,
    angleY: CubismParameterIds.ParamAngleY,
    angleZ: CubismParameterIds.ParamAngleZ,
    leftEyeOpen: CubismParameterIds.ParamEyeLOpen,
    rightEyeOpen: CubismParameterIds.ParamEyeROpen,
    leftEyeSmile: CubismParameterIds.ParamEyeLSmile,
    rightEyeSmile: CubismParameterIds.ParamEyeRSmile,
    eyeBallX: CubismParameterIds.ParamEyeBallX,
    eyeBallY: CubismParameterIds.ParamEyeBallY,
    leftEyebrowLR: CubismParameterIds.ParamBrowLX,
    rightEyebrowLR: CubismParameterIds.ParamBrowRX,
    leftEyebrowY: CubismParameterIds.ParamBrowLY,
    rightEyebrowY: CubismParameterIds.ParamBrowRY,
    leftEyebrowAngle: CubismParameterIds.ParamBrowLAngle,
    rightEyebrowAngle: CubismParameterIds.ParamBrowRAngle,
    leftEyebrowForm: CubismParameterIds.ParamBrowLForm,
    rightEyebrowForm: CubismParameterIds.ParamBrowRForm,
    mouthOpen: CubismParameterIds.ParamMouthOpenY,
    mouthForm: CubismParameterIds.ParamMouthForm,
    bodyAngleX: CubismParameterIds.ParamBodyAngleX,
    bodyAngleY: CubismParameterIds.ParamBodyAngleY,
    bodyAngleZ: CubismParameterIds.ParamBodyAngleZ,
    cheek: CubismParameterIds.ParamCheek,
    breath: CubismParameterIds.ParamBreath,
  }
  
  return mapping[paramName]
}

/**
 * Check if a parameter exists in the model
 */
export function hasParameter(availableParams: string[], paramId: CubismParameterId): boolean {
  return availableParams.includes(paramId)
}

/**
 * Filter parameters to only include those available in the model
 */
export function filterAvailableParameters(
  params: PartialLive2DParameters,
  availableParams: string[],
): PartialLive2DParameters {
  const filtered: PartialLive2DParameters = {}
  
  for (const [key, value] of Object.entries(params)) {
    const cubismId = mapParameterToCubismId(key as keyof Live2DModelParameters)
    if (hasParameter(availableParams, cubismId)) {
      filtered[key as keyof Live2DModelParameters] = value
    }
  }
  
  return filtered
}

/**
 * Calculate distance between two parameter sets (for similarity measurement)
 */
export function calculateParameterDistance(
  params1: PartialLive2DParameters,
  params2: PartialLive2DParameters,
): number {
  const allKeys = new Set([...Object.keys(params1), ...Object.keys(params2)])
  let sumSquares = 0
  
  for (const key of allKeys) {
    const k = key as keyof Live2DModelParameters
    const val1 = params1[k] ?? 0
    const val2 = params2[k] ?? 0
    sumSquares += (val1 - val2) ** 2
  }
  
  return Math.sqrt(sumSquares)
}

/**
 * Interpolate between two parameter sets
 */
export function interpolateParameters(
  params1: PartialLive2DParameters,
  params2: PartialLive2DParameters,
  t: number,
): PartialLive2DParameters {
  const result: PartialLive2DParameters = {}
  const allKeys = new Set([...Object.keys(params1), ...Object.keys(params2)])
  
  for (const key of allKeys) {
    const k = key as keyof Live2DModelParameters
    const val1 = params1[k] ?? 0
    const val2 = params2[k] ?? 0
    result[k] = val1 + (val2 - val1) * t
  }
  
  return result
}

/**
 * Create a parameter snapshot for state saving
 */
export function createParameterSnapshot(params: Live2DModelParameters): Live2DModelParameters {
  return { ...params }
}

/**
 * Compare two parameter sets for equality
 */
export function areParametersEqual(
  params1: PartialLive2DParameters,
  params2: PartialLive2DParameters,
  tolerance: number = 0.001,
): boolean {
  const allKeys = new Set([...Object.keys(params1), ...Object.keys(params2)])
  
  for (const key of allKeys) {
    const k = key as keyof Live2DModelParameters
    const val1 = params1[k] ?? 0
    const val2 = params2[k] ?? 0
    
    if (Math.abs(val1 - val2) > tolerance) {
      return false
    }
  }
  
  return true
}
