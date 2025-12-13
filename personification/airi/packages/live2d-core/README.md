# @proj-airi/live2d-core

Live2D Cubism SDK integration utilities and core functionality for AIRI.

## Features

- 🎭 **Emotion Mapping** - Convert emotions to realistic facial expressions
- 🎬 **Parameter Animation** - Smooth transitions with multiple easing functions
- ✅ **Model Validation** - Validate model compatibility and available parameters
- 🛠️ **Utilities** - Helper functions for parameter management and manipulation
- 📘 **Type Safety** - Comprehensive TypeScript type definitions

## Installation

This package is part of the AIRI monorepo and is typically used internally. To use it in your package:

```json
{
  "dependencies": {
    "@proj-airi/live2d-core": "workspace:^"
  }
}
```

## Usage

### Emotion Mapping

Map emotions to Live2D model parameters for realistic facial expressions:

```typescript
import { createEmotionMapper, Emotion, EmotionIntensity } from '@proj-airi/live2d-core'

const emotionMapper = createEmotionMapper()

// Get parameters for a specific emotion
const happyParams = emotionMapper.getParametersForEmotion(Emotion.Happy, EmotionIntensity.Strong)

// Blend two emotions together
const blendedParams = emotionMapper.blendEmotions(Emotion.Happy, Emotion.Excited, 0.5)

// Update custom emotion mapping
emotionMapper.updateEmotionMap(Emotion.Happy, {
  leftEyeSmile: 1.0,
  rightEyeSmile: 1.0,
  mouthOpen: 0.6,
})
```

### Parameter Animation

Animate model parameters with smooth transitions:

```typescript
import { createParameterAnimator, Easing } from '@proj-airi/live2d-core'

const animator = createParameterAnimator()

// Animate a single parameter
animator.animateParameter('mouthOpen', 0.5, 300, Easing.easeOutCubic)

// Animate multiple parameters
animator.animateParameters(
  {
    leftEyeOpen: 0.8,
    rightEyeOpen: 0.8,
    mouthOpen: 0.4,
  },
  500,
  Easing.easeInOutCubic
)

// In your animation loop
function animate() {
  const currentParams = animator.update()
  // Apply currentParams to your Live2D model
  requestAnimationFrame(animate)
}
```

### Model Validation

Validate model compatibility and check for required parameters:

```typescript
import { validateModel, getDefaultParameters, clampParameters } from '@proj-airi/live2d-core'

// Validate model parameters
const availableParams = model.getParameterIds()
const validation = validateModel(availableParams)

if (!validation.isValid) {
  console.error('Model validation failed:', validation.errors)
  console.warn('Warnings:', validation.warnings)
}

// Get default parameters
const defaultParams = getDefaultParameters()

// Clamp parameter values to valid ranges
const clampedParams = clampParameters({
  leftEyeOpen: 1.5, // Will be clamped to 1.0
  mouthOpen: -0.5, // Will be clamped to -1.0
})
```

### Parameter Utilities

Helper functions for working with parameters:

```typescript
import {
  mergeParameters,
  mapParameterToCubismId,
  filterAvailableParameters,
  interpolateParameters,
} from '@proj-airi/live2d-core'

// Merge parameter updates
const merged = mergeParameters(currentParams, updates)

// Map friendly names to Cubism IDs
const cubismId = mapParameterToCubismId('leftEyeOpen') // 'ParamEyeLOpen'

// Filter to only available parameters
const filtered = filterAvailableParameters(params, availableParamIds)

// Interpolate between two parameter sets
const interpolated = interpolateParameters(params1, params2, 0.5)
```

## API Reference

### Types

- `Live2DModelParameters` - Complete set of model parameters
- `PartialLive2DParameters` - Partial parameter updates
- `Emotion` - Available emotion types
- `EmotionIntensity` - Emotion intensity levels
- `CubismParameterId` - Standard Cubism parameter IDs
- `ModelValidationResult` - Model validation results

### Classes

- `EmotionMapper` - Emotion to parameter conversion
- `ParameterAnimator` - Parameter animation management

### Functions

#### Emotion Mapping
- `createEmotionMapper(customMap?)` - Create emotion mapper instance
- `getParametersForEmotion(emotion, intensity)` - Get parameters for emotion
- `blendEmotions(emotion1, emotion2, blendFactor)` - Blend two emotions

#### Animation
- `createParameterAnimator()` - Create animator instance
- Various easing functions in `Easing` object

#### Utilities
- `validateModel(availableParams)` - Validate model parameters
- `getDefaultParameters()` - Get default parameter values
- `clampParameters(params)` - Clamp to valid ranges
- `mergeParameters(current, updates)` - Merge parameters
- `mapParameterToCubismId(paramName)` - Map to Cubism ID
- `filterAvailableParameters(params, available)` - Filter parameters
- `interpolateParameters(params1, params2, t)` - Interpolate parameters

## Integration with Existing Code

This package addresses several TODOs in the existing codebase:

1. **Emotion Mapper**: Provides dynamic parameter mapping for facial expressions
2. **Parameter Animation**: Smooth transitions with various easing functions
3. **Model Validation**: Ensures model compatibility and parameter availability
4. **Type Safety**: Comprehensive TypeScript definitions for all operations

### Upgrading Existing Code

The existing `useLive2d` store can be enhanced with these utilities:

```typescript
import { createEmotionMapper, createParameterAnimator, Emotion } from '@proj-airi/live2d-core'

const emotionMapper = createEmotionMapper()
const animator = createParameterAnimator()

// Set emotion
function setEmotion(emotion: Emotion, intensity: number = 1.0) {
  const params = emotionMapper.getParametersForEmotion(emotion, intensity)
  animator.animateParameters(params, 300)
}

// In animation loop
function updateModel() {
  const currentParams = animator.update()
  // Apply to Live2D model
  for (const [key, value] of Object.entries(currentParams)) {
    const cubismId = mapParameterToCubismId(key)
    coreModel.setParameterValueById(cubismId, value)
  }
}
```

## Development

```bash
# Build the package
pnpm build

# Watch for changes
pnpm dev

# Type check
pnpm typecheck
```

## License

MIT © Moeru AI Project AIRI Team
