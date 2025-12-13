# Live2D Cubism SDK Integration Guide

This document provides a comprehensive guide for integrating and using Live2D Cubism SDK functionality in AIRI.

## Overview

AIRI uses the `pixi-live2d-display` library as a wrapper around the Live2D Cubism SDK. The integration has been enhanced with the following new packages and utilities:

1. **@proj-airi/live2d-core** - Core utilities for Live2D integration
2. **Enhanced composables** - Reactive utilities for Vue.js integration
3. **Type-safe parameter management** - Comprehensive TypeScript definitions
4. **Emotion mapping** - Convert emotions to facial expressions
5. **Smooth animations** - Parameter interpolation with easing functions

## Architecture

```
┌─────────────────────────────────────────────┐
│         Application Layer                   │
│  (Vue Components, Stores, Composables)      │
└─────────────────┬───────────────────────────┘
                  │
┌─────────────────▼───────────────────────────┐
│      @proj-airi/live2d-core                 │
│  - Emotion Mapper                           │
│  - Parameter Animator                       │
│  - Model Utilities                          │
│  - Type Definitions                         │
└─────────────────┬───────────────────────────┘
                  │
┌─────────────────▼───────────────────────────┐
│      pixi-live2d-display                    │
│  (Wraps Live2D Cubism SDK)                  │
└─────────────────┬───────────────────────────┘
                  │
┌─────────────────▼───────────────────────────┐
│      Live2D Cubism Core                     │
│  (Native SDK)                               │
└─────────────────────────────────────────────┘
```

## New Packages and Files

### 1. @proj-airi/live2d-core Package

Located at: `packages/live2d-core/`

**Files:**
- `src/types.ts` - Type definitions for parameters and emotions
- `src/emotion-mapper.ts` - Emotion to parameter mapping
- `src/parameter-animator.ts` - Smooth parameter animations
- `src/model-utils.ts` - Utility functions for model operations
- `src/index.ts` - Main export file
- `README.md` - Package documentation
- `package.json` - Package configuration
- `tsconfig.json` - TypeScript configuration
- `tsdown.config.ts` - Build configuration

### 2. Enhanced Composables

Located at: `packages/stage-ui/src/composables/`

**New Files:**
- `use-live2d-parameter-controller.ts` - Reactive parameter controller

**Updated Files:**
- `index.ts` - Export new composables

### 3. Updated Store

Located at: `packages/stage-ui/src/stores/`

**Updated Files:**
- `live2d.ts` - Now uses types from @proj-airi/live2d-core

## Key Features

### 1. Emotion Mapping

The emotion mapper allows you to convert high-level emotions into Live2D model parameters:

```typescript
import { createEmotionMapper, Emotion } from '@proj-airi/live2d-core'

const mapper = createEmotionMapper()
const happyParams = mapper.getParametersForEmotion(Emotion.Happy)
```

**Supported Emotions:**
- Neutral, Happy, Sad, Angry
- Surprised, Disgusted, Fearful, Contempt
- Excited, Confused, Bored, Thoughtful
- Amused, Embarrassed

### 2. Parameter Animation

Smooth parameter transitions with multiple easing functions:

```typescript
import { createParameterAnimator, Easing } from '@proj-airi/live2d-core'

const animator = createParameterAnimator()
animator.animateParameter('mouthOpen', 0.5, 300, Easing.easeOutCubic)
```

**Available Easing Functions:**
- Linear
- Quad (In, Out, InOut)
- Cubic (In, Out, InOut)
- Quart (In, Out, InOut)
- Sine (In, Out, InOut)
- Expo (In, Out, InOut)
- Elastic (In, Out)
- Bounce (In, Out)

### 3. Model Validation

Validate model compatibility and check for required parameters:

```typescript
import { validateModel } from '@proj-airi/live2d-core'

const validation = validateModel(availableParams)
if (!validation.isValid) {
  console.error('Model validation failed:', validation.errors)
}
```

### 4. Type Safety

Comprehensive TypeScript definitions for all operations:

```typescript
import type { 
  Live2DModelParameters,
  PartialLive2DParameters,
  Emotion,
  EmotionIntensity,
  CubismParameterId,
} from '@proj-airi/live2d-core'
```

## Usage Examples

### Basic Emotion Control

```typescript
import { useLive2DParameterController } from '@proj-airi/stage-ui/composables'
import { Emotion, EmotionIntensity } from '@proj-airi/live2d-core'

const controller = useLive2DParameterController()

// Set emotion with animation
controller.setEmotion(Emotion.Happy, EmotionIntensity.Strong, 300)

// Blend two emotions
controller.blendEmotions(Emotion.Happy, Emotion.Excited, 0.5, 500)

// Reset to neutral
controller.reset()
```

### Advanced Parameter Control

```typescript
// Set individual parameter
controller.setParameter('mouthOpen', 0.5, 300)

// Set multiple parameters
controller.setParameters({
  leftEyeOpen: 0.8,
  rightEyeOpen: 0.8,
  mouthForm: 1.0,
}, 500)

// Immediate update (no animation)
controller.setParameterImmediate('cheek', 0.6)
```

### Integration with Animation Loop

```typescript
import { useLive2DParameterController } from '@proj-airi/stage-ui/composables'

const controller = useLive2DParameterController()

// In your animation loop
function animate() {
  const currentParams = controller.update()
  
  // Apply to Live2D model
  for (const [key, value] of Object.entries(currentParams)) {
    const cubismId = mapParameterToCubismId(key)
    coreModel.setParameterValueById(cubismId, value)
  }
  
  requestAnimationFrame(animate)
}
```

### Reactive Vue Integration

```vue
<script setup lang="ts">
import { ref } from 'vue'
import { useLive2DParameterController, useAutoUpdateLive2DParameters } from '@proj-airi/stage-ui/composables'
import { Emotion } from '@proj-airi/live2d-core'

const controller = useLive2DParameterController()
const modelParams = ref({})

// Auto-update parameters reactively
useAutoUpdateLive2DParameters(controller, modelParams)

// Trigger emotion change
function onHappy() {
  controller.setEmotion(Emotion.Happy)
}
</script>
```

## Migration Guide

### From Old Implementation

The new implementation is backward compatible. To migrate:

1. **Update imports:**
```typescript
// Old
import { defaultModelParameters } from '../stores/live2d'

// New
import { getDefaultParameters } from '@proj-airi/live2d-core'
const defaultParameters = getDefaultParameters()
```

2. **Use emotion mapper instead of hardcoded values:**
```typescript
// Old
const happyParams = {
  leftEyeSmile: 1.0,
  rightEyeSmile: 1.0,
  mouthOpen: 0.4,
}

// New
import { createEmotionMapper, Emotion } from '@proj-airi/live2d-core'
const mapper = createEmotionMapper()
const happyParams = mapper.getParametersForEmotion(Emotion.Happy)
```

3. **Use animator for smooth transitions:**
```typescript
// Old
coreModel.setParameterValueById('ParamMouthOpenY', value)

// New
const animator = createParameterAnimator()
animator.animateParameter('mouthOpen', value, 300)
// In animation loop:
const params = animator.update()
```

## TODOs Addressed

This implementation addresses the following TODOs from the codebase:

1. ✅ **Emotion Mapper** (Model.vue:222, animation.ts:28)
   - Implemented in `emotion-mapper.ts`
   - Provides dynamic parameter mapping for all facial expressions

2. ✅ **Stage Editor** (Model.vue:222, animation.ts:28)
   - Parameter utilities in `model-utils.ts`
   - Validation and filtering functions for parameter management

3. ✅ **Motion Management** (Model.vue:378)
   - Type definitions for motion info
   - Helper functions in `model-utils.ts`

4. ✅ **Eye Parameter Control** (Model.vue:222, animation.ts:28)
   - Comprehensive eye parameter types
   - Separate control for each eye with full parameter set

## Best Practices

1. **Use emotion mapping for high-level control:**
   - Prefer `setEmotion()` over manual parameter setting
   - Customize emotion mappings only when needed

2. **Validate models before use:**
   - Always validate models to check for missing parameters
   - Provide fallbacks for missing functionality

3. **Use animations for smooth transitions:**
   - Avoid jarring immediate changes
   - Choose appropriate easing functions for natural movement

4. **Clamp parameter values:**
   - Use `clampParameters()` to ensure valid ranges
   - Enable `autoClamp` in parameter controller

5. **Optimize performance:**
   - Stop animations when not needed
   - Use `setParameterImmediate()` for instant updates
   - Batch parameter updates when possible

## API Reference

See individual package READMEs for detailed API documentation:

- [@proj-airi/live2d-core README](../packages/live2d-core/README.md)
- [useLive2DParameterController source](../packages/stage-ui/src/composables/use-live2d-parameter-controller.ts)

## Troubleshooting

### Model parameters not updating
- Check that parameters are available in the model
- Use `validateModel()` to check compatibility
- Ensure animation loop is running

### Animations not smooth
- Increase animation duration
- Try different easing functions
- Check frame rate of animation loop

### Type errors
- Ensure `@proj-airi/live2d-core` is installed
- Run `pnpm install` in monorepo root
- Check TypeScript version compatibility

## Future Enhancements

Potential future improvements:

1. **Physics-based animation** - Spring physics for natural movement
2. **Lip sync integration** - Automatic mouth movement from audio
3. **Gaze tracking** - Eye tracking based on cursor/camera position
4. **Preset library** - Pre-built emotion and expression presets
5. **Animation sequences** - Chainable parameter animations
6. **Real-time debugging** - Visual parameter editor and preview

## Contributing

When adding new features to the Live2D integration:

1. Add types to `types.ts`
2. Implement functionality in appropriate module
3. Update READMEs
4. Add usage examples
5. Write tests (future)

## License

MIT © Moeru AI Project AIRI Team
