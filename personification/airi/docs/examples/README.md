# Live2D Integration Examples

This directory contains example components demonstrating the new Live2D integration features.

## Examples

### Live2DEmotionControl.vue

A complete example showing how to use the emotion mapping and parameter animation features.

**Features Demonstrated:**
- Emotion selection with predefined emotions
- Intensity control for scaling emotion effects
- Emotion blending between two states
- Manual parameter control (wink, smile)
- Integration with animation loop
- Reactive state management

**Usage:**
```vue
<script setup>
import Live2DEmotionControl from './Live2DEmotionControl.vue'
</script>

<template>
  <Live2DEmotionControl />
</template>
```

## Running Examples

To use these examples in your project:

1. Ensure you have the Live2D integration packages installed:
   ```bash
   pnpm install
   ```

2. Import the example component in your application:
   ```typescript
   import Live2DEmotionControl from '@/docs/examples/Live2DEmotionControl.vue'
   ```

3. The examples require a Live2D model to be loaded. Integrate with your existing Live2D setup.

## Integration with Actual Models

To connect these examples to a real Live2D model:

1. Get access to the model's internal core:
   ```typescript
   const coreModel = model.internalModel.coreModel
   ```

2. Apply parameters in the animation loop:
   ```typescript
   import { mapParameterToCubismId } from '@proj-airi/live2d-core'
   
   const params = controller.update()
   for (const [key, value] of Object.entries(params)) {
     const cubismId = mapParameterToCubismId(key)
     if (coreModel.hasParameter(cubismId)) {
       coreModel.setParameterValueById(cubismId, value)
     }
   }
   ```

## Creating Custom Examples

To create your own examples:

1. Import the necessary utilities:
   ```typescript
   import { useLive2DParameterController } from '@proj-airi/stage-ui/composables'
   import { Emotion, EmotionIntensity } from '@proj-airi/live2d-core'
   ```

2. Create a parameter controller:
   ```typescript
   const controller = useLive2DParameterController({
     defaultDuration: 300,
     autoClamp: true,
   })
   ```

3. Use the controller methods to manipulate the model:
   ```typescript
   controller.setEmotion(Emotion.Happy, EmotionIntensity.Strong)
   controller.setParameter('mouthOpen', 0.5, 300)
   ```

See the [Integration Guide](../LIVE2D_INTEGRATION.md) for detailed documentation.

## Contributing

Feel free to add more examples demonstrating:
- Different emotion combinations
- Custom parameter animations
- Integration with speech synthesis
- Reactive emotion changes based on events
- Complex animation sequences

Submit your examples via pull request!
