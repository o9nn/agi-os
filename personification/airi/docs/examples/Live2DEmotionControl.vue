/**
 * Example: Live2D Emotion Control Component
 * 
 * This example demonstrates how to use the new Live2D utilities
 * to control emotions and facial expressions on a Live2D model.
 */

<script setup lang="ts">
import { ref, onMounted, onUnmounted } from 'vue'
import { useLive2DParameterController } from '@proj-airi/stage-ui/composables'
import { Emotion, EmotionIntensity } from '@proj-airi/live2d-core'

// Create the parameter controller
const controller = useLive2DParameterController({
  defaultDuration: 300,
  autoClamp: true,
})

// Current emotion state
const currentEmotion = ref<Emotion>(Emotion.Neutral)
const emotionIntensity = ref(EmotionIntensity.Strong)

// Available emotions for the UI
const availableEmotions = [
  { value: Emotion.Neutral, label: 'Neutral', icon: '😐' },
  { value: Emotion.Happy, label: 'Happy', icon: '😊' },
  { value: Emotion.Sad, label: 'Sad', icon: '😢' },
  { value: Emotion.Angry, label: 'Angry', icon: '😠' },
  { value: Emotion.Surprised, label: 'Surprised', icon: '😲' },
  { value: Emotion.Excited, label: 'Excited', icon: '🤩' },
  { value: Emotion.Confused, label: 'Confused', icon: '😕' },
  { value: Emotion.Bored, label: 'Bored', icon: '😴' },
  { value: Emotion.Thoughtful, label: 'Thoughtful', icon: '🤔' },
  { value: Emotion.Amused, label: 'Amused', icon: '😏' },
  { value: Emotion.Embarrassed, label: 'Embarrassed', icon: '😳' },
]

// Handle emotion change
function setEmotion(emotion: Emotion) {
  currentEmotion.value = emotion
  controller.setEmotion(emotion, emotionIntensity.value, 400)
}

// Handle intensity change
function setIntensity(intensity: number) {
  emotionIntensity.value = intensity
  if (currentEmotion.value !== Emotion.Neutral) {
    controller.setEmotion(currentEmotion.value, intensity, 300)
  }
}

// Blend two emotions (for advanced control)
function blendWithHappy() {
  if (currentEmotion.value !== Emotion.Happy) {
    controller.blendEmotions(currentEmotion.value, Emotion.Happy, 0.5, 500)
  }
}

// Manual parameter control examples
function wink() {
  controller.setParameter('rightEyeOpen', 0.0, 150)
  setTimeout(() => {
    controller.setParameter('rightEyeOpen', 1.0, 150)
  }, 300)
}

function smile() {
  controller.setParameters({
    leftEyeSmile: 1.0,
    rightEyeSmile: 1.0,
    mouthForm: 1.0,
  }, 300)
}

function resetFace() {
  controller.reset(300)
}

// Animation loop integration
let animationFrameId: number | null = null

function startAnimation() {
  const animate = () => {
    // Update controller and get current parameters
    const params = controller.update()
    
    // Here you would apply params to your Live2D model
    // Example:
    // for (const [key, value] of Object.entries(params)) {
    //   const cubismId = mapParameterToCubismId(key)
    //   model.internalModel.coreModel.setParameterValueById(cubismId, value)
    // }
    
    animationFrameId = requestAnimationFrame(animate)
  }
  
  animationFrameId = requestAnimationFrame(animate)
}

function stopAnimation() {
  if (animationFrameId !== null) {
    cancelAnimationFrame(animationFrameId)
    animationFrameId = null
  }
}

onMounted(() => {
  startAnimation()
})

// Cleanup
onUnmounted(() => {
  stopAnimation()
})
</script>

<template>
  <div class="live2d-emotion-control">
    <h2>Live2D Emotion Control</h2>
    
    <!-- Emotion Selection -->
    <div class="emotion-grid">
      <button
        v-for="emotion in availableEmotions"
        :key="emotion.value"
        :class="{ active: currentEmotion === emotion.value }"
        @click="setEmotion(emotion.value)"
      >
        <span class="icon">{{ emotion.icon }}</span>
        <span class="label">{{ emotion.label }}</span>
      </button>
    </div>
    
    <!-- Intensity Control -->
    <div class="intensity-control">
      <label>Emotion Intensity:</label>
      <input
        type="range"
        min="0"
        max="1"
        step="0.1"
        :value="emotionIntensity"
        @input="setIntensity(Number(($event.target as HTMLInputElement).value))"
      >
      <span>{{ emotionIntensity.toFixed(1) }}</span>
    </div>
    
    <!-- Quick Actions -->
    <div class="quick-actions">
      <h3>Quick Actions</h3>
      <button @click="wink">Wink</button>
      <button @click="smile">Smile</button>
      <button @click="blendWithHappy">Blend with Happy</button>
      <button @click="resetFace">Reset</button>
    </div>
    
    <!-- Status -->
    <div class="status">
      <p>Current Emotion: {{ currentEmotion }}</p>
      <p>Intensity: {{ emotionIntensity }}</p>
      <p>Animating: {{ controller.isAnimating.value ? 'Yes' : 'No' }}</p>
    </div>
  </div>
</template>

<style scoped>
.live2d-emotion-control {
  padding: 20px;
  font-family: sans-serif;
}

.emotion-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(120px, 1fr));
  gap: 10px;
  margin: 20px 0;
}

.emotion-grid button {
  display: flex;
  flex-direction: column;
  align-items: center;
  padding: 15px;
  border: 2px solid #ddd;
  border-radius: 8px;
  background: white;
  cursor: pointer;
  transition: all 0.2s;
}

.emotion-grid button:hover {
  background: #f0f0f0;
  transform: translateY(-2px);
}

.emotion-grid button.active {
  border-color: #4CAF50;
  background: #e8f5e9;
}

.emotion-grid .icon {
  font-size: 32px;
  margin-bottom: 8px;
}

.emotion-grid .label {
  font-size: 14px;
  color: #333;
}

.intensity-control {
  margin: 20px 0;
  display: flex;
  align-items: center;
  gap: 10px;
}

.intensity-control input {
  flex: 1;
  max-width: 300px;
}

.quick-actions {
  margin: 20px 0;
  padding: 15px;
  background: #f5f5f5;
  border-radius: 8px;
}

.quick-actions button {
  margin: 5px;
  padding: 8px 16px;
  border: 1px solid #ddd;
  border-radius: 4px;
  background: white;
  cursor: pointer;
}

.quick-actions button:hover {
  background: #e0e0e0;
}

.status {
  margin: 20px 0;
  padding: 15px;
  background: #e3f2fd;
  border-radius: 8px;
}

.status p {
  margin: 5px 0;
  font-size: 14px;
}
</style>
