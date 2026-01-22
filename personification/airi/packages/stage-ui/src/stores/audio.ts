import { useDevicesList, useUserMedia } from '@vueuse/core'
import { defineStore } from 'pinia'
import { computed, nextTick, onUnmounted, ref, shallowRef, watch } from 'vue'
function calculateVolumeWithLinearNormalize(analyser: AnalyserNode) {
  const dataBuffer = new Uint8Array(analyser.frequencyBinCount)
  analyser.getByteFrequencyData(dataBuffer)
  const volumeVector: Array<number> = []
  for (let i = 0; i < 700; i += 80)
    volumeVector.push(dataBuffer[i])
  const volumeSum = dataBuffer
    .map(v => v ** 1.2)
    .map(v => v * 1.2)
    .reduce((acc, cur) => acc + cur, 0)
  return (volumeSum / dataBuffer.length / 100)
}
function calculateVolumeWithMinMaxNormalize(analyser: AnalyserNode) {
  const dataBuffer = new Uint8Array(analyser.frequencyBinCount)
  analyser.getByteFrequencyData(dataBuffer)
  const volumeVector: Array<number> = []
  for (let i = 0; i < 700; i += 80)
    volumeVector.push(dataBuffer[i])
  const amplifiedVolumeVector = dataBuffer.map(v => v ** 1.5)
  const min = Math.min(...amplifiedVolumeVector)
  const max = Math.max(...amplifiedVolumeVector)
  const range = max - min
  let normalizedVolumeVector
  if (range === 0) {
    normalizedVolumeVector = amplifiedVolumeVector.map(() => 0) 
  }
  else {
    normalizedVolumeVector = amplifiedVolumeVector.map(v => (v - min) / range)
  }
  const volumeSum = normalizedVolumeVector.reduce((acc, cur) => acc + cur, 0)
  return volumeSum / dataBuffer.length
}
function calculateVolume(analyser: AnalyserNode, mode: 'linear' | 'minmax' = 'linear') {
  switch (mode) {
    case 'linear':
      return calculateVolumeWithLinearNormalize(analyser)
    case 'minmax':
      return calculateVolumeWithMinMaxNormalize(analyser)
  }
}
export const useAudioContext = defineStore('audio-context', () => {
  const audioContext = shallowRef<AudioContext>(new AudioContext())
  onUnmounted(async () => {
    if (audioContext)
      await audioContext.value.suspend()
  })
  return {
    audioContext,
    calculateVolume,
  }
})
export function useAudioDevice(requestPermission: boolean = false) {
  const devices = useDevicesList({ constraints: { audio: true }, requestPermissions: requestPermission })
  const audioInputs = computed(() => devices.audioInputs.value)
  const selectedAudioInput = ref<string>(devices.audioInputs.value.find(device => device.deviceId === 'default')?.deviceId || '')
  const deviceConstraints = computed<MediaStreamConstraints>(() => ({ audio: { deviceId: { exact: selectedAudioInput.value }, autoGainControl: true, echoCancellation: true, noiseSuppression: true } }))
  const { stream, stop: stopStream, start: startStream } = useUserMedia({ constraints: deviceConstraints, enabled: false, autoSwitch: true })
  watch(audioInputs, () => {
    if (!selectedAudioInput.value && audioInputs.value.length > 0) {
      selectedAudioInput.value = audioInputs.value.find(input => input.deviceId === 'default')?.deviceId || audioInputs.value[0].deviceId
    }
  })
  function askPermission() {
    devices.ensurePermissions()
      .then(() => nextTick())
      .then(() => {
        if (audioInputs.value.length > 0 && !selectedAudioInput.value) {
          selectedAudioInput.value = audioInputs.value.find(input => input.deviceId === 'default')?.deviceId || audioInputs.value[0].deviceId
        }
      })
      .catch((error) => {
        console.error('Error ensuring permissions:', error)
      })
  }
  return {
    audioInputs,
    selectedAudioInput,
    stream,
    deviceConstraints,
    askPermission,
    startStream,
    stopStream,
  }
}
export const useSpeakingStore = defineStore('character-speaking', () => {
  const nowSpeakingAvatarBorderOpacityMin = 30
  const nowSpeakingAvatarBorderOpacityMax = 100
  const mouthOpenSize = ref(0)
  const nowSpeaking = ref(false)
  const nowSpeakingAvatarBorderOpacity = computed<number>(() => {
    if (!nowSpeaking.value)
      return nowSpeakingAvatarBorderOpacityMin
    return ((nowSpeakingAvatarBorderOpacityMin
      + (nowSpeakingAvatarBorderOpacityMax - nowSpeakingAvatarBorderOpacityMin) * mouthOpenSize.value) / 100)
  })
  return {
    mouthOpenSize,
    nowSpeaking,
    nowSpeakingAvatarBorderOpacity,
  }
})