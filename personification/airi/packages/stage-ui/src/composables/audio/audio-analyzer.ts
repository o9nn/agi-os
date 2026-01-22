import { onUnmounted, ref } from 'vue'
export function useAudioAnalyzer() {
  const analyzer = ref<AnalyserNode>()
  const dataArray = ref<Uint8Array<ArrayBuffer>>()
  const animationFrame = ref<number>()
  const onAnalyzerUpdateHooks = ref<Array<(volumeLevel: number) => void | Promise<void>>>([])
  const volumeLevel = ref(0) 
  const error = ref<string>()
  const amplification = 3 
  function onAnalyzerUpdate(callback: (volumeLevel: number) => void | Promise<void>) {
    onAnalyzerUpdateHooks.value.push(callback)
    return () => {
      onAnalyzerUpdateHooks.value = onAnalyzerUpdateHooks.value.filter(cb => cb !== callback)
    }
  }
  function start() {
    if (animationFrame.value)
      return 
    const analyze = () => {
      if (!analyzer.value || !dataArray.value)
        return
      analyzer.value.getByteFrequencyData(dataArray.value)
      let sum = 0
      for (let i = 0; i < dataArray.value.length; i++) {
        sum += dataArray.value[i]! * dataArray.value[i]!
      }
      const rms = Math.sqrt(sum / dataArray.value.length)
      volumeLevel.value = Math.min(100, (rms / 255) * 100 * amplification) 
      for (const hook of onAnalyzerUpdateHooks.value) {
        hook(volumeLevel.value)
      }
      animationFrame.value = requestAnimationFrame(analyze)
    }
    analyze()
  }
  function startAnalyzer(audioContext: AudioContext) {
    if (!audioContext) {
      throw new Error('AudioContext is not initialized')
    }
    try {
      analyzer.value = audioContext.createAnalyser()
      analyzer.value.fftSize = 256
      analyzer.value.smoothingTimeConstant = 0.3
      const bufferLength = analyzer.value.frequencyBinCount
      dataArray.value = new Uint8Array(bufferLength) as Uint8Array<ArrayBuffer>
      start()
      return analyzer.value
    }
    catch (err) {
      console.error('Error setting up audio monitoring:', err)
      error.value = err instanceof Error ? err.message : String(err)
    }
  }
  function stopAnalyzer() {
    if (animationFrame.value) {
      cancelAnimationFrame(animationFrame.value)
      animationFrame.value = undefined
    }
    analyzer.value = undefined
    dataArray.value = undefined
  }
  onUnmounted(() => {
    stopAnalyzer()
  })
  return {
    volumeLevel,
    error,
    startAnalyzer,
    stopAnalyzer,
    onAnalyzerUpdate,
  }
}