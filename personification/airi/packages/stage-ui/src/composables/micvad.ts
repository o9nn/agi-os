import type { RealTimeVADOptions } from '@ricky0123/vad-web'
import type { MaybeRef } from 'vue'
import { merge } from '@moeru/std'
import { getDefaultRealTimeVADOptions, MicVAD } from '@ricky0123/vad-web'
import { usePermission } from '@vueuse/core'
import { tryOnMounted } from '@vueuse/shared'
import { onUnmounted, ref, toRef, unref, watch } from 'vue'
export function useMicVAD(deviceId: MaybeRef<ConstrainDOMString | undefined>, options: Partial<RealTimeVADOptions> & { auto?: boolean } = {}) {
const opts = merge<Omit<RealTimeVADOptions, 'stream'> & { auto?: boolean }, Partial<RealTimeVADOptions> & { auto?: boolean }>({
...getDefaultRealTimeVADOptions('v5'),
preSpeechPadMs: 30,
positiveSpeechThreshold: 0.5,
negativeSpeechThreshold: 0.5 - 0.15,
minSpeechMs: 30,
auto: true,
}, options)
const micVad = ref<MicVAD>()
const microphoneAccess = usePermission('microphone')
async function update() {
if (micVad.value) {
micVad.value.destroy()
micVad.value = undefined
console.warn('existing MicVAD destroyed')
}
if (!microphoneAccess.value)
return
const id = unref(deviceId)
if (!id)
return
const media = await navigator.mediaDevices.getUserMedia({ audio: { deviceId: id } })
micVad.value = await MicVAD.new({
...opts,
getStream: async () => {
return media
},
})
if (opts.auto)
micVad.value.start()
}
watch(microphoneAccess, update, { immediate: true })
watch(toRef(deviceId), update, { immediate: true })
tryOnMounted(update)
onUnmounted(() => {
if (micVad.value) {
micVad.value.destroy()
micVad.value = undefined
}
})
return {
destroy: () => {
if (micVad.value) {
micVad.value.destroy()
micVad.value = undefined
}
},
start: () => {
if (micVad.value) {
micVad.value.start()
}
},
}
}