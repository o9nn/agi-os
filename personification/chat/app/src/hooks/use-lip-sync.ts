import type { VRM } from '@pixiv/three-vrm'
import type { Profile } from 'wlipsync'
import { useFrame } from '@react-three/fiber'
import { useEffect } from 'react'
import useSWR from 'swr'
import { createWLipSyncNode } from 'wlipsync'
import profile from '~/assets/lip-sync/profile.json' with { type: 'json' }
import { useAudioContext } from '~/context/audio-context'
const lipSyncMap = {
A: 'aa',
E: 'ee',
I: 'ih',
O: 'oh',
U: 'ou',
}
export const useLipSync = (audioNode: AudioNode, vrm: VRM) => {
const audioContext = useAudioContext()
const { data: lipSyncNode } = useSWR('wlipsync/createWLipSyncNode', async () => createWLipSyncNode(audioContext, profile as Profile))
useEffect(() => {
if (lipSyncNode)
audioNode.connect(lipSyncNode)
return () => {
audioNode.disconnect()
}
}, [audioNode, lipSyncNode])
useFrame(() => {
if (lipSyncNode) {
for (const key of Object.keys(lipSyncNode.weights)) {
const weight = lipSyncNode.weights[key] * lipSyncNode.volume
vrm.expressionManager?.setValue(lipSyncMap[key as keyof typeof lipSyncMap], weight)
}
}
})
}