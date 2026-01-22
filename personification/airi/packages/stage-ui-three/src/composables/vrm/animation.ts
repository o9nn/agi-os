import type { VRMAnimation } from '@pixiv/three-vrm-animation'
import type { VRMCore } from '@pixiv/three-vrm-core'
import type { AnimationClip } from 'three'
import type { Ref } from 'vue'
import { createVRMAnimationClip } from '@pixiv/three-vrm-animation'
import { Object3D, Vector3, VectorKeyframeTrack } from 'three'
import { randFloat } from 'three/src/math/MathUtils.js'
import { ref } from 'vue'
import { useVRMLoader } from './loader'
import { randomSaccadeInterval } from './utils/eye-motions'
export interface GLTFUserdata extends Record<string, any> {
vrmAnimations: VRMAnimation[]
}
export async function loadVRMAnimation(url: string) {
const loader = useVRMLoader()
const gltf = await loader.loadAsync(url)
const userData = gltf.userData as GLTFUserdata
if (!userData.vrmAnimations) {
console.warn('No VRM animations found in the .vrma file')
return
}
if (userData.vrmAnimations.length === 0) {
console.warn('No VRM animations found in the .vrma file')
return
}
return userData.vrmAnimations[0]
}
export async function clipFromVRMAnimation(vrm?: VRMCore, animation?: VRMAnimation) {
if (!vrm) {
console.warn('No VRM found')
return
}
if (!animation) {
return
}
return createVRMAnimationClip(animation, vrm)
}
export function reAnchorRootPositionTrack(clip: AnimationClip, _vrm: VRMCore) {
const hipNode = _vrm.humanoid?.getNormalizedBoneNode('hips')
if (!hipNode) {
console.warn('No hips node found in VRM model.')
return
}
hipNode.updateMatrixWorld(true)
const defaultHipPos = new Vector3()
hipNode.getWorldPosition(defaultHipPos)
const hipsTrack = clip.tracks.find(track =>
track instanceof VectorKeyframeTrack
&& track.name === `${hipNode.name}.position`,
)
if (!(hipsTrack instanceof VectorKeyframeTrack)) {
console.warn('No Hips.position track of type VectorKeyframeTrack found in animation.')
return
}
const animeHipPos = new Vector3(
hipsTrack.values[0],
hipsTrack.values[1],
hipsTrack.values[2],
)
const animeDelta = new Vector3().subVectors(animeHipPos, defaultHipPos)
clip.tracks.forEach((track) => {
if (track.name.endsWith('.position') && track instanceof VectorKeyframeTrack) {
for (let i = 0; i < track.values.length; i += 3) {
track.values[i] -= animeDelta.x
track.values[i + 1] -= animeDelta.y
track.values[i + 2] -= animeDelta.z
}
}
})
}
export function useBlink() {
const isBlinking = ref(false)
const blinkProgress = ref(0)
const timeSinceLastBlink = ref(0)
const BLINK_DURATION = 0.2
const MIN_BLINK_INTERVAL = 1
const MAX_BLINK_INTERVAL = 6
const nextBlinkTime = ref(Math.random() * (MAX_BLINK_INTERVAL - MIN_BLINK_INTERVAL) + MIN_BLINK_INTERVAL)
function update(vrm: VRMCore | undefined, delta: number) {
if (!vrm?.expressionManager)
return
timeSinceLastBlink.value += delta
if (!isBlinking.value && timeSinceLastBlink.value >= nextBlinkTime.value) {
isBlinking.value = true
blinkProgress.value = 0
}
if (isBlinking.value) {
blinkProgress.value += delta / BLINK_DURATION
const blinkValue = Math.sin(Math.PI * blinkProgress.value)
vrm.expressionManager.setValue('blink', blinkValue)
if (blinkProgress.value >= 1) {
isBlinking.value = false
timeSinceLastBlink.value = 0
vrm.expressionManager.setValue('blink', 0)
nextBlinkTime.value = Math.random() * (MAX_BLINK_INTERVAL - MIN_BLINK_INTERVAL) + MIN_BLINK_INTERVAL
}
}
}
return { update }
}
export function useIdleEyeSaccades() {
let nextSaccadeAfter = -1
const fixationTarget = new Vector3()
let timeSinceLastSaccade = 0
function updateFixationTarget(lookAtTarget: Ref<{ x: number, y: number, z: number }>) {
fixationTarget.set(
lookAtTarget.value.x + randFloat(-0.25, 0.25),
lookAtTarget.value.y + randFloat(-0.25, 0.25),
lookAtTarget.value.z,
)
}
function update(vrm: VRMCore | undefined, lookAtTarget: Ref<{ x: number, y: number, z: number }>, delta: number) {
if (!vrm?.expressionManager || !vrm.lookAt)
return
if (timeSinceLastSaccade >= nextSaccadeAfter) {
updateFixationTarget(lookAtTarget)
timeSinceLastSaccade = 0
nextSaccadeAfter = randomSaccadeInterval() / 1000
}
else if (!fixationTarget) {
updateFixationTarget(lookAtTarget)
}
if (!vrm.lookAt.target) {
vrm.lookAt.target = new Object3D() as unknown as Object3D
}
vrm.lookAt.target?.position.lerp(fixationTarget!, 1)
vrm.lookAt?.update(delta)
timeSinceLastSaccade += delta
}
function instantUpdate(vrm: VRMCore | undefined, lookAtTarget: { x: number, y: number, z: number }) {
fixationTarget.set(
lookAtTarget.x,
lookAtTarget.y,
lookAtTarget.z,
)
if (!vrm?.expressionManager || !vrm.lookAt)
return
if (!vrm.lookAt.target) {
vrm.lookAt.target = new Object3D() as unknown as Object3D
}
vrm.lookAt.target?.position.lerp(fixationTarget!, 1)
vrm.lookAt?.update(0.016)
}
return { update, instantUpdate }
}