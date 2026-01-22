import type { VmdObject } from 'babylon-mmd/esm/Loader/Parser/vmdObject'
import type { SkinnedMesh } from 'three'
import {
AnimationClip,
Euler,
Interpolant,
NumberKeyframeTrack,
Quaternion,
QuaternionKeyframeTrack,
Vector3,
VectorKeyframeTrack,
} from 'three'
class AnimationBuilder {
build(vmd: VmdObject, mesh: SkinnedMesh): AnimationClip {
const tracks = this.buildSkeletalAnimation(vmd, mesh).tracks
const tracks2 = this.buildMorphAnimation(vmd, mesh).tracks
for (let i = 0, il = tracks2.length; i < il; i++) {
tracks.push(tracks2[i])
}
return new AnimationClip('', -1, tracks)
}
buildCameraAnimation(vmd: VmdObject): AnimationClip {
const pushVector3 = (array: number[], vec: Vector3) => {
array.push(vec.x)
array.push(vec.y)
array.push(vec.z)
}
const pushQuaternion = (array: number[], q: Quaternion) => {
array.push(q.x)
array.push(q.y)
array.push(q.z)
array.push(q.w)
}
const pushInterpolation = (array: number[], interpolation: number[], index: number) => {
array.push(interpolation[index * 4 + 0] / 127)
array.push(interpolation[index * 4 + 1] / 127)
array.push(interpolation[index * 4 + 2] / 127)
array.push(interpolation[index * 4 + 3] / 127)
}
const cameras: VmdObject.CameraKeyFrame[] = []
for (let i = 0; i < vmd.cameraKeyFrames.length; i++) {
cameras.push(vmd.cameraKeyFrames.get(i))
}
cameras.sort((a, b) => a.frameNumber - b.frameNumber)
const times = []
const centers: number[] = []
const quaternions: number[] = []
const positions: number[] = []
const fovs = []
const cInterpolations: number[] = []
const qInterpolations: number[] = []
const pInterpolations: number[] = []
const fInterpolations: number[] = []
const quaternion = new Quaternion()
const euler = new Euler()
const position = new Vector3()
const center = new Vector3()
for (let i = 0, il = cameras.length; i < il; i++) {
const motion = cameras[i]
const time = motion.frameNumber / 30
const pos = motion.position
const rot = motion.rotation
const distance = motion.distance
const fov = motion.fov
const interpolation = Array.from(motion.interpolation)
times.push(time)
position.set(0, 0, -distance)
center.set(pos[0], pos[1], pos[2])
euler.set(-rot[0], -rot[1], -rot[2])
quaternion.setFromEuler(euler)
position.add(center)
position.applyQuaternion(quaternion)
pushVector3(centers, center)
pushQuaternion(quaternions, quaternion)
pushVector3(positions, position)
fovs.push(fov)
for (let j = 0; j < 3; j++) {
pushInterpolation(cInterpolations, interpolation, j)
}
pushInterpolation(qInterpolations, interpolation, 3)
for (let j = 0; j < 3; j++) {
pushInterpolation(pInterpolations, interpolation, 4)
}
pushInterpolation(fInterpolations, interpolation, 5)
}
const tracks = []
tracks.push(this._createTrack('target.position', VectorKeyframeTrack, times, centers, cInterpolations))
tracks.push(this._createTrack('.quaternion', QuaternionKeyframeTrack, times, quaternions, qInterpolations))
tracks.push(this._createTrack('.position', VectorKeyframeTrack, times, positions, pInterpolations))
tracks.push(this._createTrack('.fov', NumberKeyframeTrack, times, fovs, fInterpolations))
return new AnimationClip('', -1, tracks)
}
private _createTrack(node: string, TypedKeyframeTrack: typeof NumberKeyframeTrack | typeof QuaternionKeyframeTrack | typeof VectorKeyframeTrack, times: number[], values: number[], interpolations: number[]): InstanceType<typeof TypedKeyframeTrack> {
if (times.length > 2) {
times = times.slice()
values = values.slice()
interpolations = interpolations.slice()
const stride = values.length / times.length
const interpolateStride = interpolations.length / times.length
let index = 1
for (let aheadIndex = 2, endIndex = times.length; aheadIndex < endIndex; aheadIndex++) {
for (let i = 0; i < stride; i++) {
if (values[index * stride + i] !== values[(index - 1) * stride + i]
|| values[index * stride + i] !== values[aheadIndex * stride + i]) {
index++
break
}
}
if (aheadIndex > index) {
times[index] = times[aheadIndex]
for (let i = 0; i < stride; i++) {
values[index * stride + i] = values[aheadIndex * stride + i]
}
for (let i = 0; i < interpolateStride; i++) {
interpolations[index * interpolateStride + i] = interpolations[aheadIndex * interpolateStride + i]
}
}
}
times.length = index + 1
values.length = (index + 1) * stride
interpolations.length = (index + 1) * interpolateStride
}
const track = new TypedKeyframeTrack(node, times, values)
track.createInterpolant = function InterpolantFactoryMethodCubicBezier(result: number[]) {
return new CubicBezierInterpolation(this.times, this.values, this.getValueSize(), result, new Float32Array(interpolations))
}
return track
}
private buildMorphAnimation(vmd: VmdObject, mesh: SkinnedMesh): AnimationClip {
const tracks = []
const morphs: Record<string, VmdObject.MorphKeyFrame[]> = {}
const morphTargetDictionary = mesh.morphTargetDictionary!
for (let i = 0; i < vmd.morphKeyFrames.length; i++) {
const morph = vmd.morphKeyFrames.get(i)
const morphName = morph.morphName
if (morphTargetDictionary[morphName] == null)
continue
morphs[morphName] = morphs[morphName] ?? []
morphs[morphName].push(morph)
}
for (const [key, array] of Object.entries(morphs)) {
array.sort((a, b) => a.frameNumber - b.frameNumber)
const times = []
const values = []
for (let i = 0, il = array.length; i < il; i++) {
times.push(array[i].frameNumber / 30)
values.push(array[i].weight)
}
tracks.push(new NumberKeyframeTrack(`.morphTargetInfluences[${morphTargetDictionary[key]}]`, times, values))
}
return new AnimationClip('', -1, tracks)
}
private buildSkeletalAnimation(vmd: VmdObject, mesh: SkinnedMesh): AnimationClip {
const pushInterpolation = (array: number[], interpolation: number[], index: number) => {
array.push(interpolation[index + 0] / 127)
array.push(interpolation[index + 8] / 127)
array.push(interpolation[index + 4] / 127)
array.push(interpolation[index + 12] / 127)
}
const tracks = []
const motions: Record<string, VmdObject.BoneKeyFrame[]> = {}
const bones = mesh.skeleton.bones
const boneNameDictionary: Record<string, boolean> = {}
for (let i = 0, il = bones.length; i < il; i++) {
boneNameDictionary[bones[i].name] = true
}
for (let i = 0; i < vmd.boneKeyFrames.length; i++) {
const motion = vmd.boneKeyFrames.get(i)
const boneName = motion.boneName
if (boneNameDictionary[boneName] == null)
continue
motions[boneName] = motions[boneName] ?? []
motions[boneName].push(motion)
}
for (const [key, array] of Object.entries(motions)) {
array.sort((a, b) => a.frameNumber - b.frameNumber)
const times: number[] = []
const positions: number[] = []
const rotations: number[] = []
const pInterpolations: number[] = []
const rInterpolations: number[] = []
const basePosition = mesh.skeleton.getBoneByName(key)!.position.toArray()
for (let i = 0, il = array.length; i < il; i++) {
const time = array[i].frameNumber / 30
const position = array[i].position
const rotation = array[i].rotation
const interpolation = Array.from(array[i].interpolation)
times.push(time)
for (let j = 0; j < 3; j++) positions.push(basePosition[j] + position[j])
for (let j = 0; j < 4; j++) rotations.push(rotation[j])
for (let j = 0; j < 3; j++) pushInterpolation(pInterpolations, interpolation, j)
pushInterpolation(rInterpolations, interpolation, 3)
}
const targetName = `.bones[${key}]`
tracks.push(this._createTrack(`${targetName}.position`, VectorKeyframeTrack, times, positions, pInterpolations))
tracks.push(this._createTrack(`${targetName}.quaternion`, QuaternionKeyframeTrack, times, rotations, rInterpolations))
}
return new AnimationClip('', -1, tracks)
}
}
class CubicBezierInterpolation extends Interpolant {
readonly interpolationParams: ArrayLike<number>
declare parameterPositions: ArrayLike<number>
declare resultBuffer: number[]
declare sampleSize: number
declare sampleValues: ArrayLike<number>
constructor(
parameterPositions: ArrayLike<number>,
sampleValues: ArrayLike<number>,
sampleSize: number,
resultBuffer: number[],
params: ArrayLike<number>,
) {
super(parameterPositions, sampleValues, sampleSize, resultBuffer)
this.interpolationParams = params
}
_calculate(x1: number, x2: number, y1: number, y2: number, x: number): number {
let c = 0.5
let t = c
let s = 1.0 - t
const loop = 15
const eps = 1e-5
const math = Math
let sst3: number, stt3: number, ttt: number
for (let i = 0; i < loop; i++) {
sst3 = 3.0 * s * s * t
stt3 = 3.0 * s * t * t
ttt = t * t * t
const ft = (sst3 * x1) + (stt3 * x2) + (ttt) - x
if (math.abs(ft) < eps)
break
c /= 2.0
t += (ft < 0) ? c : -c
s = 1.0 - t
}
return (sst3! * y1) + (stt3! * y2) + ttt!
}
interpolate_(i1: number, t0: number, t: number, t1: number): number[] {
const result = this.resultBuffer
const values = this.sampleValues as number[]
const stride = this.valueSize
const params = this.interpolationParams
const offset1 = i1 * stride
const offset0 = offset1 - stride
const weight1 = ((t1 - t0) < 1 / 30 * 1.5) ? 0.0 : (t - t0) / (t1 - t0)
if (stride === 4) {
const x1 = params[i1 * 4 + 0]
const x2 = params[i1 * 4 + 1]
const y1 = params[i1 * 4 + 2]
const y2 = params[i1 * 4 + 3]
const ratio = this._calculate(x1, x2, y1, y2, weight1)
Quaternion.slerpFlat(result, 0, values, offset0, values, offset1, ratio)
}
else if (stride === 3) {
for (let i = 0; i < stride; ++i) {
const x1 = params[i1 * 12 + i * 4 + 0]
const x2 = params[i1 * 12 + i * 4 + 1]
const y1 = params[i1 * 12 + i * 4 + 2]
const y2 = params[i1 * 12 + i * 4 + 3]
const ratio = this._calculate(x1, x2, y1, y2, weight1)
result[i] = values[offset0 + i] * (1 - ratio) + values[offset1 + i] * ratio
}
}
else {
const x1 = params[i1 * 4 + 0]
const x2 = params[i1 * 4 + 1]
const y1 = params[i1 * 4 + 2]
const y2 = params[i1 * 4 + 3]
const ratio = this._calculate(x1, x2, y1, y2, weight1)
result[0] = values[offset0] * (1 - ratio) + values[offset1] * ratio
}
return result
}
}
export const buildAnimation = (vmd: VmdObject, mesh: SkinnedMesh) =>
new AnimationBuilder().build(vmd, mesh)
export const buildCameraAnimation = (vmd: VmdObject) =>
new AnimationBuilder().buildCameraAnimation(vmd)