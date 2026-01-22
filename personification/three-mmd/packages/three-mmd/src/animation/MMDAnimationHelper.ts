import type { PmxBoneInfo } from '@noname0310/mmd-parser'
import type {
  AnimationClip,
  Audio,
  Bone,
  Camera,
  PerspectiveCamera,
  PropertyMixer,
  SkinnedMesh,
} from 'three'
import {
  AnimationMixer,
  Object3D,
  Quaternion,
  Vector3,
} from 'three'
import { CCDIKSolver } from 'three/addons/animation/CCDIKSolver.js'
import type { MMDPhysicsParameter } from './mmd-physics'
import { AudioManager } from './mmd-animation-helper/audio-manager'
import { GrantSolver } from './mmd-animation-helper/grant-solver'
import { MMDPhysics } from './mmd-physics'
const _quaternions: Quaternion[] = []
let _quaternionIndex = 0
const getQuaternion = (): Quaternion => {
  if (_quaternionIndex >= _quaternions.length)
    _quaternions.push(new Quaternion())
  return _quaternions[_quaternionIndex++]
}
const _grantResultMap = new Map()
const updateOne = (mesh: SkinnedMesh, boneIndex: number, ikSolver: CCDIKSolver | null, grantSolver: GrantSolver | null) => {
  const bones = mesh.skeleton.bones
  const bonesData = mesh.geometry.userData.MMD.bones
  const boneData = bonesData[boneIndex]
  const bone = bones[boneIndex]
  if (_grantResultMap.has(boneIndex))
    return
  const quaternion = getQuaternion()
  _grantResultMap.set(boneIndex, quaternion.copy(bone.quaternion))
  if (grantSolver && boneData.grant
    && !boneData.grant.isLocal && boneData.grant.affectRotation) {
    const parentIndex = boneData.grant.parentIndex
    const ratio = boneData.grant.ratio
    if (!_grantResultMap.has(parentIndex)) {
      updateOne(mesh, parentIndex, ikSolver, grantSolver)
    }
    grantSolver.addGrantRotation(bone, _grantResultMap.get(parentIndex), ratio)
  }
  if (ikSolver && boneData.ik) {
    mesh.updateMatrixWorld(true)
    ikSolver.updateOne(boneData.ik)
    const links = boneData.ik.links
    for (let i = 0, il = links.length; i < il; i++) {
      const link = links[i]
      if (link.enabled === false)
        continue
      const linkIndex = link.index
      if (_grantResultMap.has(linkIndex)) {
        _grantResultMap.set(linkIndex, _grantResultMap.get(linkIndex).copy(bones[linkIndex].quaternion))
      }
    }
  }
  quaternion.copy(bone.quaternion)
}
export interface MMDAnimationHelperAddParameter extends MMDPhysicsParameter {
  animation?: AnimationClip | AnimationClip[]
  animationWarmup?: boolean
  delayTime?: number
  physics?: boolean
  warmup?: number
}
export interface MMDAnimationHelperMixer {
  backupBones?: Float32Array
  duration?: number
  grantSolver?: GrantSolver
  ikSolver?: CCDIKSolver
  looped?: boolean
  mixer?: AnimationMixer & {
    _actions?: {
      _clip: AnimationClip
    }[]
    _bindings?: PropertyMixer[]
  }
  physics?: MMDPhysics
  sortedBonesData?: PmxBoneInfo[]
}
export interface MMDAnimationHelperParameter {
  afterglow?: number
  pmxAnimation?: boolean
  resetPhysicsOnLoop?: boolean
  sync?: boolean
}
export interface MMDAnimationHelperPoseParameter {
  grant?: boolean
  ik?: boolean
  resetPose?: boolean
}
export class MMDAnimationHelper {
  meshes: SkinnedMesh[]
  audio: Audio | null
  audioManager: AudioManager | null
  camera: Camera | null
  cameraTarget: Object3D
  configuration: {
    afterglow: number
    pmxAnimation: boolean
    resetPhysicsOnLoop: boolean
    sync: boolean
  }
  enabled: {
    animation: boolean
    cameraAnimation: boolean
    grant: boolean
    ik: boolean
    physics: boolean
  }
  masterPhysics: MMDPhysics | null
  objects: WeakMap<AnimationClip | AudioManager | Camera | SkinnedMesh, MMDAnimationHelperMixer>
  onBeforePhysics: (mesh: SkinnedMesh) => void
  sharedPhysics: boolean
  constructor(params: MMDAnimationHelperParameter = {}) {
    this.meshes = []
    this.camera = null
    this.cameraTarget = new Object3D()
    this.cameraTarget.name = 'target'
    this.audio = null
    this.audioManager = null
    this.objects = new WeakMap()
    this.configuration = {
      afterglow: params.afterglow !== undefined ? params.afterglow : 0.0,
      pmxAnimation: params.pmxAnimation !== undefined ? params.pmxAnimation : false,
      resetPhysicsOnLoop: params.resetPhysicsOnLoop !== undefined ? params.resetPhysicsOnLoop : true,
      sync: params.sync !== undefined ? params.sync : true,
    }
    this.enabled = {
      animation: true,
      cameraAnimation: true,
      grant: true,
      ik: true,
      physics: true,
    }
    this.onBeforePhysics = () => { }
    this.sharedPhysics = false
    this.masterPhysics = null
  }
  private _addMesh(mesh: SkinnedMesh, params: MMDAnimationHelperAddParameter) {
    if (this.meshes.includes(mesh)) {
      throw new Error('MMDAnimationHelper._addMesh: '
        + `SkinnedMesh '${mesh.name}' has already been added.`)
    }
    this.meshes.push(mesh)
    this.objects.set(mesh, { looped: false })
    this._setupMeshAnimation(mesh, params.animation!)
    if (params.physics !== false) {
      this._setupMeshPhysics(mesh, params)
    }
    return this
  }
  private _animateCamera(camera: Camera, delta: number) {
    const mixer = this.objects.get(camera)!.mixer
    if (mixer && this.enabled.cameraAnimation) {
      mixer.update(delta);
      (camera as PerspectiveCamera).updateProjectionMatrix()
      camera.up.set(0, 1, 0)
      camera.up.applyQuaternion(camera.quaternion)
      camera.lookAt(this.cameraTarget.position)
    }
  }
  private _animateMesh(mesh: SkinnedMesh, delta: number) {
    const objects = this.objects.get(mesh)!
    const mixer = objects.mixer
    const ikSolver = objects.ikSolver
    const grantSolver = objects.grantSolver
    const physics = objects.physics
    const looped = objects.looped
    if (mixer && this.enabled.animation) {
      this._restoreBones(mesh)
      mixer.update(delta)
      this._saveBones(mesh)
      if (this.configuration.pmxAnimation
        && mesh.geometry.userData.MMD && mesh.geometry.userData.MMD.format === 'pmx') {
        if (!objects.sortedBonesData)
          objects.sortedBonesData = this._sortBoneDataArray(mesh.geometry.userData.MMD.bones.slice())
        this._animatePMXMesh(
          mesh,
          objects.sortedBonesData,
          ikSolver && this.enabled.ik ? ikSolver : null,
          grantSolver && this.enabled.grant ? grantSolver : null,
        )
      }
      else {
        if (ikSolver && this.enabled.ik) {
          mesh.updateMatrixWorld(true)
          ikSolver.update()
        }
        if (grantSolver && this.enabled.grant) {
          grantSolver.update()
        }
      }
    }
    if (looped === true && this.enabled.physics) {
      if (physics && this.configuration.resetPhysicsOnLoop)
        physics.reset()
      objects.looped = false
    }
    if (physics && this.enabled.physics && !this.sharedPhysics) {
      this.onBeforePhysics(mesh)
      physics.update(delta)
    }
  }
  private _animatePMXMesh(mesh: SkinnedMesh, sortedBonesData: PmxBoneInfo[], ikSolver: CCDIKSolver | null, grantSolver: GrantSolver | null) {
    _quaternionIndex = 0
    _grantResultMap.clear()
    for (let i = 0, il = sortedBonesData.length; i < il; i++) {
      updateOne(mesh, sortedBonesData[i].index, ikSolver, grantSolver)
    }
    mesh.updateMatrixWorld(true)
    return this
  }
  private _clearAudio(audio: Audio) {
    if (audio !== this.audio) {
      throw new Error('MMDAnimationHelper._clearAudio: '
        + `Audio '${audio.name}' has not been set yet.`)
    }
    this.objects.delete(this.audioManager!)
    this.audio = null
    this.audioManager = null
    return this
  }
  private _clearCamera(camera: Camera) {
    if (camera !== this.camera) {
      throw new Error('MMDAnimationHelper._clearCamera: '
        + `Camera '${camera.name}' has not been set yet.`)
    }
    this.camera.remove(this.cameraTarget)
    this.objects.delete(this.camera)
    this.camera = null
    return this
  }
  private _createCCDIKSolver(mesh: SkinnedMesh) {
    if (CCDIKSolver === undefined) {
      throw new Error('MMDAnimationHelper: Import CCDIKSolver.')
    }
    return new CCDIKSolver(mesh, mesh.geometry.userData.MMD.iks)
  }
  private _createMMDPhysics(mesh: SkinnedMesh, params: MMDAnimationHelperAddParameter) {
    if (MMDPhysics === undefined) {
      throw new Error('MMDPhysics: Import MMDPhysics.')
    }
    return new MMDPhysics(
      mesh,
      mesh.geometry.userData.MMD.rigidBodies,
      mesh.geometry.userData.MMD.constraints,
      params,
    )
  }
  private _getMasterPhysics(): MMDPhysics | null {
    if (this.masterPhysics !== null)
      return this.masterPhysics
    for (let i = 0, il = this.meshes.length; i < il; i++) {
      const physics = this.meshes[i].physics
      if (physics !== undefined && physics !== null) {
        this.masterPhysics = physics
        return this.masterPhysics!
      }
    }
    return null
  }
  private _optimizeIK(mesh: SkinnedMesh, physicsEnabled: boolean) {
    const iks = mesh.geometry.userData.MMD.iks
    const bones = mesh.geometry.userData.MMD.bones
    for (let i = 0, il = iks.length; i < il; i++) {
      const ik = iks[i]
      const links = ik.links
      for (let j = 0, jl = links.length; j < jl; j++) {
        const link = links[j]
        if (physicsEnabled === true) {
          link.enabled = bones[link.index].rigidBodyType <= 0
        }
        else {
          link.enabled = true
        }
      }
    }
  }
  private _removeMesh(mesh: SkinnedMesh) {
    let found = false
    let writeIndex = 0
    for (let i = 0, il = this.meshes.length; i < il; i++) {
      if (this.meshes[i] === mesh) {
        this.objects.delete(mesh)
        found = true
        continue
      }
      this.meshes[writeIndex++] = this.meshes[i]
    }
    if (!found) {
      throw new Error('THREE.MMDAnimationHelper._removeMesh: '
        + `SkinnedMesh '${mesh.name}' has not been added yet.`)
    }
    this.meshes.length = writeIndex
    return this
  }
  private _restoreBones(mesh: SkinnedMesh) {
    const objects = this.objects.get(mesh)!
    const backupBones = objects.backupBones
    if (backupBones === undefined)
      return
    const bones = mesh.skeleton.bones
    for (let i = 0, il = bones.length; i < il; i++) {
      const bone = bones[i]
      bone.position.fromArray(backupBones, i * 7)
      bone.quaternion.fromArray(backupBones, i * 7 + 3)
    }
  }
  private _saveBones(mesh: SkinnedMesh) {
    const objects = this.objects.get(mesh)!
    const bones = mesh.skeleton.bones
    let backupBones = objects.backupBones
    if (backupBones === undefined) {
      backupBones = new Float32Array(bones.length * 7)
      objects.backupBones = backupBones
    }
    for (let i = 0, il = bones.length; i < il; i++) {
      const bone = bones[i]
      bone.position.toArray(backupBones, i * 7)
      bone.quaternion.toArray(backupBones, i * 7 + 3)
    }
  }
  private _setupAudio(audio: Audio, params: MMDAnimationHelperAddParameter) {
    if (this.audio === audio) {
      throw new Error('MMDAnimationHelper._setupAudio: '
        + `Audio '${audio.name}' has already been set.`)
    }
    if (this.audio)
      this._clearAudio(this.audio)
    this.audio = audio
    this.audioManager = new AudioManager(audio, params)
    this.objects.set(this.audioManager, {
      duration: this.audioManager.duration,
    })
    return this
  }
  private _setupCamera(camera: Camera, params: MMDAnimationHelperAddParameter) {
    if (this.camera === camera) {
      throw new Error('MMDAnimationHelper._setupCamera: '
        + `Camera '${camera.name}' has already been set.`)
    }
    if (this.camera)
      this._clearCamera(this.camera)
    this.camera = camera
    camera.add(this.cameraTarget)
    this.objects.set(camera, {})
    if (params.animation !== undefined) {
      this._setupCameraAnimation(camera, params.animation)
    }
    return this
  }
  private _setupCameraAnimation(camera: Camera, animation: AnimationClip | AnimationClip[]) {
    const animations = Array.isArray(animation)
      ? animation
      : [animation]
    const objects = this.objects.get(camera)!
    objects.mixer = new AnimationMixer(camera)
    for (let i = 0, il = animations.length; i < il; i++) {
      objects.mixer.clipAction(animations[i]).play()
    }
  }
  private _setupMeshAnimation(mesh: SkinnedMesh, animation: AnimationClip | AnimationClip[]) {
    const objects = this.objects.get(mesh)!
    if (animation !== undefined) {
      const animations = Array.isArray(animation)
        ? animation
        : [animation]
      objects.mixer = new AnimationMixer(mesh)
      for (let i = 0, il = animations.length; i < il; i++) {
        objects.mixer.clipAction(animations[i]).play()
      }
      objects.mixer.addEventListener('loop', (event) => {
        const tracks = event.action._clip.tracks
        if (tracks.length > 0 && tracks[0].name.slice(0, 6) !== '.bones')
          return
        objects.looped = true
      })
    }
    objects.ikSolver = this._createCCDIKSolver(mesh)
    objects.grantSolver = this.createGrantSolver(mesh)
    return this
  }
  private _setupMeshPhysics(mesh: SkinnedMesh, params: MMDAnimationHelperAddParameter) {
    const objects = this.objects.get(mesh)!
    if (params.world === undefined && this.sharedPhysics) {
      const masterPhysics = this._getMasterPhysics()
      if (masterPhysics !== null)
        world = masterPhysics.world
    }
    objects.physics = this._createMMDPhysics(mesh, params)
    if (objects.mixer && params.animationWarmup !== false) {
      this._animateMesh(mesh, 0)
      objects.physics.reset()
    }
    objects.physics.warmup(params.warmup !== undefined ? params.warmup : 60)
    this._optimizeIK(mesh, true)
  }
  private _sortBoneDataArray(boneDataArray: PmxBoneInfo[]) {
    return boneDataArray.sort((a, b) => {
      if (a.transformationClass !== b.transformationClass) {
        return a.transformationClass - b.transformationClass
      }
      else {
        return a.index - b.index
      }
    })
  }
  private _syncDuration() {
    let max = 0.0
    const objects = this.objects
    const meshes = this.meshes
    const camera = this.camera
    const audioManager = this.audioManager
    for (let i = 0, il = meshes.length; i < il; i++) {
      const mixer = this.objects.get(meshes[i])!.mixer
      if (mixer === undefined)
        continue
      for (let j = 0; j < mixer._actions!.length; j++) {
        const clip = mixer._actions![j]._clip
        if (!objects.has(clip)) {
          objects.set(clip, {
            duration: clip.duration,
          })
        }
        max = Math.max(max, objects.get(clip)!.duration!)
      }
    }
    if (camera !== null) {
      const mixer = this.objects.get(camera)!.mixer
      if (mixer !== undefined) {
        for (let i = 0, il = mixer._actions!.length; i < il; i++) {
          const clip = mixer._actions![i]._clip
          if (!objects.has(clip)) {
            objects.set(clip, {
              duration: clip.duration,
            })
          }
          max = Math.max(max, objects.get(clip)!.duration!)
        }
      }
    }
    if (audioManager !== null) {
      max = Math.max(max, objects.get(audioManager)!.duration!)
    }
    max += this.configuration.afterglow
    for (let i = 0, il = this.meshes.length; i < il; i++) {
      const mixer = this.objects.get(this.meshes[i])!.mixer
      if (mixer === undefined)
        continue
      for (let j = 0, jl = mixer._actions!.length; j < jl; j++) {
        mixer._actions![j]._clip.duration = max
      }
    }
    if (camera !== null) {
      const mixer = this.objects.get(camera)!.mixer
      if (mixer !== undefined) {
        for (let i = 0, il = mixer._actions!.length; i < il; i++) {
          mixer._actions![i]._clip.duration = max
        }
      }
    }
    if (audioManager !== null) {
      audioManager.duration = max
    }
  }
  private _updatePropertyMixersBuffer(mesh: SkinnedMesh) {
    const mixer = this.objects.get(mesh)!.mixer!
    const propertyMixers = mixer._bindings!
    const accuIndex = mixer._accuIndex
    for (let i = 0, il = propertyMixers.length; i < il; i++) {
      const propertyMixer = propertyMixers[i]
      const buffer = propertyMixer.buffer
      const stride = propertyMixer.valueSize
      const offset = (accuIndex + 1) * stride
      propertyMixer.binding.getValue(buffer, offset)
    }
  }
  private _updateSharedPhysics(delta: number) {
    if (this.meshes.length === 0 || !this.enabled.physics || !this.sharedPhysics)
      return
    const physics = this._getMasterPhysics()
    if (physics === null)
      return
    for (let i = 0, il = this.meshes.length; i < il; i++) {
      const p = this.meshes[i].physics
      if (p !== null && p !== undefined) {
        p.updateRigidBodies()
      }
    }
    physics.stepSimulation(delta)
    for (let i = 0, il = this.meshes.length; i < il; i++) {
      const p = this.meshes[i].physics
      if (p !== null && p !== undefined) {
        p.updateBones()
      }
    }
  }
  add(object: Audio | Camera | SkinnedMesh, params: MMDAnimationHelperAddParameter = {}): this {
    if (object.isSkinnedMesh) {
      this._addMesh(object as SkinnedMesh, params)
    }
    else if (object.isCamera) {
      this._setupCamera(object as Camera, params)
    }
    else if (object.type === 'Audio') {
      this._setupAudio(object as Audio, params)
    }
    else {
      throw new Error('MMDAnimationHelper.add: '
        + 'accepts only '
        + 'THREE.SkinnedMesh or '
        + 'THREE.Camera or '
        + 'THREE.Audio instance.')
    }
    if (this.configuration.sync)
      this._syncDuration()
    return this
  }
  createGrantSolver(mesh: SkinnedMesh): GrantSolver {
    return new GrantSolver(mesh, mesh.geometry.userData.MMD.grants)
  }
  enable(key: keyof typeof this.enabled, enabled: boolean): this {
    if (this.enabled[key] === undefined) {
      throw new Error('MMDAnimationHelper.enable: '
        + `unknown key ${key}`)
    }
    this.enabled[key] = enabled
    if (key === 'physics') {
      for (let i = 0, il = this.meshes.length; i < il; i++) {
        this._optimizeIK(this.meshes[i], enabled)
      }
    }
    return this
  }
  pose(mesh: SkinnedMesh, vpd: object, params: MMDAnimationHelperPoseParameter = {}) {
    if (params.resetPose !== false)
      mesh.pose()
    const bones = mesh.skeleton.bones
    const boneParams = vpd.bones as Bone[]
    const boneNameDictionary: Record<string, number> = {}
    for (let i = 0, il = bones.length; i < il; i++) {
      boneNameDictionary[bones[i].name] = i
    }
    const vector = new Vector3()
    const quaternion = new Quaternion()
    for (let i = 0, il = boneParams.length; i < il; i++) {
      const boneParam = boneParams[i]
      const boneIndex = boneNameDictionary[boneParam.name]
      if (boneIndex === undefined)
        continue
      const bone = bones[boneIndex]
      bone.position.add(vector.fromArray(boneParam.translation))
      bone.quaternion.multiply(quaternion.fromArray(boneParam.quaternion))
    }
    mesh.updateMatrixWorld(true)
    if (this.configuration.pmxAnimation
      && mesh.geometry.userData.MMD && mesh.geometry.userData.MMD.format === 'pmx') {
      const sortedBonesData = this._sortBoneDataArray(mesh.geometry.userData.MMD.bones.slice())
      const ikSolver = params.ik !== false ? this._createCCDIKSolver(mesh) : null
      const grantSolver = params.grant !== false ? this.createGrantSolver(mesh) : null
      this._animatePMXMesh(mesh, sortedBonesData, ikSolver, grantSolver)
    }
    else {
      if (params.ik !== false) {
        this._createCCDIKSolver(mesh).update()
      }
      if (params.grant !== false) {
        this.createGrantSolver(mesh).update()
      }
    }
    return this
  }
  remove(object: Audio | Camera | SkinnedMesh): this {
    if ('isSkinnedMesh' in object && object.isSkinnedMesh === true) {
      this._removeMesh(object)
    }
    else if ('isCamera' in object && object.isCamera === true) {
      this._clearCamera(object)
    }
    else if (object.type === 'Audio') {
      this._clearAudio(object as Audio)
    }
    else {
      throw new Error('MMDAnimationHelper.remove: '
        + 'accepts only '
        + 'THREE.SkinnedMesh or '
        + 'THREE.Camera or '
        + 'THREE.Audio instance.')
    }
    if (this.configuration.sync)
      this._syncDuration()
    return this
  }
  update(delta: number): this {
    if (this.audioManager !== null)
      this.audioManager.control(delta)
    for (let i = 0; i < this.meshes.length; i++) {
      this._animateMesh(this.meshes[i], delta)
    }
    if (this.sharedPhysics)
      this._updateSharedPhysics(delta)
    if (this.camera !== null)
      this._animateCamera(this.camera, delta)
    return this
  }
}