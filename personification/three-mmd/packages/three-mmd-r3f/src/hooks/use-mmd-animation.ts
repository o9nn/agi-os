import type { Camera } from '@react-three/fiber'
import type { AnimationClip, SkinnedMesh } from 'three'
import { createMMDAnimationClip } from '@moeru/three-mmd'
import { useVMD } from './use-vmd'
const useMMDAnimation = (vmdPath: string, object: Camera | SkinnedMesh, name?: string): AnimationClip => {
  const vmd = useVMD(vmdPath)
  const clip = createMMDAnimationClip(vmd, object)
  if (name != null)
    clip.name = name
  return clip
}
useMMDAnimation.preload = useVMD.preload
useMMDAnimation.clear = useVMD.clear
export { useMMDAnimation }