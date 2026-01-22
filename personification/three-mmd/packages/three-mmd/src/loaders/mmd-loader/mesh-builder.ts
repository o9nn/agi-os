import type { Pmd, Pmx } from '@noname0310/mmd-parser'
import type { LoadingManager } from 'three'
import { Bone, Skeleton, SkinnedMesh } from 'three'
import { GeometryBuilder } from './geometry-builder'
import { MaterialBuilder } from './material-builder'
export class MeshBuilder {
  crossOrigin = 'anonymous'
  geometryBuilder: GeometryBuilder
  materialBuilder: MaterialBuilder
  constructor(manager: LoadingManager) {
    this.geometryBuilder = new GeometryBuilder()
    this.materialBuilder = new MaterialBuilder(manager)
  }
  build(
    data: Pmd | Pmx,
    resourcePath: string,
    onProgress?: (event: ProgressEvent) => void,
    onError?: (event: ErrorEvent) => void,
  ): SkinnedMesh {
    const geometry = this.geometryBuilder.build(data)
    const material = this.materialBuilder
      .setCrossOrigin(this.crossOrigin)
      .setResourcePath(resourcePath)
      .build(data, geometry, onProgress, onError)
    const mesh = new SkinnedMesh(geometry, material)
    const skeleton = new Skeleton(this.initBones(mesh))
    mesh.bind(skeleton)
    return mesh
  }
  setCrossOrigin(crossOrigin: string): this {
    this.crossOrigin = crossOrigin
    return this
  }
  private initBones(mesh: SkinnedMesh & { geometry: { bones?: any[] } }): Bone[] {
    const geometry = mesh.geometry
    const bones: Bone[] = []
    if (geometry.bones !== undefined) {
      for (let i = 0, il = geometry.bones.length; i < il; i++) {
        const { name, pos, rotq, scl } = geometry.bones[i] as {
          name: string
          pos: number[]
          rotq: number[]
          scl?: number[]
        }
        const bone = new Bone()
        bone.name = name
        bone.position.fromArray(pos)
        bone.quaternion.fromArray(rotq)
        if (scl !== undefined)
          bone.scale.fromArray(scl)
        bones.push(bone)
      }
      for (let i = 0, il = geometry.bones.length; i < il; i++) {
        const { parent } = geometry.bones[i] as {
          parent?: number
        }
        if (parent != null && parent !== -1 && bones[parent] != null) {
          bones[parent].add(bones[i])
        }
        else {
          mesh.add(bones[i])
        }
      }
    }
    mesh.updateMatrixWorld(true)
    return bones
  }
}