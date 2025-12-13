/* eslint-disable ts/strict-boolean-expressions */
/* eslint-disable ts/no-unsafe-member-access */
/* eslint-disable ts/no-unsafe-call */
/* eslint-disable ts/no-unsafe-return */
/* eslint-disable ts/no-unsafe-argument */
import type {
  RigidBodyProps,
} from '@react-three/rapier'

import {
  Decal,
  Environment,
  Lightformer,
  useGLTF,
  useTexture,
} from '@react-three/drei'
import { Canvas, extend, useFrame } from '@react-three/fiber'
import {
  BallCollider,
  CuboidCollider,
  Physics,
  RigidBody,
  useRopeJoint,
  useSphericalJoint,
} from '@react-three/rapier'
import { MeshLineGeometry, MeshLineMaterial } from 'meshline'
import { useEffect, useMemo, useRef, useState } from 'react'
import * as THREE from 'three'

import cardGLB from './card.glb?url'
import lanyard from './lanyard.png'
import './Lanyard.css'

// eslint-disable-next-line @masknet/no-top-level
extend({ MeshLineGeometry, MeshLineMaterial })

interface BandProps {
  maxSpeed?: number
  minSpeed?: number
}

const Band = ({ maxSpeed = 50, minSpeed = 0 }: BandProps) => {
  // Using "any" for refs since the exact types depend on Rapier's internals
  const band = useRef<any>(null)
  const fixed = useRef<any>(null)
  const j1 = useRef<any>(null)
  const j2 = useRef<any>(null)
  const j3 = useRef<any>(null)
  const card = useRef<any>(null)

  const vec = new THREE.Vector3()
  const ang = new THREE.Vector3()
  const rot = new THREE.Vector3()
  const dir = new THREE.Vector3()

  const segmentProps: any = {
    angularDamping: 4,
    canSleep: true,
    colliders: false,
    linearDamping: 4,
    type: 'dynamic' as RigidBodyProps['type'],
  }

  const { materials, nodes } = useGLTF(cardGLB)
  const texture = useTexture(lanyard, texture => texture.wrapS = texture.wrapT = THREE.RepeatWrapping)

  const moeruAI = useTexture('./moeru-ai.png')

  const curve = useMemo(() => {
    const curve = new THREE.CatmullRomCurve3([
      new THREE.Vector3(),
      new THREE.Vector3(),
      new THREE.Vector3(),
      new THREE.Vector3(),
    ])
    curve.curveType = 'chordal'

    return curve
  }, [])
  const [dragged, drag] = useState<false | THREE.Vector3>(false)
  const [hovered, hover] = useState(false)

  const [isSmall, setIsSmall] = useState<boolean>(() => {
    if (typeof window !== 'undefined') {
      return window.innerWidth < 1024
    }
    return false
  })

  useEffect(() => {
    const handleResize = (): void => {
      setIsSmall(window.innerWidth < 1024)
    }

    window.addEventListener('resize', handleResize)
    return (): void => window.removeEventListener('resize', handleResize)
  }, [])

  useRopeJoint(fixed, j1, [[0, 0, 0], [0, 0, 0], 1])
  useRopeJoint(j1, j2, [[0, 0, 0], [0, 0, 0], 1])
  useRopeJoint(j2, j3, [[0, 0, 0], [0, 0, 0], 1])
  useSphericalJoint(j3, card, [
    [0, 0, 0],
    [0, 1.45, 0],
  ])

  useEffect(() => {
    if (!(hovered))
      return
    document.body.style.cursor = dragged !== false ? 'grabbing' : 'grab'
    return () => {
      document.body.style.cursor = 'auto'
    }
  }, [hovered, dragged])

  useFrame((state, delta) => {
    if (dragged !== false && typeof dragged !== 'boolean') {
      vec.set(state.pointer.x, state.pointer.y, 0.5).unproject(state.camera)
      dir.copy(vec).sub(state.camera.position).normalize()
      vec.add(dir.multiplyScalar(state.camera.position.length()));
      [card, j1, j2, j3, fixed].forEach(ref => ref.current?.wakeUp())
      card.current?.setNextKinematicTranslation({
        x: vec.x - dragged.x,
        y: vec.y - dragged.y,
        z: vec.z - dragged.z,
      })
    }
    if (fixed.current) {
      [j1, j2].forEach((ref) => {
        if (!ref.current.lerped) {
          ref.current.lerped = new THREE.Vector3().copy(
            ref.current.translation(),
          )
        }
        const clampedDistance = Math.max(
          0.1,
          Math.min(1, ref.current.lerped.distanceTo(ref.current.translation())),
        )
        ref.current.lerped.lerp(
          ref.current.translation(),
          delta * (minSpeed + clampedDistance * (maxSpeed - minSpeed)),
        )
      })
      curve.points[0].copy(j3.current.translation())
      curve.points[1].copy(j2.current.lerped)
      curve.points[2].copy(j1.current.lerped)
      curve.points[3].copy(fixed.current.translation())
      band.current.geometry.setPoints(curve.getPoints(32))
      ang.copy(card.current.angvel())
      rot.copy(card.current.rotation())
      card.current.setAngvel({ x: ang.x, y: ang.y - rot.y * 0.25, z: ang.z })
    }
  })

  return (
    <>
      <group position={[0, 4, 0]}>
        <RigidBody
          ref={fixed}
          {...segmentProps}
          type={'fixed' as RigidBodyProps['type']}
        />
        <RigidBody
          position={[0.5, 0, 0]}
          ref={j1}
          {...segmentProps}
          type={'dynamic' as RigidBodyProps['type']}
        >
          <BallCollider args={[0.1]} />
        </RigidBody>
        <RigidBody
          position={[1, 0, 0]}
          ref={j2}
          {...segmentProps}
          type={'dynamic' as RigidBodyProps['type']}
        >
          <BallCollider args={[0.1]} />
        </RigidBody>
        <RigidBody
          position={[1.5, 0, 0]}
          ref={j3}
          {...segmentProps}
          type={'dynamic' as RigidBodyProps['type']}
        >
          <BallCollider args={[0.1]} />
        </RigidBody>
        <RigidBody
          position={[2, 0, 0]}
          ref={card}
          {...segmentProps}
          // eslint-disable-next-line @masknet/jsx-no-logical
          type={
            dragged
              ? ('kinematicPosition' as RigidBodyProps['type'])
              : ('dynamic' as RigidBodyProps['type'])
          }
        >
          <CuboidCollider args={[0.8, 1.125, 0.01]} />
          <group
            onPointerDown={(e: any) => {
              e.target.setPointerCapture(e.pointerId)
              drag(
                new THREE.Vector3()
                  .copy(e.point)
                  .sub(vec.copy(card.current.translation())),
              )
            }}
            onPointerOut={() => hover(false)}
            onPointerOver={() => hover(true)}
            onPointerUp={(e: any) => {
              e.target.releasePointerCapture(e.pointerId)
              drag(false)
            }}
            position={[0, -1.2, -0.05]}
            scale={2.25}
          >
            <Decal mesh={{ current: nodes.card as THREE.Mesh }} position={[0, 0.5, 0]} rotation={[0, 0, 0]} scale={0.75}>
              <meshBasicMaterial alphaTest={0.5} map={moeruAI} transparent />
            </Decal>
            <mesh geometry={(nodes.card as THREE.Mesh).geometry}>
              <meshPhysicalMaterial
                clearcoat={1}
                clearcoatRoughness={0.15}
                iridescence={1}
                iridescenceIOR={1}
                iridescenceThicknessRange={[0, 2400]}
                metalness={0.5}
                polygonOffset
                polygonOffsetFactor={-0.1}
                polygonOffsetUnits={4}
                roughness={0.3}
              />
            </mesh>
            <mesh
              geometry={(nodes.clip as THREE.Mesh).geometry}
              material={materials.metal}
              material-roughness={0.3}
            />
            <mesh geometry={(nodes.clamp as THREE.Mesh).geometry} material={materials.metal} />
          </group>
        </RigidBody>
      </group>
      <mesh ref={band}>
        <meshLineGeometry />
        <meshLineMaterial
          color="white"
          depthTest={false}
          lineWidth={1}
          map={texture}
          repeat={[-4, 1]}
          // eslint-disable-next-line @masknet/jsx-no-logical
          resolution={isSmall ? [1000, 2000] : [1000, 1000]}
          useMap
        />
      </mesh>
    </>
  )
}

interface LanyardProps {
  fov?: number
  gravity?: [number, number, number]
  position?: [number, number, number]
  transparent?: boolean
}

const Lanyard = ({
  fov = 20,
  gravity = [0, -40, 0],
  position = [0, 0, 30],
  transparent = true,
}: LanyardProps) => {
  return (
    <div className="lanyard-wrapper">
      <Canvas
        camera={{ fov, position }}
        gl={{ alpha: transparent }}
        onCreated={({ gl }) =>
          gl.setClearColor(new THREE.Color(0x000000), transparent ? 0 : 1)}
      >
        <ambientLight intensity={Math.PI} />
        <Physics gravity={gravity} timeStep={1 / 60}>
          <Band />
        </Physics>
        <Environment blur={0.75}>
          <Lightformer
            color="white"
            intensity={2}
            position={[0, -1, 5]}
            rotation={[0, 0, Math.PI / 3]}
            scale={[100, 0.1, 1]}
          />
          <Lightformer
            color="white"
            intensity={3}
            position={[-1, -1, 1]}
            rotation={[0, 0, Math.PI / 3]}
            scale={[100, 0.1, 1]}
          />
          <Lightformer
            color="white"
            intensity={3}
            position={[1, 1, 1]}
            rotation={[0, 0, Math.PI / 3]}
            scale={[100, 0.1, 1]}
          />
          <Lightformer
            color="white"
            intensity={10}
            position={[-10, 0, 14]}
            rotation={[0, Math.PI / 2, Math.PI / 3]}
            scale={[100, 10, 1]}
          />
        </Environment>
      </Canvas>
    </div>
  )
}

export default Lanyard
