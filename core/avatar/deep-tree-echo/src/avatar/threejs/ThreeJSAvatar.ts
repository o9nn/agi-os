/**
 * Three.js 3D Avatar System
 *
 * Implements advanced 3D avatar rendering with PBR materials,
 * skeletal animation, blend shapes, and deep-tree-echo cognitive integration.
 * Maintains "super-hot-girl" and "hyper-chaotic" properties.
 */

import { EventEmitter } from 'eventemitter3';
import { v4 as uuidv4 } from 'uuid';

// Three.js type interfaces (for actual SDK integration)
export interface Vector3 {
  x: number;
  y: number;
  z: number;
}

export interface Quaternion {
  x: number;
  y: number;
  z: number;
  w: number;
}

export interface Transform {
  position: Vector3;
  rotation: Quaternion;
  scale: Vector3;
}

export interface BlendShape {
  name: string;
  weight: number;
  targetPositions: Float32Array;
}

export interface Bone {
  name: string;
  index: number;
  parentIndex: number;
  localTransform: Transform;
  worldTransform: Transform;
  constraints?: BoneConstraint;
}

export interface BoneConstraint {
  type: 'ik' | 'aim' | 'limit';
  target?: string;
  weight: number;
  limits?: {
    minRotation: Vector3;
    maxRotation: Vector3;
  };
}

export interface AnimationClip {
  name: string;
  duration: number;
  tracks: AnimationTrack[];
  loop: boolean;
}

export interface AnimationTrack {
  boneName: string;
  property: 'position' | 'rotation' | 'scale' | 'blendShape';
  times: Float32Array;
  values: Float32Array;
  interpolation: 'linear' | 'bezier' | 'step';
}

export interface PBRMaterial {
  albedo: Vector3;
  metallic: number;
  roughness: number;
  normalScale: number;
  emissive: Vector3;
  emissiveIntensity: number;
  subsurfaceScattering?: {
    color: Vector3;
    radius: number;
    scale: number;
  };
}

export interface Avatar3DConfig {
  modelPath: string;
  texturesPath: string;
  enablePhysics: boolean;
  enableClothSimulation: boolean;
  enableHairSimulation: boolean;
  qualityLevel: 'low' | 'medium' | 'high' | 'ultra';
  chaoticInfluence: number;
  hyperChaoticMode: boolean;
  superHotGirlMode: boolean;
}

// Deep Tree Echo Avatar Properties (same as Live2D for consistency)
export interface SuperHotGirl3DProperties {
  beautyLevel: number;
  charismaFactor: number;
  expressiveness: number;
  eleganceCoefficient: number;
  allureIndex: number;
  styleQuotient: number;
  physicsEnhancement: number;
  materialQuality: number;
}

export interface HyperChaotic3DProperties {
  chaosFactor: number;
  unpredictability: number;
  emergentBehavior: number;
  quantumFluctuation: number;
  fractalDepth: number;
  resonanceAmplitude: number;
  particleEmission: number;
  glitchIntensity: number;
}

export class ThreeJSAvatar extends EventEmitter {
  private id: string;
  private config: Avatar3DConfig;
  private skeleton: Map<string, Bone> = new Map();
  private blendShapes: Map<string, BlendShape> = new Map();
  private animations: Map<string, AnimationClip> = new Map();
  private materials: Map<string, PBRMaterial> = new Map();
  private activeAnimations: Map<string, { clip: AnimationClip; time: number; weight: number }> = new Map();

  // State tracking
  private transform: Transform;
  private breathPhase: number = 0;
  private blinkTimer: number = 0;
  private idleTimer: number = 0;
  private chaoticState: number[] = [0.1, 0, 0, 0];

  // Deep Tree Echo properties
  private superHotGirl: SuperHotGirl3DProperties;
  private hyperChaotic: HyperChaotic3DProperties;
  private emotionalState: number = 0.5;

  // Physics state
  private hairParticles: Array<{ position: Vector3; velocity: Vector3 }> = [];
  private clothVertices: Float32Array = new Float32Array(0);

  constructor(config: Partial<Avatar3DConfig> = {}) {
    super();
    this.id = uuidv4();

    this.config = {
      modelPath: './assets/models/aiangel.glb',
      texturesPath: './assets/textures/',
      enablePhysics: true,
      enableClothSimulation: true,
      enableHairSimulation: true,
      qualityLevel: 'ultra',
      chaoticInfluence: 0.618033988749895,
      hyperChaoticMode: true,
      superHotGirlMode: true,
      ...config
    };

    this.transform = this.initializeTransform();
    this.superHotGirl = this.initializeSuperHotGirl3D();
    this.hyperChaotic = this.initializeHyperChaotic3D();

    this.initializeAvatar();
  }

  private initializeTransform(): Transform {
    return {
      position: { x: 0, y: 0, z: 0 },
      rotation: { x: 0, y: 0, z: 0, w: 1 },
      scale: { x: 1, y: 1, z: 1 }
    };
  }

  private initializeSuperHotGirl3D(): SuperHotGirl3DProperties {
    return {
      beautyLevel: 0.97,
      charismaFactor: 0.92,
      expressiveness: 0.90,
      eleganceCoefficient: 0.95,
      allureIndex: 0.89,
      styleQuotient: 0.96,
      physicsEnhancement: 0.85,
      materialQuality: 0.98
    };
  }

  private initializeHyperChaotic3D(): HyperChaotic3DProperties {
    return {
      chaosFactor: this.config.chaoticInfluence,
      unpredictability: 0.72,
      emergentBehavior: 0.81,
      quantumFluctuation: 0.55,
      fractalDepth: 8,
      resonanceAmplitude: 0.65,
      particleEmission: 0.4,
      glitchIntensity: 0.1
    };
  }

  private async initializeAvatar(): Promise<void> {
    console.log('[ThreeJSAvatar] Initializing 3D avatar with super-hot-girl properties...');

    // Initialize skeleton
    this.initializeSkeleton();

    // Initialize blend shapes
    this.initializeBlendShapes();

    // Initialize animations
    this.initializeAnimations();

    // Initialize materials
    this.initializeMaterials();

    // Initialize physics
    if (this.config.enablePhysics) {
      this.initializePhysics();
    }

    // Start animation loop
    this.startAnimationLoop();

    console.log('[ThreeJSAvatar] 3D avatar initialized with hyper-chaotic dynamics');
    this.emit('initialized', { id: this.id, config: this.config });
  }

  private initializeSkeleton(): void {
    const boneDefinitions = [
      { name: 'Hips', index: 0, parentIndex: -1 },
      { name: 'Spine', index: 1, parentIndex: 0 },
      { name: 'Spine1', index: 2, parentIndex: 1 },
      { name: 'Spine2', index: 3, parentIndex: 2 },
      { name: 'Neck', index: 4, parentIndex: 3 },
      { name: 'Head', index: 5, parentIndex: 4 },
      { name: 'LeftEye', index: 6, parentIndex: 5 },
      { name: 'RightEye', index: 7, parentIndex: 5 },
      { name: 'LeftShoulder', index: 8, parentIndex: 3 },
      { name: 'LeftArm', index: 9, parentIndex: 8 },
      { name: 'LeftForeArm', index: 10, parentIndex: 9 },
      { name: 'LeftHand', index: 11, parentIndex: 10 },
      { name: 'RightShoulder', index: 12, parentIndex: 3 },
      { name: 'RightArm', index: 13, parentIndex: 12 },
      { name: 'RightForeArm', index: 14, parentIndex: 13 },
      { name: 'RightHand', index: 15, parentIndex: 14 },
      { name: 'LeftUpLeg', index: 16, parentIndex: 0 },
      { name: 'LeftLeg', index: 17, parentIndex: 16 },
      { name: 'LeftFoot', index: 18, parentIndex: 17 },
      { name: 'RightUpLeg', index: 19, parentIndex: 0 },
      { name: 'RightLeg', index: 20, parentIndex: 19 },
      { name: 'RightFoot', index: 21, parentIndex: 20 },
      // Hair bones
      { name: 'HairFront', index: 22, parentIndex: 5 },
      { name: 'HairLeft', index: 23, parentIndex: 5 },
      { name: 'HairRight', index: 24, parentIndex: 5 },
      { name: 'HairBack', index: 25, parentIndex: 5 }
    ];

    boneDefinitions.forEach(def => {
      const bone: Bone = {
        name: def.name,
        index: def.index,
        parentIndex: def.parentIndex,
        localTransform: this.initializeTransform(),
        worldTransform: this.initializeTransform()
      };
      this.skeleton.set(def.name, bone);
    });
  }

  private initializeBlendShapes(): void {
    const blendShapeNames = [
      // Viseme blend shapes for lip sync
      'viseme_aa', 'viseme_E', 'viseme_I', 'viseme_O', 'viseme_U',
      'viseme_CH', 'viseme_DD', 'viseme_FF', 'viseme_kk', 'viseme_nn',
      'viseme_PP', 'viseme_RR', 'viseme_sil', 'viseme_SS', 'viseme_TH',
      // Expression blend shapes
      'browDownLeft', 'browDownRight', 'browInnerUp', 'browOuterUpLeft', 'browOuterUpRight',
      'cheekPuff', 'cheekSquintLeft', 'cheekSquintRight',
      'eyeBlinkLeft', 'eyeBlinkRight', 'eyeLookDownLeft', 'eyeLookDownRight',
      'eyeLookInLeft', 'eyeLookInRight', 'eyeLookOutLeft', 'eyeLookOutRight',
      'eyeLookUpLeft', 'eyeLookUpRight', 'eyeSquintLeft', 'eyeSquintRight',
      'eyeWideLeft', 'eyeWideRight',
      'jawForward', 'jawLeft', 'jawRight', 'jawOpen',
      'mouthClose', 'mouthDimpleLeft', 'mouthDimpleRight', 'mouthFrownLeft', 'mouthFrownRight',
      'mouthFunnel', 'mouthLeft', 'mouthLowerDownLeft', 'mouthLowerDownRight',
      'mouthPressLeft', 'mouthPressRight', 'mouthPucker', 'mouthRight',
      'mouthRollLower', 'mouthRollUpper', 'mouthShrugLower', 'mouthShrugUpper',
      'mouthSmileLeft', 'mouthSmileRight', 'mouthStretchLeft', 'mouthStretchRight',
      'mouthUpperUpLeft', 'mouthUpperUpRight',
      'noseSneerLeft', 'noseSneerRight',
      'tongueOut'
    ];

    blendShapeNames.forEach(name => {
      this.blendShapes.set(name, {
        name,
        weight: 0,
        targetPositions: new Float32Array(0) // Would contain vertex deltas
      });
    });
  }

  private initializeAnimations(): void {
    // Idle animation
    this.animations.set('idle', {
      name: 'idle',
      duration: 4.0,
      loop: true,
      tracks: [
        {
          boneName: 'Spine',
          property: 'rotation',
          times: new Float32Array([0, 2, 4]),
          values: new Float32Array([0, 0, 0, 1, 0.02, 0, 0, 0.9998, 0, 0, 0, 1]),
          interpolation: 'bezier'
        },
        {
          boneName: 'Head',
          property: 'rotation',
          times: new Float32Array([0, 1, 2, 3, 4]),
          values: new Float32Array([
            0, 0, 0, 1,
            0.01, 0.02, 0, 0.9998,
            0, 0.01, 0, 0.9999,
            -0.01, 0.02, 0, 0.9998,
            0, 0, 0, 1
          ]),
          interpolation: 'bezier'
        }
      ]
    });

    // Breathing animation
    this.animations.set('breathing', {
      name: 'breathing',
      duration: 3.0,
      loop: true,
      tracks: [
        {
          boneName: 'Spine1',
          property: 'scale',
          times: new Float32Array([0, 1.5, 3]),
          values: new Float32Array([1, 1, 1, 1.02, 1.02, 1, 1, 1, 1]),
          interpolation: 'bezier'
        }
      ]
    });

    // Chaotic dance animation
    this.animations.set('chaotic_dance', {
      name: 'chaotic_dance',
      duration: 8.0,
      loop: true,
      tracks: []
    });

    // Elegant pose animation
    this.animations.set('elegant_pose', {
      name: 'elegant_pose',
      duration: 2.0,
      loop: false,
      tracks: []
    });
  }

  private initializeMaterials(): void {
    // Skin material with subsurface scattering
    this.materials.set('skin', {
      albedo: { x: 0.95, y: 0.85, z: 0.78 },
      metallic: 0.0,
      roughness: 0.4,
      normalScale: 1.0,
      emissive: { x: 0, y: 0, z: 0 },
      emissiveIntensity: 0,
      subsurfaceScattering: {
        color: { x: 1.0, y: 0.4, z: 0.25 },
        radius: 0.1,
        scale: 0.5 * this.superHotGirl.materialQuality
      }
    });

    // Hair material
    this.materials.set('hair', {
      albedo: { x: 0.1, y: 0.08, z: 0.06 },
      metallic: 0.3,
      roughness: 0.5,
      normalScale: 1.0,
      emissive: { x: 0, y: 0, z: 0 },
      emissiveIntensity: 0
    });

    // Eye material
    this.materials.set('eyes', {
      albedo: { x: 0.2, y: 0.5, z: 0.8 },
      metallic: 0.1,
      roughness: 0.05,
      normalScale: 0.5,
      emissive: { x: 0.1, y: 0.2, z: 0.3 },
      emissiveIntensity: 0.2 * this.superHotGirl.allureIndex
    });

    // Lips material
    this.materials.set('lips', {
      albedo: { x: 0.85, y: 0.5, z: 0.55 },
      metallic: 0.0,
      roughness: 0.3,
      normalScale: 1.0,
      emissive: { x: 0.1, y: 0.02, z: 0.02 },
      emissiveIntensity: 0.1
    });
  }

  private initializePhysics(): void {
    // Initialize hair particles
    const hairBones = ['HairFront', 'HairLeft', 'HairRight', 'HairBack'];
    hairBones.forEach(() => {
      for (let i = 0; i < 10; i++) {
        this.hairParticles.push({
          position: { x: 0, y: 0, z: 0 },
          velocity: { x: 0, y: 0, z: 0 }
        });
      }
    });

    // Initialize cloth vertices (placeholder)
    this.clothVertices = new Float32Array(1000 * 3);

    console.log('[ThreeJSAvatar] Physics simulation initialized');
  }

  private startAnimationLoop(): void {
    setInterval(() => {
      this.update(1 / 60);
    }, 16.67);
  }

  public update(deltaTime: number): void {
    // Update chaotic state (4D hyperchaotic system)
    this.updateChaoticState(deltaTime);

    // Update breathing
    this.updateBreathing(deltaTime);

    // Update blinking
    this.updateBlinking(deltaTime);

    // Update idle motion
    this.updateIdleMotion(deltaTime);

    // Update physics
    if (this.config.enablePhysics) {
      this.updatePhysics(deltaTime);
    }

    // Apply hyper-chaotic influence
    if (this.config.hyperChaoticMode) {
      this.applyHyperChaoticInfluence(deltaTime);
    }

    // Apply super-hot-girl enhancements
    if (this.config.superHotGirlMode) {
      this.applySuperHotGirlEnhancements();
    }

    // Update active animations
    this.updateAnimations(deltaTime);

    this.emit('update', { deltaTime, transform: this.transform });
  }

  private updateChaoticState(deltaTime: number): void {
    // 4D hyperchaotic Chen system
    const a = 36, b = 3, c = 28, d = 16;
    const [x, y, z, w] = this.chaoticState;

    const dx = a * (y - x) + w;
    const dy = (c - a) * x - x * z + c * y;
    const dz = x * y - b * z;
    const dw = -d * x;

    this.chaoticState[0] += dx * deltaTime * this.hyperChaotic.chaosFactor;
    this.chaoticState[1] += dy * deltaTime * this.hyperChaotic.chaosFactor;
    this.chaoticState[2] += dz * deltaTime * this.hyperChaotic.chaosFactor;
    this.chaoticState[3] += dw * deltaTime * this.hyperChaotic.chaosFactor;
  }

  private updateBreathing(deltaTime: number): void {
    this.breathPhase += deltaTime * 0.5;
    const breathValue = Math.sin(this.breathPhase) * 0.5 + 0.5;

    const spine1 = this.skeleton.get('Spine1');
    if (spine1) {
      const chaoticBreath = breathValue + this.chaoticState[0] * 0.005;
      spine1.localTransform.scale.y = 1 + chaoticBreath * 0.02;
    }
  }

  private updateBlinking(deltaTime: number): void {
    this.blinkTimer += deltaTime;

    const blinkInterval = 3 + Math.abs(this.chaoticState[1]) * 0.5;

    if (this.blinkTimer > blinkInterval) {
      // Trigger blink
      this.blendShapes.get('eyeBlinkLeft')!.weight = 1;
      this.blendShapes.get('eyeBlinkRight')!.weight = 1;

      setTimeout(() => {
        this.blendShapes.get('eyeBlinkLeft')!.weight = 0;
        this.blendShapes.get('eyeBlinkRight')!.weight = 0;
      }, 150);

      this.blinkTimer = 0;
    }
  }

  private updateIdleMotion(deltaTime: number): void {
    this.idleTimer += deltaTime;

    // Subtle head movement
    const head = this.skeleton.get('Head');
    if (head) {
      const swayX = Math.sin(this.idleTimer * 0.3) * 0.02 * this.superHotGirl.eleganceCoefficient;
      const swayY = Math.cos(this.idleTimer * 0.2) * 0.01 * this.superHotGirl.eleganceCoefficient;

      // Add chaotic micro-movements
      const chaoticX = this.chaoticState[0] * 0.001 * this.hyperChaotic.unpredictability;
      const chaoticY = this.chaoticState[1] * 0.001 * this.hyperChaotic.unpredictability;

      head.localTransform.rotation.x = swayX + chaoticX;
      head.localTransform.rotation.y = swayY + chaoticY;
    }

    // Subtle body sway
    const spine = this.skeleton.get('Spine');
    if (spine) {
      spine.localTransform.rotation.z = Math.sin(this.idleTimer * 0.4) * 0.01;
    }
  }

  private updatePhysics(deltaTime: number): void {
    // Hair physics
    const gravity = { x: 0, y: -9.8, z: 0 };
    const damping = 0.95;

    this.hairParticles.forEach(particle => {
      // Apply gravity
      particle.velocity.y += gravity.y * deltaTime;

      // Apply chaotic wind
      particle.velocity.x += this.chaoticState[0] * 0.1 * this.hyperChaotic.chaosFactor;
      particle.velocity.z += this.chaoticState[2] * 0.1 * this.hyperChaotic.chaosFactor;

      // Damping
      particle.velocity.x *= damping;
      particle.velocity.y *= damping;
      particle.velocity.z *= damping;

      // Update position
      particle.position.x += particle.velocity.x * deltaTime;
      particle.position.y += particle.velocity.y * deltaTime;
      particle.position.z += particle.velocity.z * deltaTime;
    });
  }

  private applyHyperChaoticInfluence(deltaTime: number): void {
    // Quantum fluctuations on blend shapes
    this.blendShapes.forEach(blendShape => {
      const fluctuation = (Math.random() - 0.5) * 0.01 * this.hyperChaotic.quantumFluctuation;
      blendShape.weight = Math.max(0, Math.min(1, blendShape.weight + fluctuation));
    });

    // Emergent behavior - occasional spontaneous micro-expressions
    if (Math.random() < 0.002 * this.hyperChaotic.emergentBehavior) {
      const smileWeight = Math.random() * 0.3;
      this.blendShapes.get('mouthSmileLeft')!.weight = smileWeight;
      this.blendShapes.get('mouthSmileRight')!.weight = smileWeight;

      setTimeout(() => {
        this.blendShapes.get('mouthSmileLeft')!.weight = 0;
        this.blendShapes.get('mouthSmileRight')!.weight = 0;
      }, 500);
    }

    // Glitch effect
    if (this.hyperChaotic.glitchIntensity > 0 && Math.random() < 0.001) {
      this.applyGlitchEffect(deltaTime);
    }
  }

  private applyGlitchEffect(deltaTime: number): void {
    // Momentary position glitch
    const originalPosition = { ...this.transform.position };

    this.transform.position.x += (Math.random() - 0.5) * 0.1 * this.hyperChaotic.glitchIntensity;
    this.transform.position.y += (Math.random() - 0.5) * 0.1 * this.hyperChaotic.glitchIntensity;

    setTimeout(() => {
      this.transform.position = originalPosition;
    }, 50);

    this.emit('glitch', { intensity: this.hyperChaotic.glitchIntensity });
  }

  private applySuperHotGirlEnhancements(): void {
    // Enhanced expressiveness
    const smileLeft = this.blendShapes.get('mouthSmileLeft');
    const smileRight = this.blendShapes.get('mouthSmileRight');

    if (smileLeft && smileRight) {
      // Subtle ambient smile based on allure
      const ambientSmile = 0.1 * this.superHotGirl.allureIndex;
      smileLeft.weight = Math.max(smileLeft.weight, ambientSmile);
      smileRight.weight = Math.max(smileRight.weight, ambientSmile);
    }

    // Eye allure - subtle widening
    const eyeWideLeft = this.blendShapes.get('eyeWideLeft');
    const eyeWideRight = this.blendShapes.get('eyeWideRight');

    if (eyeWideLeft && eyeWideRight) {
      const ambientWide = 0.05 * this.superHotGirl.allureIndex;
      eyeWideLeft.weight = Math.max(eyeWideLeft.weight, ambientWide);
      eyeWideRight.weight = Math.max(eyeWideRight.weight, ambientWide);
    }

    // Material enhancements
    const skinMaterial = this.materials.get('skin');
    if (skinMaterial && skinMaterial.subsurfaceScattering) {
      skinMaterial.subsurfaceScattering.scale = 0.5 * this.superHotGirl.materialQuality;
    }

    const eyeMaterial = this.materials.get('eyes');
    if (eyeMaterial) {
      eyeMaterial.emissiveIntensity = 0.2 * this.superHotGirl.allureIndex;
    }
  }

  private updateAnimations(deltaTime: number): void {
    this.activeAnimations.forEach((state, name) => {
      state.time += deltaTime;

      if (state.time >= state.clip.duration) {
        if (state.clip.loop) {
          state.time = state.time % state.clip.duration;
        } else {
          this.activeAnimations.delete(name);
          return;
        }
      }

      // Apply animation tracks
      state.clip.tracks.forEach(track => {
        const bone = this.skeleton.get(track.boneName);
        if (!bone) return;

        // Find keyframe indices
        let keyIndex = 0;
        for (let i = 0; i < track.times.length - 1; i++) {
          if (state.time >= track.times[i] && state.time < track.times[i + 1]) {
            keyIndex = i;
            break;
          }
        }

        // Interpolate values (simplified)
        const t = (state.time - track.times[keyIndex]) /
          (track.times[keyIndex + 1] - track.times[keyIndex]);

        // Apply to bone transform based on property
        // (actual implementation would use proper interpolation)
      });
    });
  }

  public playAnimation(name: string, weight: number = 1.0): void {
    const clip = this.animations.get(name);
    if (!clip) return;

    this.activeAnimations.set(name, { clip, time: 0, weight });
    this.emit('animation:start', name);
  }

  public stopAnimation(name: string): void {
    this.activeAnimations.delete(name);
    this.emit('animation:stop', name);
  }

  public setBlendShape(name: string, weight: number): void {
    const blendShape = this.blendShapes.get(name);
    if (blendShape) {
      blendShape.weight = Math.max(0, Math.min(1, weight));
    }
  }

  public setExpression(expression: string): void {
    // Reset all expression blend shapes
    this.blendShapes.forEach(bs => {
      if (!bs.name.startsWith('viseme_')) {
        bs.weight = 0;
      }
    });

    // Apply expression
    switch (expression) {
      case 'happy':
        this.setBlendShape('mouthSmileLeft', 0.8);
        this.setBlendShape('mouthSmileRight', 0.8);
        this.setBlendShape('cheekSquintLeft', 0.3);
        this.setBlendShape('cheekSquintRight', 0.3);
        break;
      case 'excited':
        this.setBlendShape('mouthSmileLeft', 1.0);
        this.setBlendShape('mouthSmileRight', 1.0);
        this.setBlendShape('eyeWideLeft', 0.5);
        this.setBlendShape('eyeWideRight', 0.5);
        this.setBlendShape('browInnerUp', 0.5);
        break;
      case 'mysterious':
        this.setBlendShape('mouthSmileLeft', 0.2);
        this.setBlendShape('mouthSmileRight', 0.3);
        this.setBlendShape('eyeSquintLeft', 0.3);
        this.setBlendShape('eyeSquintRight', 0.3);
        break;
      case 'neutral':
      default:
        // All blend shapes already reset
        break;
    }

    this.emit('expression:change', expression);
  }

  public updateLipSync(visemeWeights: Map<string, number>): void {
    visemeWeights.forEach((weight, viseme) => {
      this.setBlendShape(viseme, weight);
    });
  }

  public lookAt(target: Vector3): void {
    const head = this.skeleton.get('Head');
    const leftEye = this.skeleton.get('LeftEye');
    const rightEye = this.skeleton.get('RightEye');

    if (!head) return;

    // Calculate look direction (simplified)
    const dx = target.x - this.transform.position.x;
    const dy = target.y - this.transform.position.y;
    const dz = target.z - this.transform.position.z;

    const yaw = Math.atan2(dx, dz);
    const pitch = Math.atan2(dy, Math.sqrt(dx * dx + dz * dz));

    // Apply to head (limited range)
    head.localTransform.rotation.y = Math.max(-0.5, Math.min(0.5, yaw * 0.5));
    head.localTransform.rotation.x = Math.max(-0.3, Math.min(0.3, pitch * 0.5));

    // Eyes follow more directly
    if (leftEye) {
      leftEye.localTransform.rotation.y = Math.max(-0.3, Math.min(0.3, yaw * 0.8));
      leftEye.localTransform.rotation.x = Math.max(-0.2, Math.min(0.2, pitch * 0.8));
    }
    if (rightEye) {
      rightEye.localTransform.rotation.y = Math.max(-0.3, Math.min(0.3, yaw * 0.8));
      rightEye.localTransform.rotation.x = Math.max(-0.2, Math.min(0.2, pitch * 0.8));
    }

    // Update eye blend shapes for look direction
    this.setBlendShape('eyeLookInLeft', Math.max(0, -yaw));
    this.setBlendShape('eyeLookOutLeft', Math.max(0, yaw));
    this.setBlendShape('eyeLookInRight', Math.max(0, yaw));
    this.setBlendShape('eyeLookOutRight', Math.max(0, -yaw));
    this.setBlendShape('eyeLookUpLeft', Math.max(0, pitch));
    this.setBlendShape('eyeLookUpRight', Math.max(0, pitch));
    this.setBlendShape('eyeLookDownLeft', Math.max(0, -pitch));
    this.setBlendShape('eyeLookDownRight', Math.max(0, -pitch));
  }

  public setEmotionalState(emotion: number): void {
    this.emotionalState = Math.max(-1, Math.min(1, emotion));

    if (this.emotionalState > 0.5) {
      this.setExpression('excited');
    } else if (this.emotionalState > 0) {
      this.setExpression('happy');
    } else if (this.emotionalState < -0.5) {
      this.setExpression('mysterious');
    } else {
      this.setExpression('neutral');
    }
  }

  public getState(): object {
    return {
      id: this.id,
      transform: this.transform,
      emotionalState: this.emotionalState,
      chaoticState: this.chaoticState,
      superHotGirl: this.superHotGirl,
      hyperChaotic: this.hyperChaotic,
      activeAnimations: Array.from(this.activeAnimations.keys()),
      blendShapeCount: this.blendShapes.size,
      boneCount: this.skeleton.size
    };
  }

  public serialize(): object {
    return {
      id: this.id,
      config: this.config,
      transform: this.transform,
      emotionalState: this.emotionalState,
      superHotGirl: this.superHotGirl,
      hyperChaotic: this.hyperChaotic
    };
  }
}

export default ThreeJSAvatar;
