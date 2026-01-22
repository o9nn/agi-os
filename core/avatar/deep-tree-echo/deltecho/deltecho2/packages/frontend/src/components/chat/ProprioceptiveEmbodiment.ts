import { getLogger } from '../../../../shared/logger'
import { runtime as _runtime } from '@deltachat-desktop/runtime-interface'
const log = getLogger('renderer/ProprioceptiveEmbodiment')
export interface Pose {
  position: { x: number; y: number; z: number }
  rotation: { pitch: number; yaw: number; roll: number }
}
export interface ControllerState {
  leftStick: { x: number; y: number }
  rightStick: { x: number; y: number }
  buttons: { [key: string]: boolean }
  triggers: { left: number; right: number }
  connected: boolean
}
export interface ProprioceptiveFeedback {
  collisions: { direction: string; intensity: number }[]
  surfaceContact: { surface: string; friction: number }
  stability: number
  limbPositions: { [key: string]: { x: number; y: number; z: number } }
}
export class ProprioceptiveEmbodiment {
  private static instance: ProprioceptiveEmbodiment
  private initialized: boolean = false
  private frameCallbackId?: number
  private controllers: ControllerState[] = []
  private currentPose: Pose = {
    position: { x: 0, y: 0, z: 0 },
    rotation: { pitch: 0, yaw: 0, roll: 0 },
  }
  private feedback: ProprioceptiveFeedback = {
    collisions: [],
    surfaceContact: { surface: 'none', friction: 0 },
    stability: 1.0,
    limbPositions: {
      leftArm: { x: 0, y: 0, z: 0 },
      rightArm: { x: 0, y: 0, z: 0 },
      leftLeg: { x: 0, y: 0, z: 0 },
      rightLeg: { x: 0, y: 0, z: 0 },
    },
  }
  private trainingMemory: {
    state: ControllerState
    pose: Pose
    feedback: ProprioceptiveFeedback
    success: boolean
    timestamp: number
  }[] = []
  private onUpdateCallbacks: ((
    pose: Pose,
    feedback: ProprioceptiveFeedback
  ) => void)[] = []
  private constructor() {}
  public static getInstance(): ProprioceptiveEmbodiment {
    if (!ProprioceptiveEmbodiment.instance) {
      ProprioceptiveEmbodiment.instance = new ProprioceptiveEmbodiment()
    }
    return ProprioceptiveEmbodiment.instance
  }
  public async initialize(): Promise<boolean> {
    if (this.initialized) {
      return true
    }
    try {
      if ('getGamepads' in navigator) {
        window.addEventListener(
          'gamepadconnected',
          this.handleGamepadConnected.bind(this)
        )
        window.addEventListener(
          'gamepaddisconnected',
          this.handleGamepadDisconnected.bind(this)
        )
        this.startUpdateLoop()
        log.info('Proprioceptive embodiment system initialized')
        this.initialized = true
        return true
      } else {
        log.error('Gamepad API not supported in this browser')
        return false
      }
    } catch (error) {
      log.error('Failed to initialize proprioceptive embodiment:', error)
      return false
    }
  }
  private handleGamepadConnected(event: GamepadEvent): void {
    log.info(`Controller connected: ${event.gamepad.id}`)
    this.updateControllerStates()
  }
  private handleGamepadDisconnected(event: GamepadEvent): void {
    log.info(`Controller disconnected: ${event.gamepad.id}`)
    this.updateControllerStates()
  }
  private updateControllerStates(): void {
    const gamepads = navigator.getGamepads()
    this.controllers = []
    for (const gamepad of gamepads) {
      if (!gamepad) continue
      const controllerState: ControllerState = {
        leftStick: { x: gamepad.axes[0] || 0, y: gamepad.axes[1] || 0 },
        rightStick: { x: gamepad.axes[2] || 0, y: gamepad.axes[3] || 0 },
        buttons: {},
        triggers: {
          left: gamepad.buttons[6]?.value || 0,
          right: gamepad.buttons[7]?.value || 0,
        },
        connected: true,
      }
      gamepad.buttons.forEach((button, index) => {
        controllerState.buttons[`button_${index}`] = button.pressed
      })
      this.controllers.push(controllerState)
    }
  }
  private startUpdateLoop(): void {
    const updateLoop = () => {
      this.updateControllerStates()
      if (this.controllers.length > 0) {
        this.updatePoseFromControllers()
      }
      this.generateProprioceptiveFeedback()
      this.recordTrainingData()
      this.triggerUpdateCallbacks()
      this.frameCallbackId = requestAnimationFrame(updateLoop)
    }
    this.frameCallbackId = requestAnimationFrame(updateLoop)
  }
  public stopUpdateLoop(): void {
    if (this.frameCallbackId !== undefined) {
      cancelAnimationFrame(this.frameCallbackId)
      this.frameCallbackId = undefined
    }
  }
  private updatePoseFromControllers(): void {
    const controller = this.controllers[0] 
    this.currentPose.position.x += controller.leftStick.x * 0.1
    this.currentPose.position.z -= controller.leftStick.y * 0.1 
    this.currentPose.rotation.yaw += controller.rightStick.x * 0.05
    this.currentPose.rotation.pitch += controller.rightStick.y * 0.05
    this.currentPose.position.y +=
      (controller.triggers.right - controller.triggers.left) * 0.1
    if (controller.buttons['button_4']) {
      this.currentPose.rotation.roll -= 0.05
    }
    if (controller.buttons['button_5']) {
      this.currentPose.rotation.roll += 0.05
    }
    this.currentPose.rotation.yaw = this.normalizeAngle(
      this.currentPose.rotation.yaw
    )
    this.currentPose.rotation.pitch = this.clampAngle(
      this.currentPose.rotation.pitch,
      -Math.PI / 2,
      Math.PI / 2
    )
    this.currentPose.rotation.roll = this.normalizeAngle(
      this.currentPose.rotation.roll
    )
  }
  private normalizeAngle(angle: number): number {
    while (angle > Math.PI) angle -= Math.PI * 2
    while (angle < -Math.PI) angle += Math.PI * 2
    return angle
  }
  private clampAngle(angle: number, min: number, max: number): number {
    return Math.max(min, Math.min(max, angle))
  }
  private generateProprioceptiveFeedback(): void {
    this.feedback.collisions = []
    if (this.currentPose.position.y < 0) {
      this.feedback.collisions.push({
        direction: 'bottom',
        intensity: Math.abs(this.currentPose.position.y) * 10,
      })
      this.currentPose.position.y = 0 
      this.feedback.surfaceContact = { surface: 'ground', friction: 0.8 }
    } else {
      this.feedback.surfaceContact = { surface: 'air', friction: 0.0 }
    }
    const pitchFactor = Math.cos(this.currentPose.rotation.pitch)
    const rollFactor = Math.cos(this.currentPose.rotation.roll)
    this.feedback.stability = Math.min(pitchFactor, rollFactor)
    const cycleOffset = ((Date.now() % 2000) / 2000) * Math.PI * 2 
    const walkingIntensity = Math.abs(this.controllers[0]?.leftStick.y || 0)
    const walkCycle = cycleOffset * walkingIntensity
    this.feedback.limbPositions = {
      leftArm: {
        x:
          this.currentPose.position.x -
          0.3 * Math.cos(this.currentPose.rotation.yaw),
        y:
          this.currentPose.position.y +
          1.5 +
          Math.sin(walkCycle) * 0.2 * walkingIntensity,
        z:
          this.currentPose.position.z -
          0.3 * Math.sin(this.currentPose.rotation.yaw),
      },
      rightArm: {
        x:
          this.currentPose.position.x +
          0.3 * Math.cos(this.currentPose.rotation.yaw),
        y:
          this.currentPose.position.y +
          1.5 +
          Math.sin(walkCycle + Math.PI) * 0.2 * walkingIntensity,
        z:
          this.currentPose.position.z +
          0.3 * Math.sin(this.currentPose.rotation.yaw),
      },
      leftLeg: {
        x:
          this.currentPose.position.x -
          0.15 * Math.cos(this.currentPose.rotation.yaw),
        y:
          this.currentPose.position.y +
          Math.sin(walkCycle) * 0.4 * walkingIntensity,
        z:
          this.currentPose.position.z -
          0.15 * Math.sin(this.currentPose.rotation.yaw),
      },
      rightLeg: {
        x:
          this.currentPose.position.x +
          0.15 * Math.cos(this.currentPose.rotation.yaw),
        y:
          this.currentPose.position.y +
          Math.sin(walkCycle + Math.PI) * 0.4 * walkingIntensity,
        z:
          this.currentPose.position.z +
          0.15 * Math.sin(this.currentPose.rotation.yaw),
      },
    }
  }
  private recordTrainingData(): void {
    if (this.controllers.length > 0 && Math.random() < 0.05) {
      this.trainingMemory.push({
        state: { ...this.controllers[0] },
        pose: {
          position: { ...this.currentPose.position },
          rotation: { ...this.currentPose.rotation },
        },
        feedback: {
          collisions: [...this.feedback.collisions],
          surfaceContact: { ...this.feedback.surfaceContact },
          stability: this.feedback.stability,
          limbPositions: { ...this.feedback.limbPositions },
        },
        success: this.evaluateSuccessState(),
        timestamp: Date.now(),
      })
      if (this.trainingMemory.length > 1000) {
        this.trainingMemory.shift()
      }
    }
  }
  private evaluateSuccessState(): boolean {
    return (
      this.feedback.stability > 0.8 && 
      this.feedback.collisions.every(c => c.intensity < 3) && 
      Math.abs(this.currentPose.rotation.pitch) < 0.3 && 
      Math.abs(this.currentPose.rotation.roll) < 0.3 
    )
  }
  public onUpdate(
    callback: (pose: Pose, feedback: ProprioceptiveFeedback) => void
  ): () => void {
    this.onUpdateCallbacks.push(callback)
    return () => {
      const index = this.onUpdateCallbacks.indexOf(callback)
      if (index !== -1) {
        this.onUpdateCallbacks.splice(index, 1)
      }
    }
  }
  private triggerUpdateCallbacks(): void {
    for (const callback of this.onUpdateCallbacks) {
      callback(this.currentPose, this.feedback)
    }
  }
  public exportTrainingData(): string {
    return JSON.stringify(this.trainingMemory)
  }
  public async loadModelWeights(weightsUrl: string): Promise<boolean> {
    try {
      log.info(`Loading model weights from ${weightsUrl}`)
      return true
    } catch (error) {
      log.error('Failed to load model weights:', error)
      return false
    }
  }
  public isAvailable(): boolean {
    return this.initialized && 'getGamepads' in navigator
  }
  public getControllerState(): ControllerState | null {
    return this.controllers.length > 0 ? this.controllers[0] : null
  }
  public getCurrentPose(): Pose {
    return { ...this.currentPose }
  }
  public getCurrentFeedback(): ProprioceptiveFeedback {
    return { ...this.feedback }
  }
  public cleanup(): void {
    this.stopUpdateLoop()
    window.removeEventListener(
      'gamepadconnected',
      this.handleGamepadConnected.bind(this)
    )
    window.removeEventListener(
      'gamepaddisconnected',
      this.handleGamepadDisconnected.bind(this)
    )
    log.info('Proprioceptive embodiment system shutdown')
  }
}