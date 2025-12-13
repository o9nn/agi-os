/**
 * Avatar Bridge
 *
 * Synchronizes state between Live2D and 3D avatar systems,
 * enabling seamless hybrid rendering and consistent behavior.
 */

import { EventEmitter } from 'eventemitter3';
import { Live2DAvatar } from '../live2d/Live2DAvatar';
import { ThreeJSAvatar } from '../threejs/ThreeJSAvatar';

export interface BridgeConfig {
  syncExpression: boolean;
  syncLipSync: boolean;
  syncEyeTracking: boolean;
  syncEmotion: boolean;
  live2dPrimary: boolean;
}

export class AvatarBridge extends EventEmitter {
  private live2d: Live2DAvatar;
  private threejs: ThreeJSAvatar;
  private config: BridgeConfig;
  private isSyncing: boolean = false;

  constructor(
    live2d: Live2DAvatar,
    threejs: ThreeJSAvatar,
    config: Partial<BridgeConfig> = {}
  ) {
    super();

    this.live2d = live2d;
    this.threejs = threejs;

    this.config = {
      syncExpression: true,
      syncLipSync: true,
      syncEyeTracking: true,
      syncEmotion: true,
      live2dPrimary: true,
      ...config
    };

    this.setupEventListeners();
    console.log('[AvatarBridge] Bridge established between Live2D and 3D avatars');
  }

  private setupEventListeners(): void {
    // Sync from Live2D to 3D
    if (this.config.live2dPrimary) {
      this.live2d.on('expression:change', (expression) => {
        if (this.config.syncExpression && !this.isSyncing) {
          this.isSyncing = true;
          this.threejs.setExpression(expression);
          this.isSyncing = false;
          this.emit('sync', { type: 'expression', source: 'live2d', target: 'threejs', value: expression });
        }
      });
    } else {
      // Sync from 3D to Live2D
      this.threejs.on('expression:change', (expression) => {
        if (this.config.syncExpression && !this.isSyncing) {
          this.isSyncing = true;
          this.live2d.setExpression(expression);
          this.isSyncing = false;
          this.emit('sync', { type: 'expression', source: 'threejs', target: 'live2d', value: expression });
        }
      });
    }
  }

  public setExpression(expression: string): void {
    this.isSyncing = true;
    this.live2d.setExpression(expression);
    this.threejs.setExpression(expression);
    this.isSyncing = false;
    this.emit('expression', expression);
  }

  public setEmotionalState(emotion: number): void {
    if (!this.config.syncEmotion) return;

    this.isSyncing = true;
    this.live2d.setEmotionalState(emotion);
    this.threejs.setEmotionalState(emotion);
    this.isSyncing = false;
    this.emit('emotion', emotion);
  }

  public updateLipSync(audioLevel: number): void {
    if (!this.config.syncLipSync) return;

    this.live2d.updateLipSync(audioLevel);

    // Convert audio level to viseme weights for 3D avatar
    const visemeWeights = new Map<string, number>();
    visemeWeights.set('viseme_aa', audioLevel * 0.8);
    visemeWeights.set('viseme_O', audioLevel * 0.4);
    this.threejs.updateLipSync(visemeWeights);
  }

  public updateEyeTracking(x: number, y: number): void {
    if (!this.config.syncEyeTracking) return;

    this.live2d.updateEyeTracking(x, y);
    this.threejs.lookAt({ x: x * 2, y: y * 2, z: 1 });
  }

  public getState(): object {
    return {
      config: this.config,
      live2d: this.live2d.getState(),
      threejs: this.threejs.getState()
    };
  }

  public setConfig(config: Partial<BridgeConfig>): void {
    this.config = { ...this.config, ...config };
    this.emit('config:change', this.config);
  }
}

export default AvatarBridge;
