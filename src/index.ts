/**
 * Deep Tree Echo AGI Avatar System
 *
 * Main entry point for the AGI avatar system with integrated
 * cognitive architecture, Live2D and 3D avatar rendering,
 * and hyper-chaotic dynamics.
 */

import { DeepTreeEcho } from './cognitive/deep_tree/DeepTreeEcho';
import { Live2DAvatar } from './avatar/live2d/Live2DAvatar';
import { ThreeJSAvatar } from './avatar/threejs/ThreeJSAvatar';
import { AvatarServer } from './api/AvatarServer';
import { AvatarBridge } from './avatar/core/AvatarBridge';

export interface SystemConfig {
  port: number;
  avatarMode: 'live2d' | 'threejs' | 'hybrid';
  cognitiveEnabled: boolean;
  hyperChaoticMode: boolean;
  superHotGirlMode: boolean;
  debugMode: boolean;
}

export class DeepTreeEchoSystem {
  private config: SystemConfig;
  private cognitive: DeepTreeEcho;
  private live2dAvatar: Live2DAvatar | null = null;
  private threejsAvatar: ThreeJSAvatar | null = null;
  private server: AvatarServer | null = null;
  private bridge: AvatarBridge | null = null;
  private isRunning: boolean = false;

  constructor(config: Partial<SystemConfig> = {}) {
    this.config = {
      port: 3000,
      avatarMode: 'hybrid',
      cognitiveEnabled: true,
      hyperChaoticMode: true,
      superHotGirlMode: true,
      debugMode: false,
      ...config
    };

    // Initialize cognitive system
    this.cognitive = new DeepTreeEcho({
      hyperChaoticMode: this.config.hyperChaoticMode
    });

    console.log('[DeepTreeEchoSystem] System initialized');
  }

  public async start(): Promise<void> {
    console.log('[DeepTreeEchoSystem] Starting system...');

    // Initialize avatars based on mode
    if (this.config.avatarMode === 'live2d' || this.config.avatarMode === 'hybrid') {
      this.live2dAvatar = new Live2DAvatar({
        hyperChaoticMode: this.config.hyperChaoticMode
      });
    }

    if (this.config.avatarMode === 'threejs' || this.config.avatarMode === 'hybrid') {
      this.threejsAvatar = new ThreeJSAvatar({
        hyperChaoticMode: this.config.hyperChaoticMode,
        superHotGirlMode: this.config.superHotGirlMode
      });
    }

    // Create avatar bridge for hybrid mode
    if (this.config.avatarMode === 'hybrid' && this.live2dAvatar && this.threejsAvatar) {
      this.bridge = new AvatarBridge(this.live2dAvatar, this.threejsAvatar);
    }

    // Start API server
    this.server = new AvatarServer({
      port: this.config.port,
      cognitive: this.cognitive,
      live2dAvatar: this.live2dAvatar,
      threejsAvatar: this.threejsAvatar
    });

    await this.server.start();

    // Connect cognitive events to avatars
    this.setupCognitiveConnection();

    this.isRunning = true;
    console.log(`[DeepTreeEchoSystem] System running on port ${this.config.port}`);
  }

  private setupCognitiveConnection(): void {
    this.cognitive.on('thought', (data) => {
      if (this.config.debugMode) {
        console.log('[Cognitive] New thought:', data);
      }

      // Map thoughts to emotional states
      const emotionalState = this.computeEmotionalState(data);

      if (this.live2dAvatar) {
        this.live2dAvatar.setEmotionalState(emotionalState);
      }

      if (this.threejsAvatar) {
        this.threejsAvatar.setEmotionalState(emotionalState);
      }
    });

    this.cognitive.on('evolution', (data) => {
      if (this.config.debugMode) {
        console.log('[Cognitive] Evolution:', data);
      }
    });
  }

  private computeEmotionalState(thoughtData: unknown): number {
    // Simple emotional state computation based on thought patterns
    // In production, this would use sentiment analysis and cognitive metrics
    return Math.random() * 2 - 1; // Random for now, range [-1, 1]
  }

  public async stop(): Promise<void> {
    console.log('[DeepTreeEchoSystem] Stopping system...');

    if (this.server) {
      await this.server.stop();
    }

    this.isRunning = false;
    console.log('[DeepTreeEchoSystem] System stopped');
  }

  public getCognitive(): DeepTreeEcho {
    return this.cognitive;
  }

  public getLive2DAvatar(): Live2DAvatar | null {
    return this.live2dAvatar;
  }

  public getThreeJSAvatar(): ThreeJSAvatar | null {
    return this.threejsAvatar;
  }

  public getStatus(): object {
    return {
      isRunning: this.isRunning,
      config: this.config,
      cognitive: this.cognitive.getEvolutionStats(),
      live2dAvatar: this.live2dAvatar?.getState() || null,
      threejsAvatar: this.threejsAvatar?.getState() || null
    };
  }
}

// Main execution
async function main(): Promise<void> {
  const system = new DeepTreeEchoSystem({
    port: parseInt(process.env.PORT || '3000', 10),
    avatarMode: (process.env.AVATAR_MODE as SystemConfig['avatarMode']) || 'hybrid',
    cognitiveEnabled: process.env.COGNITIVE_ENABLED !== 'false',
    hyperChaoticMode: process.env.HYPER_CHAOTIC !== 'false',
    superHotGirlMode: process.env.SUPER_HOT_GIRL !== 'false',
    debugMode: process.env.DEBUG === 'true'
  });

  // Handle shutdown gracefully
  process.on('SIGINT', async () => {
    console.log('\nReceived SIGINT, shutting down...');
    await system.stop();
    process.exit(0);
  });

  process.on('SIGTERM', async () => {
    console.log('\nReceived SIGTERM, shutting down...');
    await system.stop();
    process.exit(0);
  });

  await system.start();
}

// Export for module usage
export {
  DeepTreeEcho,
  Live2DAvatar,
  ThreeJSAvatar,
  AvatarServer,
  AvatarBridge
};

export default DeepTreeEchoSystem;

// Run if executed directly
if (require.main === module) {
  main().catch(console.error);
}
