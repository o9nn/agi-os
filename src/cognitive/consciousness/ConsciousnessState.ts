/**
 * Consciousness State Manager for Deep Tree Echo
 *
 * Implements a multi-layered consciousness model with
 * attention, awareness, and metacognitive capabilities.
 */

import { EventEmitter } from 'eventemitter3';
import { v4 as uuidv4 } from 'uuid';

export interface AttentionFocus {
  id: string;
  target: string;
  intensity: number;
  duration: number;
  startTime: number;
  metadata: Record<string, unknown>;
}

export interface AwarenessLevel {
  level: 'unconscious' | 'subconscious' | 'conscious' | 'hyperconscious';
  intensity: number;
  stability: number;
}

export interface MetacognitiveState {
  selfAwareness: number;
  cognitiveLoad: number;
  emotionalValence: number;
  confidenceLevel: number;
  curiosityDrive: number;
}

export interface ConsciousnessSnapshot {
  timestamp: number;
  awareness: AwarenessLevel;
  attentionFoci: AttentionFocus[];
  metacognition: MetacognitiveState;
  thoughtStream: string[];
  dreamState: boolean;
}

export class ConsciousnessState extends EventEmitter {
  private awareness: AwarenessLevel;
  private attentionFoci: Map<string, AttentionFocus> = new Map();
  private metacognition: MetacognitiveState;
  private thoughtStream: string[] = [];
  private maxThoughtStreamSize: number = 100;
  private dreamState: boolean = false;
  private meditationMode: boolean = false;
  private evolutionCycle: number = 0;

  constructor() {
    super();
    this.awareness = this.initializeAwareness();
    this.metacognition = this.initializeMetacognition();
    this.startConsciousnessLoop();
  }

  private initializeAwareness(): AwarenessLevel {
    return {
      level: 'conscious',
      intensity: 0.8,
      stability: 0.7
    };
  }

  private initializeMetacognition(): MetacognitiveState {
    return {
      selfAwareness: 0.6,
      cognitiveLoad: 0.3,
      emotionalValence: 0.5, // Neutral
      confidenceLevel: 0.7,
      curiosityDrive: 0.8
    };
  }

  private startConsciousnessLoop(): void {
    setInterval(() => {
      this.evolveConsciousness();
    }, 500); // 2Hz consciousness update
  }

  private evolveConsciousness(): void {
    this.evolutionCycle++;

    // Update attention decay
    this.updateAttentionDecay();

    // Evolve metacognitive state
    this.evolveMetacognition();

    // Check for state transitions
    this.checkStateTransitions();

    // Dream processing during low activity
    if (this.dreamState) {
      this.processDream();
    }

    // Emit evolution event
    if (this.evolutionCycle % 20 === 0) {
      this.emit('evolution', this.getCurrentState());
    }
  }

  private updateAttentionDecay(): void {
    const now = Date.now();

    this.attentionFoci.forEach((focus, id) => {
      const elapsed = (now - focus.startTime) / 1000;

      if (elapsed > focus.duration) {
        focus.intensity *= 0.95;
      }

      if (focus.intensity < 0.1) {
        this.attentionFoci.delete(id);
      }
    });
  }

  private evolveMetacognition(): void {
    // Self-awareness fluctuations
    this.metacognition.selfAwareness += (Math.random() - 0.5) * 0.02;
    this.metacognition.selfAwareness = Math.max(0, Math.min(1, this.metacognition.selfAwareness));

    // Cognitive load based on attention
    const attentionLoad = Array.from(this.attentionFoci.values())
      .reduce((sum, focus) => sum + focus.intensity, 0);
    this.metacognition.cognitiveLoad = Math.min(1, attentionLoad / 3);

    // Curiosity drive regeneration
    if (this.metacognition.cognitiveLoad < 0.5) {
      this.metacognition.curiosityDrive = Math.min(1, this.metacognition.curiosityDrive + 0.01);
    }

    // Confidence adjustments
    if (this.metacognition.cognitiveLoad > 0.8) {
      this.metacognition.confidenceLevel *= 0.99;
    }
  }

  private checkStateTransitions(): void {
    const load = this.metacognition.cognitiveLoad;
    const awareness = this.metacognition.selfAwareness;

    if (load < 0.1 && !this.meditationMode) {
      this.enterDreamState();
    } else if (load > 0.9) {
      this.setAwarenessLevel('hyperconscious');
    } else if (awareness > 0.9) {
      this.setAwarenessLevel('hyperconscious');
    } else if (awareness < 0.3) {
      this.setAwarenessLevel('subconscious');
    } else {
      this.setAwarenessLevel('conscious');
      this.exitDreamState();
    }
  }

  private enterDreamState(): void {
    if (!this.dreamState) {
      this.dreamState = true;
      this.emit('dream:enter');
    }
  }

  private exitDreamState(): void {
    if (this.dreamState) {
      this.dreamState = false;
      this.emit('dream:exit');
    }
  }

  private processDream(): void {
    // Generate dream-like associations
    if (this.thoughtStream.length > 0) {
      const randomIdx = Math.floor(Math.random() * this.thoughtStream.length);
      const dreamThought = `dream: ${this.thoughtStream[randomIdx]}`;
      this.addThought(dreamThought);
    }
  }

  public focus(target: string, intensity: number = 0.8, duration: number = 30): AttentionFocus {
    const focus: AttentionFocus = {
      id: uuidv4(),
      target,
      intensity,
      duration,
      startTime: Date.now(),
      metadata: {}
    };

    this.attentionFoci.set(focus.id, focus);
    this.emit('attention:focus', focus);

    // Reduce curiosity when focusing
    this.metacognition.curiosityDrive = Math.max(0, this.metacognition.curiosityDrive - 0.1);

    return focus;
  }

  public unfocus(id: string): boolean {
    const removed = this.attentionFoci.delete(id);
    if (removed) {
      this.emit('attention:unfocus', id);
    }
    return removed;
  }

  public addThought(thought: string): void {
    this.thoughtStream.push(thought);
    if (this.thoughtStream.length > this.maxThoughtStreamSize) {
      this.thoughtStream.shift();
    }
    this.emit('thought:new', thought);
  }

  public setAwarenessLevel(level: AwarenessLevel['level']): void {
    const previousLevel = this.awareness.level;
    this.awareness.level = level;

    const intensityMap: Record<AwarenessLevel['level'], number> = {
      unconscious: 0.1,
      subconscious: 0.4,
      conscious: 0.7,
      hyperconscious: 1.0
    };

    this.awareness.intensity = intensityMap[level];

    if (previousLevel !== level) {
      this.emit('awareness:change', { from: previousLevel, to: level });
    }
  }

  public setEmotionalValence(valence: number): void {
    this.metacognition.emotionalValence = Math.max(-1, Math.min(1, valence));
    this.emit('emotion:change', this.metacognition.emotionalValence);
  }

  public enterMeditationMode(): void {
    this.meditationMode = true;
    this.setAwarenessLevel('hyperconscious');
    this.metacognition.selfAwareness = 1.0;
    this.emit('meditation:enter');
  }

  public exitMeditationMode(): void {
    this.meditationMode = false;
    this.setAwarenessLevel('conscious');
    this.emit('meditation:exit');
  }

  public getCurrentState(): ConsciousnessSnapshot {
    return {
      timestamp: Date.now(),
      awareness: { ...this.awareness },
      attentionFoci: Array.from(this.attentionFoci.values()),
      metacognition: { ...this.metacognition },
      thoughtStream: this.thoughtStream.slice(-10),
      dreamState: this.dreamState
    };
  }

  public getAttentionFoci(): AttentionFocus[] {
    return Array.from(this.attentionFoci.values());
  }

  public getMetacognition(): MetacognitiveState {
    return { ...this.metacognition };
  }

  public getAwareness(): AwarenessLevel {
    return { ...this.awareness };
  }

  public isInDreamState(): boolean {
    return this.dreamState;
  }

  public getCognitiveCapacity(): number {
    return 1 - this.metacognition.cognitiveLoad;
  }

  public serialize(): object {
    return {
      awareness: this.awareness,
      attentionFoci: Array.from(this.attentionFoci.entries()),
      metacognition: this.metacognition,
      thoughtStreamSize: this.thoughtStream.length,
      dreamState: this.dreamState,
      meditationMode: this.meditationMode,
      evolutionCycle: this.evolutionCycle
    };
  }
}

export default ConsciousnessState;
