/**
 * Chaotic Attractor Systems for Deep Tree Echo
 *
 * Implements various strange attractor dynamics (Lorenz, Rossler, Chen)
 * to generate hyper-chaotic state evolution for cognitive processing.
 */

export type AttractorType = 'lorenz' | 'rossler' | 'chen' | 'hyperchaotic';

export interface AttractorConfig {
  sigma?: number;
  rho?: number;
  beta?: number;
  a?: number;
  b?: number;
  c?: number;
  d?: number;
}

export class ChaoticAttractor {
  private type: AttractorType;
  private state: number[];
  private config: AttractorConfig;
  private history: number[][] = [];
  private maxHistorySize: number = 1000;

  constructor(type: AttractorType = 'lorenz', chaosCoefficient: number = 1.0) {
    this.type = type;
    this.state = this.initializeState();
    this.config = this.initializeConfig(chaosCoefficient);
  }

  private initializeState(): number[] {
    // Initialize with small random perturbations
    return [
      0.1 + Math.random() * 0.01,
      0.0 + Math.random() * 0.01,
      0.0 + Math.random() * 0.01,
      0.0 + Math.random() * 0.01 // Fourth dimension for hyperchaotic
    ];
  }

  private initializeConfig(chaosCoefficient: number): AttractorConfig {
    const baseConfigs: Record<AttractorType, AttractorConfig> = {
      lorenz: {
        sigma: 10.0 * chaosCoefficient,
        rho: 28.0 * chaosCoefficient,
        beta: 8.0 / 3.0 * chaosCoefficient
      },
      rossler: {
        a: 0.2 * chaosCoefficient,
        b: 0.2 * chaosCoefficient,
        c: 5.7 * chaosCoefficient
      },
      chen: {
        a: 35.0 * chaosCoefficient,
        b: 3.0 * chaosCoefficient,
        c: 28.0 * chaosCoefficient
      },
      hyperchaotic: {
        a: 36.0 * chaosCoefficient,
        b: 3.0 * chaosCoefficient,
        c: 28.0 * chaosCoefficient,
        d: 16.0 * chaosCoefficient
      }
    };

    return baseConfigs[this.type];
  }

  public step(dt: number): number[] {
    switch (this.type) {
      case 'lorenz':
        return this.lorenzStep(dt);
      case 'rossler':
        return this.rosslerStep(dt);
      case 'chen':
        return this.chenStep(dt);
      case 'hyperchaotic':
        return this.hyperchaotichStep(dt);
      default:
        return this.lorenzStep(dt);
    }
  }

  private lorenzStep(dt: number): number[] {
    const [x, y, z] = this.state;
    const { sigma, rho, beta } = this.config;

    const dx = sigma! * (y - x);
    const dy = x * (rho! - z) - y;
    const dz = x * y - beta! * z;

    this.state[0] += dx * dt;
    this.state[1] += dy * dt;
    this.state[2] += dz * dt;

    this.recordHistory();
    return this.state.slice();
  }

  private rosslerStep(dt: number): number[] {
    const [x, y, z] = this.state;
    const { a, b, c } = this.config;

    const dx = -y - z;
    const dy = x + a! * y;
    const dz = b! + z * (x - c!);

    this.state[0] += dx * dt;
    this.state[1] += dy * dt;
    this.state[2] += dz * dt;

    this.recordHistory();
    return this.state.slice();
  }

  private chenStep(dt: number): number[] {
    const [x, y, z] = this.state;
    const { a, b, c } = this.config;

    const dx = a! * (y - x);
    const dy = (c! - a!) * x - x * z + c! * y;
    const dz = x * y - b! * z;

    this.state[0] += dx * dt;
    this.state[1] += dy * dt;
    this.state[2] += dz * dt;

    this.recordHistory();
    return this.state.slice();
  }

  private hyperchaotichStep(dt: number): number[] {
    const [x, y, z, w] = this.state;
    const { a, b, c, d } = this.config;

    const dx = a! * (y - x) + w;
    const dy = (c! - a!) * x - x * z + c! * y;
    const dz = x * y - b! * z;
    const dw = -d! * x;

    this.state[0] += dx * dt;
    this.state[1] += dy * dt;
    this.state[2] += dz * dt;
    this.state[3] += dw * dt;

    this.recordHistory();
    return this.state.slice();
  }

  private recordHistory(): void {
    this.history.push(this.state.slice());
    if (this.history.length > this.maxHistorySize) {
      this.history.shift();
    }
  }

  public getCurrentState(): number[] {
    return this.state.slice();
  }

  public getHistory(): number[][] {
    return this.history.slice();
  }

  public getLyapunovExponent(): number {
    // Compute approximate largest Lyapunov exponent
    if (this.history.length < 100) return 0;

    let sum = 0;
    const n = Math.min(100, this.history.length - 1);

    for (let i = 0; i < n; i++) {
      const prev = this.history[this.history.length - n + i - 1];
      const curr = this.history[this.history.length - n + i];

      let dist = 0;
      for (let j = 0; j < prev.length; j++) {
        dist += (curr[j] - prev[j]) ** 2;
      }

      if (dist > 0) {
        sum += Math.log(Math.sqrt(dist));
      }
    }

    return sum / n;
  }

  public getAttractorDimension(): number {
    // Kaplan-Yorke dimension estimate
    const lyapunov = this.getLyapunovExponent();
    return 2 + Math.abs(lyapunov) / (1 + Math.abs(lyapunov));
  }

  public setType(type: AttractorType): void {
    this.type = type;
    this.config = this.initializeConfig(1.0);
  }

  public perturb(magnitude: number = 0.01): void {
    for (let i = 0; i < this.state.length; i++) {
      this.state[i] += (Math.random() - 0.5) * magnitude;
    }
  }

  public reset(): void {
    this.state = this.initializeState();
    this.history = [];
  }

  public serialize(): object {
    return {
      type: this.type,
      state: this.state,
      config: this.config,
      historyLength: this.history.length,
      lyapunovExponent: this.getLyapunovExponent(),
      dimension: this.getAttractorDimension()
    };
  }
}

export default ChaoticAttractor;
