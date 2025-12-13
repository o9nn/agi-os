/**
 * Reservoir Computing Network for Deep Tree Echo
 *
 * Implements echo state networks with chaotic dynamics for
 * temporal pattern processing and state memory.
 */

export class ReservoirNetwork {
  private size: number;
  private inputWeights: Float32Array;
  private reservoirWeights: Float32Array;
  private state: Float32Array;
  private leakRate: number;
  private spectralRadius: number;
  private inputScaling: number;
  private noiseLevel: number;

  constructor(
    size: number = 1024,
    spectralRadius: number = 0.9,
    chaosCoefficient: number = 1.0
  ) {
    this.size = size;
    this.spectralRadius = spectralRadius * chaosCoefficient;
    this.leakRate = 0.3;
    this.inputScaling = 1.0;
    this.noiseLevel = 0.001 * chaosCoefficient;

    this.inputWeights = this.initializeInputWeights();
    this.reservoirWeights = this.initializeReservoirWeights();
    this.state = new Float32Array(size);

    this.initializeState();
  }

  private initializeInputWeights(): Float32Array {
    const weights = new Float32Array(this.size);
    for (let i = 0; i < this.size; i++) {
      weights[i] = (Math.random() - 0.5) * 2 * this.inputScaling;
    }
    return weights;
  }

  private initializeReservoirWeights(): Float32Array {
    // Sparse random matrix with specified spectral radius
    const weights = new Float32Array(this.size * this.size);
    const sparsity = 0.1;

    for (let i = 0; i < this.size; i++) {
      for (let j = 0; j < this.size; j++) {
        if (Math.random() < sparsity) {
          weights[i * this.size + j] = (Math.random() - 0.5) * 2;
        }
      }
    }

    // Normalize to spectral radius
    this.normalizeSpectralRadius(weights);

    return weights;
  }

  private normalizeSpectralRadius(weights: Float32Array): void {
    // Power iteration to estimate spectral radius
    let eigenVector = new Float32Array(this.size);
    for (let i = 0; i < this.size; i++) {
      eigenVector[i] = Math.random();
    }

    for (let iter = 0; iter < 100; iter++) {
      const newVector = new Float32Array(this.size);

      for (let i = 0; i < this.size; i++) {
        for (let j = 0; j < this.size; j++) {
          newVector[i] += weights[i * this.size + j] * eigenVector[j];
        }
      }

      // Normalize
      let norm = 0;
      for (let i = 0; i < this.size; i++) {
        norm += newVector[i] * newVector[i];
      }
      norm = Math.sqrt(norm);

      if (norm > 0) {
        for (let i = 0; i < this.size; i++) {
          newVector[i] /= norm;
        }
      }

      eigenVector = newVector;
    }

    // Estimate spectral radius
    let radius = 0;
    const tempVector = new Float32Array(this.size);

    for (let i = 0; i < this.size; i++) {
      for (let j = 0; j < this.size; j++) {
        tempVector[i] += weights[i * this.size + j] * eigenVector[j];
      }
    }

    for (let i = 0; i < this.size; i++) {
      radius += tempVector[i] * eigenVector[i];
    }

    // Scale weights
    if (radius > 0) {
      const scale = this.spectralRadius / Math.abs(radius);
      for (let i = 0; i < weights.length; i++) {
        weights[i] *= scale;
      }
    }
  }

  private initializeState(): void {
    for (let i = 0; i < this.size; i++) {
      this.state[i] = (Math.random() - 0.5) * 0.1;
    }
  }

  public process(input: Float32Array): Float32Array {
    const newState = new Float32Array(this.size);

    // Input contribution
    for (let i = 0; i < this.size && i < input.length; i++) {
      newState[i] += this.inputWeights[i] * input[i];
    }

    // Reservoir contribution
    for (let i = 0; i < this.size; i++) {
      for (let j = 0; j < this.size; j++) {
        newState[i] += this.reservoirWeights[i * this.size + j] * this.state[j];
      }
    }

    // Apply activation function (tanh) with leaky integration
    for (let i = 0; i < this.size; i++) {
      const activation = Math.tanh(newState[i]);
      this.state[i] = (1 - this.leakRate) * this.state[i] + this.leakRate * activation;

      // Add noise for chaotic dynamics
      this.state[i] += (Math.random() - 0.5) * this.noiseLevel;
    }

    return this.state.slice();
  }

  public getState(): Float32Array {
    return this.state.slice();
  }

  public setState(state: Float32Array): void {
    if (state.length === this.size) {
      this.state = state.slice();
    }
  }

  public getEnergy(): number {
    let energy = 0;
    for (let i = 0; i < this.size; i++) {
      energy += this.state[i] * this.state[i];
    }
    return Math.sqrt(energy / this.size);
  }

  public getEntropy(): number {
    // Shannon entropy of state distribution
    const bins = 100;
    const histogram = new Float32Array(bins);

    for (let i = 0; i < this.size; i++) {
      const value = (Math.tanh(this.state[i]) + 1) / 2; // Normalize to [0, 1]
      const bin = Math.min(Math.floor(value * bins), bins - 1);
      histogram[bin]++;
    }

    let entropy = 0;
    for (let i = 0; i < bins; i++) {
      if (histogram[i] > 0) {
        const p = histogram[i] / this.size;
        entropy -= p * Math.log2(p);
      }
    }

    return entropy;
  }

  public perturbState(magnitude: number = 0.1): void {
    for (let i = 0; i < this.size; i++) {
      this.state[i] += (Math.random() - 0.5) * magnitude;
    }
  }

  public reset(): void {
    this.initializeState();
  }

  public serialize(): object {
    return {
      size: this.size,
      leakRate: this.leakRate,
      spectralRadius: this.spectralRadius,
      energy: this.getEnergy(),
      entropy: this.getEntropy()
    };
  }
}

export default ReservoirNetwork;
