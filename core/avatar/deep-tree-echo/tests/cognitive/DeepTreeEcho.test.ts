/**
 * Deep Tree Echo Cognitive System Tests
 */

import { DeepTreeEcho } from '../../src/cognitive/deep_tree/DeepTreeEcho';
import { ChaoticAttractor } from '../../src/cognitive/deep_tree/ChaoticAttractor';
import { ReservoirNetwork } from '../../src/cognitive/deep_tree/ReservoirNetwork';

describe('DeepTreeEcho', () => {
  let cognitive: DeepTreeEcho;

  beforeEach(() => {
    cognitive = new DeepTreeEcho({
      treeDepth: 5,
      branchingFactor: 3,
      hyperChaoticMode: true
    });
  });

  afterEach(() => {
    // Clean up any intervals
  });

  describe('initialization', () => {
    it('should initialize with default config', () => {
      const system = new DeepTreeEcho();
      expect(system).toBeDefined();
      const stats = system.getEvolutionStats();
      expect(stats.thoughtCount).toBeGreaterThan(0);
    });

    it('should accept custom configuration', () => {
      const system = new DeepTreeEcho({
        treeDepth: 10,
        chaosCoefficient: 0.5
      });
      expect(system).toBeDefined();
    });
  });

  describe('thinking', () => {
    it('should process thoughts and return nodes', () => {
      const thoughts = cognitive.think('test thought input');
      expect(thoughts).toBeDefined();
      expect(thoughts.length).toBeGreaterThan(0);
      expect(thoughts[0].content).toBeDefined();
    });

    it('should generate hyper-chaotic branches', () => {
      const thoughts = cognitive.think('complex thought requiring branching');
      expect(thoughts.length).toBeGreaterThanOrEqual(1);
    });

    it('should emit thought events', (done) => {
      cognitive.on('thought', (data) => {
        expect(data.input).toBeDefined();
        expect(data.newThoughts).toBeDefined();
        done();
      });

      cognitive.think('event test');
    });
  });

  describe('synthesis', () => {
    it('should synthesize responses', async () => {
      const result = await cognitive.synthesize('test query');
      expect(result).toBeDefined();
      const parsed = JSON.parse(result);
      expect(parsed.query).toBe('test query');
    });
  });

  describe('evolution', () => {
    it('should track evolution statistics', () => {
      const stats = cognitive.getEvolutionStats();
      expect(stats.evolutionCounter).toBeDefined();
      expect(stats.thoughtCount).toBeDefined();
      expect(stats.chaoticState).toBeDefined();
    });

    it('should evolve over time', async () => {
      const initialStats = cognitive.getEvolutionStats();
      await new Promise(resolve => setTimeout(resolve, 200));
      const laterStats = cognitive.getEvolutionStats();
      expect(laterStats.evolutionCounter).toBeGreaterThan(initialStats.evolutionCounter as number);
    });
  });

  describe('state management', () => {
    it('should return current cognitive state', () => {
      const state = cognitive.getState();
      expect(state.consciousness).toBeDefined();
      expect(state.activeThoughts).toBeDefined();
      expect(state.echoes).toBeDefined();
      expect(state.chaoticDynamics).toBeDefined();
    });
  });
});

describe('ChaoticAttractor', () => {
  describe('lorenz attractor', () => {
    it('should initialize with lorenz type', () => {
      const attractor = new ChaoticAttractor('lorenz', 1.0);
      expect(attractor.getCurrentState()).toBeDefined();
      expect(attractor.getCurrentState().length).toBe(4);
    });

    it('should evolve state over time', () => {
      const attractor = new ChaoticAttractor('lorenz', 1.0);
      const initial = [...attractor.getCurrentState()];

      for (let i = 0; i < 100; i++) {
        attractor.step(0.01);
      }

      const final = attractor.getCurrentState();
      expect(final[0]).not.toBe(initial[0]);
    });

    it('should compute Lyapunov exponent', () => {
      const attractor = new ChaoticAttractor('lorenz', 1.0);

      for (let i = 0; i < 200; i++) {
        attractor.step(0.01);
      }

      const lyapunov = attractor.getLyapunovExponent();
      expect(typeof lyapunov).toBe('number');
    });
  });

  describe('hyperchaotic attractor', () => {
    it('should support 4D hyperchaotic dynamics', () => {
      const attractor = new ChaoticAttractor('hyperchaotic', 1.0);
      const state = attractor.getCurrentState();
      expect(state.length).toBe(4);
    });
  });

  describe('perturbation', () => {
    it('should allow state perturbation', () => {
      const attractor = new ChaoticAttractor('lorenz', 1.0);
      const before = [...attractor.getCurrentState()];
      attractor.perturb(0.1);
      const after = attractor.getCurrentState();

      expect(after[0]).not.toBe(before[0]);
    });
  });
});

describe('ReservoirNetwork', () => {
  let reservoir: ReservoirNetwork;

  beforeEach(() => {
    reservoir = new ReservoirNetwork(256, 0.9, 1.0);
  });

  it('should process input vectors', () => {
    const input = new Float32Array(256);
    for (let i = 0; i < 256; i++) {
      input[i] = Math.random();
    }

    const output = reservoir.process(input);
    expect(output).toBeDefined();
    expect(output.length).toBe(256);
  });

  it('should maintain state between processing', () => {
    const input1 = new Float32Array(256).fill(0.5);
    const input2 = new Float32Array(256).fill(0.5);

    const output1 = reservoir.process(input1);
    const output2 = reservoir.process(input2);

    // Outputs should be different due to recurrent dynamics
    expect(output1[0]).not.toBe(output2[0]);
  });

  it('should compute energy', () => {
    const input = new Float32Array(256).fill(0.5);
    reservoir.process(input);

    const energy = reservoir.getEnergy();
    expect(energy).toBeGreaterThan(0);
  });

  it('should compute entropy', () => {
    const input = new Float32Array(256).fill(0.5);
    reservoir.process(input);

    const entropy = reservoir.getEntropy();
    expect(entropy).toBeGreaterThanOrEqual(0);
  });

  it('should allow state reset', () => {
    const input = new Float32Array(256).fill(1.0);
    reservoir.process(input);
    const beforeReset = reservoir.getEnergy();

    reservoir.reset();
    const afterReset = reservoir.getEnergy();

    expect(afterReset).toBeLessThan(beforeReset);
  });
});
