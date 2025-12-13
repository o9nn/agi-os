/**
 * Hypergraph Memory System for Deep Tree Echo
 *
 * Implements a hypergraph-based memory storage system allowing
 * complex relationships between memories, supporting n-ary
 * associations and efficient semantic retrieval.
 */

import { v4 as uuidv4 } from 'uuid';

export interface MemoryNode {
  id: string;
  content: string;
  embedding: Float32Array;
  timestamp: number;
  accessCount: number;
  lastAccessed: number;
  importance: number;
  decay: number;
  metadata: Record<string, unknown>;
}

export interface HyperEdge {
  id: string;
  nodes: string[];
  relationshipType: string;
  strength: number;
  timestamp: number;
  metadata: Record<string, unknown>;
}

export interface MemoryQueryResult {
  node: MemoryNode;
  score: number;
  relatedNodes: MemoryNode[];
}

export class HypergraphMemory {
  private nodes: Map<string, MemoryNode> = new Map();
  private edges: Map<string, HyperEdge> = new Map();
  private nodeIndex: Map<string, Set<string>> = new Map(); // node -> edges
  private embeddingDimension: number = 1024;
  private decayRate: number = 0.999;
  private consolidationThreshold: number = 0.7;

  constructor() {
    this.startDecayLoop();
  }

  private startDecayLoop(): void {
    setInterval(() => {
      this.applyDecay();
    }, 60000); // Decay every minute
  }

  private applyDecay(): void {
    const now = Date.now();

    this.nodes.forEach((node, id) => {
      const timeDiff = (now - node.lastAccessed) / (1000 * 60 * 60); // Hours
      node.decay = Math.exp(-0.01 * timeDiff) * this.decayRate;

      // Remove nodes with very low decay
      if (node.decay < 0.01 && node.importance < 0.5) {
        this.removeNode(id);
      }
    });
  }

  public addNode(nodeData: Partial<MemoryNode> & { content: string }): MemoryNode {
    const id = nodeData.id || uuidv4();
    const now = Date.now();

    const node: MemoryNode = {
      id,
      content: nodeData.content,
      embedding: nodeData.embedding || this.generatePlaceholderEmbedding(),
      timestamp: nodeData.timestamp || now,
      accessCount: nodeData.accessCount || 0,
      lastAccessed: now,
      importance: nodeData.importance || 0.5,
      decay: 1.0,
      metadata: nodeData.metadata || {}
    };

    this.nodes.set(id, node);
    this.nodeIndex.set(id, new Set());

    return node;
  }

  private generatePlaceholderEmbedding(): Float32Array {
    const embedding = new Float32Array(this.embeddingDimension);
    for (let i = 0; i < this.embeddingDimension; i++) {
      embedding[i] = (Math.random() - 0.5) * 2;
    }
    return embedding;
  }

  public removeNode(id: string): boolean {
    const edges = this.nodeIndex.get(id);
    if (edges) {
      edges.forEach(edgeId => {
        const edge = this.edges.get(edgeId);
        if (edge) {
          edge.nodes = edge.nodes.filter(nodeId => nodeId !== id);
          if (edge.nodes.length < 2) {
            this.edges.delete(edgeId);
          }
        }
      });
    }

    this.nodeIndex.delete(id);
    return this.nodes.delete(id);
  }

  public addHyperEdge(nodeIds: string[], relationshipType: string, strength: number = 1.0): HyperEdge | null {
    // Verify all nodes exist
    for (const nodeId of nodeIds) {
      if (!this.nodes.has(nodeId)) {
        return null;
      }
    }

    const edge: HyperEdge = {
      id: uuidv4(),
      nodes: nodeIds,
      relationshipType,
      strength,
      timestamp: Date.now(),
      metadata: {}
    };

    this.edges.set(edge.id, edge);

    // Update node index
    nodeIds.forEach(nodeId => {
      const edges = this.nodeIndex.get(nodeId);
      if (edges) {
        edges.add(edge.id);
      }
    });

    return edge;
  }

  public async query(queryContent: string, topK: number = 5): Promise<MemoryQueryResult[]> {
    const queryEmbedding = this.generatePlaceholderEmbedding();

    const scores: Array<{ node: MemoryNode; score: number }> = [];

    this.nodes.forEach(node => {
      const similarity = this.cosineSimilarity(queryEmbedding, node.embedding);
      const recencyBoost = node.decay;
      const importanceBoost = node.importance;
      const accessBoost = Math.log1p(node.accessCount) / 10;

      const finalScore = similarity * 0.5 + recencyBoost * 0.2 + importanceBoost * 0.2 + accessBoost * 0.1;

      scores.push({ node, score: finalScore });
    });

    // Sort and get top K
    scores.sort((a, b) => b.score - a.score);
    const topNodes = scores.slice(0, topK);

    // Enrich with related nodes
    const results: MemoryQueryResult[] = topNodes.map(({ node, score }) => {
      const relatedNodes = this.getRelatedNodes(node.id);

      // Update access statistics
      node.accessCount++;
      node.lastAccessed = Date.now();

      return {
        node,
        score,
        relatedNodes
      };
    });

    return results;
  }

  private cosineSimilarity(a: Float32Array, b: Float32Array): number {
    let dotProduct = 0;
    let normA = 0;
    let normB = 0;

    for (let i = 0; i < a.length; i++) {
      dotProduct += a[i] * b[i];
      normA += a[i] * a[i];
      normB += b[i] * b[i];
    }

    const denom = Math.sqrt(normA) * Math.sqrt(normB);
    return denom > 0 ? dotProduct / denom : 0;
  }

  private getRelatedNodes(nodeId: string): MemoryNode[] {
    const relatedNodes: MemoryNode[] = [];
    const edges = this.nodeIndex.get(nodeId);

    if (edges) {
      edges.forEach(edgeId => {
        const edge = this.edges.get(edgeId);
        if (edge) {
          edge.nodes.forEach(relatedId => {
            if (relatedId !== nodeId) {
              const relatedNode = this.nodes.get(relatedId);
              if (relatedNode) {
                relatedNodes.push(relatedNode);
              }
            }
          });
        }
      });
    }

    return relatedNodes;
  }

  public consolidate(): void {
    // Find similar memories and merge them
    const nodeArray = Array.from(this.nodes.values());

    for (let i = 0; i < nodeArray.length; i++) {
      for (let j = i + 1; j < nodeArray.length; j++) {
        const similarity = this.cosineSimilarity(nodeArray[i].embedding, nodeArray[j].embedding);

        if (similarity > this.consolidationThreshold) {
          // Create hyperedge connecting similar memories
          this.addHyperEdge([nodeArray[i].id, nodeArray[j].id], 'semantic_similarity', similarity);
        }
      }
    }
  }

  public getStats(): Record<string, unknown> {
    let totalImportance = 0;
    let totalDecay = 0;

    this.nodes.forEach(node => {
      totalImportance += node.importance;
      totalDecay += node.decay;
    });

    const avgImportance = this.nodes.size > 0 ? totalImportance / this.nodes.size : 0;
    const avgDecay = this.nodes.size > 0 ? totalDecay / this.nodes.size : 0;

    return {
      nodeCount: this.nodes.size,
      edgeCount: this.edges.size,
      averageImportance: avgImportance,
      averageDecay: avgDecay,
      memoryUtilization: this.estimateMemoryUsage()
    };
  }

  private estimateMemoryUsage(): number {
    const nodeMemory = this.nodes.size * (this.embeddingDimension * 4 + 200);
    const edgeMemory = this.edges.size * 100;
    return (nodeMemory + edgeMemory) / (1024 * 1024); // MB
  }

  public serialize(): object {
    return {
      nodes: Array.from(this.nodes.entries()),
      edges: Array.from(this.edges.entries()),
      stats: this.getStats()
    };
  }

  public clear(): void {
    this.nodes.clear();
    this.edges.clear();
    this.nodeIndex.clear();
  }
}

export default HypergraphMemory;
