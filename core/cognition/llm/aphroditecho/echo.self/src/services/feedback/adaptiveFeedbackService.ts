import {
  HypergraphSchemeCore,
  HypergraphNode,
  SalienceMetrics,
} from "./hypergraphSchemeCore";
export interface ProjectModel {
  id: string;
  name: string;
  description: string;
  version: string;
  lastModified: Date;
  usageCount: number;
  communityFeedback: CommunityFeedback[];
  salienceScore: number;
}
export interface CommunityFeedback {
  id: string;
  modelId: string;
  userId: string;
  type: "improvement" | "bug" | "feature_request" | "performance";
  priority: "low" | "medium" | "high" | "urgent";
  description: string;
  timestamp: Date;
  votes: number;
}
export interface CopilotRequest {
  modelId: string;
  priority: number;
  requestType: "upgrade" | "optimization" | "feature_addition";
  context: string;
  requirements: string[];
}
export interface CopilotResponse {
  requestId: string;
  modelId: string;
  upgradedContent: string;
  improvements: string[];
  version: string;
  confidence: number;
}
export interface AdaptiveThresholds {
  cognitiveLoad: number;
  recentActivity: number;
  feedbackUrgency: number;
  attentionThreshold: number;
}
export class AdaptiveFeedbackService {
  private static instance: AdaptiveFeedbackService;
  private hypergraphCore: HypergraphSchemeCore;
  private projectModels: Map<string, ProjectModel> = new Map();
  private communityFeedback: Map<string, CommunityFeedback> = new Map();
  private adaptiveThresholds: AdaptiveThresholds;
  private lastFeedbackCycle: Date = new Date();
  private feedbackCycleInterval: number = 5 * 60 * 1000; 
  private constructor() {
    this.hypergraphCore = new HypergraphSchemeCore();
    this.adaptiveThresholds = {
      cognitiveLoad: 0.3,
      recentActivity: 0.7,
      feedbackUrgency: 0.5,
      attentionThreshold: 0.6,
    };
    this.initializeFeedbackLoop();
  }
  public static getInstance(): AdaptiveFeedbackService {
    if (!AdaptiveFeedbackService.instance) {
      AdaptiveFeedbackService.instance = new AdaptiveFeedbackService();
    }
    return AdaptiveFeedbackService.instance;
  }
  private initializeFeedbackLoop(): void {
    this.hypergraphCore.createNode(
      "feedback-collector",
      "procedure",
      {
        description: "Collects salient project models and community feedback",
        priority: 0.9,
      },
      []
    );
    this.hypergraphCore.createNode(
      "salience-scorer",
      "procedure",
      {
        description: "Scores models using semantic salience",
        priority: 0.85,
      },
      ["feedback-collector"]
    );
    this.hypergraphCore.createNode(
      "copilot-interface",
      "procedure",
      {
        description: "Queries Copilot with prioritized wishlist",
        priority: 0.8,
      },
      ["salience-scorer"]
    );
    this.hypergraphCore.createNode(
      "model-integrator",
      "procedure",
      {
        description: "Integrates upgrades into local repository",
        priority: 0.75,
      },
      ["copilot-interface"]
    );
    this.hypergraphCore.createNode(
      "community-broadcaster",
      "procedure",
      {
        description: "Broadcasts improvements to community",
        priority: 0.7,
      },
      ["model-integrator"]
    );
    this.startFeedbackCycle();
  }
  private startFeedbackCycle(): void {
    setInterval(() => {
      this.executeFeedbackLoop();
    }, this.feedbackCycleInterval);
  }
  public async executeFeedbackLoop(): Promise<void> {
    console.log("🔄 Starting adaptive feedback loop cycle...");
    try {
      this.updateAdaptiveThresholds();
      const salientModels = await this.collectSalientModels();
      const scoredModels = this.scoreModelsSalience(salientModels);
      const prioritizedRequests = this.buildCopilotWishlist(scoredModels);
      const copilotResponses = await this.queryCopilot(prioritizedRequests);
      const integratedModels = await this.integrateUpgrades(copilotResponses);
      await this.broadcastImprovements(integratedModels);
      this.updateHypergraphPatterns();
      console.log(
        `✅ Feedback loop completed. Processed ${scoredModels.length} models, integrated ${integratedModels.length} upgrades`
      );
    } catch (error) {
      console.error("❌ Error in feedback loop execution:", error);
    }
    this.lastFeedbackCycle = new Date();
  }
  private updateAdaptiveThresholds(): void {
    const currentLoad = this.calculateCognitiveLoad();
    const recentActivity = this.calculateRecentActivity();
    const newThreshold = this.hypergraphCore.adaptiveAttention(
      currentLoad,
      recentActivity
    );
    this.adaptiveThresholds.attentionThreshold = newThreshold;
    this.hypergraphCore.updateAttentionThreshold(newThreshold);
    console.log(
      `🧠 Adaptive thresholds updated: load=${currentLoad.toFixed(3)}, activity=${recentActivity.toFixed(3)}, threshold=${newThreshold.toFixed(3)}`
    );
  }
  private calculateCognitiveLoad(): number {
    const activeNodes = this.hypergraphCore.getAllNodes().length;
    const feedbackVolume = this.communityFeedback.size;
    const processingQueue = this.projectModels.size;
    const load = Math.min(
      (activeNodes + feedbackVolume + processingQueue) / 100,
      1.0
    );
    this.adaptiveThresholds.cognitiveLoad = load;
    return load;
  }
  private calculateRecentActivity(): number {
    const now = Date.now();
    const hourAgo = now - 60 * 60 * 1000;
    const recentFeedback = Array.from(this.communityFeedback.values()).filter(
      feedback => feedback.timestamp.getTime() > hourAgo
    ).length;
    const recentModels = Array.from(this.projectModels.values()).filter(
      model => model.lastModified.getTime() > hourAgo
    ).length;
    const activity = Math.min((recentFeedback + recentModels) / 10, 1.0);
    this.adaptiveThresholds.recentActivity = activity;
    return activity;
  }
  private async collectSalientModels(): Promise<ProjectModel[]> {
    const attentionNodes = this.hypergraphCore.getAttentionFilteredNodes(
      this.adaptiveThresholds.attentionThreshold
    );
    const salientModels: ProjectModel[] = [];
    for (const node of attentionNodes) {
      if (node.type === "model" || node.type === "concept") {
        const model = this.nodeToProjectModel(node);
        if (model) {
          salientModels.push(model);
        }
      }
    }
    const urgentModels = this.getModelsWithUrgentFeedback();
    salientModels.push(...urgentModels);
    const uniqueModels = salientModels.filter(
      (model, index, self) => index === self.findIndex(m => m.id === model.id)
    );
    console.log(
      `📊 Collected ${uniqueModels.length} salient models (threshold: ${this.adaptiveThresholds.attentionThreshold.toFixed(3)})`
    );
    return uniqueModels;
  }
  private nodeToProjectModel(node: HypergraphNode): ProjectModel | null {
    try {
      return {
        id: node.id,
        name: node.content.name || node.id,
        description:
          node.content.description || "Generated from hypergraph node",
        version: node.content.version || "1.0.0",
        lastModified: node.lastUpdated,
        usageCount: node.content.usageCount || 0,
        communityFeedback: this.getFeedbackForModel(node.id),
        salienceScore: node.salience,
      };
    } catch (error) {
      console.warn(
        `Warning: Failed to convert node ${node.id} to project model:`,
        error
      );
      return null;
    }
  }
  private getFeedbackForModel(modelId: string): CommunityFeedback[] {
    return Array.from(this.communityFeedback.values()).filter(
      feedback => feedback.modelId === modelId
    );
  }
  private getModelsWithUrgentFeedback(): ProjectModel[] {
    const urgentFeedback = Array.from(this.communityFeedback.values()).filter(
      feedback => feedback.priority === "urgent"
    );
    const urgentModelIds = Array.from(
      new Set(urgentFeedback.map(f => f.modelId))
    );
    return urgentModelIds
      .map(id => this.projectModels.get(id))
      .filter(model => model !== undefined) as ProjectModel[];
  }
  private scoreModelsSalience(models: ProjectModel[]): ProjectModel[] {
    return models
      .map(model => {
        const usageHistory = Array(model.usageCount).fill(null);
        const metrics = this.hypergraphCore.calculateSalienceMetrics(
          model.id,
          usageHistory
        );
        const feedbackUrgency = this.calculateFeedbackUrgency(model);
        const combinedScore =
          metrics.demand * 0.4 +
          metrics.freshness * 0.3 +
          feedbackUrgency * 0.3;
        model.salienceScore = combinedScore;
        return model;
      })
      .sort((a, b) => b.salienceScore - a.salienceScore);
  }
  private calculateFeedbackUrgency(model: ProjectModel): number {
    if (model.communityFeedback.length === 0) return 0;
    const urgencyWeights = { urgent: 1.0, high: 0.8, medium: 0.6, low: 0.3 };
    const weightedUrgency =
      model.communityFeedback.reduce((sum, feedback) => {
        return (
          sum + urgencyWeights[feedback.priority] * (1 + feedback.votes * 0.1)
        );
      }, 0) / model.communityFeedback.length;
    return Math.min(weightedUrgency, 1.0);
  }
  private buildCopilotWishlist(scoredModels: ProjectModel[]): CopilotRequest[] {
    const maxRequests = 5; 
    return scoredModels.slice(0, maxRequests).map((model, index) => ({
      modelId: model.id,
      priority: model.salienceScore,
      requestType: this.determineRequestType(model),
      context: this.buildModelContext(model),
      requirements: this.extractRequirements(model),
    }));
  }
  private determineRequestType(
    model: ProjectModel
  ): CopilotRequest["requestType"] {
    const feedbackTypes = model.communityFeedback.map(f => f.type);
    if (feedbackTypes.includes("performance")) return "optimization";
    if (feedbackTypes.includes("feature_request")) return "feature_addition";
    return "upgrade";
  }
  private buildModelContext(model: ProjectModel): string {
    const feedback = model.communityFeedback
      .map(f => `${f.type}: ${f.description}`)
      .join("; ");
    return `Model: ${model.name} (v${model.version})\nDescription: ${model.description}\nFeedback: ${feedback}`;
  }
  private extractRequirements(model: ProjectModel): string[] {
    return model.communityFeedback
      .filter(f => f.priority === "high" || f.priority === "urgent")
      .map(f => f.description);
  }
  private async queryCopilot(
    requests: CopilotRequest[]
  ): Promise<CopilotResponse[]> {
    console.log(
      `🤖 Querying Copilot with ${requests.length} prioritized requests...`
    );
    const responses: CopilotResponse[] = [];
    for (const request of requests) {
      await new Promise(resolve => setTimeout(resolve, 500));
      const mockResponse: CopilotResponse = {
        requestId: `req-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
        modelId: request.modelId,
        upgradedContent: this.generateMockUpgrade(request),
        improvements: this.generateMockImprovements(request),
        version: this.incrementVersion(request.modelId),
        confidence: 0.85 + Math.random() * 0.15, 
      };
      responses.push(mockResponse);
    }
    console.log(`✨ Received ${responses.length} Copilot responses`);
    return responses;
  }
  private generateMockUpgrade(request: CopilotRequest): string {
    return `
${request.requirements.map(req => `
(define (enhanced-${request.modelId.replace(/[^a-zA-Z0-9]/g, "-")} context)
  ;; Enhanced cognitive processing with improved salience
  (let ((processed-context (apply-salience-filter context ${request.priority.toFixed(3)})))
    (hypergraph-encode processed-context)))
`;
  }
  private generateMockImprovements(request: CopilotRequest): string[] {
    const baseImprovements = [
      "Enhanced hypergraph pattern encoding",
      "Improved adaptive attention allocation",
      "Optimized semantic salience calculation",
      "Better integration with cognitive framework",
    ];
    const typeSpecificImprovements = {
      upgrade: ["Updated core algorithms", "Enhanced performance metrics"],
      optimization: ["Reduced cognitive load", "Faster pattern recognition"],
      feature_addition: [
        "New community feedback integration",
        "Enhanced broadcasting capabilities",
      ],
    };
    return [
      ...baseImprovements,
      ...typeSpecificImprovements[request.requestType],
    ];
  }
  private incrementVersion(modelId: string): string {
    const model = this.projectModels.get(modelId);
    if (!model) return "1.0.1";
    const [major, minor, patch] = model.version.split(".").map(Number);
    return `${major}.${minor}.${patch + 1}`;
  }
  private async integrateUpgrades(
    responses: CopilotResponse[]
  ): Promise<ProjectModel[]> {
    console.log(`🔧 Integrating ${responses.length} Copilot upgrades...`);
    const integratedModels: ProjectModel[] = [];
    for (const response of responses) {
      try {
        const existingModel = this.projectModels.get(response.modelId);
        const upgradedModel: ProjectModel = {
          id: response.modelId,
          name: existingModel?.name || response.modelId,
          description: `${existingModel?.description || "Model"} - Enhanced by Copilot`,
          version: response.version,
          lastModified: new Date(),
          usageCount: existingModel?.usageCount || 0,
          communityFeedback: existingModel?.communityFeedback || [],
          salienceScore: existingModel?.salienceScore || 0.5,
        };
        this.projectModels.set(response.modelId, upgradedModel);
        this.hypergraphCore.createNode(
          `${response.modelId}-v${response.version}`,
          "model",
          {
            content: response.upgradedContent,
            improvements: response.improvements,
            confidence: response.confidence,
            version: response.version,
          },
          [response.modelId] 
        );
        integratedModels.push(upgradedModel);
        console.log(
          `✅ Integrated ${response.modelId} v${response.version} (confidence: ${response.confidence.toFixed(3)})`
        );
      } catch (error) {
        console.error(`❌ Failed to integrate ${response.modelId}:`, error);
      }
    }
    return integratedModels;
  }
  private async broadcastImprovements(models: ProjectModel[]): Promise<void> {
    console.log(
      `📡 Broadcasting ${models.length} model improvements to community...`
    );
    for (const model of models) {
      const broadcastMessage = {
        type: "model_improvement",
        modelId: model.id,
        version: model.version,
        timestamp: new Date(),
        improvements: this.extractImprovements(model),
        salienceScore: model.salienceScore,
      };
      const communityNodes = ["project-alpha", "project-beta", "community-hub"];
      for (const node of communityNodes) {
        await this.sendBroadcast(node, broadcastMessage);
      }
    }
    console.log(`✅ Broadcast completed to community nodes`);
  }
  private extractImprovements(model: ProjectModel): string[] {
    const node = this.hypergraphCore.getNode(`${model.id}-v${model.version}`);
    return (
      node?.content?.improvements || ["General improvements and optimizations"]
    );
  }
  private async sendBroadcast(nodeId: string, message: any): Promise<void> {
    await new Promise(resolve => setTimeout(resolve, 100));
    console.log(
      `📤 Broadcast sent to ${nodeId}: ${message.type} for ${message.modelId}`
    );
  }
  private updateHypergraphPatterns(): void {
    const newPatterns = this.hypergraphCore.mineCognitivePatterns(0.7);
    newPatterns
      .filter(pattern => pattern.strength > 0.8)
      .forEach(pattern => {
        this.hypergraphCore.embodyPattern(pattern);
      });
    const highSalienceNodes = this.hypergraphCore
      .getAllNodes()
      .filter(node => node.salience > 0.8);
    highSalienceNodes.forEach(node => {
      this.hypergraphCore.spreadAttention(node.id, 0.1);
    });
    console.log(
      `🧩 Updated hypergraph: ${newPatterns.length} new patterns, ${highSalienceNodes.length} attention spreads`
    );
  }
  public addCommunityFeedback(
    feedback: Omit<CommunityFeedback, "id" | "timestamp">
  ): void {
    const id = `feedback-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
    const completeFeedback: CommunityFeedback = {
      ...feedback,
      id,
      timestamp: new Date(),
    };
    this.communityFeedback.set(id, completeFeedback);
    if (feedback.priority === "urgent") {
      this.adaptiveThresholds.feedbackUrgency = Math.min(
        this.adaptiveThresholds.feedbackUrgency + 0.2,
        1.0
      );
    }
  }
  public registerProjectModel(
    model: Omit<ProjectModel, "communityFeedback" | "salienceScore">
  ): void {
    const completeModel: ProjectModel = {
      ...model,
      communityFeedback: [],
      salienceScore: 0.5,
    };
    this.projectModels.set(model.id, completeModel);
    this.hypergraphCore.createNode(
      model.id,
      "model",
      {
        name: model.name,
        description: model.description,
        version: model.version,
        usageCount: model.usageCount,
      },
      []
    );
  }
  public getSystemStatus() {
    return {
      adaptiveThresholds: this.adaptiveThresholds,
      lastFeedbackCycle: this.lastFeedbackCycle,
      projectModelsCount: this.projectModels.size,
      communityFeedbackCount: this.communityFeedback.size,
      hypergraphNodesCount: this.hypergraphCore.getAllNodes().length,
      attentionFilteredNodes:
        this.hypergraphCore.getAttentionFilteredNodes().length,
    };
  }
  public async triggerFeedbackLoop(): Promise<void> {
    return this.executeFeedbackLoop();
  }
  public setFeedbackCycleInterval(intervalMs: number): void {
    this.feedbackCycleInterval = Math.max(intervalMs, 30000); 
  }
}