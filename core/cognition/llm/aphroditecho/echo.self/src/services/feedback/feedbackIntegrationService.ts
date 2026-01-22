import {
  AdaptiveFeedbackService,
  ProjectModel,
  CommunityFeedback,
} from "./adaptiveFeedbackService";
import { OrchestratorService } from "../orchestratorService";
export class FeedbackIntegrationService {
  private static instance: FeedbackIntegrationService;
  private feedbackService: AdaptiveFeedbackService;
  private orchestratorService: OrchestratorService;
  private isInitialized: boolean = false;
  private constructor() {
    this.feedbackService = AdaptiveFeedbackService.getInstance();
    this.orchestratorService = OrchestratorService.getInstance();
    this.initializeIntegration();
  }
  public static getInstance(): FeedbackIntegrationService {
    if (!FeedbackIntegrationService.instance) {
      FeedbackIntegrationService.instance = new FeedbackIntegrationService();
    }
    return FeedbackIntegrationService.instance;
  }
  private initializeIntegration(): void {
    this.orchestratorService.registerComponent("adaptive-feedback", {
      status: "active",
      type: "admin-service",
      capabilities: [
        "model-collection",
        "salience-scoring",
        "copilot-integration",
        "community-broadcasting",
      ],
      getStatus: () => this.feedbackService.getSystemStatus(),
      triggerCycle: () => this.feedbackService.triggerFeedbackLoop(),
    });
    this.orchestratorService.addEventListener("component_registered", (data: any) => {
      console.log(`🔗 Component registered: ${data.componentId}`);
    });
    this.orchestratorService.addEventListener("content_transfer", (data: any) => {
      console.log(`📤 Content transfer: ${data.from} → ${data.to}`);
    });
    this.isInitialized = true;
    console.log("✅ Feedback integration service initialized");
  }
  public addFeedback(
    feedback: Omit<CommunityFeedback, "id" | "timestamp">
  ): void {
    this.feedbackService.addCommunityFeedback(feedback);
  }
  public registerModel(
    model: Omit<ProjectModel, "communityFeedback" | "salienceScore">
  ): void {
    this.feedbackService.registerProjectModel(model);
  }
  public getIntegrationStatus() {
    return {
      isInitialized: this.isInitialized,
      feedbackService: this.feedbackService.getSystemStatus(),
      orchestratorConnected:
        this.orchestratorService.isComponentRegistered("adaptive-feedback"),
    };
  }
  public async triggerFeedbackLoop(): Promise<void> {
    console.log("🎯 Feedback loop triggered via orchestrator integration");
    return this.feedbackService.triggerFeedbackLoop();
  }
}
export const useAdaptiveFeedback = () => {
  const integrationService = FeedbackIntegrationService.getInstance();
  const feedbackService = AdaptiveFeedbackService.getInstance();
  return {
    addFeedback: (feedback: Omit<CommunityFeedback, "id" | "timestamp">) =>
      integrationService.addFeedback(feedback),
    registerModel: (
      model: Omit<ProjectModel, "communityFeedback" | "salienceScore">
    ) => integrationService.registerModel(model),
    getStatus: () => integrationService.getIntegrationStatus(),
    triggerFeedbackLoop: () => integrationService.triggerFeedbackLoop(),
    setFeedbackInterval: (intervalMs: number) =>
      feedbackService.setFeedbackCycleInterval(intervalMs),
    getFeedbackService: () => feedbackService,
  };
};
export default FeedbackIntegrationService;