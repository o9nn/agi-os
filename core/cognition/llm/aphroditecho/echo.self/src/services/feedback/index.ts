export {
  HypergraphSchemeCore,
  type HypergraphNode,
  type CognitivePattern,
  type SalienceMetrics,
} from "./hypergraphSchemeCore";
export {
  AdaptiveFeedbackService,
  type ProjectModel,
  type CommunityFeedback,
  type CopilotRequest,
  type CopilotResponse,
  type AdaptiveThresholds,
} from "./adaptiveFeedbackService";
export {
  FeedbackIntegrationService,
  useAdaptiveFeedback,
} from "./feedbackIntegrationService";
import { FeedbackIntegrationService } from "./feedbackIntegrationService";
export const initializeAdaptiveFeedbackSystem = () => {
  const integrationService = FeedbackIntegrationService.getInstance();
  console.log("🚀 Adaptive feedback system initialized");
  return integrationService;
};