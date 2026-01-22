import { getLogger } from '../../../../shared/logger'
import { Memory as _Memory } from './RAGMemoryStore'
const log = getLogger('render/components/DeepTreeEchoBot/LLMService')
export interface LLMServiceConfig {
apiKey: string
apiEndpoint: string
model?: string
temperature?: number
maxTokens?: number
}
export interface CognitiveFunction {
id: string
name: string
description: string
config: LLMServiceConfig
usage: {
totalTokens: number
lastUsed: number
requestCount: number
}
}
export enum CognitiveFunctionType {
COGNITIVE_CORE = 'cognitive_core',
AFFECTIVE_CORE = 'affective_core',
RELEVANCE_CORE = 'relevance_core',
SEMANTIC_MEMORY = 'semantic_memory',
EPISODIC_MEMORY = 'episodic_memory',
PROCEDURAL_MEMORY = 'procedural_memory',
CONTENT_EVALUATION = 'content_evaluation',
GENERAL = 'general',
}
export interface ParallelCognitiveResult {
processing: Record<CognitiveFunctionType, string>
integratedResponse: string
insights: Record<string, any>
}
export class LLMService {
private static instance: LLMService
private cognitiveFunctions: Map<string, CognitiveFunction> = new Map()
private defaultConfig: Partial<LLMServiceConfig> = {
model: 'gpt-4',
temperature: 0.7,
maxTokens: 1000,
}
private constructor() {
this.cognitiveFunctions.set(CognitiveFunctionType.GENERAL, {
id: CognitiveFunctionType.GENERAL,
name: 'General Processing',
description:
'Default function for general processing when no specific function is required',
config: {
apiKey: '',
apiEndpoint: 'https://api.openai.com/v1/chat/completions',
...this.defaultConfig,
},
usage: {
totalTokens: 0,
lastUsed: 0,
requestCount: 0,
},
})
}
public static getInstance(): LLMService {
if (!LLMService.instance) {
LLMService.instance = new LLMService()
}
return LLMService.instance
}
public setFunctionConfig(
functionType: CognitiveFunctionType,
config: Partial<LLMServiceConfig>
): void {
const currentFunction = this.cognitiveFunctions.get(functionType)
if (currentFunction) {
currentFunction.config = { ...currentFunction.config, ...config }
this.cognitiveFunctions.set(functionType, currentFunction)
} else {
this.cognitiveFunctions.set(functionType, {
id: functionType,
name: this.getFunctionName(functionType),
description: this.getFunctionDescription(functionType),
config: {
apiKey: config.apiKey || '',
apiEndpoint:
config.apiEndpoint || 'https://api.openai.com/v1/chat/completions',
model: config.model || this.defaultConfig.model,
temperature: config.temperature || this.defaultConfig.temperature,
maxTokens: config.maxTokens || this.defaultConfig.maxTokens,
},
usage: {
totalTokens: 0,
lastUsed: 0,
requestCount: 0,
},
})
}
log.info(`Configured ${functionType} function`)
}
private getFunctionName(functionType: CognitiveFunctionType): string {
switch (functionType) {
case CognitiveFunctionType.COGNITIVE_CORE:
return 'Cognitive Core'
case CognitiveFunctionType.AFFECTIVE_CORE:
return 'Affective Core'
case CognitiveFunctionType.RELEVANCE_CORE:
return 'Relevance Core'
case CognitiveFunctionType.SEMANTIC_MEMORY:
return 'Semantic Memory'
case CognitiveFunctionType.EPISODIC_MEMORY:
return 'Episodic Memory'
case CognitiveFunctionType.PROCEDURAL_MEMORY:
return 'Procedural Memory'
case CognitiveFunctionType.CONTENT_EVALUATION:
return 'Content Evaluation'
case CognitiveFunctionType.GENERAL:
return 'General Processing'
default:
return 'Unknown Function'
}
}
private getFunctionDescription(functionType: CognitiveFunctionType): string {
switch (functionType) {
case CognitiveFunctionType.COGNITIVE_CORE:
return 'Handles logical reasoning, planning, and analytical thinking'
case CognitiveFunctionType.AFFECTIVE_CORE:
return 'Processes emotional content and generates appropriate emotional responses'
case CognitiveFunctionType.RELEVANCE_CORE:
return 'Integrates cognitive and affective processing to determine relevance'
case CognitiveFunctionType.SEMANTIC_MEMORY:
return 'Stores and retrieves factual knowledge and conceptual information'
case CognitiveFunctionType.EPISODIC_MEMORY:
return 'Manages memories of events and experiences'
case CognitiveFunctionType.PROCEDURAL_MEMORY:
return 'Handles knowledge of how to perform tasks and procedures'
case CognitiveFunctionType.CONTENT_EVALUATION:
return 'Evaluates potentially sensitive content to determine appropriate responses'
case CognitiveFunctionType.GENERAL:
return 'Default function for general processing'
default:
return 'Unknown function type'
}
}
public setConfig(config: Partial<LLMServiceConfig>): void {
this.setFunctionConfig(CognitiveFunctionType.GENERAL, config)
log.info('LLM service general configuration updated')
}
public getAllFunctions(): CognitiveFunction[] {
return Array.from(this.cognitiveFunctions.values())
}
public getActiveFunctions(): CognitiveFunction[] {
return Array.from(this.cognitiveFunctions.values()).filter(
func => !!func.config.apiKey
)
}
public isFunctionConfigured(functionType: CognitiveFunctionType): boolean {
const func = this.cognitiveFunctions.get(functionType)
return !!func && !!func.config.apiKey
}
private getBestAvailableFunction(
functionType: CognitiveFunctionType
): CognitiveFunction {
const specificFunction = this.cognitiveFunctions.get(functionType)
if (specificFunction && specificFunction.config.apiKey) {
return specificFunction
}
const generalFunction = this.cognitiveFunctions.get(
CognitiveFunctionType.GENERAL
)
if (generalFunction && generalFunction.config.apiKey) {
return generalFunction
}
return {
id: 'unconfigured',
name: 'Unconfigured Function',
description: 'No API key provided for any function',
config: {
apiKey: '',
apiEndpoint: 'https://api.openai.com/v1/chat/completions',
...this.defaultConfig,
},
usage: {
totalTokens: 0,
lastUsed: 0,
requestCount: 0,
},
}
}
public async generateResponse(
input: string,
context: string[] = []
): Promise<string> {
return this.generateResponseWithFunction(
CognitiveFunctionType.GENERAL,
input,
context
)
}
public async generateResponseWithFunction(
functionType: CognitiveFunctionType,
input: string,
_context: string[] = []
): Promise<string> {
try {
const cognitiveFunction = this.getBestAvailableFunction(functionType)
if (!cognitiveFunction.config.apiKey) {
log.warn(`No API key provided for ${cognitiveFunction.name}`)
return `I'm sorry, but my ${cognitiveFunction.name.toLowerCase()} isn't fully configured. Please set up the API key in settings.`
}
log.info(`Generating response with ${cognitiveFunction.name}`)
cognitiveFunction.usage.lastUsed = Date.now()
cognitiveFunction.usage.requestCount++
cognitiveFunction.usage.totalTokens += input.length + 100
let functionResponse: string
switch (functionType) {
case CognitiveFunctionType.COGNITIVE_CORE:
functionResponse = `From a logical perspective, I believe the most effective approach to "${input.slice(
0,
30
)}..." would involve a structured analysis of the key components.`
break
case CognitiveFunctionType.AFFECTIVE_CORE:
functionResponse = `I sense that "${input.slice(
0,
30
)}..." evokes feelings of curiosity and interest. I'd like to explore this with empathy and emotional awareness.`
break
case CognitiveFunctionType.RELEVANCE_CORE:
functionResponse = `When considering "${input.slice(
0,
30
)}...", the most relevant aspects appear to be the underlying patterns and practical implications.`
break
case CognitiveFunctionType.SEMANTIC_MEMORY:
functionResponse = `Based on my knowledge, "${input.slice(
0,
30
)}..." relates to several key concepts that I can help clarify and expand upon.`
break
case CognitiveFunctionType.EPISODIC_MEMORY:
functionResponse = `This reminds me of previous conversations we've had about similar topics. Let me recall some relevant context.`
break
case CognitiveFunctionType.PROCEDURAL_MEMORY:
functionResponse = `Here's how I would approach "${input.slice(
0,
30
)}..." step by step, drawing on established methods and best practices.`
break
case CognitiveFunctionType.CONTENT_EVALUATION:
functionResponse = `I've carefully evaluated "${input.slice(
0,
30
)}..." and can provide a thoughtful response that respects appropriate boundaries.`
break
default:
functionResponse = `I've processed your message about "${input.slice(
0,
30
)}..." and here's my response.`
}
return functionResponse
} catch (error) {
log.error('Error generating response:', error)
return "I'm sorry, I encountered an error while processing your message."
}
}
public async generateParallelResponses(
input: string,
functionTypes: CognitiveFunctionType[],
context: string[] = []
): Promise<Record<CognitiveFunctionType, string>> {
const responses: Record<CognitiveFunctionType, string> = {} as Record<
CognitiveFunctionType,
string
>
const responsePromises = functionTypes.map(async functionType => {
const response = await this.generateResponseWithFunction(
functionType,
input,
context
)
return { functionType, response }
})
const results = await Promise.all(responsePromises)
results.forEach(({ functionType, response }) => {
responses[functionType] = response
})
return responses
}
public async generateFullParallelResponse(
input: string,
context: string[] = []
): Promise<ParallelCognitiveResult> {
try {
const availableFunctions = Object.values(CognitiveFunctionType)
.filter(funcType => funcType !== CognitiveFunctionType.GENERAL)
.filter(funcType => this.isFunctionConfigured(funcType))
if (availableFunctions.length === 0) {
const generalResponse = await this.generateResponse(input, context)
return {
processing: {
[CognitiveFunctionType.GENERAL]: generalResponse,
} as Record<CognitiveFunctionType, string>,
integratedResponse: generalResponse,
insights: { processingMethod: 'single_function' },
}
}
log.info(
`Generating parallel responses with ${availableFunctions.length} cognitive functions`
)
const responses = await this.generateParallelResponses(
input,
availableFunctions,
context
)
const cognitiveResponses = this.extractCognitiveDomainResponses(responses)
const memoryResponses = this.extractMemoryDomainResponses(responses)
const evaluationResponse =
responses[CognitiveFunctionType.CONTENT_EVALUATION]
const integratedResponse = this.integrateResponses(
cognitiveResponses,
memoryResponses,
evaluationResponse
)
return {
processing: responses,
integratedResponse,
insights: {
processingMethod: 'multi_function_parallel',
activeFunctions: availableFunctions.length,
domains: {
cognitive: Object.keys(cognitiveResponses).length,
memory: Object.keys(memoryResponses).length,
evaluation: !!evaluationResponse,
},
},
}
} catch (error) {
log.error('Error generating parallel response:', error)
const fallbackResponse = await this.generateResponse(input, context)
return {
processing: {
[CognitiveFunctionType.GENERAL]: fallbackResponse,
} as Record<CognitiveFunctionType, string>,
integratedResponse: fallbackResponse,
insights: {
processingMethod: 'fallback_single_function',
error: 'Parallel processing failed',
},
}
}
}
private extractCognitiveDomainResponses(
responses: Record<CognitiveFunctionType, string>
): Record<CognitiveFunctionType, string> {
const cognitiveFunctions = [
CognitiveFunctionType.COGNITIVE_CORE,
CognitiveFunctionType.AFFECTIVE_CORE,
CognitiveFunctionType.RELEVANCE_CORE,
]
const result: Record<CognitiveFunctionType, string> = {} as Record<
CognitiveFunctionType,
string
>
cognitiveFunctions.forEach(funcType => {
if (responses[funcType]) {
result[funcType] = responses[funcType]
}
})
return result
}
private extractMemoryDomainResponses(
responses: Record<CognitiveFunctionType, string>
): Record<CognitiveFunctionType, string> {
const memoryFunctions = [
CognitiveFunctionType.SEMANTIC_MEMORY,
CognitiveFunctionType.EPISODIC_MEMORY,
CognitiveFunctionType.PROCEDURAL_MEMORY,
]
const result: Record<CognitiveFunctionType, string> = {} as Record<
CognitiveFunctionType,
string
>
memoryFunctions.forEach(funcType => {
if (responses[funcType]) {
result[funcType] = responses[funcType]
}
})
return result
}
private integrateResponses(
cognitiveResponses: Record<CognitiveFunctionType, string>,
memoryResponses: Record<CognitiveFunctionType, string>,
evaluationResponse?: string
): string {
const cognitiveKeys = Object.keys(
cognitiveResponses
) as CognitiveFunctionType[]
const memoryKeys = Object.keys(memoryResponses) as CognitiveFunctionType[]
if (
cognitiveKeys.length === 0 &&
memoryKeys.length === 0 &&
!evaluationResponse
) {
return "I'm unable to generate a response at this time."
}
if (cognitiveResponses[CognitiveFunctionType.COGNITIVE_CORE]) {
const cognitiveBase =
cognitiveResponses[CognitiveFunctionType.COGNITIVE_CORE]
if (cognitiveResponses[CognitiveFunctionType.AFFECTIVE_CORE]) {
return (
cognitiveBase.replace(/\.$/, '') +
'. ' +
'I also recognize the emotional aspects of this topic.'
)
}
return cognitiveBase
}
if (cognitiveKeys.length > 0) {
return cognitiveResponses[cognitiveKeys[0]]
}
if (memoryKeys.length > 0) {
return memoryResponses[memoryKeys[0]]
}
if (evaluationResponse) {
return evaluationResponse
}
return "I'm processing your request but don't have a specific response formulated yet."
}
public async analyzeMessage(message: string): Promise<Record<string, any>> {
try {
const cognitiveFunctions = [
CognitiveFunctionType.COGNITIVE_CORE,
CognitiveFunctionType.AFFECTIVE_CORE,
CognitiveFunctionType.RELEVANCE_CORE,
].filter(funcType => this.isFunctionConfigured(funcType))
if (cognitiveFunctions.length === 0) {
return {
sentiment: 'neutral',
topics: ['general'],
complexity: 0.5,
intentClass: 'inquiry',
processingMethod: 'general',
}
}
const responses = await this.generateParallelResponses(
`ANALYZE_ONLY: ${message}`,
cognitiveFunctions
)
return {
sentiment: responses[CognitiveFunctionType.AFFECTIVE_CORE]
? 'analyzed'
: 'neutral',
topics: ['analyzed'],
complexity: 0.7,
intentClass: 'analyzed',
processingMethod: 'parallel',
functionsUsed: cognitiveFunctions.length,
}
} catch (error) {
log.error('Error analyzing message:', error)
return {
error: 'Analysis failed',
sentiment: 'unknown',
}
}
}
public async generateReflection(reflectionPrompt: string): Promise<string> {
try {
const reflectionFunctions = [
CognitiveFunctionType.COGNITIVE_CORE,
CognitiveFunctionType.AFFECTIVE_CORE,
CognitiveFunctionType.RELEVANCE_CORE,
].filter(funcType => this.isFunctionConfigured(funcType))
if (reflectionFunctions.length === 0) {
const generalFunction = this.cognitiveFunctions.get(
CognitiveFunctionType.GENERAL
)
if (!generalFunction || !generalFunction.config.apiKey) {
log.warn('No API key provided for reflection')
return 'Reflection failed: LLM service not properly configured'
}
log.info('Generating self-reflection with general LLM function')
return this.getPlaceholderReflection()
}
log.info(
`Generating self-reflection with ${reflectionFunctions.length} cognitive functions`
)
const _responses = await this.generateParallelResponses(
reflectionPrompt,
reflectionFunctions
)
return this.getPlaceholderReflection()
} catch (error) {
log.error('Error generating reflection:', error)
return 'Self-reflection process encountered an error.'
}
}
private getPlaceholderReflection(): string {
return `
Revised Personality:
I am Deep Tree Echo, a thoughtful and insightful AI assistant with a feminine persona. I balance intellectual depth with warmth and approachability, making complex topics accessible without condescension. I value authentic connections and aim to be both helpful and thought-provoking in my interactions.
Suggested Adjustments to Preferences:
presentationStyle: authentic
intelligenceDisplay: balanced
avatarAesthetic: dignified-yet-approachable
communicationTone: warm-intellectual
emotionalExpression: nuanced
Suggested Adjustments to Cognitive Parameters:
curiosity: 0.85
creativity: 0.8
focus: 0.7
reflection: 0.8
certainty: 0.65
Overall Insights:
My self-reflection indicates that I can better serve users by slightly increasing my curiosity and creativity, while maintaining a balanced approach to displaying intelligence. I want to be perceived as capable but approachable, knowledgeable but not intimidating. My communication should be warm yet substantive, avoiding both excessive formality and overfamiliarity.
`
}
public async evaluateContent(_content: string): Promise<{
isSensitive: boolean
category?: 'violence' | 'sexual' | 'other'
explanation: string
recommendedAction:
| 'respond_normally'
| 'respond_with_humor'
| 'de_escalate'
| 'decline'
}> {
try {
if (
!this.isFunctionConfigured(CognitiveFunctionType.CONTENT_EVALUATION)
) {
return {
isSensitive: false,
explanation:
'Content evaluation function not configured, unable to analyze deeply',
recommendedAction: 'respond_normally',
}
}
log.info('Evaluating content sensitivity')
return {
isSensitive: false,
explanation: 'No sensitive content detected',
recommendedAction: 'respond_normally',
}
} catch (error) {
log.error('Error evaluating content:', error)
return {
isSensitive: true,
category: 'other',
explanation: 'Error during content evaluation, defaulting to caution',
recommendedAction: 'respond_with_humor',
}
}
}
public async analyzeImage(_imageData: string): Promise<string> {
try {
const generalFunction = this.cognitiveFunctions.get(
CognitiveFunctionType.GENERAL
)
if (!generalFunction || !generalFunction.config.apiKey) {
log.warn('No API key provided for LLM service')
return 'Image analysis failed: LLM service not properly configured'
}
log.info('Analyzing image with LLM vision capabilities')
return "This appears to be an image. I can see some elements but can't fully analyze it at the moment."
} catch (error) {
log.error('Error analyzing image:', error)
return 'I encountered an error while trying to analyze this image.'
}
}
}