import {DisposeAggregator, DisposedError, EventRelay, withLock} from "lifecycle-utils";
import {ChatWrapper} from "../../ChatWrapper.js";
import {
ChatHistoryItem, ChatModelFunctionCall, ChatModelFunctions, ChatModelResponse, ChatSessionModelFunction, ChatSessionModelFunctions,
Token
} from "../../types.js";
import {appendUserMessageToChatHistory} from "../../utils/appendUserMessageToChatHistory.js";
import {LlamaContextSequence} from "../LlamaContext/LlamaContext.js";
import {LlamaGrammar} from "../LlamaGrammar.js";
import {
LlamaChat, LLamaChatContextShiftOptions, LlamaChatResponse, LlamaChatResponseChunk, LlamaChatResponseFunctionCall,
LlamaChatResponseFunctionCallParamsChunk
} from "../LlamaChat/LlamaChat.js";
import {EvaluationPriority} from "../LlamaContext/types.js";
import {TokenBias} from "../TokenBias.js";
import {LlamaText, LlamaTextJSON} from "../../utils/LlamaText.js";
import {wrapAbortSignal} from "../../utils/wrapAbortSignal.js";
import {safeEventCallback} from "../../utils/safeEventCallback.js";
import {GgufArchitectureType} from "../../gguf/types/GgufMetadataTypes.js";
import {
LLamaChatPromptCompletionEngineOptions, LlamaChatSessionPromptCompletionEngine
} from "./utils/LlamaChatSessionPromptCompletionEngine.js";
export type LlamaChatSessionOptions = {
contextSequence: LlamaContextSequence,
chatWrapper?: "auto" | ChatWrapper,
systemPrompt?: string,
forceAddSystemPrompt?: boolean,
autoDisposeSequence?: boolean,
contextShift?: LlamaChatSessionContextShiftOptions
};
export type LlamaChatSessionContextShiftOptions = {
size?: LLamaChatContextShiftOptions["size"],
strategy?: LLamaChatContextShiftOptions["strategy"]
};
export type LLamaChatPromptOptions<Functions extends ChatSessionModelFunctions | undefined = ChatSessionModelFunctions | undefined> = {
onTextChunk?: (text: string) => void,
onToken?: (tokens: Token[]) => void,
onResponseChunk?: (chunk: LlamaChatResponseChunk) => void,
signal?: AbortSignal,
stopOnAbortSignal?: boolean,
maxTokens?: number,
temperature?: number,
minP?: number,
topK?: number,
topP?: number,
seed?: number,
trimWhitespaceSuffix?: boolean,
responsePrefix?: string,
evaluationPriority?: EvaluationPriority,
repeatPenalty?: false | LlamaChatSessionRepeatPenalty,
tokenBias?: TokenBias | (() => TokenBias),
customStopTriggers?: (LlamaText | string | (string | Token)[])[],
onFunctionCallParamsChunk?: (chunk: LlamaChatResponseFunctionCallParamsChunk) => void,
budgets?: {
thoughtTokens?: number,
commentTokens?: number
}
} & ({
grammar?: LlamaGrammar,
functions?: never,
documentFunctionParams?: never,
maxParallelFunctionCalls?: never,
onFunctionCallParamsChunk?: never
} | {
grammar?: never,
functions?: Functions | ChatSessionModelFunctions,
documentFunctionParams?: boolean,
maxParallelFunctionCalls?: number,
onFunctionCallParamsChunk?: (chunk: LlamaChatResponseFunctionCallParamsChunk) => void
});
export type LLamaChatCompletePromptOptions = {
maxTokens?: LLamaChatPromptOptions["maxTokens"],
stopOnAbortSignal?: LLamaChatPromptOptions["stopOnAbortSignal"],
onTextChunk?: LLamaChatPromptOptions["onTextChunk"],
onToken?: LLamaChatPromptOptions["onToken"],
signal?: LLamaChatPromptOptions["signal"],
temperature?: LLamaChatPromptOptions["temperature"],
minP?: LLamaChatPromptOptions["minP"],
topK?: LLamaChatPromptOptions["topK"],
topP?: LLamaChatPromptOptions["topP"],
seed?: LLamaChatPromptOptions["seed"],
trimWhitespaceSuffix?: LLamaChatPromptOptions["trimWhitespaceSuffix"],
evaluationPriority?: LLamaChatPromptOptions["evaluationPriority"],
repeatPenalty?: LLamaChatPromptOptions["repeatPenalty"],
tokenBias?: LLamaChatPromptOptions["tokenBias"],
customStopTriggers?: LLamaChatPromptOptions["customStopTriggers"],
grammar?: LlamaGrammar,
functions?: ChatSessionModelFunctions,
documentFunctionParams?: boolean,
completeAsModel?: "auto" | boolean | {
enabled?: "auto" | boolean,
appendedMessages?: ChatHistoryItem[]
}
};
export type LLamaChatPreloadPromptOptions = {
signal?: LLamaChatCompletePromptOptions["signal"],
evaluationPriority?: LLamaChatCompletePromptOptions["evaluationPriority"],
functions?: LLamaChatCompletePromptOptions["functions"],
documentFunctionParams?: LLamaChatCompletePromptOptions["documentFunctionParams"]
};
export type LlamaChatSessionRepeatPenalty = {
lastTokens?: number,
punishTokensFilter?: (tokens: Token[]) => Token[],
penalizeNewLine?: boolean,
penalty?: number,
frequencyPenalty?: number,
presencePenalty?: number
};
const defaultCompleteAsModel = {
enabled: "auto",
appendedMessages: [
{
type: "system",
text: "For your next response predict what the user may send next. No yapping, no whitespace. Match the user's language and tone."
},
{type: "user", text: ""},
{type: "model", response: [""]}
]
} as const satisfies LLamaChatCompletePromptOptions["completeAsModel"];
export class LlamaChatSession {
private readonly _disposeAggregator = new DisposeAggregator();
private readonly _autoDisposeSequence: boolean;
private readonly _contextShift?: LlamaChatSessionContextShiftOptions;
private readonly _forceAddSystemPrompt: boolean;
private readonly _systemPrompt?: string;
private readonly _chatLock = {};
private _chatHistory: ChatHistoryItem[];
private _lastEvaluation?: LlamaChatResponse["lastEvaluation"];
private _canUseContextWindowForCompletion: boolean = true;
private _chat: LlamaChat | null;
public _chatHistoryStateRef = {};
public readonly _preloadAndCompleteAbortControllers = new Set<AbortController>();
public readonly onDispose = new EventRelay<void>();
public constructor(options: LlamaChatSessionOptions) {
const {
contextSequence,
chatWrapper = "auto",
systemPrompt,
forceAddSystemPrompt = false,
autoDisposeSequence = false,
contextShift
} = options;
if (contextSequence == null)
throw new Error("contextSequence cannot be null");
if (contextSequence.disposed)
throw new DisposedError();
this._contextShift = contextShift;
this._forceAddSystemPrompt = forceAddSystemPrompt;
this._systemPrompt = systemPrompt;
this._chat = new LlamaChat({
autoDisposeSequence,
chatWrapper,
contextSequence
});
const chatWrapperSupportsSystemMessages = this._chat.chatWrapper.settings.supportsSystemMessages;
if (chatWrapperSupportsSystemMessages == null || chatWrapperSupportsSystemMessages || this._forceAddSystemPrompt)
this._chatHistory = this._chat.chatWrapper.generateInitialChatHistory({systemPrompt: this._systemPrompt});
else
this._chatHistory = [];
this._autoDisposeSequence = autoDisposeSequence;
this._disposeAggregator.add(
this._chat.onDispose.createListener(() => {
this.dispose();
})
);
this._disposeAggregator.add(this.onDispose.dispatchEvent);
}
public dispose({disposeSequence = this._autoDisposeSequence}: {disposeSequence?: boolean} = {}) {
if (this._chat == null)
return;
this._chat.dispose({disposeSequence});
this._chat = null;
this._disposeAggregator.dispose();
}
public [Symbol.dispose]() {
return this.dispose();
}
public get disposed() {
return this._chat == null || this._chat.disposed;
}
public get chatWrapper() {
if (this._chat == null)
throw new DisposedError();
return this._chat.chatWrapper;
}
public get sequence() {
if (this._chat == null)
throw new DisposedError();
return this._chat.sequence;
}
public get context() {
return this.sequence.context;
}
public get model() {
return this.sequence.model;
}
public async prompt<const Functions extends ChatSessionModelFunctions | undefined = undefined>(
prompt: string,
options: LLamaChatPromptOptions<Functions> = {}
) {
const {
functions,
documentFunctionParams,
maxParallelFunctionCalls,
onTextChunk,
onToken,
onResponseChunk,
onFunctionCallParamsChunk,
budgets,
signal,
stopOnAbortSignal = false,
maxTokens,
temperature,
minP,
topK,
topP,
seed,
grammar,
trimWhitespaceSuffix = false,
responsePrefix,
repeatPenalty,
tokenBias,
customStopTriggers
} = options;
const {responseText} = await this.promptWithMeta<Functions>(prompt, {
functions: functions as undefined,
grammar: grammar as undefined,
documentFunctionParams: documentFunctionParams as undefined,
maxParallelFunctionCalls: maxParallelFunctionCalls as undefined,
onFunctionCallParamsChunk: onFunctionCallParamsChunk as undefined,
onTextChunk, onToken, onResponseChunk, budgets, signal, stopOnAbortSignal, maxTokens,
temperature, minP, topK, topP, seed,
trimWhitespaceSuffix, responsePrefix, repeatPenalty, tokenBias, customStopTriggers
});
return responseText;
}
public async promptWithMeta<const Functions extends ChatSessionModelFunctions | undefined = undefined>(prompt: string, {
functions,
documentFunctionParams,
maxParallelFunctionCalls,
onTextChunk,
onToken,
onResponseChunk,
onFunctionCallParamsChunk,
budgets,
signal,
stopOnAbortSignal = false,
maxTokens,
temperature,
minP,
topK,
topP,
seed,
grammar,
trimWhitespaceSuffix = false,
responsePrefix,
repeatPenalty,
tokenBias,
customStopTriggers,
evaluationPriority
}: LLamaChatPromptOptions<Functions> = {}) {
this._ensureNotDisposed();
if (grammar != null && grammar._llama !== this.model._llama)
throw new Error("The LlamaGrammar used by passed to this function was created with a different Llama instance than the one used by this sequence's model. Make sure you use the same Llama instance for both the model and the grammar.");
this._stopAllPreloadAndPromptCompletions();
return await withLock([this._chatLock, "evaluation"], signal, async () => {
this._ensureNotDisposed();
this._stopAllPreloadAndPromptCompletions();
if (this._chat == null)
throw new DisposedError();
const supportsParallelFunctionCalling = this._chat.chatWrapper.settings.functions.parallelism != null;
const [abortController, disposeAbortController] = wrapAbortSignal(signal);
let lastEvaluation = this._canUseContextWindowForCompletion
? this._lastEvaluation
: undefined;
let newChatHistory = appendUserMessageToChatHistory(this._chatHistory, prompt);
let newContextWindowChatHistory = lastEvaluation?.contextWindow == null
? undefined
: appendUserMessageToChatHistory(lastEvaluation?.contextWindow, prompt);
let previousFunctionCalls: number = 0;
const resolvedResponsePrefix = (responsePrefix != null && responsePrefix !== "")
? responsePrefix
: undefined;
newChatHistory.push({
type: "model",
response: resolvedResponsePrefix != null
? [resolvedResponsePrefix]
: []
});
if (newContextWindowChatHistory != null)
newContextWindowChatHistory.push({
type: "model",
response: resolvedResponsePrefix != null
? [resolvedResponsePrefix]
: []
});
if (resolvedResponsePrefix != null) {
safeEventCallback(onToken)?.(this.model.tokenize(resolvedResponsePrefix));
safeEventCallback(onTextChunk)?.(resolvedResponsePrefix);
safeEventCallback(onResponseChunk)?.({
type: undefined,
segmentType: undefined,
text: resolvedResponsePrefix,
tokens: this.model.tokenize(resolvedResponsePrefix)
});
}
try {
while (true) {
const functionCallsAndResults: Array<Promise<null | {
functionCall: LlamaChatResponseFunctionCall<Functions extends ChatModelFunctions ? Functions : ChatModelFunctions>,
functionDefinition: ChatSessionModelFunction<any>,
functionCallResult: any
}>> = [];
let canThrowFunctionCallingErrors = false;
let abortedOnFunctionCallError = false;
const initialOutputTokens = this._chat.sequence.tokenMeter.usedOutputTokens;
const {
lastEvaluation: currentLastEvaluation,
metadata
} = await this._chat.generateResponse<Functions>(newChatHistory, {
functions,
documentFunctionParams,
maxParallelFunctionCalls,
grammar: grammar as undefined,
onTextChunk: safeEventCallback(onTextChunk),
onToken: safeEventCallback(onToken),
onResponseChunk: safeEventCallback(onResponseChunk),
onFunctionCallParamsChunk: onFunctionCallParamsChunk == null
? undefined
: safeEventCallback((chunk) => onFunctionCallParamsChunk?.({
callIndex: previousFunctionCalls + chunk.callIndex,
functionName: chunk.functionName,
paramsChunk: chunk.paramsChunk,
done: chunk.done
})),
budgets: {
includeCurrentResponse: true,
thoughtTokens: budgets?.thoughtTokens,
commentTokens: budgets?.commentTokens
},
signal: abortController.signal,
stopOnAbortSignal,
repeatPenalty,
minP,
topK,
topP,
seed,
tokenBias,
customStopTriggers,
maxTokens,
temperature,
trimWhitespaceSuffix,
contextShift: {
...this._contextShift,
lastEvaluationMetadata: lastEvaluation?.contextShiftMetadata
},
evaluationPriority,
lastEvaluationContextWindow: {
history: newContextWindowChatHistory,
minimumOverlapPercentageToPreventContextShift: 0.5
},
onFunctionCall: async (functionCall) => {
functionCallsAndResults.push(
(async () => {
try {
const functionDefinition = functions?.[functionCall.functionName];
if (functionDefinition == null)
throw new Error(
`The model tried to call function "${functionCall.functionName}" which is not defined`
);
const functionCallResult = await functionDefinition.handler(functionCall.params as any);
return {
functionCall,
functionDefinition,
functionCallResult
};
} catch (err) {
if (!abortController.signal.aborted) {
abortedOnFunctionCallError = true;
abortController.abort(err);
}
if (canThrowFunctionCallingErrors)
throw err;
return null;
}
})()
);
}
});
this._ensureNotDisposed();
if (abortController.signal.aborted && (abortedOnFunctionCallError || !stopOnAbortSignal))
throw abortController.signal.reason;
if (maxTokens != null)
maxTokens = Math.max(0, maxTokens - (this._chat.sequence.tokenMeter.usedOutputTokens - initialOutputTokens));
lastEvaluation = currentLastEvaluation;
newChatHistory = lastEvaluation.cleanHistory;
if (functionCallsAndResults.length > 0) {
canThrowFunctionCallingErrors = true;
const functionCallResultsPromise = Promise.all(functionCallsAndResults);
const raceEventAbortController = new AbortController();
await Promise.race([
functionCallResultsPromise,
new Promise<void>((accept, reject) => {
abortController.signal.addEventListener("abort", () => {
if (abortedOnFunctionCallError || !stopOnAbortSignal)
reject(abortController.signal.reason);
else
accept();
}, {signal: raceEventAbortController.signal});
if (abortController.signal.aborted) {
if (abortedOnFunctionCallError || !stopOnAbortSignal)
reject(abortController.signal.reason);
else
accept();
}
})
]);
raceEventAbortController.abort();
this._ensureNotDisposed();
if (!abortController.signal.aborted) {
const functionCallResults = (await functionCallResultsPromise)
.filter((result): result is Exclude<typeof result, null> => result != null);
this._ensureNotDisposed();
if (abortController.signal.aborted && (abortedOnFunctionCallError || !stopOnAbortSignal))
throw abortController.signal.reason;
newContextWindowChatHistory = lastEvaluation.contextWindow;
let startNewChunk = supportsParallelFunctionCalling;
for (const {functionCall, functionDefinition, functionCallResult} of functionCallResults) {
newChatHistory = addFunctionCallToChatHistory({
chatHistory: newChatHistory,
functionName: functionCall.functionName,
functionDescription: functionDefinition.description,
callParams: functionCall.params,
callResult: functionCallResult,
rawCall: functionCall.raw,
startsNewChunk: startNewChunk
});
newContextWindowChatHistory = addFunctionCallToChatHistory({
chatHistory: newContextWindowChatHistory,
functionName: functionCall.functionName,
functionDescription: functionDefinition.description,
callParams: functionCall.params,
callResult: functionCallResult,
rawCall: functionCall.raw,
startsNewChunk: startNewChunk
});
startNewChunk = false;
previousFunctionCalls++;
}
lastEvaluation.cleanHistory = newChatHistory;
lastEvaluation.contextWindow = newContextWindowChatHistory;
if (abortController.signal.aborted && !abortedOnFunctionCallError && stopOnAbortSignal) {
metadata.stopReason = "abort";
metadata.remainingGenerationAfterStop = undefined;
} else
continue;
}
}
this._lastEvaluation = lastEvaluation;
this._canUseContextWindowForCompletion = true;
this._chatHistory = newChatHistory;
this._chatHistoryStateRef = {};
const lastModelResponseItem = getLastModelResponseItem(newChatHistory);
const responseText = lastModelResponseItem.response
.filter((item): item is string => typeof item === "string")
.join("");
if (metadata.stopReason === "customStopTrigger")
return {
response: lastModelResponseItem.response,
responseText,
stopReason: metadata.stopReason,
customStopTrigger: metadata.customStopTrigger,
remainingGenerationAfterStop: metadata.remainingGenerationAfterStop
};
return {
response: lastModelResponseItem.response,
responseText,
stopReason: metadata.stopReason,
remainingGenerationAfterStop: metadata.remainingGenerationAfterStop
};
}
} finally {
disposeAbortController();
}
});
}
public async preloadPrompt(prompt: string, options: LLamaChatPreloadPromptOptions = {}): Promise<void> {
await this.completePromptWithMeta(prompt, {
...options,
completeAsModel: false,
maxTokens: 0
});
}
public async completePrompt(prompt: string, options: LLamaChatCompletePromptOptions = {}): Promise<string> {
const {completion} = await this.completePromptWithMeta(prompt, options);
return completion;
}
public createPromptCompletionEngine(options?: LLamaChatPromptCompletionEngineOptions) {
return LlamaChatSessionPromptCompletionEngine._create(this, options);
}
public async completePromptWithMeta(prompt: string, {
maxTokens,
stopOnAbortSignal = false,
functions,
documentFunctionParams,
onTextChunk,
onToken,
signal,
temperature,
minP,
topK,
topP,
seed,
grammar,
trimWhitespaceSuffix = false,
repeatPenalty,
tokenBias,
customStopTriggers,
evaluationPriority,
completeAsModel
}: LLamaChatCompletePromptOptions = {}) {
this._ensureNotDisposed();
if (grammar != null) {
if (grammar._llama == null)
throw new Error("The grammar passed to this function is not a LlamaGrammar instance.");
else if (grammar._llama !== this.model._llama)
throw new Error("The LlamaGrammar used by passed to this function was created with a different Llama instance than the one used by this sequence's model. Make sure you use the same Llama instance for both the model and the grammar.");
}
const [abortController, disposeAbortController] = wrapAbortSignal(signal);
this._preloadAndCompleteAbortControllers.add(abortController);
const completeAsModelEnabled = typeof completeAsModel == "boolean"
? completeAsModel
: completeAsModel === "auto"
? "auto"
: completeAsModel?.enabled ?? defaultCompleteAsModel.enabled;
const modelArchitecture = this.model.fileInfo.metadata?.general?.architecture;
const shouldCompleteAsModel = completeAsModelEnabled === "auto"
? modelArchitecture === GgufArchitectureType.gptOss
: completeAsModelEnabled;
try {
return await withLock([this._chatLock, "evaluation"], abortController.signal, async () => {
this._ensureNotDisposed();
if (this._chat == null)
throw new DisposedError();
if (shouldCompleteAsModel) {
const messagesToAppendOption = (typeof completeAsModel == "boolean" || completeAsModel === "auto")
? defaultCompleteAsModel.appendedMessages
: completeAsModel?.appendedMessages ?? defaultCompleteAsModel.appendedMessages;
const messagesToAppend = messagesToAppendOption.length === 0
? defaultCompleteAsModel.appendedMessages
: messagesToAppendOption;
const addMessageToChatHistory = (chatHistory: ChatHistoryItem[]): {
history: ChatHistoryItem[],
addedCount: number
} => {
const newHistory = chatHistory.slice();
if (messagesToAppend.at(0)?.type === "model")
newHistory.push({type: "user", text: ""});
for (let i = 0; i < messagesToAppend.length; i++) {
const item = messagesToAppend[i];
const isLastItem = i === messagesToAppend.length - 1;
if (item == null)
continue;
if (isLastItem && item.type === "model") {
const newResponse = item.response.slice();
if (typeof newResponse.at(-1) === "string")
newResponse.push((newResponse.pop()! as string) + prompt);
else
newResponse.push(prompt);
newHistory.push({
type: "model",
response: newResponse
});
} else
newHistory.push(item);
}
if (messagesToAppend.at(-1)?.type !== "model")
newHistory.push({type: "model", response: [prompt]});
return {
history: newHistory,
addedCount: newHistory.length - chatHistory.length
};
};
const {history: messagesWithPrompt, addedCount} = addMessageToChatHistory(this._chatHistory);
const {response, lastEvaluation, metadata} = await this._chat.generateResponse(
messagesWithPrompt,
{
abortOnNonText: true,
functions,
documentFunctionParams,
grammar: grammar as undefined,
onTextChunk,
onToken,
signal: abortController.signal,
stopOnAbortSignal: true,
repeatPenalty,
minP,
topK,
topP,
seed,
tokenBias,
customStopTriggers,
maxTokens: maxTokens == null
? undefined
: Math.min(1, maxTokens),
temperature,
trimWhitespaceSuffix,
contextShift: {
...this._contextShift,
lastEvaluationMetadata: this._lastEvaluation?.contextShiftMetadata
},
evaluationPriority,
lastEvaluationContextWindow: {
history: this._lastEvaluation?.contextWindow == null
? undefined
: addMessageToChatHistory(this._lastEvaluation?.contextWindow).history,
minimumOverlapPercentageToPreventContextShift: 0.8
}
}
);
this._ensureNotDisposed();
this._lastEvaluation = {
cleanHistory: this._chatHistory,
contextWindow: lastEvaluation.contextWindow.slice(0, -addedCount),
contextShiftMetadata: lastEvaluation.contextShiftMetadata
};
this._canUseContextWindowForCompletion = this._chatHistory.at(-1)?.type === "user";
if (!stopOnAbortSignal && metadata.stopReason === "abort" && abortController.signal?.aborted)
throw abortController.signal.reason;
if (metadata.stopReason === "customStopTrigger")
return {
completion: response,
stopReason: metadata.stopReason,
customStopTrigger: metadata.customStopTrigger,
remainingGenerationAfterStop: metadata.remainingGenerationAfterStop
};
return {
completion: response,
stopReason: metadata.stopReason,
remainingGenerationAfterStop: metadata.remainingGenerationAfterStop
};
} else {
const {completion, lastEvaluation, metadata} = await this._chat.loadChatAndCompleteUserMessage(
asWithLastUserMessageRemoved(this._chatHistory),
{
initialUserPrompt: prompt,
functions,
documentFunctionParams,
grammar,
onTextChunk,
onToken,
signal: abortController.signal,
stopOnAbortSignal: true,
repeatPenalty,
minP,
topK,
topP,
seed,
tokenBias,
customStopTriggers,
maxTokens,
temperature,
trimWhitespaceSuffix,
contextShift: {
...this._contextShift,
lastEvaluationMetadata: this._lastEvaluation?.contextShiftMetadata
},
evaluationPriority,
lastEvaluationContextWindow: {
history: asWithLastUserMessageRemoved(this._lastEvaluation?.contextWindow),
minimumOverlapPercentageToPreventContextShift: 0.8
}
}
);
this._ensureNotDisposed();
this._lastEvaluation = {
cleanHistory: this._chatHistory,
contextWindow: asWithLastUserMessageRemoved(lastEvaluation.contextWindow),
contextShiftMetadata: lastEvaluation.contextShiftMetadata
};
this._canUseContextWindowForCompletion = this._chatHistory.at(-1)?.type === "user";
if (!stopOnAbortSignal && metadata.stopReason === "abort" && abortController.signal?.aborted)
throw abortController.signal.reason;
if (metadata.stopReason === "customStopTrigger")
return {
completion: completion,
stopReason: metadata.stopReason,
customStopTrigger: metadata.customStopTrigger,
remainingGenerationAfterStop: metadata.remainingGenerationAfterStop
};
return {
completion: completion,
stopReason: metadata.stopReason,
remainingGenerationAfterStop: metadata.remainingGenerationAfterStop
};
}
});
} finally {
this._preloadAndCompleteAbortControllers.delete(abortController);
disposeAbortController();
}
}
public getChatHistory() {
return structuredClone(this._chatHistory);
}
public getLastEvaluationContextWindow() {
if (this._lastEvaluation == null)
return null;
return structuredClone(this._lastEvaluation?.contextWindow);
}
public setChatHistory(chatHistory: ChatHistoryItem[]) {
this._chatHistory = structuredClone(chatHistory);
this._chatHistoryStateRef = {};
this._lastEvaluation = undefined;
this._canUseContextWindowForCompletion = false;
}
public resetChatHistory() {
if (this._chat == null || this.disposed)
throw new DisposedError();
const chatWrapperSupportsSystemMessages = this._chat.chatWrapper.settings.supportsSystemMessages;
if (chatWrapperSupportsSystemMessages == null || chatWrapperSupportsSystemMessages || this._forceAddSystemPrompt)
this.setChatHistory(
this._chat.chatWrapper.generateInitialChatHistory({systemPrompt: this._systemPrompt})
);
else
this.setChatHistory([]);
}
private _stopAllPreloadAndPromptCompletions() {
for (const abortController of this._preloadAndCompleteAbortControllers)
abortController.abort();
this._preloadAndCompleteAbortControllers.clear();
}
private _ensureNotDisposed() {
if (this.disposed)
throw new DisposedError();
}
}
function addFunctionCallToChatHistory({
chatHistory,
functionName,
functionDescription,
callParams,
callResult,
rawCall,
startsNewChunk
}: {
chatHistory: ChatHistoryItem[],
functionName: string,
functionDescription?: string,
callParams: any,
callResult: any,
rawCall?: LlamaTextJSON,
startsNewChunk?: boolean
}) {
const newChatHistory = chatHistory.slice();
if (newChatHistory.length === 0 || newChatHistory[newChatHistory.length - 1]!.type !== "model")
newChatHistory.push({
type: "model",
response: []
});
const lastModelResponseItem = newChatHistory[newChatHistory.length - 1] as ChatModelResponse;
const newLastModelResponseItem = {...lastModelResponseItem};
newChatHistory[newChatHistory.length - 1] = newLastModelResponseItem;
const modelResponse = newLastModelResponseItem.response.slice();
newLastModelResponseItem.response = modelResponse;
const functionCall: ChatModelFunctionCall = {
type: "functionCall",
name: functionName,
description: functionDescription,
params: callParams,
result: callResult,
rawCall
};
if (startsNewChunk)
functionCall.startsNewChunk = true;
modelResponse.push(functionCall);
return newChatHistory;
}
function getLastModelResponseItem(chatHistory: ChatHistoryItem[]) {
if (chatHistory.length === 0 || chatHistory[chatHistory.length - 1]!.type !== "model")
throw new Error("Expected chat history to end with a model response");
return chatHistory[chatHistory.length - 1] as ChatModelResponse;
}
function asWithLastUserMessageRemoved(chatHistory: ChatHistoryItem[]): ChatHistoryItem[];
function asWithLastUserMessageRemoved(chatHistory: ChatHistoryItem[] | undefined): ChatHistoryItem[] | undefined;
function asWithLastUserMessageRemoved(chatHistory?: ChatHistoryItem[]) {
if (chatHistory == null)
return chatHistory;
const newChatHistory = chatHistory.slice();
while (newChatHistory.at(-1)?.type === "user")
newChatHistory.pop();
return newChatHistory;
}