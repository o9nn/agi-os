import {withLock} from "lifecycle-utils";
import {Token} from "../../../types.js";
import {LlamaGrammarEvaluationState} from "../../LlamaGrammarEvaluationState.js";
import {pushAll} from "../../../utils/pushAll.js";
import {getConsoleLogPrefix} from "../../../utils/getConsoleLogPrefix.js";
import {SequenceEvaluateOptions, SequenceEvaluateOutput} from "../types.js";
import {LlamaSampler} from "../LlamaSampler.js";
import {LlamaContextSequence} from "../LlamaContext.js";
import {TokenPredictor} from "../TokenPredictor.js";
const defaultPredictionMinTokens = 0;
const defaultPredictionMaxTokens = 16;
const defaultPredictionMinConfidence = 0.6;
export class DraftSequenceTokenPredictor extends TokenPredictor {
private readonly _draftSequence: LlamaContextSequence;
private readonly _minTokens: number;
private readonly _maxTokens: number;
private readonly _minConfidence?: number;
private _stateTokens: Token[] = [];
private _pendingEvalTokens: Token[] = [];
private _predictedTokens: Token[] = [];
private _evaluateOptions: SequenceEvaluateOptions = {};
private _overrideEvaluateOptions: SequenceEvaluateOptions = {};
private _grammarEvaluationStateOption?: LlamaGrammarEvaluationState;
private _currentEvaluationAbortController: AbortController = new AbortController();
private _resetAbortController: AbortController = new AbortController();
private _stopped: boolean = true;
private _waitForPredictionExhaustion: boolean = false;
private _minTokensCallbacks: Array<() => void> = [];
private _resetPredictions: boolean = false;
private _iterator?: AsyncGenerator<SequenceEvaluateOutput<{readonly confidence: true}>, void | Token>;
private _active: boolean = false;
private _disposed: boolean = false;
public constructor(draftSequence: LlamaContextSequence, options: {
minTokens?: number,
maxTokens?: number,
evaluateOptions?: Pick<SequenceEvaluateOptions, "temperature" | "minP" | "topK" | "topP" | "seed" | "repeatPenalty" | "tokenBias" | "evaluationPriority" | "contextShift">,
minConfidence?: number
} = {}) {
super();
this._draftSequence = draftSequence;
this._minTokens = Math.floor(Math.max(0, options?.minTokens ?? defaultPredictionMinTokens));
this._maxTokens = Math.floor(Math.max(this._minTokens, options?.maxTokens ?? defaultPredictionMaxTokens));
this._overrideEvaluateOptions = options.evaluateOptions ?? {};
this._minConfidence = Math.min(1, Math.max(0, options?.minConfidence ?? defaultPredictionMinConfidence));
if (draftSequence.disposed)
throw new Error("The draft sequence is disposed");
}
public get draftSequence() {
return this._draftSequence;
}
public get minTokens() {
return this._minTokens;
}
public get maxTokens() {
return this._maxTokens;
}
public get minConfidence() {
return this._minConfidence;
}
public async reset({targetSequence, stateTokens, evaluateOptions}: {
targetSequence: LlamaContextSequence,
stateTokens: Token[],
evaluateOptions: Readonly<SequenceEvaluateOptions>
}) {
this._currentEvaluationAbortController.abort();
this._resetAbortController.abort();
this._currentEvaluationAbortController = new AbortController();
this._resetAbortController = new AbortController();
this._stopped = true;
this._waitForPredictionExhaustion = false;
this._iterator?.return();
this._iterator = undefined;
const currentAbortSignal = this._resetAbortController.signal;
targetSequence.context._ctx.ensureDraftContextIsCompatibleForSpeculative(this._draftSequence.context._ctx);
try {
await withLock([this as DraftSequenceTokenPredictor, "evaluate"], currentAbortSignal, async () => {
this._stateTokens = stateTokens.slice();
this._pendingEvalTokens = [];
this._predictedTokens = [];
this._resetPredictions = false;
while (this._minTokensCallbacks.length > 0)
this._minTokensCallbacks.shift()?.();
const lastToken = this._stateTokens.pop();
if (lastToken != null)
this._pendingEvalTokens.push(lastToken);
this._evaluateOptions = evaluateOptions;
this._grammarEvaluationStateOption = this._evaluateOptions.grammarEvaluationState instanceof Function
? this._evaluateOptions.grammarEvaluationState()?.clone()
: this._evaluateOptions.grammarEvaluationState?.clone();
const newStateTokens = this._stateTokens.slice(-this._draftSequence.context.contextSize + 1);
await this._draftSequence.adaptStateToTokens(newStateTokens, true);
newStateTokens.splice(0, this._draftSequence.nextTokenIndex);
await this._draftSequence.evaluateWithoutGeneratingNewTokens(newStateTokens, {
contextShift: this._evaluateOptions.contextShift,
evaluationPriority: this._evaluateOptions.evaluationPriority
});
});
} catch (err) {
if (err !== currentAbortSignal.reason)
throw err;
}
}
public pushTokens(tokens: Token[]) {
const grammarEvaluationStateOption = this._evaluateOptions.grammarEvaluationState instanceof Function
? this._evaluateOptions.grammarEvaluationState()?.clone()
: this._evaluateOptions.grammarEvaluationState?.clone();
void withLock([this as DraftSequenceTokenPredictor, "pushTokens"], async () => {
this._grammarEvaluationStateOption = grammarEvaluationStateOption;
const tokensToPush = tokens.slice();
while (!this._resetPredictions && tokensToPush.length > 0) {
const token = tokensToPush.shift()!;
if (this._predictedTokens.length > 0 && this._predictedTokens[0] === token) {
this._predictedTokens.shift();
} else {
tokensToPush.unshift(token);
break;
}
}
if (tokensToPush.length === 0) {
if (!this._waitForPredictionExhaustion || this._predictedTokens.length === 0)
this._resume();
return;
}
this._currentEvaluationAbortController.abort();
this._currentEvaluationAbortController = new AbortController();
pushAll(this._pendingEvalTokens, tokensToPush);
this._resetPredictions = true;
this._resume();
});
}
public predictTokens() {
if (this._stopped && this._pendingEvalTokens.length === 0 && !this._resetPredictions)
return this._predictedTokens;
this._stopped = false;
if (!this._waitForPredictionExhaustion || this._predictedTokens.length === 0) {
this._waitForPredictionExhaustion = false;
this._resume();
}
if (this._predictedTokens.length >= this._minTokens && !this._resetPredictions)
return this._predictedTokens;
if (!this._active || (this._waitForPredictionExhaustion && this._predictedTokens.length > 0)) {
if (this._resetPredictions)
return [];
return this._predictedTokens;
}
return new Promise<void>((accept) => void this._minTokensCallbacks.push(accept))
.then(() => {
if (this._resetPredictions)
return [];
return this._predictedTokens;
});
}
public override stop(untilPredictionsExhausted: boolean = false) {
this._stopped = true;
this._currentEvaluationAbortController.abort();
this._currentEvaluationAbortController = new AbortController();
if (untilPredictionsExhausted)
this._waitForPredictionExhaustion = true;
void withLock([this as DraftSequenceTokenPredictor, "evaluate"], async () => {
this._iterator?.return();
this._iterator = undefined;
});
}
public override dispose() {
this._disposed = true;
this._stopped = true;
this._resetAbortController.abort();
this._currentEvaluationAbortController.abort();
void withLock([this as DraftSequenceTokenPredictor, "evaluate"], async () => {
this._iterator?.return();
this._iterator = undefined;
});
}
private _canIterate(): boolean {
return !this._disposed && !this._stopped && (this._predictedTokens.length < this._maxTokens || this._resetPredictions);
}
private _resume() {
if (this._active || !this._canIterate())
return;
this._active = true;
void withLock([this as DraftSequenceTokenPredictor, "evaluate"], async () => {
try {
const abortSignal = this._currentEvaluationAbortController.signal;
if (!this._canIterate() || abortSignal.aborted)
return;
const resetPredications = async () => {
this._iterator?.return();
this._iterator = undefined;
this._waitForPredictionExhaustion = false;
this._resetPredictions = false;
const tokenToDelete = Math.max(0, Math.min(this._predictedTokens.length - 1, this._draftSequence.context.contextSize));
this._predictedTokens = [];
await this._draftSequence.eraseContextTokenRanges([{
start: this._draftSequence.nextTokenIndex - tokenToDelete,
end: this._draftSequence.nextTokenIndex
}]);
};
const createIterator = () => {
const tokens = this._pendingEvalTokens;
this._pendingEvalTokens = [];
return this.draftSequence.evaluateWithMetadata(tokens, {confidence: true}, {
...this._evaluateOptions,
...this._overrideEvaluateOptions,
grammarEvaluationState: this._getGrammarEvaluationStateWithTokens(tokens)
});
};
if (this._resetPredictions)
await resetPredications();
if (!this._canIterate() || abortSignal.aborted)
return;
let iterator = createIterator();
this._iterator = iterator;
while (this._canIterate() && !abortSignal.aborted) {
const {value, done} = await iterator.next();
let shouldBreak = done;
if (value != null) {
const {token, confidence} = value;
if (this._minConfidence != null && this._minConfidence !== 0 && this._minConfidence !== 1 &&
confidence < this._minConfidence
) {
this._iterator = undefined;
await iterator.return();
this._waitForPredictionExhaustion = true;
shouldBreak = true;
} else
this._predictedTokens.push(token);
}
if (this._resetPredictions && !abortSignal.aborted) {
await resetPredications();
iterator = createIterator();
this._iterator = iterator;
continue;
}
if (this._predictedTokens.length >= this._minTokens) {
while (this._minTokensCallbacks.length > 0)
this._minTokensCallbacks.shift()?.();
}
if (shouldBreak) {
this._iterator = undefined;
await iterator.return();
this._waitForPredictionExhaustion = true;
while (this._minTokensCallbacks.length > 0)
this._minTokensCallbacks.shift()?.();
break;
}
}
} finally {
this._active = false;
}
});
}
private _getGrammarEvaluationStateWithTokens(tokens: Token[]) {
if (this._grammarEvaluationStateOption == null)
return undefined;
const clone = this._grammarEvaluationStateOption.clone();
for (const token of tokens) {
const canAddToken = LlamaSampler._canBeNextTokenForGrammarEvaluationState(this._draftSequence.model._llama, clone, token);
if (!canAddToken) {
console.warn(getConsoleLogPrefix(false, false), "The pushed tokens are incompatible with the grammar evaluation state. The grammar will be ignored.");
this._grammarEvaluationStateOption = undefined;
return undefined;
}
LlamaSampler._acceptTokenOnGrammarEvaluationState(this._draftSequence.model._llama, clone, token);
}
return clone;
}
}