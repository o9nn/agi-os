import {AsyncDisposeAggregator, EventRelay, splitText, withLock} from "lifecycle-utils";
import {Token} from "../types.js";
import {LlamaText} from "../utils/LlamaText.js";
import {tokenizeInput} from "../utils/tokenizeInput.js";
import {resolveBeginningTokenToPrepend, resolveEndTokenToAppend} from "../utils/tokenizerUtils.js";
import {isRankingTemplateValid, parseRankingTemplate} from "../gguf/insights/GgufInsights.js";
import type {LlamaModel} from "./LlamaModel/LlamaModel.js";
import type {LlamaContext, LlamaContextSequence} from "./LlamaContext/LlamaContext.js";
export type LlamaRankingContextOptions = {
contextSize?: "auto" | number | {
min?: number,
max?: number
},
batchSize?: number,
threads?: number,
createSignal?: AbortSignal,
template?: `${string}{{query}}${string}{{document}}${string}` | `${string}{{document}}${string}{{query}}${string}`,
ignoreMemorySafetyChecks?: boolean
};
export class LlamaRankingContext {
private readonly _llamaContext: LlamaContext;
private readonly _template: string | undefined;
private readonly _sequence: LlamaContextSequence;
private readonly _disposeAggregator = new AsyncDisposeAggregator();
public readonly onDispose = new EventRelay<void>();
private constructor({
_llamaContext,
_template
}: {
_llamaContext: LlamaContext,
_template: string | undefined
}) {
this._llamaContext = _llamaContext;
this._template = _template;
this._sequence = this._llamaContext.getSequence();
this._disposeAggregator.add(
this._llamaContext.onDispose.createListener(() => {
void this._disposeAggregator.dispose();
})
);
this._disposeAggregator.add(this.onDispose.dispatchEvent);
this._disposeAggregator.add(async () => {
await this._llamaContext.dispose();
});
}
public async rank(query: Token[] | string | LlamaText, document: Token[] | string | LlamaText) {
const resolvedInput = this._getEvaluationInput(query, document);
if (resolvedInput.length > this._llamaContext.contextSize)
throw new Error(
"The input length exceed the context size. " +
`Try to increase the context size to at least ${resolvedInput.length + 1} ` +
"or use another model that supports longer contexts."
);
return this._evaluateRankingForInput(resolvedInput);
}
public async rankAll(query: Token[] | string | LlamaText, documents: Array<Token[] | string | LlamaText>): Promise<number[]> {
const resolvedTokens = documents.map((document) => this._getEvaluationInput(query, document));
const maxInputTokensLength = resolvedTokens.reduce((max, tokens) => Math.max(max, tokens.length), 0);
if (maxInputTokensLength > this._llamaContext.contextSize)
throw new Error(
"The input lengths of some of the given documents exceed the context size. " +
`Try to increase the context size to at least ${maxInputTokensLength + 1} ` +
"or use another model that supports longer contexts."
);
else if (resolvedTokens.length === 0)
return [];
return await Promise.all(
resolvedTokens.map((tokens) => this._evaluateRankingForInput(tokens))
);
}
public async rankAndSort<const T extends string>(query: Token[] | string | LlamaText, documents: T[]): Promise<Array<{
document: T,
score: number
}>> {
const scores = await this.rankAll(query, documents);
return documents
.map((document, index) => ({document: document as T, score: scores[index]!}))
.sort((a, b) => b.score - a.score);
}
public async dispose() {
await this._disposeAggregator.dispose();
}
public [Symbol.asyncDispose]() {
return this.dispose();
}
public get disposed() {
return this._llamaContext.disposed;
}
public get model() {
return this._llamaContext.model;
}
private _getEvaluationInput(query: Token[] | string | LlamaText, document: Token[] | string | LlamaText) {
if (this._template != null) {
const resolvedInput = splitText(this._template, ["{{query}}", "{{document}}"])
.flatMap((item) => {
if (typeof item === "string")
return this._llamaContext.model.tokenize(item, true, "trimLeadingSpace");
else if (item.separator === "{{query}}")
return tokenizeInput(query, this._llamaContext.model.tokenizer, "trimLeadingSpace", false);
else if (item.separator === "{{document}}")
return tokenizeInput(document, this._llamaContext.model.tokenizer, "trimLeadingSpace", false);
else
void (item satisfies never);
void (item satisfies never);
return [];
});
const beginningTokens = resolveBeginningTokenToPrepend(this.model.vocabularyType, this.model.tokens);
const endToken = resolveEndTokenToAppend(this.model.vocabularyType, this.model.tokens);
if (beginningTokens != null && resolvedInput.at(0) !== beginningTokens)
resolvedInput.unshift(beginningTokens);
if (endToken != null && resolvedInput.at(-1) !== endToken)
resolvedInput.unshift(endToken);
return resolvedInput;
}
if (this.model.tokens.eos == null && this.model.tokens.sep == null)
throw new Error("Computing rankings is not supported for this model.");
const resolvedQuery = tokenizeInput(query, this._llamaContext.model.tokenizer, "trimLeadingSpace", false);
const resolvedDocument = tokenizeInput(document, this._llamaContext.model.tokenizer, "trimLeadingSpace", false);
if (resolvedQuery.length === 0 && resolvedDocument.length === 0)
return [];
const resolvedInput = [
...(this.model.tokens.bos == null ? [] : [this.model.tokens.bos]),
...resolvedQuery,
...(this.model.tokens.eos == null ? [] : [this.model.tokens.eos]),
...(this.model.tokens.sep == null ? [] : [this.model.tokens.sep]),
...resolvedDocument,
...(this.model.tokens.eos == null ? [] : [this.model.tokens.eos])
];
return resolvedInput;
}
private _evaluateRankingForInput(input: Token[]): Promise<number> {
if (input.length === 0)
return Promise.resolve(0);
return withLock([this as LlamaRankingContext, "evaluate"], async () => {
await this._sequence.eraseContextTokenRanges([{
start: 0,
end: this._sequence.nextTokenIndex
}]);
const iterator = this._sequence.evaluate(input, {_noSampling: true});
for await (const token of iterator) {
break;
}
const embedding = this._llamaContext._ctx.getEmbedding(input.length, 1);
if (embedding.length === 0)
return 0;
const logit = embedding[0]!;
const probability = logitToSigmoid(logit);
return probability;
});
}
public static async _create({
_model
}: {
_model: LlamaModel
}, {
contextSize,
batchSize,
threads = 6,
createSignal,
template,
ignoreMemorySafetyChecks
}: LlamaRankingContextOptions) {
const resolvedTemplate = template ?? parseRankingTemplate(_model.fileInfo.metadata?.tokenizer?.["chat_template.rerank"]);
if (_model.tokens.eos == null && _model.tokens.sep == null) {
if (!isRankingTemplateValid(resolvedTemplate)) {
if (resolvedTemplate === _model.fileInfo.metadata?.tokenizer?.["chat_template.rerank"])
throw new Error("The model's builtin template is invalid. It must contain both {query} and {document} placeholders.");
else
throw new Error("The provided template is invalid. It must contain both {{query}} and {{document}} placeholders.");
} else if (resolvedTemplate == null)
throw new Error("Computing rankings is not supported for this model.");
}
if (_model.fileInsights.hasEncoder && _model.fileInsights.hasDecoder)
throw new Error("Computing rankings is not supported for encoder-decoder models.");
if (!_model.fileInsights.supportsRanking)
throw new Error("Computing rankings is not supported for this model.");
const llamaContext = await _model.createContext({
contextSize,
batchSize,
threads,
createSignal,
ignoreMemorySafetyChecks,
_embeddings: true,
_ranking: true
});
return new LlamaRankingContext({
_llamaContext: llamaContext,
_template: resolvedTemplate
});
}
}
function logitToSigmoid(logit: number) {
return 1 / (1 + Math.exp(-logit));
}