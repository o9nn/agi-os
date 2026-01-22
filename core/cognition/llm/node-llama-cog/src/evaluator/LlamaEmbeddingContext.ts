import {AsyncDisposeAggregator, EventRelay, withLock} from "lifecycle-utils";
import {Token} from "../types.js";
import {LlamaText} from "../utils/LlamaText.js";
import {tokenizeInput} from "../utils/tokenizeInput.js";
import {resolveBeginningTokenToPrepend, resolveEndTokenToAppend} from "../utils/tokenizerUtils.js";
import {LlamaEmbedding} from "./LlamaEmbedding.js";
import type {LlamaModel} from "./LlamaModel/LlamaModel.js";
import type {LlamaContext, LlamaContextSequence} from "./LlamaContext/LlamaContext.js";
export type LlamaEmbeddingContextOptions = {
    contextSize?: "auto" | number | {
        min?: number,
        max?: number
    },
    batchSize?: number,
    threads?: number,
    createSignal?: AbortSignal,
    ignoreMemorySafetyChecks?: boolean
};
export class LlamaEmbeddingContext {
     private readonly _llamaContext: LlamaContext;
     private readonly _sequence: LlamaContextSequence;
     private readonly _disposeAggregator = new AsyncDisposeAggregator();
    public readonly onDispose = new EventRelay<void>();
    private constructor({
        _llamaContext
    }: {
        _llamaContext: LlamaContext
    }) {
        this._llamaContext = _llamaContext;
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
    public async getEmbeddingFor(input: Token[] | string | LlamaText) {
        const resolvedInput = tokenizeInput(input, this._llamaContext.model.tokenizer, undefined, true);
        if (resolvedInput.length > this._llamaContext.contextSize)
            throw new Error(
                "Input is longer than the context size. " +
                "Try to increase the context size or use another model that supports longer contexts."
            );
        else if (resolvedInput.length === 0)
            return new LlamaEmbedding({
                vector: []
            });
        const beginningToken = resolveBeginningTokenToPrepend(this.model.vocabularyType, this.model.tokens);
        if (beginningToken != null && resolvedInput[0] !== beginningToken)
            resolvedInput.unshift(beginningToken);
        const endToken = resolveEndTokenToAppend(this.model.vocabularyType, this.model.tokens);
        if (endToken != null && resolvedInput.at(-1) !== endToken)
            resolvedInput.push(endToken);
        return await withLock([this as LlamaEmbeddingContext, "evaluate"], async () => {
            await this._sequence.eraseContextTokenRanges([{
                start: 0,
                end: this._sequence.nextTokenIndex
            }]);
            const iterator = this._sequence.evaluate(resolvedInput, {_noSampling: true});
            for await (const token of iterator) {
                break; 
            }
            const embedding = this._llamaContext._ctx.getEmbedding(resolvedInput.length);
            const embeddingVector = Array.from(embedding);
            return new LlamaEmbedding({
                vector: embeddingVector
            });
        });
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
    public static async _create({
        _model
    }: {
        _model: LlamaModel
    }, {
        contextSize,
        batchSize,
        threads = 6,
        createSignal,
        ignoreMemorySafetyChecks
    }: LlamaEmbeddingContextOptions) {
        if (_model.fileInsights.hasEncoder && _model.fileInsights.hasDecoder)
            throw new Error("Computing embeddings is not supported for encoder-decoder models.");
        const llamaContext = await _model.createContext({
            contextSize,
            batchSize,
            threads,
            createSignal,
            ignoreMemorySafetyChecks,
            _embeddings: true
        });
        return new LlamaEmbeddingContext({
            _llamaContext: llamaContext
        });
    }
}