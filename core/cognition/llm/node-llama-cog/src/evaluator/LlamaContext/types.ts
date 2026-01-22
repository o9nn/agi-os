import {PickOptions} from "../../utils/utilTypes.js";
import type {LlamaGrammarEvaluationState} from "../LlamaGrammarEvaluationState.js";
import type {TokenBias} from "../TokenBias.js";
import type {Token} from "../../types.js";
import type {LlamaContextSequence} from "./LlamaContext.js";
export type LlamaContextOptions = {
    sequences?: number,
    contextSize?: "auto" | number | {
        min?: number,
        max?: number
    },
    batchSize?: number,
    flashAttention?: boolean,
    threads?: number | {
        ideal?: number,
        min?: number
    },
    batching?: BatchingOptions,
    swaFullCache?: boolean,
    lora?: string | {
        adapters: Array<{
            filePath: string,
            scale?: number
        }>,
        onLoadProgress?(loadProgress: number): void
    },
    createSignal?: AbortSignal,
    ignoreMemorySafetyChecks?: boolean,
    failedCreationRemedy?: false | {
        retries?: number,
        autoContextSizeShrink?: number | ((contextSize: number) => number)
    },
    performanceTracking?: boolean,
    _embeddings?: boolean,
    _ranking?: boolean
};
export type LlamaContextSequenceRepeatPenalty = {
    punishTokens: Token[] | (() => Token[]),
    maxPunishTokens?: number,
    penalty?: number,
    frequencyPenalty?: number,
    presencePenalty?: number
};
export type BatchingOptions = {
    dispatchSchedule?: "nextCycle" | CustomBatchingDispatchSchedule,
    itemPrioritizationStrategy?: "maximumParallelism" | "firstInFirstOut" | CustomBatchingPrioritizationStrategy
};
export type CustomBatchingDispatchSchedule = (dispatch: () => void) => void;
export type CustomBatchingPrioritizationStrategy = (options: {
    items: readonly BatchItem[],
    size: number
}) => PrioritizedBatchItem[];
export type ContextShiftOptions = {
    size?: number | ((sequence: LlamaContextSequence) => number | Promise<number>),
    strategy?: "eraseBeginning" | ((options: {
        sequence: LlamaContextSequence,
        size: number
    }) => ContextTokensDeleteRange[] | Promise<ContextTokensDeleteRange[]>)
};
export type ContextTokensDeleteRange = {
    start: number,
    end: number
};
export type SequenceEvaluateOptions = {
    temperature?: number, minP?: number, topK?: number, topP?: number,
    seed?: number,
    grammarEvaluationState?: LlamaGrammarEvaluationState | (() => LlamaGrammarEvaluationState | undefined),
    repeatPenalty?: LlamaContextSequenceRepeatPenalty,
    tokenBias?: TokenBias | (() => TokenBias),
    evaluationPriority?: EvaluationPriority,
    contextShift?: ContextShiftOptions,
    yieldEogToken?: boolean,
    _noSampling?: boolean
};
export type SequenceEvaluateMetadataOptions = {
    readonly confidence?: boolean,
    readonly probabilities?: boolean
};
export type SequenceEvaluateOutput<
    Options extends {
        readonly confidence?: boolean,
        readonly probabilities?: boolean
    } = {
        readonly confidence: true,
        readonly probabilities: true
    }
> = PickOptions<{
    token: Token,
    confidence: number,
    probabilities: Map<Token, number>
}, Options & {token: true}>;
export type ControlledEvaluateInputItem = Token | [token: Token, options: {
    generateNext?: {
        probabilities?: boolean,
        confidence?: boolean,
        token?: boolean,
        options?: {
            temperature?: number, minP?: number, topK?: number, topP?: number,
            seed?: number,
            repeatPenalty?: LlamaContextSequenceRepeatPenalty,
            tokenBias?: TokenBias | (() => TokenBias)
        }
    }
}];
export type ControlledEvaluateIndexOutput = {
    next: {
        token?: Token | null,
        confidence?: number,
        probabilities?: Map<Token, number>
    }
};
export type EvaluationPriority = 1 | 2 | 3 | 4 | 5;
export type BatchItem = {
    readonly tokens: readonly Token[],
    readonly logits: readonly (true | undefined)[],
    readonly evaluationPriority: EvaluationPriority
};
export type PrioritizedBatchItem = {
    item: BatchItem,
    processAmount: number
};