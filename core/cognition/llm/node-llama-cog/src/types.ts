import {GbnfJsonSchema, GbnfJsonSchemaToType} from "./utils/gbnfJson/types.js";
import {LlamaText, BuiltinSpecialTokenValue, LlamaTextJSON} from "./utils/LlamaText.js";
import type {GgufFileInfo} from "./gguf/types/GgufFileInfoTypes.js";
export type Token = number & {
    __token: never
};
export type Detokenizer = {
    detokenize(tokens: readonly Token[], specialTokens?: boolean, lastTokens?: readonly Token[]): string
}["detokenize"];
export type Tokenizer = {
    tokenize(text: string, specialTokens?: boolean, options?: "trimLeadingSpace"): Token[],
    tokenize(text: BuiltinSpecialTokenValue, specialTokens: "builtin"): Token[]
}["tokenize"] & {
    readonly detokenize: Detokenizer,
    isSpecialToken(token: Token): boolean,
    isEogToken(token: Token): boolean
};
export type ChatWrapperSettings = {
    readonly supportsSystemMessages: boolean,
    readonly functions: {
        readonly call: {
            readonly optionalPrefixSpace: boolean,
            readonly prefix: string | LlamaText,
            readonly paramsPrefix: string | LlamaText,
            readonly suffix: string | LlamaText,
            readonly emptyCallParamsPlaceholder?: object | string | number | boolean | null
        },
        readonly result: {
            readonly prefix: string | LlamaText,
            readonly suffix: string | LlamaText
        },
        readonly parallelism?: {
            readonly call: {
                readonly sectionPrefix: string | LlamaText,
                readonly betweenCalls?: string | LlamaText,
                readonly sectionSuffix?: string | LlamaText
            },
            readonly result?: {
                readonly sectionPrefix?: string | LlamaText,
                readonly betweenResults?: string | LlamaText,
                readonly sectionSuffix?: string | LlamaText
            }
        }
    },
    readonly segments?: {
        readonly closeAllSegments?: string | LlamaText,
        readonly reiterateStackAfterFunctionCalls?: boolean,
        readonly thought?: ChatWrapperSettingsSegment & {
            reopenAfterFunctionCalls?: boolean
        },
        readonly comment?: ChatWrapperSettingsSegment
    }
};
export type ChatWrapperSettingsSegment = {
    readonly prefix: string | LlamaText,
    readonly suffix?: string | LlamaText
};
export type ChatWrapperGenerateContextStateOptions = {
    chatHistory: readonly ChatHistoryItem[],
    availableFunctions?: ChatModelFunctions,
    documentFunctionParams?: boolean
};
export type ChatWrapperCheckModelCompatibilityParams = {
    tokenizer?: Tokenizer,
    fileInfo?: GgufFileInfo
};
export type ChatWrapperGeneratedContextState =
    ChatWrapperGeneratedPrefixTriggersContextState | ChatWrapperGeneratedInitiallyEngagedFunctionsContextState;
export type ChatWrapperGeneratedPrefixTriggersContextState = {
    contextText: LlamaText,
    stopGenerationTriggers: LlamaText[],
    prefixTriggers?: Array<{
        triggers: LlamaText[],
        type: "functionCall",
        replaceTrigger?: boolean,
        inject?: LlamaText
    } | {
        triggers: LlamaText[],
        type: "segment",
        segmentType: ChatModelSegmentType,
        inject?: LlamaText
    } | {
        triggers: LlamaText[],
        type: "response",
        inject?: LlamaText
    }>,
    noPrefixTrigger?: {
        type: "functionCall",
        inject: LlamaText
    } | {
        type: "segment",
        segmentType: ChatModelSegmentType,
        inject: LlamaText
    } | {
        type: "response",
        inject: LlamaText
    },
    rerender?: {
        triggers: LlamaText[],
        action?: "closeResponseItem"
    },
    detectFunctionCalls?: boolean,
    ignoreStartText?: never,
    functionCall?: never
};
export type ChatWrapperGeneratedInitiallyEngagedFunctionsContextState = {
    contextText: LlamaText,
    stopGenerationTriggers: LlamaText[],
    ignoreStartText?: LlamaText[],
    functionCall?: {
        initiallyEngaged: boolean,
        disengageInitiallyEngaged: LlamaText[]
    },
    detectFunctionCalls?: never,
    prefixTriggers?: never,
    noPrefixTrigger?: never,
    rerender?: never
};
export type ChatWrapperGenerateInitialHistoryOptions = {
    systemPrompt?: string
};
export type ChatHistoryItem = ChatSystemMessage | ChatUserMessage | ChatModelResponse;
export type ChatSystemMessage = {
    type: "system",
    text: string | LlamaTextJSON
};
export type ChatUserMessage = {
    type: "user",
    text: string
};
export type ChatModelResponse = {
    type: "model",
    response: Array<string | ChatModelFunctionCall | ChatModelSegment>
};
export type ChatModelFunctionCall = {
    type: "functionCall",
    name: string,
    description?: string,
    params: any,
    result: any,
    rawCall?: LlamaTextJSON,
    startsNewChunk?: boolean
};
export const allSegmentTypes = ["thought", "comment"] as const satisfies readonly ChatModelSegmentType[];
void (null as Exclude<ChatModelSegmentType, typeof allSegmentTypes[number]> satisfies never);
export type ChatModelSegmentType = "thought" | "comment";
export type ChatModelSegment = {
    type: "segment",
    segmentType: ChatModelSegmentType,
    text: string,
    ended: boolean,
    raw?: LlamaTextJSON,
    startTime?: string,
    endTime?: string
};
export type ChatModelFunctions = {
    readonly [name: string]: {
        readonly description?: string,
        readonly params?: Readonly<GbnfJsonSchema> | undefined | null
    }
};
export type ChatSessionModelFunctions = {
    readonly [name: string]: ChatSessionModelFunction<any>
};
export type ChatSessionModelFunction<Params extends GbnfJsonSchema | undefined = GbnfJsonSchema | undefined> = {
    readonly description?: string,
    readonly params?: Params,
    readonly handler: (params: GbnfJsonSchemaToType<NoInfer<Params>>) => any
};
export function isChatModelResponseFunctionCall(item: ChatModelResponse["response"][number] | undefined): item is ChatModelFunctionCall {
    if (item == null || typeof item === "string")
        return false;
    return item.type === "functionCall";
}
export function isChatModelResponseSegment(item: ChatModelResponse["response"][number] | undefined): item is ChatModelSegment {
    if (item == null || typeof item === "string")
        return false;
    return item.type === "segment";
}
export type LLamaContextualRepeatPenalty = {
    lastTokens?: number,
    punishTokensFilter?: (tokens: Token[]) => Token[],
    penalizeNewLine?: boolean,
    penalty?: number,
    frequencyPenalty?: number,
    presencePenalty?: number
};