import {Token} from "../../types.js";
import {SequenceEvaluateOptions} from "./types.js";
import {LlamaContextSequence} from "./LlamaContext.js";
export abstract class TokenPredictor {
    public abstract reset(params: {
        targetSequence: LlamaContextSequence,
        stateTokens: Token[],
        evaluateOptions: Readonly<SequenceEvaluateOptions>
    }): Promise<void> | void;
    public abstract pushTokens(tokens: Token[]): void;
    public abstract predictTokens(): Promise<Token[]> | Token[];
    public stop(untilPredictionsExhausted?: boolean): Promise<void> | void {}
    public updateInputTokens(tokens: Token[]): void {}
    public dispose(): Promise<void> | void {}
    public [Symbol.dispose]() {
        return this.dispose();
    }
}