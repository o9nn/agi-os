import {Token, Tokenizer} from "../types.js";
import {LlamaText} from "../utils/LlamaText.js";
import {tokenizeInput} from "../utils/tokenizeInput.js";
import type {LlamaModel} from "./LlamaModel/LlamaModel.js";
export class TokenBias {
     public readonly _tokenizer: Tokenizer;
     public readonly _biases = new Map<Token, number>();
    public constructor(tokenizer: Tokenizer) {
        this._tokenizer = tokenizer;
    }
    public set(input: Token | Token[] | string | LlamaText, bias: "never" | number | {logit: number}) {
        const resolvedLogit = bias === "never"
            ? -Infinity
            : typeof bias === "number"
                ? probabilityToLogit(bias)
                : bias.logit;
        for (const token of tokenizeInput(input, this._tokenizer)) {
            if (this._tokenizer.isEogToken(token))
                continue;
            this._biases.set(token, resolvedLogit);
        }
        for (const token of tokenizeInput(input, this._tokenizer, "trimLeadingSpace")) {
            if (this._tokenizer.isEogToken(token))
                continue;
            this._biases.set(token, resolvedLogit);
        }
        return this;
    }
    public static for(modelOrTokenizer: LlamaModel | Tokenizer) {
        if ((modelOrTokenizer as LlamaModel).tokenizer != null)
            return new TokenBias((modelOrTokenizer as LlamaModel).tokenizer);
        return new TokenBias(modelOrTokenizer as Tokenizer);
    }
}
function probabilityToLogit(probability: number) {
    if (probability <= -1)
        return -Infinity;
    else if (probability >= 1)
        return Infinity;
    else if (probability === 0)
        return 0;
    return Math.log(probability / (1 - probability));
}