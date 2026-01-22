import {Llama} from "../bindings/Llama.js";
import {AddonGrammarEvaluationState} from "../bindings/AddonTypes.js";
import type {LlamaGrammar} from "./LlamaGrammar.js";
import type {LlamaModel} from "./LlamaModel/LlamaModel.js";
export type LlamaGrammarEvaluationStateOptions = {
    model: LlamaModel,
    grammar: LlamaGrammar
};
export class LlamaGrammarEvaluationState {
     public readonly _llama: Llama;
     public readonly _state: AddonGrammarEvaluationState;
    public constructor(options: LlamaGrammarEvaluationStateOptions);
    public constructor(existingState: LlamaGrammarEvaluationState);
    public constructor(existingStateOrOptions: LlamaGrammarEvaluationStateOptions | LlamaGrammarEvaluationState) {
        if (existingStateOrOptions instanceof LlamaGrammarEvaluationState) {
            this._llama = existingStateOrOptions._llama;
            this._state = new this._llama._bindings.AddonGrammarEvaluationState(existingStateOrOptions._state);
        } else {
            const {model, grammar} = existingStateOrOptions;
            this._llama = model._llama;
            if (model._llama !== grammar._llama)
                throw new Error("The given LlamaModel and LlamaGrammar must be from the same Llama instance");
            this._state = new model._llama._bindings.AddonGrammarEvaluationState(model._model, grammar._grammar);
        }
    }
    public clone(): LlamaGrammarEvaluationState {
        return new LlamaGrammarEvaluationState(this);
    }
}