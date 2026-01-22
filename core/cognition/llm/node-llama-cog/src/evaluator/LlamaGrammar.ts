import path from "path";
import fs from "fs-extra";
import {getGrammarsFolder} from "../utils/getGrammarsFolder.js";
import {LlamaText} from "../utils/LlamaText.js";
import {AddonGrammar} from "../bindings/AddonTypes.js";
import {Llama} from "../bindings/Llama.js";
import {Token} from "../types.js";
export type LlamaGrammarOptions = {
    grammar: string,
    stopGenerationTriggers?: readonly (LlamaText | string | readonly (string | Token)[])[],
    trimWhitespaceSuffix?: boolean,
    rootRuleName?: string
};
export class LlamaGrammar {
     public readonly _llama: Llama;
     public readonly _grammar: AddonGrammar;
     private readonly _stopGenerationTriggers: readonly (LlamaText | string | readonly (string | Token)[])[];
     private readonly _trimWhitespaceSuffix: boolean;
     private readonly _grammarText: string;
     private readonly _rootRuleName: string;
    public constructor(llama: Llama, {
        grammar, stopGenerationTriggers = [], trimWhitespaceSuffix = false, rootRuleName = "root"
    }: LlamaGrammarOptions) {
        this._llama = llama;
        this._grammar = new this._llama._bindings.AddonGrammar(grammar, {
            addonExports: this._llama._bindings,
            rootRuleName
        });
        this._stopGenerationTriggers = stopGenerationTriggers ?? [];
        this._trimWhitespaceSuffix = trimWhitespaceSuffix;
        this._grammarText = grammar;
        this._rootRuleName = rootRuleName;
    }
    public get grammar(): string {
        return this._grammarText;
    }
    public get rootRuleName(): string {
        return this._rootRuleName;
    }
    public get stopGenerationTriggers() {
        return this._stopGenerationTriggers;
    }
    public get trimWhitespaceSuffix() {
        return this._trimWhitespaceSuffix;
    }
    public _testText(text: string): boolean {
        return this._grammar.isTextCompatible(String(text));
    }
    public static async getFor(llama: Llama, type: "json" | "json_arr" | "english" | "list" | "c" | "arithmetic" | "japanese" | "chess") {
        const grammarsFolder = await getGrammarsFolder(llama.buildType);
        const grammarFile = path.join(grammarsFolder, type + ".gbnf");
        if (await fs.pathExists(grammarFile)) {
            const grammar = await fs.readFile(grammarFile, "utf8");
            return new LlamaGrammar(llama, {
                grammar,
                stopGenerationTriggers: [LlamaText(["\n".repeat(
                    (type === "json" || type === "json_arr")
                        ? 4
                        : 10
                )])], 
                trimWhitespaceSuffix: true
            });
        }
        throw new Error(`Grammar file for type "${type}" was not found in "${grammarsFolder}"`);
    }
}