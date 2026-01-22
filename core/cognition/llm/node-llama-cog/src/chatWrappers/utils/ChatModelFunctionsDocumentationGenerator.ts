import {ChatModelFunctions} from "../../types.js";
import {getTypeScriptTypeStringForGbnfJsonSchema} from "../../utils/getTypeScriptTypeStringForGbnfJsonSchema.js";
import {jsonDumps} from "./jsonDumps.js";
export class ChatModelFunctionsDocumentationGenerator {
    public readonly chatModelFunctions?: ChatModelFunctions;
    public readonly hasAnyFunctions: boolean;
    public constructor(chatModelFunctions: ChatModelFunctions | undefined) {
        this.chatModelFunctions = chatModelFunctions;
        this.hasAnyFunctions = Object.keys(this.chatModelFunctions ?? {}).length > 0;
    }
    public getTypeScriptFunctionSignatures({documentParams = true}: {documentParams?: boolean} = {}) {
        const chatModelFunctions = this.chatModelFunctions;
        if (!this.hasAnyFunctions || chatModelFunctions == null)
            return "";
        const functionNames = Object.keys(chatModelFunctions);
        return functionNames
            .map((functionName) => {
                const functionDefinition = chatModelFunctions[functionName];
                let res = "";
                if (functionDefinition?.description != null && functionDefinition.description.trim() !== "")
                    res += "
                res += "function " + functionName + "(";
                if (documentParams && functionDefinition?.params != null)
                    res += "params: " + getTypeScriptTypeStringForGbnfJsonSchema(functionDefinition.params);
                else if (!documentParams && functionDefinition?.params != null)
                    res += "params";
                res += ");";
                return res;
            })
            .join("\n\n");
    }
    public getTypeScriptFunctionTypes({documentParams = true, reservedFunctionNames = []}: {
        documentParams?: boolean, reservedFunctionNames?: string[]
    } = {}) {
        const chatModelFunctions = this.chatModelFunctions;
        if (!this.hasAnyFunctions || chatModelFunctions == null)
            return "";
        const functionNames = Object.keys(chatModelFunctions);
        const reservedFunctionNamesSet = new Set(reservedFunctionNames);
        return functionNames
            .map((functionName) => {
                if (reservedFunctionNamesSet.has(functionName))
                    throw new Error(`Function name "${functionName}" is reserved and cannot be used`);
                const functionDefinition = chatModelFunctions[functionName];
                let res = "";
                if (functionDefinition?.description != null && functionDefinition.description.trim() !== "")
                    res += "
                res += "type " + functionName + " = (";
                if (documentParams && functionDefinition?.params != null)
                    res += "_: " + getTypeScriptTypeStringForGbnfJsonSchema(functionDefinition.params);
                res += ") => any;";
                return res;
            })
            .join("\n\n");
    }
    public getLlama3_1FunctionSignatures({documentParams = true}: {documentParams?: boolean} = {}) {
        const chatModelFunctions = this.chatModelFunctions;
        if (!this.hasAnyFunctions || chatModelFunctions == null)
            return "";
        const functionNames = Object.keys(chatModelFunctions);
        return functionNames
            .map((functionName) => {
                const functionDefinition = chatModelFunctions[functionName];
                let res = `Use the function '${functionName}'`;
                const addDescription = functionDefinition?.description != null && functionDefinition.description.trim() !== "";
                if (addDescription)
                    res += " to: " + functionDefinition.description.split("\n").join("\n
                else
                    res += ".\n";
                res += jsonDumps({
                    name: functionName,
                    ...(addDescription ? {description: functionDefinition.description} : {}),
                    ...(documentParams && functionDefinition?.params != null ? {parameters: functionDefinition.params} : {})
                });
                return res;
            })
            .join("\n\n");
    }
    public getLlama3_2LightweightFunctionSignatures({documentParams = true}: {documentParams?: boolean} = {}) {
        const chatModelFunctions = this.chatModelFunctions;
        if (!this.hasAnyFunctions || chatModelFunctions == null)
            return "";
        const functionNames = Object.keys(chatModelFunctions);
        const functionsLines = functionNames
            .map((functionName) => {
                const functionDefinition = chatModelFunctions[functionName];
                const addDescription = functionDefinition?.description != null && functionDefinition.description.trim() !== "";
                return jsonDumps({
                    name: functionName,
                    ...(addDescription ? {description: functionDefinition.description} : {}),
                    ...(documentParams && functionDefinition?.params != null ? {parameters: functionDefinition.params} : {})
                });
            })
            .join("\n\n");
        return functionsLines;
    }
    public getQwenFunctionSignatures({documentParams = true}: {documentParams?: boolean} = {}) {
        return this._convertToJinjaTools({documentParams})
            .map((tool) => jsonDumps(tool))
            .join("\n");
    }
    public getSeedFunctionSignatures({documentParams = true}: {documentParams?: boolean} = {}) {
        return jsonDumps(this._convertToJinjaTools({documentParams}));
    }
    private _convertToJinjaTools({documentParams = true}: {documentParams?: boolean} = {}) {
        const chatModelFunctions = this.chatModelFunctions;
        if (!this.hasAnyFunctions || chatModelFunctions == null)
            return [];
        return [...Object.entries(chatModelFunctions)]
            .map(([functionName, functionDefinition]) => {
                return {
                    type: "function",
                    function: {
                        name: functionName,
                        description: functionDefinition.description,
                        parameters: documentParams
                            ? functionDefinition.params
                            : undefined
                    }
                };
            });
    }
}