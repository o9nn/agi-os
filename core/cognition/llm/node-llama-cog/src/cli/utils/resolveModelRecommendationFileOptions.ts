import {resolveModelDestination} from "../../utils/resolveModelDestination.js";
export type ModelURI = (
    `${
        `http://${string}/${string}` |
        `https://${string}/${string}` |
        `hf:${string}/${string}/${string}` |
        `huggingface:${string}/${string}/${string}`
    }${
        ".gguf" | `.gguf.part${number}of${number}`
    }`
) | (
    `hf:${string}/${string}:${string}` |
    `huggingface:${string}/${string}:${string}` |
    `hf.co/${string}/${string}:${string}` |
    `huggingface.co/${string}/${string}:${string}`
);
export type ModelRecommendation = {
    name: string,
    abilities: ("code" | "chat" | "complete" | "infill" | "functionCalling" | "reasoning")[],
    description?: string,
    fileOptions: ModelURI[]
};
export function resolveModelRecommendationFileOptions(modelRecommendation: ModelRecommendation) {
    return modelRecommendation.fileOptions.map((fileOption) => {
        const resolvedModelDestination = resolveModelDestination(fileOption, true);
        if (resolvedModelDestination.type === "file")
            throw new Error(`File option "${fileOption}" is not a valid model URI`);
        if (resolvedModelDestination.type === "uri")
            return resolvedModelDestination.uri;
        return resolvedModelDestination.url;
    });
}