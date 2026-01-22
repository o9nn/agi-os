import {GgufFileInfo} from "../../../src/gguf/types/GgufFileInfoTypes.js";
export function simplifyGgufInfoForTestSnapshot(ggufFileInfo: GgufFileInfo) {
const ggufFileInfoCopy = structuredClone(ggufFileInfo);
shortenArray(ggufFileInfoCopy.metadata.tokenizer.ggml.tokens, 10);
shortenArray(ggufFileInfoCopy.metadata.tokenizer.ggml.scores, 10);
shortenArray(ggufFileInfoCopy.metadata.tokenizer.ggml.token_type, 10);
shortenArray(ggufFileInfoCopy.metadata.tokenizer.ggml.merges, 10);
shortenArray(ggufFileInfoCopy.tensorInfo, 4);
shortenArray(ggufFileInfoCopy.fullTensorInfo, 4);
return ggufFileInfoCopy;
}
function shortenArray(array?: readonly any[], maxSize: number = 10) {
if (array == null)
return;
(array as any[]).splice(maxSize);
}