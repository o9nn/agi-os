import {LlamaVocabularyType} from "../bindings/types.js";
import type {LlamaModelTokens} from "../evaluator/LlamaModel/LlamaModel.js";
export function resolveBeginningTokenToPrepend(vocabularyType: LlamaVocabularyType, tokens: LlamaModelTokens) {
if (vocabularyType === LlamaVocabularyType.rwkv)
return null;
if (vocabularyType === LlamaVocabularyType.wpm)
return tokens.bos;
if (vocabularyType === LlamaVocabularyType.ugm)
return null;
if (tokens.shouldPrependBosToken)
return tokens.bos;
return null;
}
export function resolveEndTokenToAppend(vocabularyType: LlamaVocabularyType, tokens: LlamaModelTokens) {
if (vocabularyType === LlamaVocabularyType.rwkv)
return null;
if (vocabularyType === LlamaVocabularyType.wpm)
return tokens.sep;
if (vocabularyType === LlamaVocabularyType.ugm)
return tokens.eos;
if (tokens.shouldAppendEosToken)
return tokens.eos;
return null;
}