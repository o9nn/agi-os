import {ChatWrapperSettings} from "../../../types.js";
import {parseTextTemplate} from "../../../utils/parseTextTemplate.js";
import {removeUndefinedFields} from "../../../utils/removeNullFields.js";
export function templateSegmentOptionsToChatWrapperSettings(
templateOptions?: TemplateChatWrapperSegmentsOptions
): ChatWrapperSettings["segments"] {
if (templateOptions == null)
return {};
function getThoughtSegmentOptions(): Exclude<ChatWrapperSettings["segments"], undefined>["thought"] {
if (templateOptions?.thoughtTemplate == null)
return undefined;
const parsedThoughtTemplate = parseTextTemplate(templateOptions.thoughtTemplate, [{
text: "{{content}}",
key: "content"
}]);
const prefix = parsedThoughtTemplate.content.prefix;
if (prefix.length === 0)
throw new Error("Thought template must have text before \"{{content}}\"");
return removeUndefinedFields({
prefix,
suffix: parsedThoughtTemplate.content.suffix || undefined,
reopenAfterFunctionCalls: templateOptions.reopenThoughtAfterFunctionCalls
});
}
return removeUndefinedFields({
closeAllSegments: templateOptions.closeAllSegmentsTemplate || undefined,
reiterateStackAfterFunctionCalls: templateOptions.reiterateStackAfterFunctionCalls,
thought: getThoughtSegmentOptions()
});
}
export type TemplateChatWrapperSegmentsOptions = {
thoughtTemplate?: `${string}{{content}}${string}`,
reopenThoughtAfterFunctionCalls?: boolean,
closeAllSegmentsTemplate?: string,
reiterateStackAfterFunctionCalls?: boolean
};