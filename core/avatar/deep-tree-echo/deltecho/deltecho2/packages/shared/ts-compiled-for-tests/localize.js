"use strict";
import { getLogger } from "./logger.js";
const log = getLogger("localize");
export function translate(locale, messages) {
const localeBCP47 = locale.replace("_", "-");
let pluralRules;
try {
pluralRules = new Intl.PluralRules(localeBCP47);
} catch (err) {
log.errorWithoutStackTrace(err);
pluralRules = new Intl.PluralRules("en");
}
function getMessage(key, substitutions, raw_opts) {
const translationKey = key;
let opts = {};
if (typeof raw_opts === "string") opts = { quantity: raw_opts };
else opts = Object.assign({}, raw_opts);
const entry = messages[translationKey];
if (!entry) {
log.error(`Missing translation for key '${translationKey}'`);
return translationKey;
}
let message = entry.message;
if (typeof opts.quantity !== "undefined") {
if (typeof opts.quantity === "string") {
message = entry[opts.quantity];
} else if (typeof opts.quantity === "number") {
message = entry[opts.quantity] ||
entry[pluralRules.select(opts.quantity)] ||
entry["other"];
} else {
message = void 0;
}
if (typeof message === "undefined") {
log.error(
`Missing quantity '${opts.quantity}' for key '${translationKey}'`
);
return `${translationKey}:${opts.quantity}`;
}
}
if (typeof message === "undefined") {
log.error(
`Missing 'message' for key '${translationKey}', maybe you need to specify quantity`
);
return `${translationKey}:?`;
}
if (substitutions) {
if (!Array.isArray(substitutions)) {
substitutions = [substitutions];
}
let counter = -1;
return message.replace(/(?:%\d\$[\w\d])|(?:%[\w\d])/g, (f) => {
counter++;
if (f.length > 2) {
const index = Number.parseInt(f[1]) - 1;
if (substitutions === void 0 || typeof substitutions[index] === "undefined") {
log.error(`Missing ${index} argument for key %c'${translationKey}'`);
return "";
}
return substitutions[index].toString();
}
if (substitutions === void 0 || typeof substitutions?.[counter] === "undefined") {
log.error(`Missing ${0} argument for key %c'${translationKey}'`);
return "";
}
return substitutions[counter].toString();
});
}
return message;
}
return getMessage;
}