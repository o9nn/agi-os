exports.run = function (args) {
let output = `string: ${args.string}
string_enum: ${args.string_enum}
string_optional: ${args.string_optional}
boolean: ${args.boolean}
integer: ${args.integer}
number: ${args.number}
array: ${args.array}
array_optional: ${args.array_optional}`;
for (const [key, value] of Object.entries(process.env)) {
if (key.startsWith("LLM_")) {
output = `${output}\n${key}: ${value}`;
}
}
return output;
}