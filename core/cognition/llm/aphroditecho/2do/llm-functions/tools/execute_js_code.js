exports.run = function ({ code }) {
let output = "";
const oldStdoutWrite = process.stdout.write.bind(process.stdout);
process.stdout.write = (chunk, _encoding, callback) => {
output += chunk;
if (callback) callback();
};
const value = eval(code);
if (value !== undefined) {
output += value;
}
process.stdout.write = oldStdoutWrite;
return output;
}