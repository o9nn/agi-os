export function jsonDumps(value: any) {
return JSON.stringify(value, null, 1)
.split("\n")
.map((line) => {
line = line.trim();
if (line.endsWith(","))
line += " ";
return line;
})
.join("");
}