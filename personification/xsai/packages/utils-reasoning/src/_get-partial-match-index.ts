export const getPartialMatchIndex = (text: string, matchText: string): number => {
if (text.length === 0 || matchText.length === 0) {
return -1
}
const matchIndex = text.indexOf(matchText)
if (matchIndex !== -1) {
return matchIndex
}
for (let i = Math.max(text.length - matchText.length + 1, 0); i < text.length; i++) {
if (matchText.startsWith(text.slice(i))) {
return i
}
}
return -1
}