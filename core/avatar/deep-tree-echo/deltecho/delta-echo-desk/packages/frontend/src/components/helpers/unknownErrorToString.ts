export function unknownErrorToString(error: unknown): string {
if (typeof error !== 'object' || error == null) {
return `Error: ${error}`
}
if ('message' in error) {
return `${error.message}`
}
if ('toString' in error && typeof error.toString === 'function') {
return error.toString()
}
return `Error: ${error}`
}