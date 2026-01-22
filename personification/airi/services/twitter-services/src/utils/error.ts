export function errorToMessage(error: unknown, fallbackMessage = 'Unknown error'): string {
  if (error === null || error === undefined) {
    return fallbackMessage
  }
  if (error instanceof Error) {
    return error.message
  }
  if (typeof error === 'string') {
    return error
  }
  if (typeof error === 'object') {
    if ('message' in error && typeof (error as any).message === 'string') {
      return (error as any).message
    }
    try {
      return JSON.stringify(error)
    }
    catch {
      return String(error)
    }
  }
  return String(error)
}
export function createError(
  message: string,
  originalError?: unknown,
  context?: Record<string, unknown>,
): Error {
  let errorMessage = message
  if (originalError) {
    errorMessage += `: ${errorToMessage(originalError)}`
  }
  const error = new Error(errorMessage)
  if (context) {
    Object.assign(error, { context })
  }
  return error
}