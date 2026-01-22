import type { PaginatedRequest, PaginatedResult } from '@xsmcp/shared'
import { METHOD_NOT_FOUND } from '@xsmcp/shared'
import { XSMCPError } from './error'
export const withPagination = <T>(items: T[], params: PaginatedRequest['params']): PaginatedResult & { result: T[] } => {
let currentPage: number = 0
if (params?.cursor != null) {
try {
currentPage = Number.parseInt(atob(params.cursor))
}
catch {
throw new XSMCPError(`Cursor not found: ${params.cursor}`, METHOD_NOT_FOUND, 404)
}
}
const nextPage = currentPage + 20
const result = items.slice(currentPage, nextPage)
return {
nextCursor: nextPage < items.length ? btoa(nextPage.toString()) : undefined,
result,
}
}