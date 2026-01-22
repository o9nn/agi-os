export interface CodingContext {
  file: {
    path: string
    languageId: string
    fileName: string
    workspaceFolder?: string
  }
  cursor: {
    line: number
    character: number
  }
  selection?: {
    text: string
    start: { line: number, character: number }
    end: { line: number, character: number }
  }
  currentLine: {
    lineNumber: number
    text: string
  }
  context: {
    before: string[]
    after: string[]
  }
  git?: {
    branch: string
    isDirty: boolean
  }
  timestamp: number
}
export interface AiriEvent {
  type: 'coding:context' | 'coding:save' | 'coding:switch-file'
  data: CodingContext
}