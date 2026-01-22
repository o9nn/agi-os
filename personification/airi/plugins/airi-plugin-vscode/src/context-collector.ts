import type { CodingContext } from './types'
import { useLogger } from '@guiiai/logg'
import * as vscode from 'vscode'
export class ContextCollector {
constructor(
private readonly contextLines: number = 5,
) {}
async collect(editor: vscode.TextEditor): Promise<CodingContext | null> {
try {
const document = editor.document
const position = editor.selection.active
const file = {
path: document.uri.fsPath,
languageId: document.languageId,
fileName: document.fileName,
workspaceFolder: this.getWorkspaceFolder(document.uri),
}
const cursor = {
line: position.line,
character: position.character,
}
const selection = editor.selection.isEmpty
? undefined
: {
text: document.getText(editor.selection),
start: {
line: editor.selection.start.line,
character: editor.selection.start.character,
},
end: {
line: editor.selection.end.line,
character: editor.selection.end.character,
},
}
const currentLine = {
lineNumber: position.line,
text: document.lineAt(position.line).text,
}
const context = this.getContext(document, position.line)
const git = await this.getGitInfo(document.uri)
return {
file,
cursor,
selection,
currentLine,
context,
git,
timestamp: Date.now(),
}
}
catch (error) {
useLogger().errorWithError('Failed to collect context:', error)
return null
}
}
private getContext(document: vscode.TextDocument, currentLine: number) {
const before: string[] = []
const after: string[] = []
const startLine = Math.max(0, currentLine - this.contextLines)
for (let i = startLine; i < currentLine; i++) {
before.push(document.lineAt(i).text)
}
const endLine = Math.min(document.lineCount - 1, currentLine + this.contextLines)
for (let i = currentLine + 1; i <= endLine; i++) {
after.push(document.lineAt(i).text)
}
return { before, after }
}
private getWorkspaceFolder(uri: vscode.Uri): string | undefined {
const folder = vscode.workspace.getWorkspaceFolder(uri)
return folder?.uri.fsPath
}
private async getGitInfo(uri: vscode.Uri): Promise<{ branch: string, isDirty: boolean } | undefined> {
try {
const gitExtension = vscode.extensions.getExtension('vscode.git')?.exports
if (!gitExtension)
return undefined
const git = gitExtension.getAPI(1)
const repo = git.getRepository(uri)
if (!repo)
return undefined
return {
branch: repo.state.HEAD?.name ?? 'unknown',
isDirty: repo.state.workingTreeChanges.length > 0,
}
}
catch {
return undefined
}
}
}