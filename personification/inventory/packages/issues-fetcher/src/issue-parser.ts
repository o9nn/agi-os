import type { List } from 'mdast'
import { toString } from 'mdast-util-to-string'
import remarkParse from 'remark-parse'
import { unified } from 'unified'
import { visit } from 'unist-util-visit'
export interface Model {
capabilities: string[]
endpoints: string[]
inputModalities: string[]
modelId: string
outputModalities: string[]
provider: string
}
export function extractCheckedListItems(listNode: List) {
return listNode.children
.map(item => toString(item))
.filter(text => text.startsWith('[x]'))
.map(text => text.replace(/^\[x\]\s*/i, '').trim())
}
export function parseModelIssue(markdown: string): Model {
const tree = unified().use(remarkParse).parse(markdown)
const model: Model = {
capabilities: [],
endpoints: [],
inputModalities: [],
outputModalities: [],
modelId: '',
provider: '',
}
let currentSection = ''
visit(tree, (node) => {
if (node.type === 'heading' && node.depth === 3) {
currentSection = toString(node).trim()
}
if (node.type === 'paragraph' && (currentSection === 'Provider' || currentSection === 'Model ID')) {
if (currentSection === 'Provider') {
model.provider = toString(node)
}
if (currentSection === 'Model ID') {
model.modelId = toString(node)
}
}
if (node.type === 'list') {
switch (currentSection) {
case 'Model Capabilities':
model.capabilities = extractCheckedListItems(node)
break
case 'Model Input Modalities':
model.inputModalities = extractCheckedListItems(node)
break
case 'Model Output Modalities':
model.outputModalities = extractCheckedListItems(node)
break
case 'Model Endpoints':
model.endpoints = extractCheckedListItems(node)
break
}
}
})
return model
}