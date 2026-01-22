import type { DefaultTheme } from 'vitepress/theme'
import type { ComputedRef, Ref } from 'vue'
import { useEventListener } from '@vueuse/core'
import { useData, withBase } from 'vitepress'
import {
computed,
onMounted,
ref,
watch,
watchEffect,
watchPostEffect,
} from 'vue'
export interface SidebarControl {
collapsed: Ref<boolean>
collapsible: ComputedRef<boolean>
isLink: ComputedRef<boolean>
isActiveLink: Ref<boolean>
hasActiveLink: ComputedRef<boolean>
hasChildren: ComputedRef<boolean>
toggle: () => void
}
export interface SidebarItem extends DefaultTheme.SidebarItem {}
export function useCloseSidebarOnEscape(
isOpen: Ref<boolean>,
close: () => void,
) {
let triggerElement: HTMLButtonElement | undefined
watchEffect(() => {
triggerElement = isOpen.value
? (document.activeElement as HTMLButtonElement)
: undefined
})
useEventListener('keyup', onEscape)
function onEscape(e: KeyboardEvent) {
if (e.key === 'Escape' && isOpen.value) {
close()
triggerElement?.focus()
}
}
}
export function useSidebarControl(
item: ComputedRef<DefaultTheme.SidebarItem>,
): SidebarControl {
const { page, hash } = useData()
const collapsed = ref(false)
const collapsible = computed(() => {
return item.value.collapsed != null
})
const isLink = computed(() => {
return !!item.value.link
})
const isActiveLink = ref(false)
const updateIsActiveLink = () => {
isActiveLink.value = isActive(withBase(`/${page.value.relativePath}`), item.value.link)
}
watch([page, item, hash], updateIsActiveLink)
onMounted(updateIsActiveLink)
const hasActiveLink = computed(() => {
if (isActiveLink.value)
return true
return item.value.items
? containsActiveLink(withBase(`/${page.value.relativePath}`), item.value.items)
: false
})
const hasChildren = computed(() => {
return !!(item.value.items && item.value.items.length)
})
watchEffect(() => {
collapsed.value = !!(collapsible.value && item.value.collapsed)
})
watchPostEffect(() => {
if (isActiveLink.value || hasActiveLink.value)
collapsed.value = false
})
function toggle() {
if (collapsible.value) {
collapsed.value = !collapsed.value
}
}
return {
collapsed,
collapsible,
isLink,
isActiveLink,
hasActiveLink,
hasChildren,
toggle,
}
}
const inBrowser = typeof document !== 'undefined'
const HASH_RE = /#.*$/
const HASH_OR_QUERY_RE = /[?#].*$/
const INDEX_OR_EXT_RE = /(?:(^|\/)index)?\.(?:md|html)$/
export function isActive(
currentPath: string,
matchPath?: string,
asRegex: boolean = false,
): boolean {
if (matchPath === undefined) {
return false
}
if (currentPath.startsWith('/')) {
currentPath = normalize(`${currentPath}`)
}
else {
currentPath = normalize(`/${currentPath}`)
}
if (asRegex) {
return new RegExp(matchPath).test(currentPath)
}
if (normalize(matchPath) !== currentPath) {
return false
}
const hashMatch = matchPath.match(HASH_RE)
if (hashMatch) {
return (inBrowser ? location.hash : '') === hashMatch[0]
}
return true
}
function normalize(path: string): string {
return decodeURI(path)
.replace(HASH_OR_QUERY_RE, '')
.replace(INDEX_OR_EXT_RE, '$1')
}
function containsActiveLink(
path: string,
items: any | any[],
): boolean {
if (Array.isArray(items)) {
return items.some(item => containsActiveLink(path, item))
}
return isActive(path, items.link)
? true
: items.items
? containsActiveLink(path, items.items)
: false
}
export interface SidebarLink {
text: string
link: string
docFooterText?: string
}
function ensureStartingSlash(path: string): string {
return path.startsWith('/') ? path : `/${path}`
}
export function getSidebar(
sidebar: DefaultTheme.Sidebar | undefined,
path: string,
): SidebarItem[] {
if (Array.isArray(sidebar))
return addBase(sidebar)
if (sidebar == null)
return []
path = ensureStartingSlash(path)
const dir = Object.keys(sidebar)
.sort((a, b) => {
return b.split('/').length - a.split('/').length
})
.find((dir) => {
return path.startsWith(ensureStartingSlash(dir))
})
const sidebarDir = dir ? sidebar[dir]! : []
return Array.isArray(sidebarDir)
? addBase(sidebarDir)
: addBase(sidebarDir.items, sidebarDir.base)
}
export function getSidebarGroups(sidebar: SidebarItem[]): SidebarItem[] {
const groups: SidebarItem[] = []
let lastGroupIndex: number = 0
for (const index in sidebar) {
const item = sidebar[index]!
if (item.items) {
lastGroupIndex = groups.push(item)
continue
}
if (!groups[lastGroupIndex]) {
groups.push({ items: [] })
}
groups[lastGroupIndex]!.items!.push(item)
}
return groups
}
export function getFlatSideBarLinks(sidebar: SidebarItem[]): SidebarLink[] {
const links: SidebarLink[] = []
function recursivelyExtractLinks(items: SidebarItem[]) {
for (const item of items) {
if (item.text && item.link) {
links.push({
text: item.text,
link: item.link,
docFooterText: item.docFooterText,
})
}
if (item.items) {
recursivelyExtractLinks(item.items)
}
}
}
recursivelyExtractLinks(sidebar)
return links
}
export function hasActiveLink(
path: string,
items: SidebarItem | SidebarItem[],
): boolean {
if (Array.isArray(items)) {
return items.some(item => hasActiveLink(path, item))
}
return isActive(path, items.link)
? true
: items.items
? hasActiveLink(path, items.items)
: false
}
function addBase(items: SidebarItem[], _base?: string): SidebarItem[] {
return [...items].map((_item) => {
const item = { ..._item }
const base = item.base || _base
if (base && item.link)
item.link = base + item.link
if (item.items)
item.items = addBase(item.items, base)
return item
})
}