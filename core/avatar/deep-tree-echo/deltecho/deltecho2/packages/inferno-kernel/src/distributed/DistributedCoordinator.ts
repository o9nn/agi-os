import { Atom, AtomSpace } from '../atomspace/AtomSpace.js'
export interface NodeInfo {
nodeId: string
address: string
port: number
capabilities: string[]
load: number
lastHeartbeat: number
}
export interface DistributedTask {
taskId: string
type: 'reasoning' | 'learning' | 'pattern-matching'
atomSpaceSnapshot: string
assignedNode?: string
status: 'pending' | 'running' | 'completed' | 'failed'
result?: any
}
export interface CoordinatorConfig {
nodeId: string
heartbeatInterval: number
taskTimeout: number
replicationFactor: number
}
export class DistributedCoordinator {
private config: CoordinatorConfig
private nodes: Map<string, NodeInfo>
private tasks: Map<string, DistributedTask>
private localAtomSpace: AtomSpace
private nextTaskId: number
constructor(
localAtomSpace: AtomSpace,
config: Partial<CoordinatorConfig> = {}
) {
this.localAtomSpace = localAtomSpace
this.config = {
nodeId: config.nodeId || `node_${Math.random().toString(36).substr(2, 9)}`,
heartbeatInterval: config.heartbeatInterval || 5000,
taskTimeout: config.taskTimeout || 30000,
replicationFactor: config.replicationFactor || 2,
}
this.nodes = new Map()
this.tasks = new Map()
this.nextTaskId = 1
this.initializeNode()
}
private initializeNode(): void {
const localNode: NodeInfo = {
nodeId: this.config.nodeId,
address: 'localhost',
port: 8080,
capabilities: ['reasoning', 'learning', 'pattern-matching'],
load: 0,
lastHeartbeat: Date.now(),
}
this.nodes.set(this.config.nodeId, localNode)
console.log(`[DistributedCoordinator] Node ${this.config.nodeId} initialized`)
}
registerNode(node: Omit<NodeInfo, 'lastHeartbeat'>): void {
const nodeInfo: NodeInfo = {
...node,
lastHeartbeat: Date.now(),
}
this.nodes.set(node.nodeId, nodeInfo)
console.log(`[DistributedCoordinator] Registered node ${node.nodeId}`)
}
heartbeat(nodeId: string): void {
const node = this.nodes.get(nodeId)
if (node) {
node.lastHeartbeat = Date.now()
}
}
pruneDeadNodes(): string[] {
const now = Date.now()
const deadNodes: string[] = []
const timeout = this.config.heartbeatInterval * 3
for (const [nodeId, node] of this.nodes.entries()) {
if (nodeId === this.config.nodeId) continue
if (now - node.lastHeartbeat > timeout) {
deadNodes.push(nodeId)
this.nodes.delete(nodeId)
}
}
if (deadNodes.length > 0) {
console.log(`[DistributedCoordinator] Pruned dead nodes: ${deadNodes.join(', ')}`)
}
return deadNodes
}
createTask(
type: DistributedTask['type'],
atomSpaceSnapshot: string
): string {
const taskId = `task_${this.nextTaskId++}`
const task: DistributedTask = {
taskId,
type,
atomSpaceSnapshot,
status: 'pending',
}
this.tasks.set(taskId, task)
console.log(`[DistributedCoordinator] Created task ${taskId} of type ${type}`)
return taskId
}
assignTask(taskId: string): boolean {
const task = this.tasks.get(taskId)
if (!task) return false
let bestNode: NodeInfo | null = null
let minLoad = Infinity
for (const node of this.nodes.values()) {
if (node.capabilities.includes(task.type) && node.load < minLoad) {
bestNode = node
minLoad = node.load
}
}
if (!bestNode) {
console.log(`[DistributedCoordinator] No suitable node for task ${taskId}`)
return false
}
task.assignedNode = bestNode.nodeId
task.status = 'running'
bestNode.load++
console.log(`[DistributedCoordinator] Assigned task ${taskId} to node ${bestNode.nodeId}`)
return true
}
completeTask(taskId: string, result: any): boolean {
const task = this.tasks.get(taskId)
if (!task) return false
task.status = 'completed'
task.result = result
if (task.assignedNode) {
const node = this.nodes.get(task.assignedNode)
if (node) {
node.load = Math.max(0, node.load - 1)
}
}
console.log(`[DistributedCoordinator] Task ${taskId} completed`)
return true
}
replicateAtoms(atoms: Atom[]): Map<string, Atom[]> {
const distribution = new Map<string, Atom[]>()
for (const atom of atoms) {
const nodes = this.selectNodesForReplication(this.config.replicationFactor)
for (const nodeId of nodes) {
if (!distribution.has(nodeId)) {
distribution.set(nodeId, [])
}
distribution.get(nodeId)!.push(atom)
}
}
console.log(
`[DistributedCoordinator] Replicated ${atoms.length} atoms across ${distribution.size} nodes`
)
return distribution
}
private selectNodesForReplication(count: number): string[] {
const availableNodes = Array.from(this.nodes.keys())
const selected: string[] = []
for (let i = 0; i < Math.min(count, availableNodes.length); i++) {
selected.push(availableNodes[i])
}
return selected
}
async synchronizeAtomSpace(): Promise<void> {
console.log('[DistributedCoordinator] Synchronizing AtomSpace across nodes...')
const localAtoms = this.localAtomSpace.getAllAtoms()
const distribution = this.replicateAtoms(localAtoms)
for (const [nodeId, atoms] of distribution.entries()) {
console.log(`[DistributedCoordinator] Node ${nodeId}: ${atoms.length} atoms`)
}
}
getStats(): {
totalNodes: number
activeNodes: number
totalTasks: number
pendingTasks: number
runningTasks: number
completedTasks: number
} {
const tasks = Array.from(this.tasks.values())
return {
totalNodes: this.nodes.size,
activeNodes: this.nodes.size,
totalTasks: tasks.length,
pendingTasks: tasks.filter(t => t.status === 'pending').length,
runningTasks: tasks.filter(t => t.status === 'running').length,
completedTasks: tasks.filter(t => t.status === 'completed').length,
}
}
getNodes(): NodeInfo[] {
return Array.from(this.nodes.values())
}
getTasks(): DistributedTask[] {
return Array.from(this.tasks.values())
}
}