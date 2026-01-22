class CognitiveMeshClient {
constructor(websocketUrl, options = {}) {
this.url = websocketUrl;
this.ws = null;
this.connected = false;
this.eventHandlers = new Map();
this.pendingTasks = new Map();
this.reconnectAttempts = 0;
this.maxReconnectAttempts = options.maxReconnectAttempts || 5;
this.reconnectDelay = options.reconnectDelay || 1000;
this.autoReconnect = options.autoReconnect !== false;
}
async connect() {
return new Promise((resolve, reject) => {
try {
this.ws = new WebSocket(this.url);
this.ws.onopen = () => {
console.log('Connected to cognitive mesh');
this.connected = true;
this.reconnectAttempts = 0;
this.emit('connected');
resolve();
};
this.ws.onmessage = (event) => {
this.handleMessage(JSON.parse(event.data));
};
this.ws.onerror = (error) => {
console.error('WebSocket error:', error);
this.emit('error', error);
reject(error);
};
this.ws.onclose = () => {
console.log('Disconnected from cognitive mesh');
this.connected = false;
this.emit('disconnected');
if (this.autoReconnect && this.reconnectAttempts < this.maxReconnectAttempts) {
this.reconnectAttempts++;
console.log(`Reconnect attempt ${this.reconnectAttempts}/${this.maxReconnectAttempts}`);
setTimeout(() => this.connect(), this.reconnectDelay * this.reconnectAttempts);
}
};
} catch (error) {
reject(error);
}
});
}
disconnect() {
if (this.ws) {
this.autoReconnect = false;
this.ws.close();
this.ws = null;
}
}
handleMessage(message) {
const { event, data, timestamp } = message;
if (this.eventHandlers.has(event)) {
for (const handler of this.eventHandlers.get(event)) {
handler(data, timestamp);
}
}
if (event === 'task.completion' && data.task_id) {
if (this.pendingTasks.has(data.task_id)) {
const { resolve } = this.pendingTasks.get(data.task_id);
resolve(data);
this.pendingTasks.delete(data.task_id);
}
}
if (this.eventHandlers.has('*')) {
for (const handler of this.eventHandlers.get('*')) {
handler({ event, data, timestamp });
}
}
}
on(event, handler) {
if (!this.eventHandlers.has(event)) {
this.eventHandlers.set(event, []);
}
this.eventHandlers.get(event).push(handler);
}
off(event, handler) {
if (this.eventHandlers.has(event)) {
const handlers = this.eventHandlers.get(event);
const index = handlers.indexOf(handler);
if (index > -1) {
handlers.splice(index, 1);
}
}
}
emit(event, data) {
if (this.eventHandlers.has(event)) {
for (const handler of this.eventHandlers.get(event)) {
handler(data);
}
}
}
send(message) {
if (!this.connected) {
throw new Error('Not connected to cognitive mesh');
}
this.ws.send(JSON.stringify(message));
}
async ping() {
return new Promise((resolve) => {
const handler = (data) => {
if (data.type === 'pong') {
this.off('*', handler);
resolve(data);
}
};
this.on('*', handler);
this.send({ type: 'ping' });
});
}
async subscribe(events) {
this.send({
type: 'subscribe',
events: Array.isArray(events) ? events : [events]
});
}
async submitTask(task) {
if (!this.connected) {
throw new Error('Not connected to cognitive mesh');
}
const taskId = task.task_id || this.generateTaskId();
task.task_id = taskId;
return new Promise((resolve, reject) => {
this.pendingTasks.set(taskId, { resolve, reject });
this.send({
type: 'task',
data: task
});
setTimeout(() => {
if (this.pendingTasks.has(taskId)) {
this.pendingTasks.delete(taskId);
reject(new Error('Task timeout'));
}
}, 30000);
});
}
generateTaskId() {
return `task_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
}
}
class CognitiveMeshAPI {
constructor(baseUrl) {
this.baseUrl = baseUrl || 'http://localhost:8000';
}
async getCognitiveState() {
const response = await fetch(`${this.baseUrl}/api/v1/cognitive/state`);
return await response.json();
}
async processTask(task) {
const response = await fetch(`${this.baseUrl}/api/v1/cognitive/process`, {
method: 'POST',
headers: { 'Content-Type': 'application/json' },
body: JSON.stringify(task)
});
return await response.json();
}
async getTaskResult(taskId) {
const response = await fetch(`${this.baseUrl}/api/v1/cognitive/task/${taskId}`);
return await response.json();
}
async getAttentionAllocation() {
const response = await fetch(`${this.baseUrl}/api/v1/attention/allocation`);
return await response.json();
}
async setAttentionFocus(target, weight = 1.0) {
const response = await fetch(`${this.baseUrl}/api/v1/attention/focus`, {
method: 'POST',
headers: { 'Content-Type': 'application/json' },
body: JSON.stringify({ target, weight })
});
return await response.json();
}
async registerAgent(agentType, capabilities = [], metadata = {}) {
const response = await fetch(`${this.baseUrl}/api/v1/agents/register`, {
method: 'POST',
headers: { 'Content-Type': 'application/json' },
body: JSON.stringify({
agent_type: agentType,
capabilities,
metadata
})
});
return await response.json();
}
async getAgent(agentId) {
const response = await fetch(`${this.baseUrl}/api/v1/agents/${agentId}`);
return await response.json();
}
async listAgents() {
const response = await fetch(`${this.baseUrl}/api/v1/agents`);
return await response.json();
}
async unregisterAgent(agentId) {
const response = await fetch(`${this.baseUrl}/api/v1/agents/${agentId}`, {
method: 'DELETE'
});
return await response.json();
}
async healthCheck() {
const response = await fetch(`${this.baseUrl}/api/v1/health`);
return await response.json();
}
}
if (typeof module !== 'undefined' && module.exports) {
module.exports = { CognitiveMeshClient, CognitiveMeshAPI };
}