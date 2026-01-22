let socket = null;
let serverURL = '';
let isConnected = false;
let atomData = {};
let checkedAtoms = new Map();
let serverInput, connectBtn;
let connectionStatus, serverDisplay;
let atomspaceStats, errorPanel, errorMessage;
let atomCount, nodeCount, linkCount, typeCount;
let refreshBtn, lastUpdate;
let debugCommand, sendCommand, debugResponse;
let atomTypesBreakdown, typesList;
let atomListingPanel, atomListingTitle, atomListingContent, closeAtomListing, visualizeCheckedBtn;
document.addEventListener('DOMContentLoaded', init);
function init() {
serverInput = document.getElementById('server-url');
connectBtn = document.getElementById('connect-btn');
connectionStatus = document.getElementById('connection-status');
serverDisplay = document.getElementById('server-display');
atomspaceStats = document.getElementById('atomspace-stats');
errorPanel = document.getElementById('error-panel');
errorMessage = document.getElementById('error-message');
atomCount = document.getElementById('atom-count');
nodeCount = document.getElementById('node-count');
linkCount = document.getElementById('link-count');
typeCount = document.getElementById('type-count');
refreshBtn = document.getElementById('refresh-stats');
lastUpdate = document.getElementById('last-update');
debugCommand = document.getElementById('debug-command');
sendCommand = document.getElementById('send-command');
debugResponse = document.getElementById('debug-response');
atomTypesBreakdown = document.getElementById('atom-types-breakdown');
typesList = document.getElementById('types-list');
atomListingPanel = document.getElementById('atom-listing-panel');
atomListingTitle = document.getElementById('atom-listing-title');
atomListingContent = document.getElementById('atom-listing-content');
closeAtomListing = document.getElementById('close-atom-listing');
visualizeCheckedBtn = document.getElementById('visualize-checked');
connectBtn.addEventListener('click', toggleConnection);
closeAtomListing.addEventListener('click', hideAtomListing);
visualizeCheckedBtn.addEventListener('click', visualizeCheckedAtoms);
refreshBtn.addEventListener('click', fetchAtomSpaceStats);
sendCommand.addEventListener('click', sendDebugCommand);
debugCommand.addEventListener('keypress', (e) => {
if (e.key === 'Enter') sendDebugCommand();
});
serverInput.value = serverInput.value || 'ws://localhost:18080/';
}
function toggleConnection() {
if (isConnected) {
disconnect();
} else {
connect();
}
}
function connect() {
const baseURL = serverInput.value.trim();
const endpoint = 'json';
if (!baseURL) {
showError('Please enter a CogServer URL');
return;
}
const normalizedURL = baseURL.endsWith('/') ? baseURL : baseURL + '/';
serverURL = normalizedURL + endpoint;
console.log('Connecting to:', serverURL);
try {
socket = new WebSocket(serverURL);
socket.addEventListener('open', onConnect);
socket.addEventListener('close', onDisconnect);
socket.addEventListener('message', onMessage);
socket.addEventListener('error', onError);
connectBtn.disabled = true;
connectionStatus.textContent = 'Connecting...';
connectionStatus.className = 'status-value';
hideError();
} catch (err) {
showError('Failed to create WebSocket connection: ' + err.message);
}
}
function disconnect() {
if (socket) {
console.log('Disconnecting from:', serverURL);
socket.close();
}
}
function onConnect() {
console.log('Connected to:', serverURL);
isConnected = true;
connectBtn.disabled = false;
connectBtn.innerHTML = '<span class="btn-icon">🔌</span><span class="btn-text">Disconnect</span>';
connectBtn.classList.add('connected');
connectionStatus.textContent = 'Connected';
connectionStatus.className = 'status-value connected';
serverDisplay.textContent = serverInput.value + 'json';
serverInput.disabled = true;
refreshBtn.disabled = false;
debugCommand.disabled = false;
sendCommand.disabled = false;
atomspaceStats.classList.remove('hidden');
console.log('Testing connection with version command...');
sendMessage('AtomSpace.version()');
setTimeout(() => {
fetchAtomSpaceStats();
}, 1000);
}
function onDisconnect() {
console.log('Disconnected from:', serverURL);
isConnected = false;
connectBtn.disabled = false;
connectBtn.innerHTML = '<span class="btn-icon">⚡</span><span class="btn-text">Connect</span>';
connectBtn.classList.remove('connected');
connectionStatus.textContent = 'Disconnected';
connectionStatus.className = 'status-value disconnected';
serverDisplay.textContent = 'Not connected';
serverInput.disabled = false;
refreshBtn.disabled = true;
debugCommand.disabled = true;
sendCommand.disabled = true;
atomspaceStats.classList.add('hidden');
atomTypesBreakdown.classList.add('hidden');
atomListingPanel.classList.add('hidden');
socket = null;
}
function onMessage(event) {
console.log('Received message:', event.data);
try {
const data = JSON.parse(event.data);
console.log('Parsed JSON:', data);
debugResponse.textContent = JSON.stringify(data, null, 2);
if (data.success === true && data.result !== undefined) {
const result = data.result;
if (typeof result === 'string') {
if (atomData.pendingValueRequest && result === 'null') {
console.log('Received null value at key');
const { display } = atomData.pendingValueRequest;
displayKeyValue(null, display);
atomData.pendingValueRequest = null;
} else {
console.log('Received string result:', result);
if (result.match(/^\d+\.\d+\.\d+/)) {
console.log('CogServer JSON API version:', result);
}
}
} else if (Array.isArray(result)) {
if (atomData.pendingKeysRequest) {
console.log('Received keys response:', result);
const { display, atom } = atomData.pendingKeysRequest;
displayAtomKeys(result, display, atom);
atomData.pendingKeysRequest = null;
return;
} else if (result.length === 0) {
console.log('Received empty array');
processAtomList(result);
} else if (typeof result[0] === 'string') {
console.log('Received types list with', result.length, 'types');
processTypeList(result);
} else if (typeof result[0] === 'object') {
console.log('Received atoms list with', result.length, 'atoms');
processAtomList(result);
}
} else if (typeof result === 'boolean') {
console.log('Received boolean result:', result);
} else if (typeof result === 'object' && result !== null) {
if (atomData.pendingValueRequest) {
console.log('Received value at key:', result);
const { display } = atomData.pendingValueRequest;
displayKeyValue(result, display);
atomData.pendingValueRequest = null;
} else {
const keys = Object.keys(result);
if (keys.length > 0 && keys.every(key => typeof result[key] === 'number')) {
console.log('Received atom counts:', result);
processAtomCounts(result);
} else {
console.log('Received object result:', result);
}
}
}
} else if (data.success === false) {
const errorMsg = data.error?.message || data.error || 'Unknown error';
console.error('Server returned error:', errorMsg);
showError('Server error: ' + errorMsg);
} else {
console.warn('Unknown response format:', data);
}
} catch (err) {
console.error('Failed to parse JSON:', err);
console.error('Raw message was:', event.data);
debugResponse.textContent = 'Error parsing JSON: ' + err.message + '\n\nRaw response:\n' + event.data;
showError('Invalid response from server');
}
}
function onError(event) {
console.error('WebSocket error:', event);
showError('Connection error: Unable to connect to ' + serverURL);
}
function processAtomList(atoms) {
if (!atoms || !Array.isArray(atoms)) {
console.log('Invalid or empty atom list received');
atoms = [];
}
console.log('Processing atom list:', atoms.length, 'atoms');
if (atomData.pendingTypeDisplay) {
const type = atomData.pendingTypeDisplay;
atomData.pendingTypeDisplay = null;
if (atoms.length === 0) {
atomListingContent.innerHTML = '<div class="no-atoms">No atoms of this type found</div>';
} else {
const container = document.createElement('div');
container.className = 'atom-sexpr-list';
const tempAtoms = atomData.atoms;
atomData.atoms = atoms;
atoms.forEach(atom => {
const atomElement = createClickableAtom(atom);
container.appendChild(atomElement);
});
atomData.atoms = tempAtoms;
atomListingContent.innerHTML = '';
atomListingContent.appendChild(container);
}
console.log(`Displayed ${atoms.length} atoms of type ${type}`);
return;
}
atomData.atoms = atoms;
atomData.totalCount = atoms.length;
let nodes = 0;
let links = 0;
const types = new Set();
const typeCountMap = new Map();
atoms.forEach(atom => {
if (atom.type) {
types.add(atom.type);
const currentCount = typeCountMap.get(atom.type) || 0;
typeCountMap.set(atom.type, currentCount + 1);
}
if (atom.outgoing && Array.isArray(atom.outgoing) && atom.outgoing.length > 0) {
links++;
} else {
nodes++;
}
});
console.log(`Stats - Total: ${atoms.length}, Nodes: ${nodes}, Links: ${links}, Types: ${types.size}`);
updateStats({
total: atoms.length,
nodes: nodes,
links: links,
types: types.size
});
updateAtomTypesBreakdown(typeCountMap);
}
function processTypeList(types) {
if (!types || !Array.isArray(types)) {
console.log('Invalid or empty types list received');
types = [];
}
console.log('Processing type list:', types.length, 'types');
atomData.types = types;
typeCount.textContent = types.length.toLocaleString();
}
function processAtomCounts(counts) {
console.log('Processing atom counts from reportCounts()');
let totalAtoms = 0;
let nodes = 0;
let links = 0;
const typeCountMap = new Map();
for (const [typeName, count] of Object.entries(counts)) {
totalAtoms += count;
typeCountMap.set(typeName, count);
if (typeName.endsWith('Node')) {
nodes += count;
} else if (typeName.endsWith('Link')) {
links += count;
} else {
nodes += count;
}
}
console.log(`Stats from reportCounts - Total: ${totalAtoms}, Nodes: ${nodes}, Links: ${links}, Types: ${typeCountMap.size}`);
updateStats({
total: totalAtoms,
nodes: nodes,
links: links,
types: typeCountMap.size
});
updateAtomTypesBreakdown(typeCountMap);
atomData.counts = counts;
atomData.totalCount = totalAtoms;
}
function updateStats(stats) {
atomCount.textContent = stats.total.toLocaleString();
nodeCount.textContent = stats.nodes.toLocaleString();
linkCount.textContent = stats.links.toLocaleString();
typeCount.textContent = stats.types.toLocaleString();
const now = new Date();
lastUpdate.textContent = `Last updated: ${now.toLocaleTimeString()}`;
[atomCount, nodeCount, linkCount, typeCount].forEach(elem => {
elem.parentElement.classList.add('pulse');
setTimeout(() => elem.parentElement.classList.remove('pulse'), 2000);
});
}
function updateAtomTypesBreakdown(typeCountMap) {
typesList.innerHTML = '';
if (typeCountMap.size === 0) {
atomTypesBreakdown.classList.add('hidden');
return;
}
atomTypesBreakdown.classList.remove('hidden');
const sortedTypes = Array.from(typeCountMap.entries())
.filter(([type, count]) => count > 0)
.sort((a, b) => {
if (b[1] !== a[1]) {
return b[1] - a[1];
}
return a[0].localeCompare(b[0]);
});
sortedTypes.forEach(([type, count]) => {
const typeButton = document.createElement('button');
typeButton.className = 'type-item-button';
typeButton.setAttribute('data-type', type);
typeButton.addEventListener('click', () => showAtomsOfType(type));
const typeName = document.createElement('span');
typeName.className = 'type-name';
typeName.textContent = type;
const typeCount = document.createElement('span');
typeCount.className = 'type-count';
typeCount.textContent = count.toLocaleString();
typeButton.appendChild(typeName);
typeButton.appendChild(typeCount);
typesList.appendChild(typeButton);
});
console.log(`Displayed ${sortedTypes.length} atom types`);
}
function fetchAtomSpaceStats() {
if (!isConnected || !socket) {
showError('Not connected to CogServer');
return;
}
console.log('Fetching AtomSpace stats...');
const command = 'AtomSpace.reportCounts()';
console.log('Sending command:', command);
sendMessage(command);
setTimeout(() => {
const typesCommand = 'AtomSpace.getSubTypes("TopType", true)';
console.log('Sending types command:', typesCommand);
sendMessage(typesCommand);
}, 500);
}
function sendDebugCommand() {
const command = debugCommand.value.trim();
if (!command) return;
sendMessage(command);
debugCommand.value = '';
}
function sendMessage(message) {
if (!isConnected || !socket || socket.readyState !== WebSocket.OPEN) {
showError('Not connected to CogServer');
return;
}
console.log('Sending message:', message);
socket.send(message);
}
function showError(message) {
errorMessage.textContent = message;
errorPanel.classList.remove('hidden');
setTimeout(hideError, 5000);
}
function hideError() {
errorPanel.classList.add('hidden');
}
function createClickableAtom(atom) {
const atomContainer = document.createElement('div');
atomContainer.className = 'atom-container';
const graphCheckbox = document.createElement('input');
graphCheckbox.type = 'checkbox';
graphCheckbox.className = 'graph-checkbox';
graphCheckbox.title = 'Select for graph visualization';
const checkboxId = `checkbox-${atom.type}-${atom.name || 'link'}-${Math.random().toString(36).substr(2, 9)}`;
graphCheckbox.id = checkboxId;
graphCheckbox.addEventListener('change', (e) => {
if (e.target.checked) {
checkedAtoms.set(checkboxId, atom);
} else {
checkedAtoms.delete(checkboxId);
}
});
const atomElement = document.createElement('div');
atomElement.className = 'atom-clickable';
const atomId = `atom-${atom.type}-${atom.name || 'link'}-${Math.random().toString(36).substr(2, 9)}`;
atomElement.setAttribute('data-atom-id', atomId);
const sexpr = atomToSExpression(atom);
atomElement.textContent = sexpr;
const keysDisplay = document.createElement('div');
keysDisplay.className = 'atom-keys-display hidden';
keysDisplay.id = `keys-${atomId}`;
atomElement.addEventListener('click', () => {
handleAtomClick(atom, keysDisplay, atomElement);
});
atomContainer.appendChild(graphCheckbox);
atomContainer.appendChild(atomElement);
atomContainer.appendChild(keysDisplay);
return atomContainer;
}
function handleAtomClick(atom, keysDisplay, atomElement) {
if (!keysDisplay.classList.contains('hidden')) {
keysDisplay.classList.add('hidden');
atomElement.classList.remove('expanded');
return;
}
keysDisplay.innerHTML = '<span class="loading-keys">Loading...</span>';
keysDisplay.classList.remove('hidden');
atomElement.classList.add('expanded');
let atomSpec;
if (atom.name !== undefined) {
const escapedName = JSON.stringify(atom.name);
atomSpec = `{"type": "${atom.type}", "name": ${escapedName}}`;
} else {
atomSpec = JSON.stringify(atom);
}
const command = `AtomSpace.getKeys(${atomSpec})`;
console.log('Getting keys for atom:', command);
atomData.pendingKeysRequest = {
atom: atom,
display: keysDisplay,
element: atomElement
};
if (socket && socket.readyState === WebSocket.OPEN) {
socket.send(command);
} else {
keysDisplay.innerHTML = '<span class="error">Not connected to server</span>';
}
}
function displayAtomKeys(keys, keysDisplay, parentAtom) {
if (!keys || keys.length === 0) {
keysDisplay.innerHTML = '<span class="no-keys">No keys</span>';
} else {
keysDisplay.innerHTML = '';
const keysContainer = document.createElement('div');
keysContainer.className = 'keys-container';
keys.forEach((key, index) => {
const keyRow = document.createElement('div');
keyRow.className = index === 0 ? 'key-row-first' : 'key-row';
const keyElement = document.createElement('span');
keyElement.className = 'key-clickable';
keyElement.textContent = atomToSExpression(key);
const valueDisplay = document.createElement('span');
valueDisplay.className = 'key-value-display hidden';
keyElement.addEventListener('click', () => {
handleKeyClick(parentAtom, key, valueDisplay, keyElement);
});
keyRow.appendChild(keyElement);
keyRow.appendChild(valueDisplay);
keysContainer.appendChild(keyRow);
});
keysDisplay.appendChild(keysContainer);
}
}
function handleKeyClick(atom, key, valueDisplay, keyElement) {
if (!valueDisplay.classList.contains('hidden')) {
valueDisplay.classList.add('hidden');
keyElement.classList.remove('expanded');
return;
}
valueDisplay.innerHTML = '<span class="loading-value">...</span>';
valueDisplay.classList.remove('hidden');
keyElement.classList.add('expanded');
const atomSpec = atom.name !== undefined ?
`{"type": "${atom.type}", "name": ${JSON.stringify(atom.name)}` :
JSON.stringify(atom).slice(0, -1);
const keySpec = key.name !== undefined ?
`{"type": "${key.type}", "name": ${JSON.stringify(key.name)}}` :
JSON.stringify(key);
const command = `AtomSpace.getValueAtKey(${atomSpec}, "key": ${keySpec}})`;
console.log('Getting value at key:', command);
atomData.pendingValueRequest = {
atom: atom,
key: key,
display: valueDisplay,
element: keyElement
};
if (socket && socket.readyState === WebSocket.OPEN) {
socket.send(command);
} else {
valueDisplay.innerHTML = '<span class="error">Not connected to server</span>';
}
}
function displayKeyValue(value, valueDisplay) {
if (!value || value === null) {
valueDisplay.innerHTML = '<span class="no-value">No value</span>';
} else {
const valueContent = document.createElement('span');
valueContent.className = 'value-content-inline';
valueContent.textContent = '→ ' + valueToSExpression(value);
valueDisplay.innerHTML = '';
valueDisplay.appendChild(valueContent);
}
}
function valueToSExpression(value) {
if (!value || !value.type) {
return String(value);
}
const valueType = value.type;
let values = value.value;
if (valueType === 'StringValue' && Array.isArray(values)) {
const quotedValues = values.map(v => JSON.stringify(v));
return `(${valueType} ${quotedValues.join(' ')})`;
} else if (Array.isArray(values)) {
return `(${valueType} ${values.join(' ')})`;
} else if (valueType === 'TruthValue' && values && Array.isArray(values.value)) {
return `(${valueType} ${values.value.join(' ')})`;
} else {
const valueStr = typeof values === 'object' ? JSON.stringify(values) : String(values);
return `(${valueType} ${valueStr})`;
}
}
function atomToSExpression(atom, indent = 0) {
const typeBase = atom.type.replace(/Node$/, '').replace(/Link$/, '');
const indentStr = '  '.repeat(indent);
if (!atom.outgoing || atom.outgoing.length === 0) {
if (atom.name !== undefined) {
const quotedName = JSON.stringify(atom.name);
return `(${typeBase} ${quotedName})`;
} else {
return `(${typeBase})`;
}
} else {
const nextIndent = indent + 1;
const nextIndentStr = '  '.repeat(nextIndent);
const outgoingStrs = atom.outgoing.map(outgoingItem => {
let result;
if (typeof outgoingItem === 'object' && outgoingItem !== null) {
if (outgoingItem.type) {
result = atomToSExpression(outgoingItem, nextIndent);
}
}
if (!result) {
const referencedAtom = atomData.atoms?.find(a => {
if (typeof outgoingItem === 'string' || typeof outgoingItem === 'number') {
return (a.handle === outgoingItem) ||
(a.uuid === outgoingItem) ||
(a.id === outgoingItem) ||
(a.name === outgoingItem);
}
return false;
});
if (referencedAtom) {
result = atomToSExpression(referencedAtom, nextIndent);
} else {
result = `<unresolved:${JSON.stringify(outgoingItem)}>`;
}
}
return nextIndentStr + result;
});
if (outgoingStrs.length > 0) {
return `(${typeBase}\n${outgoingStrs.join('\n')})`;
} else {
return `(${typeBase})`;
}
}
}
function showAtomsOfType(type) {
console.log(`Showing atoms of type: ${type}`);
checkedAtoms.clear();
const count = atomData.counts?.[type] || 0;
const isNode = type.endsWith('Node');
const isLink = type.endsWith('Link');
const maxNodes = 10000;
const maxLinks = 5000;
if ((isNode && count > maxNodes) || (isLink && count > maxLinks)) {
const maxAllowed = isNode ? maxNodes : maxLinks;
const atomType = isNode ? 'nodes' : 'links';
showError(`Cannot download ${count} ${atomType}. Maximum allowed is ${maxAllowed}. ` +
`Downloading this many atoms would take too long and overwhelm the browser.`);
return;
}
atomListingTitle.textContent = `${type} Atoms (${count})`;
atomListingContent.innerHTML = '';
atomListingContent.innerHTML = '<div class="loading">Loading atoms...</div>';
atomListingPanel.classList.remove('hidden');
const command = `AtomSpace.getAtoms("${type}", false)`;
console.log('Fetching atoms of type:', type);
atomData.pendingTypeDisplay = type;
if (socket && socket.readyState === WebSocket.OPEN) {
socket.send(command);
} else {
atomListingContent.innerHTML = '<div class="error">Not connected to server</div>';
}
}
function hideAtomListing() {
atomListingPanel.classList.add('hidden');
atomListingContent.innerHTML = '';
checkedAtoms.clear();
}
function openStatsPage() {
const serverUrl = serverInput.value.trim();
if (!serverUrl) {
showError('Please enter a CogServer URL first');
return;
}
let statsUrl = serverUrl.replace(/^ws:\/\
if (statsUrl.endsWith('/')) {
statsUrl = statsUrl.slice(0, -1);
}
statsUrl = statsUrl + '/stats';
console.log('Opening stats page:', statsUrl);
window.open(statsUrl, '_blank');
}
function openGraphVisualization(atom) {
const atomData = encodeURIComponent(JSON.stringify(atom));
const graphUrl = `tree-view.html?atom=${atomData}&server=${encodeURIComponent(serverInput.value)}`;
window.open(graphUrl, '_blank');
}
function visualizeCheckedAtoms() {
if (checkedAtoms.size === 0) {
showError('No atoms selected. Please check some atoms first.');
return;
}
const atoms = Array.from(checkedAtoms.values());
const atomsData = encodeURIComponent(JSON.stringify(atoms));
const graphUrl = `tree-view.html?atoms=${atomsData}&server=${encodeURIComponent(serverInput.value)}`;
window.open(graphUrl, '_blank');
}