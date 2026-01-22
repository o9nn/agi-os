class ConnectionMapperUI {
constructor() {
this.currentView = 'domains';
this.selectedDomainId = null;
this.selectedConceptId = null;
this.domains = [];
this.concepts = [];
this.networkData = {
domains: { nodes: [], edges: [] },
concepts: { nodes: [], edges: [] }
};
this.width = 0;
this.height = 0;
this.svg = null;
this.simulation = null;
this.tooltip = null;
this.nodes = [];
this.links = [];
this.initializeUI();
this.loadDomains();
this.loadDomainNetwork();
}
initializeUI() {
document.getElementById('view-domains-btn').addEventListener('click', () => this.switchView('domains'));
document.getElementById('view-concepts-btn').addEventListener('click', () => this.switchView('concepts'));
document.getElementById('add-domain-btn').addEventListener('click', () => this.showDomainForm());
document.getElementById('add-concept-btn').addEventListener('click', () => this.showConceptForm());
document.getElementById('add-connection-btn').addEventListener('click', () => this.showConnectionForm());
document.getElementById('find-paths-btn').addEventListener('click', () => this.showPathsPanel());
document.getElementById('domain-form-submit').addEventListener('click', () => this.submitDomainForm());
document.getElementById('concept-form-submit').addEventListener('click', () => this.submitConceptForm());
document.getElementById('connection-form-submit').addEventListener('click', () => this.submitConnectionForm());
document.getElementById('search-paths-btn').addEventListener('click', () => this.findPaths());
document.querySelectorAll('.modal-close, .modal-cancel').forEach(button => {
button.addEventListener('click', event => {
const modal = event.target.closest('.modal-backdrop');
if (modal) modal.classList.add('hidden');
});
});
document.getElementById('zoom-in-btn').addEventListener('click', () => this.zoomIn());
document.getElementById('zoom-out-btn').addEventListener('click', () => this.zoomOut());
document.getElementById('zoom-reset-btn').addEventListener('click', () => this.resetZoom());
document.getElementById('connection-type').addEventListener('change', event => {
const isDomainConnection = event.target.value === 'domain';
document.getElementById('domain-connection-fields').classList.toggle('hidden', !isDomainConnection);
document.getElementById('concept-connection-fields').classList.toggle('hidden', isDomainConnection);
});
this.initializeVisualization();
}
initializeVisualization() {
const container = document.getElementById('connection-map');
const loadingEl = container.querySelector('.loading');
if (loadingEl) container.removeChild(loadingEl);
this.width = container.clientWidth;
this.height = container.clientHeight;
this.svg = d3.select(container)
.append('svg')
.attr('class', 'connection-mapper-vis')
.attr('width', '100%')
.attr('height', '100%');
this.tooltip = d3.select(container)
.append('div')
.attr('class', 'vis-tooltip hidden');
const zoom = d3.zoom()
.scaleExtent([0.1, 5])
.on('zoom', (event) => {
this.svg.select('g').attr('transform', event.transform);
});
this.svg.call(zoom);
this.svg.append('g');
this.resetZoom();
}
loadDomains() {
fetch('/api/mapper/domains')
.then(response => response.json())
.then(data => {
this.domains = data.domains;
this.populateDomainSelects();
})
.catch(error => {
console.error('Error loading domains:', error);
this.showErrorMessage('Failed to load domains. Please try again later.');
});
}
loadDomainNetwork() {
fetch('/api/mapper/network/domains')
.then(response => response.json())
.then(data => {
this.networkData.domains = data;
if (this.currentView === 'domains') {
this.updateVisualization(data);
}
})
.catch(error => {
console.error('Error loading domain network:', error);
this.showErrorMessage('Failed to load domain network. Please try again later.');
});
}
loadConceptNetwork() {
fetch('/api/mapper/network/concepts')
.then(response => response.json())
.then(data => {
this.networkData.concepts = data;
if (this.currentView === 'concepts') {
this.updateVisualization(data);
}
})
.catch(error => {
console.error('Error loading concept network:', error);
this.showErrorMessage('Failed to load concept network. Please try again later.');
});
}
updateVisualization(data) {
this.svg.select('g').selectAll('*').remove();
if (this.simulation) this.simulation.stop();
if (!data || !data.nodes || !data.nodes.length) {
this.showNoDataMessage();
return;
}
this.simulation = d3.forceSimulation(data.nodes)
.force('link', d3.forceLink(data.edges).id(d => d.id).distance(150))
.force('charge', d3.forceManyBody().strength(-300))
.force('center', d3.forceCenter(this.width / 2, this.height / 2));
const link = this.svg.select('g').selectAll('.link')
.data(data.edges)
.enter()
.append('path')
.attr('class', 'link')
.attr('stroke', d => this.getConnectionColor(d.type))
.attr('stroke-width', d => Math.max(1, d.strength * 3));
const node = this.svg.select('g').selectAll('.node')
.data(data.nodes)
.enter()
.append('g')
.attr('class', 'node')
.on('mouseover', (event, d) => this.showTooltip(event, d))
.on('mouseout', () => this.hideTooltip())
.on('click', (event, d) => this.handleNodeClick(d))
.call(this.drag(this.simulation));
node.append('circle')
.attr('r', d => this.getNodeRadius(d))
.attr('fill', d => this.getNodeColor(d));
node.append('text')
.attr('dx', 12)
.attr('dy', '.35em')
.text(d => d.name)
.attr('fill', 'var(--text-primary)')
.attr('stroke', 'var(--bg-primary)')
.attr('stroke-width', 0.5)
.attr('paint-order', 'stroke');
this.simulation.on('tick', () => {
link.attr('d', d => {
const source = d.source;
const target = d.target;
return `M${source.x},${source.y}L${target.x},${target.y}`;
});
node.attr('transform', d => `translate(${d.x},${d.y})`);
});
this.nodes = node;
this.links = link;
}
getNodeRadius(node) {
let size = 10;
if (this.currentView === 'domains') {
switch (node.domain_type) {
case 'interdisciplinary':
size = 15;
break;
case 'specialized':
size = 12;
break;
case 'theoretical':
size = 11;
break;
case 'applied':
size = 12;
break;
case 'emerging':
size = 9;
break;
}
if (node.connection_count > 0) {
size += Math.min(5, node.connection_count);
}
} else {
switch (node.node_type) {
case 'theory':
size = 14;
break;
case 'principle':
size = 13;
break;
case 'process':
size = 12;
break;
case 'method':
size = 11;
break;
case 'entity':
size = 10;
break;
}
}
return size;
}
getNodeColor(node) {
if (this.currentView === 'domains') {
switch (node.domain_type) {
case 'interdisciplinary':
return '#6366f1';
case 'specialized':
return '#f59e0b';
case 'theoretical':
return '#6366f1';
case 'applied':
return '#10b981';
case 'emerging':
return '#ec4899';
default:
return '#6b7280';
}
} else {
switch (node.node_type) {
case 'theory':
return '#8b5cf6';
case 'principle':
return '#3b82f6';
case 'process':
return '#10b981';
case 'method':
return '#f59e0b';
case 'entity':
return '#ef4444';
default:
return '#6b7280';
}
}
}
getConnectionColor(type) {
switch (type) {
case 'related':
return '#6b7280';
case 'part_of':
return '#3b82f6';
case 'contains':
return '#8b5cf6';
case 'depends_on':
return '#ef4444';
case 'influences':
return '#10b981';
case 'derived_from':
return '#f59e0b';
case 'opposes':
return '#ec4899';
default:
return '#6b7280';
}
}
drag(simulation) {
function dragstarted(event) {
if (!event.active) simulation.alphaTarget(0.3).restart();
event.subject.fx = event.subject.x;
event.subject.fy = event.subject.y;
}
function dragged(event) {
event.subject.fx = event.x;
event.subject.fy = event.y;
}
function dragended(event) {
if (!event.active) simulation.alphaTarget(0);
event.subject.fx = null;
event.subject.fy = null;
}
return d3.drag()
.on('start', dragstarted)
.on('drag', dragged)
.on('end', dragended);
}
showTooltip(event, node) {
const tooltip = this.tooltip;
let content = '';
if (this.currentView === 'domains') {
content = `
<div class="tooltip-title">${node.name}</div>
<div class="tooltip-type">${node.domain_type}</div>
${node.description ? `<div class="tooltip-desc">${node.description}</div>` : ''}
<div class="tooltip-stats">
<div>Concepts: ${node.concept_count || 0}</div>
<div>Connections: ${node.connection_count || 0}</div>
</div>
`;
} else {
content = `
<div class="tooltip-title">${node.name}</div>
<div class="tooltip-type">${node.node_type}</div>
${node.description ? `<div class="tooltip-desc">${node.description}</div>` : ''}
<div class="tooltip-stats">
<div>Domain: ${node.domain_name}</div>
<div>Connections: ${node.connection_count || 0}</div>
</div>
`;
}
tooltip.html(content)
.style('left', (event.pageX + 10) + 'px')
.style('top', (event.pageY - 28) + 'px')
.classed('hidden', false);
}
hideTooltip() {
this.tooltip.classed('hidden', true);
}
handleNodeClick(node) {
if (this.currentView === 'domains') {
this.showDomainDetails(node.id);
} else {
this.showConceptDetails(node.id);
}
}
showDomainDetails(domainId) {
this.selectedDomainId = domainId;
fetch(`/api/mapper/domains/${domainId}`)
.then(response => response.json())
.then(data => {
const domain = data.domain;
if (!domain) {
console.error('Domain not found:', domainId);
return;
}
const panel = document.getElementById('domain-details');
const emptyState = panel.querySelector('.empty-state');
const domainInfo = panel.querySelector('.domain-info');
emptyState.classList.add('hidden');
domainInfo.classList.remove('hidden');
domainInfo.querySelector('.domain-name').textContent = domain.name;
domainInfo.querySelector('.domain-description').textContent = domain.description || 'No description provided.';
domainInfo.querySelector('.domain-type').textContent = domain.domain_type;
domainInfo.querySelector('.concept-count').textContent = domain.concept_count || '0';
const connectionsList = domainInfo.querySelector('.connections-list');
connectionsList.innerHTML = '';
if (domain.connections && domain.connections.length > 0) {
domain.connections.forEach(conn => {
const targetDomain = this.domains.find(d => d.id === conn.target_domain_id);
if (!targetDomain) return;
const li = document.createElement('li');
li.innerHTML = `
<span class="connection-name">${targetDomain.name}</span>
<span class="connection-type">${conn.connection_type}</span>
`;
connectionsList.appendChild(li);
});
} else {
const li = document.createElement('li');
li.textContent = 'No connections';
connectionsList.appendChild(li);
}
document.getElementById('domain-details').classList.remove('hidden');
document.getElementById('concept-details').classList.add('hidden');
document.getElementById('paths-panel').classList.add('hidden');
})
.catch(error => {
console.error('Error fetching domain details:', error);
this.showErrorMessage('Failed to load domain details. Please try again later.');
});
}
showConceptDetails(conceptId) {
this.selectedConceptId = conceptId;
fetch(`/api/mapper/concepts/${conceptId}`)
.then(response => response.json())
.then(data => {
const concept = data.concept;
if (!concept) {
console.error('Concept not found:', conceptId);
return;
}
const domain = this.domains.find(d => d.id === concept.domain_id);
const domainName = domain ? domain.name : 'Unknown';
const panel = document.getElementById('concept-details');
const emptyState = panel.querySelector('.empty-state');
const conceptInfo = panel.querySelector('.concept-info');
emptyState.classList.add('hidden');
conceptInfo.classList.remove('hidden');
conceptInfo.querySelector('.concept-name').textContent = concept.name;
conceptInfo.querySelector('.concept-description').textContent = concept.description || 'No description provided.';
conceptInfo.querySelector('.concept-type').textContent = concept.node_type;
conceptInfo.querySelector('.domain-name').textContent = domainName;
const connectionsList = conceptInfo.querySelector('.connections-list');
connectionsList.innerHTML = '';
if (concept.connections && concept.connections.length > 0) {
concept.connections.forEach(conn => {
const li = document.createElement('li');
li.innerHTML = `
<span class="connection-name">${conn.target_name || 'Unknown'}</span>
<span class="connection-type">${conn.relation_type}</span>
`;
connectionsList.appendChild(li);
});
} else {
const li = document.createElement('li');
li.textContent = 'No connections';
connectionsList.appendChild(li);
}
document.getElementById('domain-details').classList.add('hidden');
document.getElementById('concept-details').classList.remove('hidden');
document.getElementById('paths-panel').classList.add('hidden');
})
.catch(error => {
console.error('Error fetching concept details:', error);
this.showErrorMessage('Failed to load concept details. Please try again later.');
});
}
switchView(view) {
if (view === this.currentView) return;
document.getElementById('view-domains-btn').classList.toggle('active', view === 'domains');
document.getElementById('view-concepts-btn').classList.toggle('active', view === 'concepts');
this.currentView = view;
if (view === 'domains') {
this.loadDomainNetwork();
} else {
this.loadConceptNetwork();
}
this.selectedDomainId = null;
this.selectedConceptId = null;
document.getElementById('domain-details').querySelector('.empty-state').classList.remove('hidden');
document.getElementById('domain-details').querySelector('.domain-info').classList.add('hidden');
document.getElementById('concept-details').querySelector('.empty-state').classList.remove('hidden');
document.getElementById('concept-details').querySelector('.concept-info').classList.add('hidden');
document.getElementById('domain-details').classList.toggle('hidden', view !== 'domains');
document.getElementById('concept-details').classList.toggle('hidden', view !== 'concepts');
document.getElementById('paths-panel').classList.add('hidden');
}
showDomainForm() {
if (this.selectedDomainId) {
const domain = this.domains.find(d => d.id === this.selectedDomainId);
if (domain) {
document.getElementById('domain-name').value = domain.name;
document.getElementById('domain-description').value = domain.description || '';
document.getElementById('domain-type').value = domain.domain_type;
document.querySelector('#domain-form-modal .modal-title').textContent = 'Edit Knowledge Domain';
document.getElementById('domain-form-submit').textContent = 'Update Domain';
}
} else {
document.getElementById('domain-form').reset();
document.querySelector('#domain-form-modal .modal-title').textContent = 'Add Knowledge Domain';
document.getElementById('domain-form-submit').textContent = 'Create Domain';
}
document.getElementById('domain-form-modal').classList.remove('hidden');
}
showConceptForm() {
const domainSelect = document.getElementById('concept-domain');
domainSelect.innerHTML = '';
this.domains.forEach(domain => {
const option = document.createElement('option');
option.value = domain.id;
option.textContent = domain.name;
domainSelect.appendChild(option);
});
if (this.selectedConceptId) {
fetch(`/api/mapper/concepts/${this.selectedConceptId}`)
.then(response => response.json())
.then(data => {
const concept = data.concept;
if (!concept) {
console.error('Concept not found:', this.selectedConceptId);
return;
}
document.getElementById('concept-name').value = concept.name;
document.getElementById('concept-description').value = concept.description || '';
document.getElementById('concept-domain').value = concept.domain_id;
document.getElementById('concept-type').value = concept.node_type;
document.querySelector('#concept-form-modal .modal-title').textContent = 'Edit Concept';
document.getElementById('concept-form-submit').textContent = 'Update Concept';
})
.catch(error => {
console.error('Error fetching concept details:', error);
this.showErrorMessage('Failed to load concept details. Please try again later.');
});
} else {
document.getElementById('concept-form').reset();
if (this.selectedDomainId) {
document.getElementById('concept-domain').value = this.selectedDomainId;
}
document.querySelector('#concept-form-modal .modal-title').textContent = 'Add Concept';
document.getElementById('concept-form-submit').textContent = 'Create Concept';
}
document.getElementById('concept-form-modal').classList.remove('hidden');
}
showConnectionForm() {
this.populateDomainSelects();
document.getElementById('connection-form').reset();
document.getElementById('domain-connection-fields').classList.remove('hidden');
document.getElementById('concept-connection-fields').classList.add('hidden');
if (this.currentView === 'domains' && this.selectedDomainId) {
document.getElementById('source-domain-select').value = this.selectedDomainId;
} else if (this.currentView === 'concepts' && this.selectedConceptId) {
document.getElementById('connection-type').value = 'concept';
document.getElementById('domain-connection-fields').classList.add('hidden');
document.getElementById('concept-connection-fields').classList.remove('hidden');
}
document.getElementById('connection-form-modal').classList.remove('hidden');
}
showPathsPanel() {
this.populateDomainSelects();
if (this.selectedDomainId) {
document.getElementById('source-domain').value = this.selectedDomainId;
}
document.getElementById('domain-details').classList.add('hidden');
document.getElementById('concept-details').classList.add('hidden');
document.getElementById('paths-panel').classList.remove('hidden');
}
populateDomainSelects() {
const selects = [
document.getElementById('source-domain-select'),
document.getElementById('target-domain-select'),
document.getElementById('parent-domain'),
document.getElementById('source-domain'),
document.getElementById('target-domain')
];
selects.forEach(select => {
if (!select) return;
select.innerHTML = '';
if (select.id === 'parent-domain') {
const option = document.createElement('option');
option.value = '';
option.textContent = 'None';
select.appendChild(option);
}
this.domains.forEach(domain => {
const option = document.createElement('option');
option.value = domain.id;
option.textContent = domain.name;
select.appendChild(option);
});
});
}
submitDomainForm() {
const name = document.getElementById('domain-name').value;
const description = document.getElementById('domain-description').value;
const domainType = document.getElementById('domain-type').value;
const parentDomain = document.getElementById('parent-domain').value;
if (!name) {
this.showErrorMessage('Domain name is required');
return;
}
const data = {
name,
description,
domain_type: domainType
};
if (parentDomain) {
data.parent_domain = parentDomain;
}
const method = this.selectedDomainId ? 'PUT' : 'POST';
const url = this.selectedDomainId
? `/api/mapper/domains/${this.selectedDomainId}`
: '/api/mapper/domains';
fetch(url, {
method,
headers: {
'Content-Type': 'application/json'
},
body: JSON.stringify(data)
})
.then(response => response.json())
.then(result => {
if (result.message) {
document.getElementById('domain-form-modal').classList.add('hidden');
alert(result.message);
this.loadDomains();
this.loadDomainNetwork();
this.selectedDomainId = null;
}
})
.catch(error => {
console.error('Error submitting domain:', error);
this.showErrorMessage('Failed to save domain. Please try again later.');
});
}
submitConceptForm() {
const name = document.getElementById('concept-name').value;
const description = document.getElementById('concept-description').value;
const domainId = document.getElementById('concept-domain').value;
const conceptType = document.getElementById('concept-type').value;
if (!name) {
this.showErrorMessage('Concept name is required');
return;
}
if (!domainId) {
this.showErrorMessage('Domain is required');
return;
}
const data = {
name,
description,
domain_id: domainId,
node_type: conceptType
};
const method = this.selectedConceptId ? 'PUT' : 'POST';
const url = this.selectedConceptId
? `/api/mapper/concepts/${this.selectedConceptId}`
: '/api/mapper/concepts';
fetch(url, {
method,
headers: {
'Content-Type': 'application/json'
},
body: JSON.stringify(data)
})
.then(response => response.json())
.then(result => {
if (result.message) {
document.getElementById('concept-form-modal').classList.add('hidden');
alert(result.message);
if (this.currentView === 'concepts') {
this.loadConceptNetwork();
}
this.selectedConceptId = null;
}
})
.catch(error => {
console.error('Error submitting concept:', error);
this.showErrorMessage('Failed to save concept. Please try again later.');
});
}
submitConnectionForm() {
const connectionType = document.getElementById('connection-type').value;
if (connectionType === 'domain') {
this.submitDomainConnection();
} else {
this.submitConceptConnection();
}
}
submitDomainConnection() {
const sourceDomainId = document.getElementById('source-domain-select').value;
const targetDomainId = document.getElementById('target-domain-select').value;
const relationshipType = document.getElementById('domain-connection-type').value;
const strength = parseFloat(document.getElementById('connection-strength').value);
const description = document.getElementById('connection-description').value;
const bidirectional = document.getElementById('bidirectional-connection').checked;
if (!sourceDomainId) {
this.showErrorMessage('Source domain is required');
return;
}
if (!targetDomainId) {
this.showErrorMessage('Target domain is required');
return;
}
if (sourceDomainId === targetDomainId) {
this.showErrorMessage('Source and target domains must be different');
return;
}
const data = {
source_domain_id: sourceDomainId,
target_domain_id: targetDomainId,
connection_type: relationshipType,
strength,
description,
bidirectional
};
fetch('/api/mapper/connections/domains', {
method: 'POST',
headers: {
'Content-Type': 'application/json'
},
body: JSON.stringify(data)
})
.then(response => response.json())
.then(result => {
if (result.message) {
document.getElementById('connection-form-modal').classList.add('hidden');
alert(result.message);
this.loadDomainNetwork();
}
})
.catch(error => {
console.error('Error creating domain connection:', error);
this.showErrorMessage('Failed to create connection. Please try again later.');
});
}
submitConceptConnection() {
const sourceConceptId = document.getElementById('source-concept-select').value;
const targetConceptId = document.getElementById('target-concept-select').value;
const relationType = document.getElementById('concept-relation-type').value;
const strength = parseFloat(document.getElementById('connection-strength').value);
const description = document.getElementById('connection-description').value;
const bidirectional = document.getElementById('bidirectional-connection').checked;
if (!sourceConceptId) {
this.showErrorMessage('Source concept is required');
return;
}
if (!targetConceptId) {
this.showErrorMessage('Target concept is required');
return;
}
if (sourceConceptId === targetConceptId) {
this.showErrorMessage('Source and target concepts must be different');
return;
}
const data = {
source_concept_id: sourceConceptId,
target_concept_id: targetConceptId,
relation_type: relationType,
strength,
description,
bidirectional
};
fetch('/api/mapper/connections/concepts', {
method: 'POST',
headers: {
'Content-Type': 'application/json'
},
body: JSON.stringify(data)
})
.then(response => response.json())
.then(result => {
if (result.message) {
document.getElementById('connection-form-modal').classList.add('hidden');
alert(result.message);
if (this.currentView === 'concepts') {
this.loadConceptNetwork();
}
}
})
.catch(error => {
console.error('Error creating concept connection:', error);
this.showErrorMessage('Failed to create connection. Please try again later.');
});
}
findPaths() {
const sourceDomainId = document.getElementById('source-domain').value;
const targetDomainId = document.getElementById('target-domain').value;
if (!sourceDomainId) {
this.showErrorMessage('Source domain is required');
return;
}
if (!targetDomainId) {
this.showErrorMessage('Target domain is required');
return;
}
if (sourceDomainId === targetDomainId) {
this.showErrorMessage('Source and target domains must be different');
return;
}
const resultsContainer = document.querySelector('#paths-panel .paths-results');
const pathsList = document.querySelector('#paths-panel .paths-list');
pathsList.innerHTML = '<div class="loading">Searching for paths...</div>';
resultsContainer.classList.remove('hidden');
fetch(`/api/mapper/interdisciplinary-paths?source=${sourceDomainId}&target=${targetDomainId}`)
.then(response => response.json())
.then(data => {
pathsList.innerHTML = '';
if (data.paths && data.paths.length > 0) {
data.paths.forEach((path, index) => {
const pathItem = document.createElement('div');
pathItem.className = 'path-item';
const pathHeader = document.createElement('h4');
pathHeader.textContent = `Path ${index + 1}`;
pathItem.appendChild(pathHeader);
const pathSteps = document.createElement('div');
pathSteps.className = 'path-steps';
path.forEach((step, stepIndex) => {
const nodeEl = document.createElement('span');
nodeEl.className = 'path-node';
nodeEl.textContent = step.domain_name;
pathSteps.appendChild(nodeEl);
if (stepIndex < path.length - 1) {
const arrowEl = document.createElement('span');
arrowEl.className = 'path-arrow';
arrowEl.innerHTML = '→';
pathSteps.appendChild(arrowEl);
const typeEl = document.createElement('small');
typeEl.className = 'path-conn-type';
typeEl.textContent = path[stepIndex + 1].connection_type;
pathSteps.appendChild(typeEl);
const arrowEl2 = document.createElement('span');
arrowEl2.className = 'path-arrow';
arrowEl2.innerHTML = '→';
pathSteps.appendChild(arrowEl2);
}
});
pathItem.appendChild(pathSteps);
pathsList.appendChild(pathItem);
});
} else {
pathsList.innerHTML = '<div class="empty-message">No paths found between these domains.</div>';
}
})
.catch(error => {
console.error('Error finding paths:', error);
pathsList.innerHTML = '<div class="error-message">Failed to find paths. Please try again later.</div>';
});
}
showNoDataMessage() {
const container = document.getElementById('connection-map');
const g = this.svg.select('g');
g.append('text')
.attr('x', container.clientWidth / 2)
.attr('y', container.clientHeight / 2)
.attr('text-anchor', 'middle')
.attr('fill', 'var(--text-secondary)')
.text('No data available');
}
showErrorMessage(message) {
alert(message);
}
zoomIn() {
const zoom = d3.zoom().scaleExtent([0.1, 5]);
const transform = d3.zoomTransform(this.svg.node());
const newTransform = transform.scale(1.2);
this.svg.transition().duration(300).call(zoom.transform, newTransform);
}
zoomOut() {
const zoom = d3.zoom().scaleExtent([0.1, 5]);
const transform = d3.zoomTransform(this.svg.node());
const newTransform = transform.scale(0.8);
this.svg.transition().duration(300).call(zoom.transform, newTransform);
}
resetZoom() {
const zoom = d3.zoom().scaleExtent([0.1, 5]);
const width = this.width;
const height = this.height;
this.svg.transition().duration(300).call(
zoom.transform,
d3.zoomIdentity.translate(width / 2, height / 2).scale(0.8)
);
}
}