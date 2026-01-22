let network = null;
let vertices = null;  
let edges = null;
let rootAtoms = [];
let serverUrl = null;
let atomVertexMap = new Map();  
let vertexIdCounter = 1;
let operationCancelled = false;
let pendingOperation = null;
let operationStartTime = null;
let stopButtonTimer = null;
const LARGE_ATOM_THRESHOLD = 300; 
const STOP_BUTTON_DELAY = 2000;   
document.addEventListener('DOMContentLoaded', function() {
    const urlParams = new URLSearchParams(window.location.search);
    const atomParam = urlParams.get('atom');
    const atomsParam = urlParams.get('atoms');
    const serverParam = urlParams.get('server');
    if (atomParam) {
        try {
            const atom = JSON.parse(decodeURIComponent(atomParam));
            rootAtoms = [atom];
            atomSpaceCache.clear();
            atomSpaceCache.addAtom(atom);
            console.log('Root atom:', atom);
        } catch (e) {
            console.error('Failed to parse atom data:', e);
            updateStatus('Failed to parse atom data', 'error');
            return;
        }
    }
    if (atomsParam) {
        try {
            rootAtoms = JSON.parse(decodeURIComponent(atomsParam));
            atomSpaceCache.clear();
            rootAtoms.forEach(atom => atomSpaceCache.addAtom(atom));
            console.log('Root atoms:', rootAtoms);
        } catch (e) {
            console.error('Failed to parse atoms data:', e);
            updateStatus('Failed to parse atoms data', 'error');
            return;
        }
    }
    if (serverParam) {
        serverUrl = decodeURIComponent(serverParam);
        console.log('Server URL:', serverUrl);
    }
    initializeGraph();
    if (serverUrl) {
        connectToServer();
    }
    setupEventHandlers();
});
function initializeGraph() {
    if (typeof vis === 'undefined') {
        console.error('vis-network library not loaded');
        updateStatus('Error: vis-network library not loaded', 'error');
        return;
    }
    vertices = new vis.DataSet();
    edges = new vis.DataSet();
    const container = document.getElementById('mynetwork');
    if (!container) {
        console.error('Container element #mynetwork not found');
        updateStatus('Error: Container element not found', 'error');
        return;
    }
    const data = {
        nodes: vertices,  
        edges: edges
    };
    const options = {
        nodes: {  
            shape: 'box',
            font: {
                size: 14,
                face: 'monospace'
            },
            margin: 5,
            widthConstraint: {
                maximum: 150,
                minimum: 40
            }
        },
        edges: {
            arrows: {
                to: {
                    enabled: true,
                    scaleFactor: 0.5
                }
            },
            smooth: {
                enabled: false  
            }
        },
        physics: {
            enabled: true,
            solver: 'hierarchicalRepulsion',
            hierarchicalRepulsion: {
                nodeDistance: 150,
                centralGravity: 0.0,
                springLength: 100,
                springConstant: 0.01,
                damping: 0.09
            },
            stabilization: {
                iterations: 150
            }
        },
        layout: {
            hierarchical: {
                enabled: true,
                direction: 'UD',  
                sortMethod: 'hubsize',  
                levelSeparation: 150,
                nodeSpacing: 100,
                treeSpacing: 200,
                blockShifting: false,  
                edgeMinimization: false  
            }
        },
        interaction: {
            hover: true,
            tooltipDelay: 200
        }
    };
    network = new vis.Network(container, data, options);
    network.on('click', function(params) {
        if (params.nodes.length > 0) {  
            const vertexId = params.nodes[0];
            const vertex = vertices.get(vertexId);
            if (vertex && vertex.atom) {
                const layoutSelect = document.getElementById('layoutSelect');
                const layoutType = layoutSelect ? layoutSelect.value : 'hierarchical';
                const currentAtomCount = atomSpaceCache.getStats().totalAtoms;
                if (currentAtomCount > LARGE_ATOM_THRESHOLD) {
                    showWarningDialog(currentAtomCount, function() {
                        atomSpaceCache.fetchIncomingSet(vertex.atom);
                    });
                } else {
                    startOperation();
                    atomSpaceCache.fetchIncomingSet(vertex.atom);
                }
            }
        }
        else if (params.edges.length > 0) {
            const edgeId = params.edges[0];
            const edge = edges.get(edgeId);
            if (edge) {
                const layoutSelect = document.getElementById('layoutSelect');
                const layoutType = layoutSelect ? layoutSelect.value : 'hierarchical';
                if (layoutType === 'graph') {
                    removeSingleVertex(edge.from);
                } else {
                    removeVertexAndParents(edge.from);
                }
            }
        }
    });
    if (rootAtoms && rootAtoms.length > 0) {
        console.log('Adding root atoms to graph:', rootAtoms);
        rootAtoms.forEach(atom => {
            addAtomToGraph(atom, null, 0);
        });
        network.fit();
        updateStatus(`Graph initialized with ${rootAtoms.length} atom(s)`, 'connected');
    } else {
        updateStatus('No atoms to display', 'error');
        console.warn('No root atoms found');
    }
}
function connectToServer() {
    if (!serverUrl) {
        updateStatus('No server URL provided', 'error');
        return;
    }
    let jsonUrl = serverUrl;
    if (!jsonUrl.endsWith('/json')) {
        if (!jsonUrl.endsWith('/')) {
            jsonUrl += '/';
        }
        jsonUrl += 'json';
    }
    console.log('Attempting to connect to:', jsonUrl);
    updateStatus('Connecting to server...', 'loading');
    atomSpaceCache.connect(jsonUrl);
}
function addAtomToGraph(atom, parentId, depth, order = 0) {
    const atomKey = atomToKey(atom);
    if (atomVertexMap.has(atomKey)) {
        const existingVertexId = atomVertexMap.get(atomKey);
        if (parentId !== null) {
            const parentVertex = vertices.get(parentId);
            const existingVertex = vertices.get(existingVertexId);
            if (parentVertex && existingVertex) {
                if (existingVertex.level <= parentVertex.level) {
                    const newLevel = parentVertex.level + 1;
                    vertices.update({
                        id: existingVertexId,
                        level: newLevel
                    });
                    updateChildrenLevels(existingVertexId, newLevel);
                }
                addEdgeIfNotExists(parentId, existingVertexId);
            }
        }
        return existingVertexId;
    }
    const vertexId = vertexIdCounter++;
    const vertexLabel = createCompactLabel(atom);
    const vertexColor = getVertexColor(atom.type);
    vertices.add({
        id: vertexId,
        label: vertexLabel,
        color: vertexColor,
        atom: atom,
        level: depth,
        x: order * 150,  
        title: atomToSExpression(atom) 
    });
    atomVertexMap.set(atomKey, vertexId);
    if (parentId !== null) {
        edges.add({
            from: parentId,
            to: vertexId,
            arrows: {
                to: {
                    enabled: true
                }
            }
        });
    }
    if (atom.outgoing && atom.outgoing.length > 0) {
        atom.outgoing.forEach((outgoing, index) => {
            if (typeof outgoing === 'object' && outgoing !== null) {
                addAtomToGraph(outgoing, vertexId, depth + 1, index);
            }
        });
    }
    return vertexId;
}
function updateChildrenLevels(vertexId, parentLevel) {
    const childEdges = edges.get({
        filter: function(edge) {
            return edge.from === vertexId;
        }
    });
    childEdges.forEach(edge => {
        const childVertex = vertices.get(edge.to);
        if (childVertex && childVertex.level <= parentLevel) {
            const newChildLevel = parentLevel + 1;
            vertices.update({
                id: edge.to,
                level: newChildLevel
            });
            updateChildrenLevels(edge.to, newChildLevel);
        }
    });
}
function removeVertexAndParents(vertexId) {
    const vertex = vertices.get(vertexId);
    if (!vertex || !vertex.atom) {
        return;
    }
    const removedCount = atomSpaceCache.removeAtomAndParents(vertex.atom);
    const verticesToRemove = new Set();
    const edgesToRemove = new Set();
    function collectParents(currentVertexId) {
        if (verticesToRemove.has(currentVertexId)) {
            return; 
        }
        verticesToRemove.add(currentVertexId);
        const childEdges = edges.get({
            filter: function(edge) {
                return edge.from === currentVertexId;
            }
        });
        childEdges.forEach(edge => {
            edgesToRemove.add(edge.id);
        });
        const parentEdges = edges.get({
            filter: function(edge) {
                return edge.to === currentVertexId;
            }
        });
        parentEdges.forEach(edge => {
            edgesToRemove.add(edge.id);
            collectParents(edge.from);
        });
    }
    collectParents(vertexId);
    edges.remove(Array.from(edgesToRemove));
    const vertexIdsToRemove = Array.from(verticesToRemove);
    vertices.remove(vertexIdsToRemove);
    atomVertexMap.forEach((value, key) => {
        if (verticesToRemove.has(value)) {
            atomVertexMap.delete(key);
        }
    });
    updateStatus(`Removed ${verticesToRemove.size} vertex/vertices from display and ${removedCount} atom(s) from cache`, 'connected');
}
function removeSingleVertex(vertexId) {
    const vertex = vertices.get(vertexId);
    if (!vertex || !vertex.atom) {
        return;
    }
    const removedCount = atomSpaceCache.removeAtom(vertex.atom);
    const edgesToRemove = edges.get({
        filter: function(edge) {
            return edge.from === vertexId || edge.to === vertexId;
        }
    });
    const edgeIds = edgesToRemove.map(edge => edge.id);
    edges.remove(edgeIds);
    vertices.remove(vertexId);
    const atomKey = atomToKey(vertex.atom);
    atomVertexMap.delete(atomKey);
    updateStatus(`Removed 1 vertex from display and ${removedCount} atom(s) from cache`, 'connected');
}
function addEdgeIfNotExists(from, to) {
    const existingEdges = edges.get({
        filter: function(edge) {
            return edge.from === from && edge.to === to;
        }
    });
    if (existingEdges.length === 0) {
        edges.add({
            from: from,
            to: to,
            arrows: {
                to: {
                    enabled: true
                }
            }
        });
    }
}
function createCompactLabel(atom) {
    const typeBase = atom.type.replace(/Node$/, '').replace(/Link$/, '');
    if (!atom.outgoing || atom.outgoing.length === 0) {
        if (atom.name !== undefined) {
            const name = String(atom.name);
            return name.length > 14 ? name.substring(0, 14) : name;
        }
        return typeBase.length > 14 ? typeBase.substring(0, 14) : typeBase;
    } else {
        return typeBase.length > 4 ? typeBase.substring(0, 4) : typeBase;
    }
}
function atomToKey(atom) {
    if (atom.name !== undefined) {
        return `${atom.type}:${atom.name}`;
    } else if (atom.outgoing) {
        return `${atom.type}:[${atom.outgoing.map(o =>
            typeof o === 'object' ? atomToKey(o) : o
        ).join(',')}]`;
    } else {
        return `${atom.type}:${JSON.stringify(atom)}`;
    }
}
function atomToSExpression(atom, indent = 0) {
    const typeBase = atom.type.replace(/Node$/, '').replace(/Link$/, '');
    if (!atom.outgoing || atom.outgoing.length === 0) {
        if (atom.name !== undefined) {
            const quotedName = JSON.stringify(atom.name);
            return `(${typeBase} ${quotedName})`;
        }
        return `(${typeBase})`;
    } else {
        const nextIndent = indent + 1;
        const nextIndentStr = '\u00A0\u00A0\u00A0\u00A0'.repeat(nextIndent);
        const outgoingStrs = atom.outgoing.map(item => {
            if (typeof item === 'object' && item !== null) {
                return nextIndentStr + atomToSExpression(item, nextIndent);
            } else if (typeof item === 'string') {
                return nextIndentStr + `(Atom "${item}")`;
            } else {
                return nextIndentStr + String(item);
            }
        });
        return `(${typeBase}\n${outgoingStrs.join('\n')})`;
    }
}
function getVertexColor(type) {
    const colors = {
        'ConceptNode': '#4CAF50',
        'PredicateNode': '#2196F3',
        'NumberNode': '#FF9800',
        'TypeNode': '#9C27B0',
        'ListLink': '#00BCD4',
        'EvaluationLink': '#FFC107',
        'InheritanceLink': '#795548',
        'MemberLink': '#607D8B',
        'DefineLink': '#E91E63',
        'ImplicationLink': '#3F51B5'
    };
    return {
        background: colors[type] || '#9E9E9E',
        border: '#000000',
        highlight: {
            background: '#FFD700',
            border: '#000000'
        }
    };
}
function setupEventHandlers() {
    atomSpaceCache.addEventListener('cache-status', function(event) {
        const { size, maxSize, nearFull, skippedAtoms } = event.detail;
        const cacheCountElement = document.getElementById('cacheCount');
        if (cacheCountElement) {
            cacheCountElement.textContent = size + '/' + maxSize;
            if (nearFull) {
                cacheCountElement.style.color = '#F44336';  
            } else {
                cacheCountElement.style.color = '#2196F3';  
            }
        }
        const warningElement = document.getElementById('cacheWarning');
        const networkElement = document.getElementById('mynetwork');
        if (warningElement && networkElement) {
            if (nearFull && skippedAtoms) {
                warningElement.classList.add('visible');
                networkElement.classList.add('with-warning');
            } else {
                warningElement.classList.remove('visible');
                networkElement.classList.remove('with-warning');
            }
        }
    });
    atomSpaceCache.addEventListener('connection', function(event) {
        const status = event.detail.status;
        const message = event.detail.message;
        if (status === 'connected') {
            updateStatus(message, 'connected');
        } else if (status === 'disconnected' || status === 'error') {
            updateStatus(message, 'error');
        }
    });
    atomSpaceCache.addEventListener('update', function(event) {
        const updateType = event.detail.type;
        if (updateType === 'operations-cancelled') {
            endOperation();
            return;
        }
        if (updateType === 'atoms-removed') {
            return;
        }
        if (operationCancelled) {
            endOperation();
            return;
        }
        if (updateType === 'incoming-set') {
            const parent = event.detail.parent;
            const atoms = event.detail.atoms;
            const layoutSelect = document.getElementById('layoutSelect');
            const layoutType = layoutSelect ? layoutSelect.value : 'hierarchical';
            if (layoutType === 'graph') {
                if (typeof handleGraphViewCacheUpdate === 'function') {
                    handleGraphViewCacheUpdate(parent, atoms);
                }
            } else {
                if (!operationCancelled) {
                    rebuildFromAtomCache();
                    network.fit();
                    updateStatus(`Added ${atoms.length} incoming links`, 'connected');
                }
            }
        } else if (updateType === 'listlinks-complete' || updateType === 'operations-complete') {
            endOperation();
            updateStatus('Ready', 'connected');
        }
    });
    atomSpaceCache.addEventListener('error', function(event) {
        updateStatus(event.detail.message, 'error');
    });
    document.getElementById('resetBtn').addEventListener('click', function() {
        network.fit();
    });
    document.getElementById('layoutSelect').addEventListener('change', function(e) {
        const layoutType = e.target.value;
        const previousLayout = this.getAttribute('data-previous-layout');
        if (network) {
            network.destroy();
            network = null;
        }
        nodes = new vis.DataSet();
        edges = new vis.DataSet();
        atomVertexMap.clear();
        vertexIdCounter = 1;
        const container = document.getElementById('mynetwork');
        let options = {};
        if (layoutType === 'graph') {
            initializeGraphViewWithAtomCache();
            options = getGraphViewOptions();
        } else {
            rebuildFromAtomCache();
            if (layoutType === 'hierarchical') {
                options = {
                    nodes: {
                        shape: 'box',
                        font: {
                            size: 14,
                            face: 'monospace'
                        },
                        margin: 5,
                        widthConstraint: {
                            maximum: 150,
                            minimum: 40
                        }
                    },
                    edges: {
                        smooth: {
                            enabled: false  
                        },
                        arrows: {
                            to: {
                                enabled: true,
                                scaleFactor: 0.5
                            }
                        }
                    },
                    physics: {
                        enabled: true,
                        solver: 'hierarchicalRepulsion',
                        hierarchicalRepulsion: {
                            nodeDistance: 150,
                            centralGravity: 0.0,
                            springLength: 100,
                            springConstant: 0.01,
                            damping: 0.09
                        },
                        stabilization: {
                            enabled: true,
                            iterations: 1000,
                            updateInterval: 100
                        }
                    },
                    layout: {
                        hierarchical: {
                            enabled: true,
                            direction: 'UD',  
                            sortMethod: 'hubsize',
                            levelSeparation: 150,
                            nodeSpacing: 100,
                            treeSpacing: 200,
                            blockShifting: true,
                            edgeMinimization: true,
                            parentCentralization: true
                        }
                    }
                };
            } else {
                options = {
                    nodes: {
                        shape: 'box',
                        font: {
                            size: 14,
                            face: 'monospace'
                        },
                        margin: 5,
                        widthConstraint: {
                            maximum: 150,
                            minimum: 40
                        }
                    },
                    edges: {
                        smooth: {
                            enabled: true,  
                            type: 'dynamic'
                        },
                        arrows: {
                            to: {
                                enabled: true,
                                scaleFactor: 0.5
                            }
                        }
                    },
                    physics: {
                        enabled: true,
                        solver: 'forceAtlas2Based',
                        forceAtlas2Based: {
                            gravitationalConstant: -50,
                            centralGravity: 0.01,
                            springLength: 100,
                            springConstant: 0.08
                        }
                    },
                    layout: {
                        hierarchical: {
                            enabled: false
                        }
                    }
                };
            }
        }
        const data = { nodes: vertices, edges: edges };  
        network = new vis.Network(container, data, options);
        network.on('click', function(params) {
            if (params.nodes.length > 0) {  
                const vertexId = params.nodes[0];
                const vertex = vertices.get(vertexId);
                if (vertex && vertex.atom) {
                    const currentAtomCount = atomSpaceCache.getStats().totalAtoms;
                    if (currentAtomCount > LARGE_ATOM_THRESHOLD) {
                        showWarningDialog(currentAtomCount, function() {
                            atomSpaceCache.fetchIncomingSet(vertex.atom);
                        });
                    } else {
                        startOperation();
                        atomSpaceCache.fetchIncomingSet(vertex.atom);
                    }
                }
            } else if (params.edges.length > 0) {
                const edgeId = params.edges[0];
                const edge = edges.get(edgeId);
                if (edge) {
                    const layoutSelect = document.getElementById('layoutSelect');
                    const layoutType = layoutSelect ? layoutSelect.value : 'hierarchical';
                    if (layoutType === 'graph') {
                        removeSingleVertex(edge.from);
                    } else {
                        removeVertexAndParents(edge.from);
                    }
                }
            }
        });
        this.setAttribute('data-previous-layout', layoutType);
        network.stabilize();
        network.fit();
    });
    document.getElementById('refreshBtn').addEventListener('click', function() {
        refreshGraph();
    });
    const cacheLimitInput = document.getElementById('cacheLimit');
    if (cacheLimitInput) {
        cacheLimitInput.addEventListener('change', function() {
            const newLimit = parseInt(this.value, 10);
            if (!isNaN(newLimit) && newLimit > 0) {
                atomSpaceCache.setMaxCacheSize(newLimit);
            }
        });
        atomSpaceCache.setMaxCacheSize(parseInt(cacheLimitInput.value, 10));
    }
    atomSpaceCache.checkCacheWarning();
}
function refreshGraph() {
    vertices.clear();
    edges.clear();
    atomVertexMap.clear();
    vertexIdCounter = 1;
    const layoutSelect = document.getElementById('layoutSelect');
    const layoutType = layoutSelect ? layoutSelect.value : 'hierarchical';
    if (layoutType === 'graph') {
        initializeGraphViewWithAtomCache();
    } else {
        rebuildFromAtomCache();
        network.fit();
    }
    updateStatus('Graph refreshed', 'connected');
}
function updateStatus(message, className) {
    const statusElement = document.getElementById('status');
    statusElement.textContent = message;
    statusElement.className = className || '';
}
function showWarningDialog(atomCount, callback) {
    const dialog = document.getElementById('warningDialog');
    const overlay = document.getElementById('overlay');
    const message = document.getElementById('warningMessage');
    message.textContent = `This operation will process approximately ${atomCount} atoms. This may take some time and could affect performance. Do you want to continue?`;
    dialog.style.display = 'block';
    overlay.style.display = 'block';
    pendingOperation = callback;
}
function cancelLargeOperation() {
    const dialog = document.getElementById('warningDialog');
    const overlay = document.getElementById('overlay');
    dialog.style.display = 'none';
    overlay.style.display = 'none';
    pendingOperation = null;
    updateStatus('Operation cancelled', 'connected');
}
function proceedWithLargeOperation() {
    const dialog = document.getElementById('warningDialog');
    const overlay = document.getElementById('overlay');
    dialog.style.display = 'none';
    overlay.style.display = 'none';
    if (pendingOperation) {
        startOperation();  
        pendingOperation();
        pendingOperation = null;
    }
}
function startOperation() {
    operationCancelled = false;
    operationStartTime = Date.now();
    atomSpaceCache.resetCancellation();
    stopButtonTimer = setTimeout(() => {
        if (!operationCancelled) {
            document.getElementById('stopButton').style.display = 'block';
        }
    }, STOP_BUTTON_DELAY);
}
function stopCurrentOperation() {
    operationCancelled = true;
    atomSpaceCache.cancelAllOperations();
    document.getElementById('stopButton').style.display = 'none';
    if (stopButtonTimer) {
        clearTimeout(stopButtonTimer);
        stopButtonTimer = null;
    }
    if (typeof pendingGraphUpdate !== 'undefined' && pendingGraphUpdate) {
        clearTimeout(pendingGraphUpdate);
        pendingGraphUpdate = null;
    }
    if (typeof batchUpdateTimer !== 'undefined' && batchUpdateTimer) {
        clearTimeout(batchUpdateTimer);
        batchUpdateTimer = null;
    }
    if (typeof isProcessingBatch !== 'undefined') {
        isProcessingBatch = false;
    }
    if (typeof pendingListLinkFetches !== 'undefined') {
        pendingListLinkFetches = 0;
    }
    updateStatus('Processing stopped', 'connected');
    if (network) {
        network.stabilize();
    }
}
function endOperation() {
    document.getElementById('stopButton').style.display = 'none';
    if (stopButtonTimer) {
        clearTimeout(stopButtonTimer);
        stopButtonTimer = null;
    }
    operationCancelled = false;
    operationStartTime = null;
}
function isMatchingAtom(atom1, atom2) {
    if (typeof atom1 === 'string' || typeof atom2 === 'string') {
        return false; 
    }
    if (atom1.type !== atom2.type) {
        return false;
    }
    if (atom1.name !== undefined && atom2.name !== undefined) {
        return atom1.name === atom2.name;
    }
    return atomToKey(atom1) === atomToKey(atom2);
}
function calculateMaxDepth(atom, depthCache = new Map()) {
    const atomKey = atomSpaceCache.atomToKey(atom);
    if (depthCache.has(atomKey)) {
        return depthCache.get(atomKey);
    }
    let maxDepth = 0;
    if (atom.outgoing && atom.outgoing.length > 0) {
        atom.outgoing.forEach(child => {
            if (typeof child === 'object' && child !== null) {
                const childDepth = calculateMaxDepth(child, depthCache);
                maxDepth = Math.max(maxDepth, childDepth + 1);
            }
        });
    }
    depthCache.set(atomKey, maxDepth);
    return maxDepth;
}
function rebuildFromAtomCache() {
    if (operationCancelled) {
        return;
    }
    vertices.clear();
    edges.clear();
    atomVertexMap.clear();
    vertexIdCounter = 1;
    const depthCache = new Map();
    const allAtoms = atomSpaceCache.getAllAtoms();
    let maxDepthInGraph = 0;
    allAtoms.forEach(atom => {
        const depth = calculateMaxDepth(atom, depthCache);
        maxDepthInGraph = Math.max(maxDepthInGraph, depth);
    });
    allAtoms.forEach(atom => {
        const atomKey = atomSpaceCache.atomToKey(atom);
        if (atomVertexMap.has(atomKey)) {
            return; 
        }
        const maxDepth = depthCache.get(atomKey);
        const level = maxDepthInGraph - maxDepth;
        const vertexId = vertexIdCounter++;
        const vertexLabel = createCompactLabel(atom);
        const vertexColor = getVertexColor(atom.type);
        vertices.add({
            id: vertexId,
            label: vertexLabel,
            color: vertexColor,
            atom: atom,
            level: level,
            title: atomToSExpression(atom)
        });
        atomVertexMap.set(atomKey, vertexId);
    });
    allAtoms.forEach(atom => {
        const atomKey = atomSpaceCache.atomToKey(atom);
        const parentVertexId = atomVertexMap.get(atomKey);
        if (parentVertexId && atom.outgoing && atom.outgoing.length > 0) {
            atom.outgoing.forEach(child => {
                if (typeof child === 'object' && child !== null) {
                    const childKey = atomSpaceCache.atomToKey(child);
                    const childVertexId = atomVertexMap.get(childKey);
                    if (childVertexId) {
                        edges.add({
                            from: parentVertexId,
                            to: childVertexId,
                            arrows: {
                                to: {
                                    enabled: true,
                                    scaleFactor: 0.5
                                }
                            }
                        });
                    }
                }
            });
        }
    });
}