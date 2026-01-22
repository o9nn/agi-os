function isGraphEdgePattern(atom) {
    if (!atom || !atom.outgoing || atom.outgoing.length !== 2) {
        return false;
    }
    if (atom.type !== 'EdgeLink' && atom.type !== 'EvaluationLink') {
        return false;
    }
    const predicate = atom.outgoing[0];
    if (typeof predicate !== 'object' || (predicate.type !== 'PredicateNode' && predicate.type !== 'BondNode')) {
        return false;
    }
    const list = atom.outgoing[1];
    if (typeof list !== 'object' || list.type !== 'ListLink') {
        return false;
    }
    if (!list.outgoing || list.outgoing.length !== 2) {
        return false;
    }
    const from = list.outgoing[0];
    const to = list.outgoing[1];
    if (typeof from !== 'object' || !from.type.endsWith('Node')) {
        return false;
    }
    if (typeof to !== 'object' || !to.type.endsWith('Node')) {
        return false;
    }
    return true;
}
function extractGraphEdgeInfo(atom) {
    const predicate = atom.outgoing[0];
    const list = atom.outgoing[1];
    const fromNode = list.outgoing[0];
    const toNode = list.outgoing[1];
    return {
        edgeLabel: predicate.name || 'edge',
        fromNode: fromNode,
        toNode: toNode,
        edgeType: atom.type 
    };
}
function addVertexToGraph(atom) {
    const atomKey = atomToKey(atom);
    if (atomVertexMap.has(atomKey)) {
        return atomVertexMap.get(atomKey);
    }
    const vertexId = vertexIdCounter++;
    const vertexLabel = createCompactLabel(atom);
    const vertexColor = getVertexColor(atom.type);
    vertices.add({
        id: vertexId,
        label: vertexLabel,
        color: vertexColor,
        atom: atom,
        title: atomToSExpression(atom),
        shape: 'ellipse' 
    });
    atomVertexMap.set(atomKey, vertexId);
    return vertexId;
}
function addLabeledEdge(fromId, toId, label, edgeType) {
    const edgeColor = edgeType === 'EdgeLink' ? '#FF6B6B' : '#4ECDC4';
    const existingEdges = edges.get({
        filter: function(item) {
            return (item.from === fromId && item.to === toId) ||
                   (item.from === toId && item.to === fromId);
        }
    });
    const curveOffset = existingEdges.length;
    edges.add({
        from: fromId,
        to: toId,
        label: label,
        arrows: {
            to: {
                enabled: true,
                scaleFactor: 0.7
            }
        },
        color: {
            color: edgeColor,
            highlight: '#FFD93D',
            hover: '#FFD93D'
        },
        font: {
            color: '#333333',
            size: 12,
            background: 'rgba(255, 255, 255, 0.8)',
            strokeWidth: 0,
            align: 'middle'
        },
        smooth: {
            enabled: true,
            type: curveOffset === 0 ? 'dynamic' : 'curvedCW',
            roundness: 0.2 + (curveOffset * 0.15) 
        },
        width: 2,
        dashes: edgeType === 'EvaluationLink' ? [5, 5] : false
    });
}
function initializeGraphViewWithAtomCache() {
    vertices.clear();
    edges.clear();
    atomVertexMap.clear();
    vertexIdCounter = 1;
    const graphData = atomSpaceCache.getAtomsForGraphView();
    graphData.nodes.forEach(atom => {
        const atomKey = atomSpaceCache.atomToKey(atom);
        if (!atomVertexMap.has(atomKey)) {
            const vertexId = vertexIdCounter++;
            const vertexLabel = createCompactLabel(atom);
            const vertexColor = getVertexColor(atom.type);
            vertices.add({
                id: vertexId,
                label: vertexLabel,
                color: vertexColor,
                atom: atom,
                title: atomToSExpression(atom)
            });
            atomVertexMap.set(atomKey, vertexId);
        }
    });
    graphData.edges.forEach(edge => {
        const fromKey = atomSpaceCache.atomToKey(edge.from);
        const toKey = atomSpaceCache.atomToKey(edge.to);
        const fromId = atomVertexMap.get(fromKey);
        const toId = atomVertexMap.get(toKey);
        if (fromId && toId) {
            edges.add({
                from: fromId,
                to: toId,
                label: edge.label,
                arrows: {
                    to: {
                        enabled: true,
                        scaleFactor: 0.5
                    }
                },
                font: {
                    size: 10,
                    align: 'middle'
                }
            });
        }
    });
}
let pendingGraphUpdate = null;
let pendingListLinkFetches = 0;
let lastUpdateTime = 0;
const MIN_UPDATE_INTERVAL = 100; 
const AGGRESSIVE_THROTTLE_INTERVAL = 500; 
let isProcessingBatch = false; 
let batchUpdateTimer = null; 
function handleGraphViewCacheUpdate(parent, atoms, eventDetail) {
    if (typeof operationCancelled !== 'undefined' && operationCancelled) {
        pendingListLinkFetches = 0;
        if (pendingGraphUpdate) {
            clearTimeout(pendingGraphUpdate);
            pendingGraphUpdate = null;
        }
        if (batchUpdateTimer) {
            clearTimeout(batchUpdateTimer);
            batchUpdateTimer = null;
        }
        isProcessingBatch = false;
        return;
    }
    if (batchUpdateTimer) {
        clearTimeout(batchUpdateTimer);
        batchUpdateTimer = null;
    }
    const cacheNearFull = atomSpaceCache.isCacheNearFull();
    const hasPendingOps = atomSpaceCache.hasPendingOperations();
    const shouldBatch = (eventDetail && eventDetail.skippedCount > 0 && cacheNearFull) ||
                        isProcessingBatch ||
                        hasPendingOps;
    if (shouldBatch) {
        isProcessingBatch = true;
        if (pendingGraphUpdate) {
            clearTimeout(pendingGraphUpdate);
            pendingGraphUpdate = null;
        }
        const batchInterval = 1000;
        batchUpdateTimer = setTimeout(() => {
            isProcessingBatch = false;
            batchUpdateTimer = null;
            if (typeof operationCancelled === 'undefined' || !operationCancelled) {
                initializeGraphViewWithAtomCache();
                lastUpdateTime = Date.now();
            }
            if (typeof endOperation === 'function' &&
                pendingListLinkFetches === 0 &&
                !atomSpaceCache.hasPendingOperations()) {
                endOperation();
            }
        }, batchInterval);
        return;
    }
    if (batchUpdateTimer) {
        clearTimeout(batchUpdateTimer);
        batchUpdateTimer = null;
    }
    const listLinksToFetch = [];
    atoms.forEach(atom => {
        if (atom.type === 'ListLink' && atom.outgoing && atom.outgoing.length === 2) {
            const firstIsNode = atom.outgoing[0] && typeof atom.outgoing[0] === 'object' &&
                              atom.outgoing[0].type && atom.outgoing[0].type.endsWith('Node');
            const secondIsNode = atom.outgoing[1] && typeof atom.outgoing[1] === 'object' &&
                               atom.outgoing[1].type && atom.outgoing[1].type.endsWith('Node');
            if (firstIsNode && secondIsNode) {
                listLinksToFetch.push(atom);
            }
        }
    });
    if (listLinksToFetch.length > 0) {
        pendingListLinkFetches += listLinksToFetch.length;
        listLinksToFetch.forEach(listLink => {
            atomSpaceCache.fetchIncomingSet(listLink);
        });
    }
    if (parent && parent.type === 'ListLink') {
        pendingListLinkFetches = Math.max(0, pendingListLinkFetches - 1);
    }
    if (pendingGraphUpdate) {
        clearTimeout(pendingGraphUpdate);
        pendingGraphUpdate = null;
    }
    if (pendingListLinkFetches > 0) {
        pendingGraphUpdate = setTimeout(() => {
            if (typeof operationCancelled === 'undefined' || !operationCancelled) {
                pendingListLinkFetches = 0;  
                initializeGraphViewWithAtomCache();
            }
            if (typeof endOperation === 'function') {
                endOperation();
            }
            pendingGraphUpdate = null;
        }, 1000);  
    } else {
        if (typeof operationCancelled === 'undefined' || !operationCancelled) {
            initializeGraphViewWithAtomCache();
        }
        if (typeof endOperation === 'function' &&
            pendingListLinkFetches === 0 &&
            !atomSpaceCache.hasPendingOperations()) {
            endOperation();
        }
    }
}
function getGraphViewOptions() {
    return {
        nodes: {  
            shape: 'ellipse',
            font: {
                size: 14,
                face: 'monospace'
            },
            margin: 8,
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
                enabled: true,
                type: 'dynamic'
            },
            font: {
                size: 12,
                align: 'middle'
            }
        },
        physics: {
            enabled: true,
            solver: 'forceAtlas2Based',
            forceAtlas2Based: {
                gravitationalConstant: -50,
                centralGravity: 0.01,
                springLength: 150,
                springConstant: 0.08,
                damping: 0.9,  
                avoidOverlap: 0.5
            },
            stabilization: {
                enabled: true,
                iterations: 1000,  
                updateInterval: 50,
                fit: true
            },
            maxVelocity: 50,  
            minVelocity: 0.75,  
            timestep: 0.5  
        },
        layout: {
            randomSeed: 2,
            improvedLayout: true
        },
        interaction: {
            hover: true,
            tooltipDelay: 200
        }
    };
}