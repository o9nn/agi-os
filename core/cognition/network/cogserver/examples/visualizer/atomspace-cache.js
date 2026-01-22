class AtomSpaceCache extends EventTarget {
    constructor() {
        super();
        this.atoms = new Map();
        this.parents = new Map();
        this.children = new Map();
        this.roots = new Set();
        this.referencedBy = new Map();
        this.socket = null;
        this.serverUrl = null;
        this.pendingListLinkRequests = [];
        this.isProcessingListLink = false;
        this.operationsCancelled = false;
        this.maxCacheSize = 250;  
        this.skippedAtoms = false;  
    }
    atomToKey(atom) {
        if (!atom) return null;
        if (atom.handle) {
            return `handle:${atom.handle}`;
        }
        if (atom.name !== undefined) {
            return `${atom.type}:${atom.name}`;
        }
        if (atom.outgoing) {
            const outgoingKeys = atom.outgoing.map(o => this.atomToKey(o)).join(',');
            return `${atom.type}:[${outgoingKeys}]`;
        }
        return `${atom.type}:${JSON.stringify(atom)}`;
    }
    setMaxCacheSize(size) {
        const oldSize = this.maxCacheSize;
        this.maxCacheSize = Math.max(10, size);  
        this.checkCacheWarning();
        if (size > oldSize && this.canAddAtom()) {
            if (!this.isProcessingListLink && this.pendingListLinkRequests.length > 0) {
                this.processNextListLink();
            }
        }
    }
    canAddAtom() {
        return this.atoms.size < this.maxCacheSize;
    }
    isCacheNearFull() {
        return this.atoms.size >= (this.maxCacheSize * 0.96);
    }
    checkCacheWarning() {
        const nearFull = this.isCacheNearFull();
        this.dispatchEvent(new CustomEvent('cache-status', {
            detail: {
                size: this.atoms.size,
                maxSize: this.maxCacheSize,
                nearFull: nearFull,
                skippedAtoms: this.skippedAtoms
            }
        }));
    }
    addAtom(atom, parentAtom = null) {
        if (!atom) return null;
        const atomKey = this.atomToKey(atom);
        const isNewAtom = !this.atoms.has(atomKey);
        if (isNewAtom) {
            if (!this.canAddAtom()) {
                this.skippedAtoms = true;
                return null;  
            }
            this.atoms.set(atomKey, atom);
            this.parents.set(atomKey, new Set());
            this.children.set(atomKey, new Set());
            this.referencedBy.set(atomKey, new Set());
            this.checkCacheWarning();  
        }
        if (parentAtom) {
            const parentKey = this.atomToKey(parentAtom);
            if (parentKey) {
                this.parents.get(atomKey).add(parentKey);
                if (!this.children.has(parentKey)) {
                    this.children.set(parentKey, new Set());
                }
                this.children.get(parentKey).add(atomKey);
                this.roots.delete(atomKey);
            }
        } else {
            if (this.parents.get(atomKey).size === 0) {
                this.roots.add(atomKey);
            }
        }
        if (atom.outgoing && atom.outgoing.length > 0) {
            atom.outgoing.forEach(child => {
                if (typeof child === 'object' && child !== null) {
                    this.addAtom(child, null);  
                    if (isNewAtom) {
                        const childKey = this.atomToKey(child);
                        if (!this.referencedBy.has(childKey)) {
                            this.referencedBy.set(childKey, new Set());
                        }
                        this.referencedBy.get(childKey).add(atomKey);
                    }
                }
            });
        }
        return atomKey;
    }
    removeAtom(atom, removeDescendants = false) {
        const atomKey = this.atomToKey(atom);
        if (!this.atoms.has(atomKey)) return 0;
        let removedCount = 0;
        const linksToRemove = new Set(this.referencedBy.get(atomKey) || []);
        linksToRemove.forEach(linkKey => {
            const linkAtom = this.atoms.get(linkKey);
            if (linkAtom) {
                removedCount += this.removeAtom(linkAtom, false);
            }
        });
        if (removeDescendants) {
            const children = Array.from(this.children.get(atomKey) || []);
            children.forEach(childKey => {
                const childAtom = this.atoms.get(childKey);
                if (childAtom) {
                    removedCount += this.removeAtom(childAtom, true);
                }
            });
        }
        if (!this.atoms.has(atomKey)) {
            return removedCount;
        }
        const atomToRemove = this.atoms.get(atomKey);
        if (atomToRemove && atomToRemove.outgoing && atomToRemove.outgoing.length > 0) {
            atomToRemove.outgoing.forEach(child => {
                if (typeof child === 'object' && child !== null) {
                    const childKey = this.atomToKey(child);
                    const referencedBySet = this.referencedBy.get(childKey);
                    if (referencedBySet) {
                        referencedBySet.delete(atomKey);
                        if (referencedBySet.size === 0) {
                            this.referencedBy.delete(childKey);
                        }
                    }
                }
            });
        }
        const parents = this.parents.get(atomKey);
        if (parents) {
            parents.forEach(parentKey => {
                const parentChildren = this.children.get(parentKey);
                if (parentChildren) {
                    parentChildren.delete(atomKey);
                }
            });
        }
        const children = this.children.get(atomKey);
        if (children) {
            children.forEach(childKey => {
                const childParents = this.parents.get(childKey);
                if (childParents) {
                    childParents.delete(atomKey);
                    if (childParents.size === 0 && this.atoms.has(childKey)) {
                        this.roots.add(childKey);
                    }
                }
            });
        }
        this.atoms.delete(atomKey);
        this.parents.delete(atomKey);
        this.children.delete(atomKey);
        this.roots.delete(atomKey);
        this.referencedBy.delete(atomKey);
        removedCount++;
        return removedCount;
    }
    getAllAtoms() {
        return Array.from(this.atoms.values());
    }
    getRootAtoms() {
        return Array.from(this.roots).map(key => this.atoms.get(key)).filter(a => a);
    }
    getAtom(atomKey) {
        return this.atoms.get(atomKey);
    }
    hasAtom(atom) {
        const atomKey = this.atomToKey(atom);
        return this.atoms.has(atomKey);
    }
    getParents(atom) {
        const atomKey = this.atomToKey(atom);
        const parentKeys = this.parents.get(atomKey);
        if (!parentKeys) return [];
        return Array.from(parentKeys).map(key => this.atoms.get(key)).filter(a => a);
    }
    getChildren(atom) {
        const atomKey = this.atomToKey(atom);
        const childKeys = this.children.get(atomKey);
        if (!childKeys) return [];
        return Array.from(childKeys).map(key => this.atoms.get(key)).filter(a => a);
    }
    removeAtomAndParents(atom) {
        if (!atom) return 0;
        const atomsToRemove = new Set();
        const atomKey = this.atomToKey(atom);
        const collectAtomsToRemove = (currentKey) => {
            if (!currentKey || atomsToRemove.has(currentKey)) {
                return;
            }
            atomsToRemove.add(currentKey);
            const parentKeys = this.parents.get(currentKey);
            if (parentKeys) {
                parentKeys.forEach(parentKey => {
                    collectAtomsToRemove(parentKey);
                });
            }
        };
        collectAtomsToRemove(atomKey);
        let previousSize = 0;
        while (previousSize !== atomsToRemove.size) {
            previousSize = atomsToRemove.size;
            const newDependentLinks = new Set();
            atomsToRemove.forEach(keyToRemove => {
                const referencingLinks = this.referencedBy.get(keyToRemove);
                if (referencingLinks) {
                    referencingLinks.forEach(linkKey => {
                        if (!atomsToRemove.has(linkKey)) {
                            newDependentLinks.add(linkKey);
                        }
                    });
                }
            });
            newDependentLinks.forEach(linkKey => {
                atomsToRemove.add(linkKey);
            });
        }
        atomsToRemove.forEach(key => {
            const atomToRemove = this.atoms.get(key);
            if (atomToRemove && atomToRemove.outgoing && atomToRemove.outgoing.length > 0) {
                atomToRemove.outgoing.forEach(child => {
                    if (typeof child === 'object' && child !== null) {
                        const childKey = this.atomToKey(child);
                        const referencedBySet = this.referencedBy.get(childKey);
                        if (referencedBySet) {
                            referencedBySet.delete(key);
                            if (referencedBySet.size === 0) {
                                this.referencedBy.delete(childKey);
                            }
                        }
                    }
                });
            }
            this.atoms.delete(key);
            const parents = this.parents.get(key);
            if (parents) {
                parents.forEach(parentKey => {
                    const childrenSet = this.children.get(parentKey);
                    if (childrenSet) {
                        childrenSet.delete(key);
                    }
                });
            }
            this.parents.delete(key);
            const children = this.children.get(key);
            if (children) {
                children.forEach(childKey => {
                    const parentsSet = this.parents.get(childKey);
                    if (parentsSet) {
                        parentsSet.delete(key);
                        if (parentsSet.size === 0 && this.atoms.has(childKey)) {
                            this.roots.add(childKey);
                        }
                    }
                });
            }
            this.children.delete(key);
            this.roots.delete(key);
            this.referencedBy.delete(key);
        });
        if (this.skippedAtoms && !this.isCacheNearFull()) {
            this.skippedAtoms = false;
        }
        this.dispatchEvent(new CustomEvent('update', {
            detail: {
                type: 'atoms-removed',
                count: atomsToRemove.size
            }
        }));
        this.checkCacheWarning();  
        return atomsToRemove.size;
    }
    clear() {
        this.atoms.clear();
        this.parents.clear();
        this.children.clear();
        this.roots.clear();
        this.referencedBy.clear();
        this.skippedAtoms = false;
        this.checkCacheWarning();
    }
    getAtomsForGraphView() {
        const nodeMap = new Map();
        const edges = [];
        this.atoms.forEach(atom => {
            if ((atom.type === 'EdgeLink' || atom.type === 'EvaluationLink') &&
                atom.outgoing && atom.outgoing.length === 2) {
                const predicate = atom.outgoing[0];
                const list = atom.outgoing[1];
                const listKey = this.atomToKey(list);
                if (predicate && (predicate.type === 'PredicateNode' || predicate.type === 'BondNode') &&
                    list && list.type === 'ListLink' &&
                    list.outgoing && list.outgoing.length === 2 &&
                    this.atoms.has(listKey)) {  
                    const fromNode = list.outgoing[0];
                    const toNode = list.outgoing[1];
                    const fromKey = this.atomToKey(fromNode);
                    const toKey = this.atomToKey(toNode);
                    if (fromNode && fromNode.type && fromNode.type.endsWith('Node') &&
                        toNode && toNode.type && toNode.type.endsWith('Node') &&
                        this.atoms.has(fromKey) && this.atoms.has(toKey)) {
                        edges.push({
                            from: fromNode,
                            to: toNode,
                            label: predicate.name || 'edge',
                            type: atom.type
                        });
                        nodeMap.set(fromKey, fromNode);
                        nodeMap.set(toKey, toNode);
                    }
                }
            } else if (atom.type && atom.type.endsWith('Node') &&
                       atom.type !== 'BondNode' && atom.type !== 'PredicateNode') {
                const atomKey = this.atomToKey(atom);
                nodeMap.set(atomKey, atom);
            }
        });
        const nodes = Array.from(nodeMap.values());
        return { nodes, edges };
    }
    getStats() {
        return {
            totalAtoms: this.atoms.size,
            rootAtoms: this.roots.size,
            atomTypes: this.getAtomTypeDistribution()
        };
    }
    getAtomTypeDistribution() {
        const distribution = {};
        this.atoms.forEach(atom => {
            distribution[atom.type] = (distribution[atom.type] || 0) + 1;
        });
        return distribution;
    }
    connect(serverUrl) {
        if (this.socket && this.socket.readyState === WebSocket.OPEN) {
            this.socket.close();
        }
        this.serverUrl = serverUrl;
        this.socket = new WebSocket(serverUrl);
        this.socket.onopen = () => {
            console.log('Connected to CogServer');
            this.dispatchEvent(new CustomEvent('connection', {
                detail: { status: 'connected', message: 'Connected to server' }
            }));
        };
        this.socket.onclose = () => {
            console.log('Disconnected from CogServer');
            this.dispatchEvent(new CustomEvent('connection', {
                detail: { status: 'disconnected', message: 'Disconnected from server' }
            }));
        };
        this.socket.onerror = (error) => {
            console.error('WebSocket error:', error);
            this.dispatchEvent(new CustomEvent('connection', {
                detail: { status: 'error', message: 'Connection error' }
            }));
        };
        this.socket.onmessage = (event) => {
            this.handleWebSocketMessage(event);
        };
    }
    handleWebSocketMessage(event) {
        try {
            const rawResponse = JSON.parse(event.data);
            let response;
            if (rawResponse.hasOwnProperty('success')) {
                if (rawResponse.success && rawResponse.result) {
                    response = rawResponse.result;
                }
            } else {
                response = rawResponse;
            }
            if (this.isProcessingListLink && response && Array.isArray(response)) {
                this.processListLinkResponse(response);
            } else if (this.pendingRegularRequest && response && Array.isArray(response)) {
                this.processRegularAtomResponse(response);
            }
        } catch (error) {
            console.error('Error parsing WebSocket message:', error);
        }
    }
    fetchIncomingSet(atom) {
        if (this.operationsCancelled) {
            return;
        }
        if (!this.socket || this.socket.readyState !== WebSocket.OPEN) {
            this.dispatchEvent(new CustomEvent('error', {
                detail: { message: 'Not connected to server' }
            }));
            return;
        }
        let atomSpec;
        if (atom.name !== undefined) {
            const escapedName = JSON.stringify(atom.name);
            atomSpec = `{"type": "${atom.type}", "name": ${escapedName}}`;
        } else {
            atomSpec = JSON.stringify(atom);
        }
        const command = `AtomSpace.getIncoming(${atomSpec})`;
        console.log('Getting incoming set:', command);
        if (atom.type === 'ListLink') {
            this.pendingListLinkRequests.push({
                atom: atom,
                command: command
            });
            if (!this.isProcessingListLink) {
                this.processNextListLink();
            }
        } else {
            this.pendingRegularRequest = {
                atom: atom,
                command: command
            };
            this.socket.send(command);
        }
    }
    processNextListLink() {
        if (this.operationsCancelled) {
            this.pendingListLinkRequests = [];
            this.isProcessingListLink = false;
            return;
        }
        if (this.pendingListLinkRequests.length === 0) {
            this.isProcessingListLink = false;
            this.dispatchEvent(new CustomEvent('update', {
                detail: { type: 'listlinks-complete' }
            }));
            return;
        }
        this.isProcessingListLink = true;
        const request = this.pendingListLinkRequests.shift();
        this.currentListLinkRequest = request;
        this.socket.send(request.command);
    }
    processListLinkResponse(response) {
        if (!this.currentListLinkRequest) return;
        if (this.operationsCancelled) {
            this.currentListLinkRequest = null;
            this.pendingListLinkRequests = [];
            this.isProcessingListLink = false;
            return;
        }
        const targetAtom = this.currentListLinkRequest.atom;
        let addedCount = 0;
        let skippedCount = 0;
        response.forEach(atom => {
            if (atom && typeof atom === 'object') {
                const result = this.addAtom(atom, null);
                if (result !== null) {
                    addedCount++;
                    const atomKey = this.atomToKey(atom);
                    const targetKey = this.atomToKey(targetAtom);
                    if (!this.parents.has(targetKey)) {
                        this.parents.set(targetKey, new Set());
                    }
                    this.parents.get(targetKey).add(atomKey);
                    if (!this.children.has(atomKey)) {
                        this.children.set(atomKey, new Set());
                    }
                    this.children.get(atomKey).add(targetKey);
                } else {
                    skippedCount++;
                }
            }
        });
        if (!this.operationsCancelled && addedCount > 0) {
            this.dispatchEvent(new CustomEvent('update', {
                detail: {
                    type: 'incoming-set',
                    parent: targetAtom,
                    atoms: response,
                    addedCount: addedCount,
                    skippedCount: skippedCount
                }
            }));
        }
        this.currentListLinkRequest = null;
        setTimeout(() => this.processNextListLink(), 10);
    }
    processRegularAtomResponse(response) {
        if (!this.pendingRegularRequest) return;
        const targetAtom = this.pendingRegularRequest.atom;
        let addedCount = 0;
        let skippedCount = 0;
        response.forEach(atom => {
            if (atom && typeof atom === 'object') {
                const result = this.addAtom(atom, null);
                if (result !== null) {
                    addedCount++;
                    const atomKey = this.atomToKey(atom);
                    const targetKey = this.atomToKey(targetAtom);
                    if (!this.parents.has(targetKey)) {
                        this.parents.set(targetKey, new Set());
                    }
                    this.parents.get(targetKey).add(atomKey);
                    if (!this.children.has(atomKey)) {
                        this.children.set(atomKey, new Set());
                    }
                    this.children.get(atomKey).add(targetKey);
                } else {
                    skippedCount++;
                }
            }
        });
        this.pendingRegularRequest = null;
        if (addedCount > 0) {
            this.dispatchEvent(new CustomEvent('update', {
                detail: {
                    type: 'incoming-set',
                    parent: targetAtom,
                    atoms: response,
                    addedCount: addedCount,
                    skippedCount: skippedCount
                }
            }));
        }
        if (!this.hasPendingOperations()) {
            this.dispatchEvent(new CustomEvent('update', {
                detail: { type: 'operations-complete' }
            }));
        }
    }
    cancelAllOperations() {
        this.operationsCancelled = true;
        this.pendingListLinkRequests = [];
        this.isProcessingListLink = false;
        this.currentListLinkRequest = null;
        this.pendingRegularRequest = null;
        this.dispatchEvent(new CustomEvent('update', {
            detail: { type: 'operations-cancelled' }
        }));
    }
    resetCancellation() {
        this.operationsCancelled = false;
    }
    hasPendingOperations() {
        return this.isProcessingListLink || this.pendingListLinkRequests.length > 0 || this.pendingRegularRequest !== null;
    }
    disconnect() {
        if (this.socket) {
            this.socket.close();
            this.socket = null;
        }
    }
}
const atomSpaceCache = new AtomSpaceCache();