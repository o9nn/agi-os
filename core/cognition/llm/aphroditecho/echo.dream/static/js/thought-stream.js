const thoughtStream = document.getElementById('thought-stream');
const autoScrollToggle = document.getElementById('autoScrollToggle');
const stepButton = document.getElementById('step-simulation');
const resetButton = document.getElementById('reset-simulation');
const filterButtons = document.querySelectorAll('[data-filter]');
let engineSelectorThought = document.getElementById('engine-selector');
let currentFilter = 'all';
let thoughts = [];
let isPolling = true;
let pollingInterval = null;
let selectedThought = null;
let thoughtConnections = {};
let animationSpeed = 'normal'; 
let colorMode = 'default'; 
function formatTime(isoString) {
    const date = new Date(isoString);
    return date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', second: '2-digit' });
}
function createThoughtElement(thought) {
    const thoughtId = thought.timestamp;
    const thoughtElement = document.createElement('div');
    thoughtElement.className = `thought-item ${thought.type}`;
    thoughtElement.dataset.type = thought.type;
    thoughtElement.dataset.id = thoughtId;
    thoughtElement.dataset.state = thought.state;
    const typeIndicator = thought.type.charAt(0).toUpperCase() + thought.type.slice(1);
    const header = document.createElement('div');
    header.className = 'thought-header';
    const recursionLevel = thought.recursion_level !== undefined ? thought.recursion_level : 
                          (thought.recursion_depth !== undefined ? thought.recursion_depth : 0);
    const distinctionLevel = thought.distinction_level !== undefined ? thought.distinction_level : 0;
    const recursionVisual = document.createElement('div');
    recursionVisual.className = 'recursion-visual';
    for (let i = 0; i < 5; i++) {
        const dot = document.createElement('span');
        dot.className = 'recursion-level';
        if (i < recursionLevel) {
            dot.classList.add('active');
        }
        recursionVisual.appendChild(dot);
    }
    const stateLabel = document.createElement('span');
    stateLabel.className = 'state-label';
    stateLabel.textContent = `${thought.state.split('_')[0]}`;
    const headerText = document.createElement('span');
    headerText.textContent = `[${typeIndicator}] `;
    header.appendChild(headerText);
    header.appendChild(stateLabel);
    if (distinctionLevel > 0) {
        const distinctionInfo = document.createElement('span');
        distinctionInfo.className = 'distinction-info';
        distinctionInfo.textContent = ` D${distinctionLevel}`;
        header.appendChild(distinctionInfo);
    }
    header.appendChild(recursionVisual);
    const time = document.createElement('span');
    time.className = 'thought-time';
    time.textContent = formatTime(thought.timestamp);
    header.appendChild(time);
    const contentContainer = document.createElement('div');
    contentContainer.className = 'thought-content-container';
    const content = document.createElement('div');
    content.className = 'thought-content';
    content.textContent = thought.content;
    const fade = document.createElement('div');
    fade.className = 'thought-fade';
    contentContainer.appendChild(content);
    contentContainer.appendChild(fade);
    const expand = document.createElement('div');
    expand.className = 'thought-expand';
    expand.textContent = 'Show more';
    expand.addEventListener('click', (e) => {
        e.stopPropagation();
        thoughtElement.classList.toggle('expanded');
        expand.textContent = thoughtElement.classList.contains('expanded') ? 'Show less' : 'Show more';
    });
    const footer = document.createElement('div');
    footer.className = 'thought-footer';
    const actions = document.createElement('div');
    actions.className = 'thought-actions';
    const connectAction = document.createElement('span');
    connectAction.className = 'thought-action';
    connectAction.textContent = '🔗 Connect';
    connectAction.addEventListener('click', (e) => {
        e.stopPropagation();
        highlightRelatedThoughts(thought);
    });
    const focusAction = document.createElement('span');
    focusAction.className = 'thought-action';
    focusAction.textContent = '👁️ Focus';
    focusAction.addEventListener('click', (e) => {
        e.stopPropagation();
        focusOnThoughtThread(thought);
    });
    const patternAction = document.createElement('span');
    patternAction.className = 'thought-action';
    patternAction.textContent = '🧩 Pattern';
    patternAction.addEventListener('click', (e) => {
        e.stopPropagation();
        visualizePattern(thought);
    });
    actions.appendChild(connectAction);
    actions.appendChild(focusAction);
    actions.appendChild(patternAction);
    footer.appendChild(actions);
    const meta = document.createElement('div');
    meta.className = 'thought-meta';
    meta.textContent = `R:${recursionLevel} `;
    if (distinctionLevel > 0) {
        const distinctionBadge = document.createElement('span');
        distinctionBadge.className = 'distinction-badge';
        distinctionBadge.textContent = `D:${distinctionLevel}`;
        meta.appendChild(distinctionBadge);
    }
    footer.appendChild(meta);
    thoughtElement.addEventListener('click', () => {
        selectThought(thought, thoughtElement);
    });
    thoughtElement.appendChild(header);
    thoughtElement.appendChild(contentContainer);
    thoughtElement.appendChild(expand);
    thoughtElement.appendChild(footer);
    const animationDelay = Math.min(thoughts.length, 5) * 0.05;
    thoughtElement.style.animationDelay = `${animationDelay}s`;
    return thoughtElement;
}
function addThought(thought) {
    const element = createThoughtElement(thought);
    if (currentFilter !== 'all' && thought.type !== currentFilter) {
        element.classList.add('hidden');
    }
    thoughtStream.appendChild(element);
    if (autoScrollToggle.checked) {
        thoughtStream.scrollTop = thoughtStream.scrollHeight;
    }
}
async function fetchThoughts() {
    try {
        const response = await fetch('/api/simulation/thoughts');
        const data = await response.json();
        if (data.status === 'success') {
            const newThoughts = data.thoughts.filter(thought => 
                !thoughts.some(t => t.timestamp === thought.timestamp && t.content === thought.content)
            );
            thoughts = data.thoughts;
            newThoughts.forEach(thought => addThought(thought));
        } else {
            console.error("Error fetching thoughts:", data.message);
        }
    } catch (error) {
        console.error("Failed to fetch thoughts:", error);
    }
}
function applyFilter(filter) {
    currentFilter = filter;
    filterButtons.forEach(button => {
        if (button.dataset.filter === filter) {
            button.classList.add('active');
        } else {
            button.classList.remove('active');
        }
    });
    const thoughtElements = thoughtStream.querySelectorAll('.thought-item');
    thoughtElements.forEach(element => {
        if (filter === 'all' || element.dataset.type === filter) {
            element.classList.remove('hidden');
        } else {
            element.classList.add('hidden');
        }
    });
}
async function stepSimulation() {
    try {
        const response = await fetch('/api/simulation/step', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            }
        });
        const data = await response.json();
        if (data.status === 'success') {
            console.log("Simulation stepped:", data.result);
            if (typeof updateVisualization === 'function' && data.state) {
                updateVisualization(data.state);
            }
            setTimeout(fetchThoughts, 300);
        } else {
            console.error("Error stepping simulation:", data.error);
        }
    } catch (error) {
        console.error("Failed to step simulation:", error);
    }
}
async function resetSimulation() {
    if (!confirm("Are you sure you want to reset the simulation? This will clear all thoughts and return to the initial state.")) {
        return;
    }
    try {
        const response = await fetch('/api/simulation/reset', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            }
        });
        const data = await response.json();
        if (data.status === 'success') {
            console.log("Simulation reset:", data.result);
            thoughtStream.innerHTML = '';
            thoughts = [];
            if (typeof updateVisualization === 'function' && data.state) {
                updateVisualization(data.state);
            }
            setTimeout(fetchThoughts, 300);
        } else {
            console.error("Error resetting simulation:", data.error);
        }
    } catch (error) {
        console.error("Failed to reset simulation:", error);
    }
}
function startPolling() {
    if (isPolling) {
        fetchThoughts();
        pollingInterval = setInterval(fetchThoughts, 5000); 
    }
}
function stopPolling() {
    isPolling = false;
    if (pollingInterval) {
        clearInterval(pollingInterval);
        pollingInterval = null;
    }
}
async function selectEngine(engineName) {
    try {
        const response = await fetch('/api/engine/select', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ engine: engineName })
        });
        const data = await response.json();
        if (data.status === 'success') {
            console.log(`Switched to engine: ${data.current}`);
            thoughtStream.innerHTML = '';
            thoughts = [];
            setTimeout(fetchThoughts, 300);
        } else {
            console.error("Error switching engine:", data.error);
        }
    } catch (error) {
        console.error("Failed to switch engine:", error);
    }
}
filterButtons.forEach(button => {
    button.addEventListener('click', () => {
        applyFilter(button.dataset.filter);
    });
});
stepButton.addEventListener('click', stepSimulation);
resetButton.addEventListener('click', resetSimulation);
function selectThought(thought, element) {
    const previousSelected = document.querySelector('.thought-item.selected');
    if (previousSelected) {
        previousSelected.classList.remove('selected');
    }
    selectedThought = thought;
    element.classList.add('selected');
    updateThoughtVisualizations(thought);
}
function highlightRelatedThoughts(thought) {
    document.querySelectorAll('.thought-item.related').forEach(el => {
        el.classList.remove('related');
    });
    const thoughtElements = document.querySelectorAll('.thought-item');
    thoughtElements.forEach(el => {
        const elThought = thoughts.find(t => t.timestamp === el.dataset.id);
        if (!elThought) return;
        const sameRecursionLevel = 
            (elThought.recursion_level !== undefined && 
             thought.recursion_level !== undefined &&
             elThought.recursion_level === thought.recursion_level) ||
            (elThought.recursion_depth !== undefined && 
             thought.recursion_depth !== undefined &&
             elThought.recursion_depth === thought.recursion_depth);
        const sameState = elThought.state === thought.state;
        const sequentialState = isSequentialState(thought, elThought);
        const patternMatch = hasPatternMatch(thought, elThought);
        if ((sameRecursionLevel && !sameState) || 
            sequentialState || 
            patternMatch) {
            el.classList.add('related');
        }
    });
    drawConnectionLines();
}
function isSequentialState(thought1, thought2) {
    const dteStates = [
        'Initial_State', 'Pattern_Recognition', 'Recursive_Expansion',
        'Novel_Insights', 'Self_Reflection', 'Dream_State',
        'Memory_Integration', 'Pattern_Matching', 'Knowledge_Synthesis',
        'Creative_Output'
    ];
    const rdeStates = [
        'Unmarked_State', 'First_Distinction', 'Boundary_Crossing',
        'Form_Calculation', 'Re_Entry', 'Self_Reference',
        'Distinction_Collapse', 'Emergent_Pattern', 'Calculus_Integration'
    ];
    const states = thought1.recursion_depth !== undefined ? rdeStates : dteStates;
    const index1 = states.indexOf(thought1.state);
    const index2 = states.indexOf(thought2.state);
    return Math.abs(index1 - index2) === 1;
}
function hasPatternMatch(thought1, thought2) {
    if (thought1.content && thought2.content) {
        const words1 = thought1.content.split(' ');
        const words2 = thought2.content.split(' ');
        for (let i = 0; i < words1.length - 2; i++) {
            const phrase = words1.slice(i, i + 3).join(' ').toLowerCase();
            if (thought2.content.toLowerCase().includes(phrase)) {
                return true;
            }
        }
    }
    return false;
}
function drawConnectionLines() {
    document.querySelectorAll('.thought-connection').forEach(el => el.remove());
    const related = document.querySelectorAll('.thought-item.related');
    const selected = document.querySelector('.thought-item.selected');
    if (!selected || related.length === 0) return;
    const selectedRect = selected.getBoundingClientRect();
    const streamRect = thoughtStream.getBoundingClientRect();
    related.forEach(el => {
        const relatedRect = el.getBoundingClientRect();
        const line = document.createElement('div');
        line.className = 'thought-connection';
        const top = Math.min(selectedRect.top, relatedRect.top) - streamRect.top + 10;
        const height = Math.abs(selectedRect.top - relatedRect.top);
        line.style.top = `${top}px`;
        line.style.height = `${height}px`;
        thoughtStream.appendChild(line);
    });
}
function focusOnThoughtThread(thought) {
    const threadThoughts = getThreadThoughts(thought);
    document.querySelectorAll('.thought-item').forEach(el => {
        const elId = el.dataset.id;
        if (!threadThoughts.some(t => t.timestamp === elId)) {
            el.classList.add('hidden');
        } else {
            el.classList.remove('hidden');
        }
    });
    addShowAllButton();
}
function getThreadThoughts(thought) {
    return thoughts.filter(t => 
        t.type === thought.type || 
        isSequentialState(thought, t) ||
        hasPatternMatch(thought, t)
    );
}
function addShowAllButton() {
    const existingButton = document.getElementById('show-all-button');
    if (existingButton) existingButton.remove();
    const button = document.createElement('button');
    button.id = 'show-all-button';
    button.className = 'btn';
    button.textContent = 'Show All Thoughts';
    button.addEventListener('click', () => {
        document.querySelectorAll('.thought-item').forEach(el => {
            if (currentFilter === 'all' || el.dataset.type === currentFilter) {
                el.classList.remove('hidden');
            }
        });
        button.remove();
    });
    thoughtStream.insertAdjacentElement('afterbegin', button);
}
function visualizePattern(thought) {
    const popup = document.createElement('div');
    popup.className = 'pattern-popup';
    popup.innerHTML = `
        <div class="pattern-popup-header">
            <h3>Pattern Visualization</h3>
            <button class="pattern-popup-close">×</button>
        </div>
        <div class="pattern-popup-content">
            <div class="pattern-info">
                <div>Type: ${thought.type}</div>
                <div>State: ${thought.state}</div>
                <div>Recursion: ${thought.recursion_level || thought.recursion_depth || 0}</div>
                ${thought.distinction_level ? `<div>Distinction: ${thought.distinction_level}</div>` : ''}
            </div>
            <div class="pattern-structure" id="pattern-structure">
                <!-- Pattern structure visualization -->
            </div>
        </div>
    `;
    document.body.appendChild(popup);
    popup.querySelector('.pattern-popup-close').addEventListener('click', () => {
        popup.remove();
    });
    renderPatternStructure(thought, document.getElementById('pattern-structure'));
}
function renderPatternStructure(thought, container) {
    container.innerHTML = `<div class="pattern-text">${processPatternText(thought.content)}</div>`;
}
function processPatternText(text) {
    if (!text) return '';
    text = text.replace(/\(([^()]*)\)/g, '<span class="pattern-highlight">($1)</span>');
    text = text.replace(/\b(self|recursive|loop|pattern|cycle)\b/gi, 
                        '<span class="pattern-keyword">$1</span>');
    return text;
}
function updateThoughtVisualizations(thought) {
    console.log('Selected thought:', thought);
    if (typeof updateVisualizationForThought === 'function') {
        updateVisualizationForThought(thought);
    }
}
function syncEngines() {
    const currentEngine = engineSelectorThought ? engineSelectorThought.value : 'DTESimulation';
    fetch('/api/engine/sync', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({ 
            engine: currentEngine,
            action: 'sync'
        })
    })
    .then(response => response.json())
    .then(data => {
        console.log('Engine sync result:', data);
        setTimeout(fetchThoughts, 300);
    })
    .catch(error => {
        console.error('Engine sync failed:', error);
    });
}
function createSyncButton() {
    const controlPanel = document.querySelector('.controls') || document.getElementById('control-panel');
    if (controlPanel) {
        const syncButton = document.createElement('button');
        syncButton.id = 'sync-engines';
        syncButton.className = 'btn';
        syncButton.textContent = 'Sync Engines';
        syncButton.addEventListener('click', syncEngines);
        controlPanel.appendChild(syncButton);
    }
}
if (engineSelectorThought) {
    engineSelectorThought.addEventListener('change', () => {
        selectEngine(engineSelectorThought.value);
    });
}
document.addEventListener('DOMContentLoaded', () => {
    startPolling();
    createSyncButton();
    window.addEventListener('resize', () => {
        if (document.querySelector('.thought-item.related')) {
            drawConnectionLines();
        }
    });
});
window.addEventListener('beforeunload', () => {
    stopPolling();
});