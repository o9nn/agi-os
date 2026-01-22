import { Component, OnInit, AfterViewInit, OnDestroy, ViewEncapsulation, Input, OnChanges, SimpleChanges } from '@angular/core';
import { Graph } from '../models/graph';
import { AtomService, AtomServiceData } from '../atom.service';
import { VisualizerService } from './visualizer.service';
import { TranslateConfig } from './../translate/translate-config';
import { TranslateService } from './../translate/translate.service';
interface Menus {
mainMenu: any;
nodeMenu: any;
}
const version = 'VisualizerComponent 0.14.05 Beta (June-17-2018)';
const simForceStrengthNormal = -60, simForceStrengthFast = -100, simForceStrengthSlow = -20;
const simForceStrength = simForceStrengthNormal;
const simForceStrengthHighNodeCharge = -2000;
const reheatFactorMax = 6;
const isNodesConstrainedToClientArea = false;
const isPruneFilteredNodes = true;
const isXtraLevelNeighbors = true;
const dyLinkLabel = '0.38em';
const radiusNodeNameless = 6;
const radiusNode = 12;
const opacityNode = 0.85;
const opacityNodeLabel = 0.85;
const opacityHidden = 0;
const opacityLink = 0.8;
const opacityLinkLabel = 1;
const opacityLinkLabelHidden = isPruneFilteredNodes ? 0 : 0.75;
const strokeWidthLink = 1;
const strokeWidthHoverLink = 4;
const strokeWidthLabelShadow = 3;
const strokeWidthNode = 1.5;
const strokeWidthSelectedNode = 3;
const strokeWidthHoverNode = 3;
const colorSelectedNode = '#00B5AD';
const colorHoverNode = '#BFECE9';
const colorHoverLink = '#BFECE9';
const colorMarker = '#000';
const fontLink = 'normal 6px arial';
const fontfamilyNode = 'arial';
const fontweightNode = 'bold';
const maxfontsizeNode = 18;
const maxNodeLabelLength = 9;
const maxTooltipInOutLength = 100;
const nodeLabelPadding = 0.80;
const nodePositionMargin = 30;
const maxNodeFilterSize = 25;
const maxLinkFilterSize = 25;
const radiusScaleFactorPct = 50;
const isScaleRadiusTippingPoint = true;
const radiusScaleMinVal = 10;
const defaultTransitionDuration = 1000;
const versionIE = GetIEVersion();
const isDetailedTTKey = 'asv-detailed-tt';
declare var $: any;
declare var d3: any;
let simulation: any = null;
let isSimulationRunning = false;
let reheatFactor = 1;
let widthView = 0;
let heightView = 0;
let filterMenuInitialized = false;
let salientProcessingColor = {};
let salientProcessingLinkNames = {};
let previousNumberOfAtoms = 0;
function GetIEVersion() {
const sAgent = window.navigator.userAgent;
const Idx = sAgent.indexOf('MSIE');
if (Idx > 0) {
return parseInt(sAgent.substring(Idx + 5, sAgent.indexOf('.', Idx)), 10);
} else {
if (!!navigator.userAgent.match(/Trident\/7\./)) {
return 11;
} else {
return 0;
}
}
}
@Component({
selector: 'cog-visualizer',
templateUrl: './visualizer.component.html',
encapsulation: ViewEncapsulation.None,
styleUrls: ['./visualizer.component.css']
})
export class VisualizerComponent implements AfterViewInit, OnInit, OnDestroy, OnChanges {
@Input() atoms: any = null;
@Input() unordered_linktypes: string[];
@Input() custom_style: string;
@Input() language: string;
@Input() numAtoms: number;
public isInitialLoad = true;
public isSelectedNode = false;
public selectedNodeData = null;
public isDrilledNodes = false;
public isDetailedTooltips = false;
public d3zoom = d3.zoom();
public zoomScale = 1;
public svg: any;
public parsedJson: Graph = <Graph>{};
public node: any;
public link: any;
public nodeTypes = [];
public linkTypes = [];
public nodeLabel: any;
public linkLabel: any;
public linkLabelShadow: any;
public textPath: any;
public menus: Menus;
public divTooltip = null;
public isSuppressTooltip = true;
public marginTT = 30;
static ___this;
static this() { return this.___this; }
static scaleRadius(originalRadius: number, scaleVal: number) {
if (isScaleRadiusTippingPoint) {
if (scaleVal >= radiusScaleMinVal) {
return originalRadius * (1 + (radiusScaleFactorPct / 100));
} else {
return originalRadius;
}
} else {
return originalRadius + (Math.sqrt(scaleVal) * 2 *  (radiusScaleFactorPct / 100));
}
}
constructor(public visualizerService: VisualizerService, public atomService: AtomService, public _translate: TranslateService) {
console.log('Constructor called');
this.atomService.editItem
.subscribe(res => {
const as_data: AtomServiceData = res;
const atoms: any = as_data.atoms ? as_data.atoms : null;
if (atoms !== null && atoms.result.atoms.length > 0) {
this.atoms = as_data.atoms;
console.log('VisualizerComponent atoms=' + as_data.atoms);
console.log('atoms.result.atoms.length =',atoms.result.atoms.length);
}
this.numAtoms = as_data.numAtoms;
const uolinktypes: any = as_data.unordered_linktypes ? as_data.unordered_linktypes : null;
if (uolinktypes !== null && uolinktypes.length > 0) {
this.unordered_linktypes = as_data.unordered_linktypes;
}
const custstyle: any = as_data.custom_style ? as_data.custom_style : null;
if (custstyle !== null && custstyle.length > 0) {
this.custom_style = as_data.custom_style;
}
const lang: any = as_data.language ? as_data.language : null;
if (lang !== null && lang.length === 2) {
this.language = as_data.language;
this.selectLang(this.language);
}
});
}
ngOnInit(): void {
VisualizerComponent.___this = this;
this.menus = this.initContextMenus();
if (this.divTooltip === null) {
this.divTooltip = d3.select('body').append('div').attr('class', 'tooltip').style('opacity', 0);
}
const isDtlTT = window.localStorage.getItem(isDetailedTTKey);
if (isDtlTT !== null) {
this.isDetailedTooltips = isDtlTT === 'true' ? true : false;
if (this.isDetailedTooltips) {
$('.detailed-tt-toggle').prop('checked', true);
}
}
}
ngOnDestroy(): void {
this.pauseSimulation();
if (this.divTooltip) {
this.divTooltip = null;
}
}
salientIncomingOutgoingLinks() {
const numberOfNodesToShow = 20;
var sumInOut = new Array();
var sum = 0;
if (this.atoms) {
var tempParsedJson = this.parsedJson;
console.log('tempParsedJson\n', tempParsedJson);
console.log(tempParsedJson.nodes);
console.log(tempParsedJson.nodes.length);
console.log(tempParsedJson.links);
console.log(tempParsedJson.links.length);
console.log(typeof tempParsedJson.nodes);
console.log(typeof tempParsedJson.links);
console.log('this.parsedJson\n', this.parsedJson);
for (let i = 0; i < this.parsedJson.nodes.length; i++) {
salientProcessingColor[this.parsedJson.nodes[i]["id"]]=this.parsedJson.nodes[i]["color"];
}
for (let i = 0; i < tempParsedJson.nodes.length; i++) {
sum = tempParsedJson.nodes[i].incoming.length + tempParsedJson.nodes[i].outgoing.length;
sumInOut[i] = [i,sum];
}
sumInOut.sort((first,second) => {return second[1]-first[1]});
let iTempNode = 0;
for (let i = 0; i < sumInOut.length; i++) {
if(tempParsedJson.nodes[sumInOut[i][0]]["type"] == "TimeNode" || tempParsedJson.nodes[sumInOut[i][0]]["type"] == "NumberNode") {
tempParsedJson.nodes[sumInOut[i][0]]["color"] = "#C0C0C0";
continue;
}
if (iTempNode < numberOfNodesToShow){
tempParsedJson.nodes[sumInOut[i][0]]["color"] = "#146EB4";
iTempNode = iTempNode + 1;
}
else {
tempParsedJson.nodes[sumInOut[i][0]]["color"] = "#C0C0C0";
}
}
console.log('tempParsedJson.links\n',tempParsedJson.links);
for (let i = 0; i < this.parsedJson.links.length; i++) {
salientProcessingLinkNames[this.parsedJson.links[i]['index']]=this.parsedJson.links[i]['name'];
}
console.log('salientProcessingLinkNames\n',salientProcessingLinkNames);
for (let i = 0; i < tempParsedJson.links.length; i++) {
tempParsedJson.links[i]['name'] = '';
}
return tempParsedJson;
}
}
preprocessAtoms() {
for (let i = 0; i < this.atoms.result.atoms.length; i++) {
this.atoms.result.atoms[i]["name"] = this.atoms.result.atoms[i]["name"].replace(/back-/ig,"");
this.atoms.result.atoms[i]["name"] = this.atoms.result.atoms[i]["name"].replace(/back/ig,"");
this.atoms.result.atoms[i]["type"] = this.atoms.result.atoms[i]["type"].replace(/back-/ig,"");
this.atoms.result.atoms[i]["type"] = this.atoms.result.atoms[i]["type"].replace(/back/ig,"");
}
}
replaceLinkNames() {
}
ngAfterViewInit() {
if (this.atoms) {
this.preprocessAtoms();
this.parsedJson = this.visualizerService.getParsedJson(this.atoms.result.atoms);
if (this.atoms.result.atoms.length) {
let resultStr = 'Loaded ' + this.atoms.result.atoms.length + ' atoms';
if (typeof this.atoms.result.complete !== 'undefined') { resultStr += ', complete=' + this.atoms.result.complete; }
if (typeof this.atoms.result.skipped !== 'undefined') { resultStr += ', skipped=' + this.atoms.result.skipped; }
this.getFilters(this.parsedJson);
console.log(resultStr);
}
this.draw_graph();
isSimulationRunning = true;
this.isInitialLoad = false;
this.parsedJson = this.salientIncomingOutgoingLinks();
this.draw_graph();
isSimulationRunning = true;
this.isInitialLoad = false;
}
setInterval(() => {
this.update();
}, 3000);
console.log('setInterval called from visualizer component');
}
update() {
try {
if(previousNumberOfAtoms != this.atoms.result.atoms.length){
previousNumberOfAtoms = this.atoms.result.atoms.length;
this.ngAfterViewInit();
}
}
catch (error) {
console.log(error);
}
if (this.numAtoms == 0 && previousNumberOfAtoms !=0){
previousNumberOfAtoms = 0;
this.parsedJson = this.visualizerService.getParsedJson([]);
this.draw_graph();
isSimulationRunning = true;
this.isInitialLoad = false;
console.log('In if (this.numAtoms == 0 && previousNumberOfAtoms !=0)');
}
}
ngOnChanges(changes: SimpleChanges) {
if (!changes.atoms.isFirstChange()) {
this.ngAfterViewInit();
}
}
panToCenter() {
const scale = this.zoomScale;
let x = -((scale - 1) / 2) * widthView;
const y = -((scale - 1) / 2) * heightView;
x *= 0.70193340494092373791621911922664;
const __this = this;
const view = d3.select('svg');
view.transition()
.attr('transform', 'translate(' + x + ',' + y + ').scale(' + scale + ')')
.on('end', function () {
view.call(__this.d3zoom.transform, d3.zoomIdentity.translate(x, y).scale(scale));
});
}
panNodeToCenter(d) {
const scale = this.zoomScale;
const x = (widthView / 2) - (d.x * scale);
const y = (heightView / 2) - (d.y * scale);
const __this = this;
const view = d3.select('svg');
view.transition()
.attr('transform', 'translate(' + x + ',' + y + ').scale(' + scale + ')')
.on('end', function () {
view.call(__this.d3zoom.transform, d3.zoomIdentity.translate(x, y).scale(scale));
});
}
removeNodeDecorators() {
d3.selectAll('circle')
.style('r', function (d) {
let r = d.name === '' ? radiusNodeNameless : radiusNode;
r = VisualizerComponent.scaleRadius(r, d.av.sti);
return r;
}).style('stroke', '#fff');
}
pauseSimulation() {
if (simulation) {
simulation.stop();
isSimulationRunning = false;
reheatFactor = 1;
}
}
playSimulation() {
if (simulation) {
if (!isSimulationRunning) {
simulation.restart();
isSimulationRunning = true;
} else {
reheatFactor = Math.min(reheatFactorMax, reheatFactor + 1);
simulation
.alpha(1)
.alphaTarget(0.1 * reheatFactor)
.force('charge', d3.forceManyBody().strength(simForceStrength * reheatFactor).distanceMax(400))
.restart();
}
}
}
restartSimulation() {
this.pauseSimulation();
this.closeSelectedNodeProps();
this.isDrilledNodes = false;
filterMenuInitialized = false;
this.parsedJson = this.visualizerService.getParsedJson(this.atoms.result.atoms);
this.draw_graph();
isSimulationRunning = true;
this.parsedJson = this.salientIncomingOutgoingLinks();
this.draw_graph();
isSimulationRunning = true;
}
zoomIn(duration: number) {
if (this.zoomScale < 1) {
this.zoomScale = 1;
} else if (this.zoomScale < 4) {
this.zoomScale += 1;
}
const view = d3.select('svg');
view.transition().duration(duration).call(this.d3zoom.scaleTo, this.zoomScale);
}
zoomOut(duration: number) {
if (this.zoomScale === 1) {
this.zoomScale = 0.5;
} else if (this.zoomScale > 1) {
this.zoomScale -= 1;
}
const view = d3.select('svg');
view.transition().duration(duration).call(this.d3zoom.scaleTo, this.zoomScale);
}
zoomReset(duration: number) {
this.zoomScale = 1;
const view = d3.select('svg');
view.transition().duration(duration).call(this.d3zoom.scaleTo, this.zoomScale);
this.panToCenter.call(this);
}
toggleTooltips() {
this.isDetailedTooltips = !this.isDetailedTooltips;
window.localStorage.setItem(isDetailedTTKey, this.isDetailedTooltips ? 'true' : 'false');
}
closeSelectedNodeProps() {
this.isSelectedNode = false;
this.selectedNodeData = null;
filterMenuInitialized = false;
this.clearFilters();
this.removeNodeDecorators();
}
onLoadFiltering(event) {
if (filterMenuInitialized) { return; }
const strUnfiltered = this._translate.instant('Unfiltered');
const strFilterOnSelection = this._translate.instant('FilterOnSelection');
$('#filtermenu').empty();
$('#filtermenu').append('<div class=\'header\'><i class=\'tags icon\'></i><span>' + strFilterOnSelection +
'</span></div><div class=\'divider\'></div>');
this.nodeTypes.forEach(type => { $('#filtermenu').append('<div class=\'item\'><span>' + type + '</span></div>'); });
$('#filtermenu').append('<div class=\'divider\'></div>');
this.linkTypes.forEach(type => { $('#filtermenu').append('<div class=\'item\'><span>' + type + '</span></div>'); });
$('#filtermenu').append('<div class=\'divider\'></div><div class=\'item\'><span>' + strUnfiltered + '</span></div>');
filterMenuInitialized = true;
}
onClickFiltering(event) {
if (event.target.innerText) {
VisualizerComponent.this().filterByNode(event.target.innerText);
}
}
filterByNode(type) {
const filterTypeAll =  this._translate.instant('Unfiltered');
if (type === filterTypeAll) {
this.clearFilters();
return;
}
const linkedByIndex = {};
this.link.each(function (d) {
linkedByIndex[d.source.index + ',' + d.target.index] = true;
linkedByIndex[d.target.index + ',' + d.source.index] = true;
});
function neighboring(a, b) {
return linkedByIndex[a.index + ',' + b.index];
}
this.node.style('opacity', (d) => {
return (neighboring(d, this.selectedNodeData) && d.type === type) ||
d.id === this.selectedNodeData.id ? opacityNode : opacityHidden;
});
this.nodeLabel.style('opacity', (d) => {
return (neighboring(d, this.selectedNodeData) && d.type === type) ||
d.id === this.selectedNodeData.id ? opacityNodeLabel : opacityHidden;
});
this.link.style('opacity', (d) => {
if ((d.source === this.selectedNodeData) && d.target.type === type) {
return opacityLink;
} else if ((d.target === this.selectedNodeData) && d.source.type === type) {
return opacityLink;
} else {
return opacityHidden;
}
});
this.textPath.style('opacity', (d) => {
if ((d.source === this.selectedNodeData) && d.target.type === type) {
return opacityLink;
} else if ((d.target === this.selectedNodeData) && d.source.type === type) {
return opacityLink;
} else {
return opacityHidden;
}
});
this.linkLabelShadow.style('opacity', (d) => {
if ((d.source === this.selectedNodeData) && d.target.type === type) {
return opacityLinkLabel;
} else if ((d.target === this.selectedNodeData) && d.source.type === type) {
return opacityLinkLabel;
} else {
return opacityLinkLabelHidden;
}
});
this.linkLabel.style('opacity', (d) => {
if ((d.source === this.selectedNodeData) && d.target.type === type) {
return opacityLinkLabel;
} else if ((d.target === this.selectedNodeData) && d.source.type === type) {
return opacityLinkLabel;
} else {
return opacityHidden;
}
});
}
showAll() {
if (isSimulationRunning) { simulation.stop(); }
this.preprocessAtoms();
this.parsedJson = this.visualizerService.getParsedJson(this.atoms.result.atoms);
this.closeSelectedNodeProps();
this.isDrilledNodes = false;
filterMenuInitialized = false;
this.draw_graph();
this.parsedJson = this.salientIncomingOutgoingLinks();
this.closeSelectedNodeProps();
this.isDrilledNodes = false;
filterMenuInitialized = false;
this.draw_graph();
if (isSimulationRunning) { simulation.restart(); }
}
public getFilters(parsedJson) {
parsedJson.nodes.forEach(elem => {
if (elem.name !== '') {
if (this.nodeTypes.indexOf(elem.type) === -1) {
if (this.nodeTypes.length < maxNodeFilterSize) {
this.nodeTypes.push(elem.type);
} else {
console.log('Dropping Node filter for \'' + elem.type + '\' because exceeded maxNodeFilterSize (' + maxNodeFilterSize + ')');
}
}
}
});
parsedJson.links.forEach(elem => {
if (elem.name !== '') {
if (this.linkTypes.indexOf(elem.name) === -1) {
if (this.linkTypes.length < maxLinkFilterSize) {
this.linkTypes.push(elem.name);
} else {
console.log('Dropping Link filter for \'' + elem.name + '\' because exceeded maxLinkFilterSize (' + maxLinkFilterSize + ')');
}
}
}
});
this.nodeTypes.sort();
this.linkTypes.sort();
}
public clearFilters() {
this.node.each(function (d) { d.fx = d.fy = null; });
simulation.force('charge', d3.forceManyBody().strength(function (d) {
d.charge = simForceStrength;
return simForceStrength;
}));
if (isSimulationRunning) { simulation.restart(); }
this.link.style('stroke-width', strokeWidthLink).style('opacity', opacityLink);
this.textPath.style('opacity', opacityLink);
this.node.style('opacity', opacityNode).style('stroke', '#fff');
d3.selectAll('text.node-labels').style('opacity', opacityNodeLabel);
d3.selectAll('text.edgelabel').style('opacity', opacityLinkLabel);
d3.selectAll('text.edgelabelshadow').style('opacity', opacityLinkLabel);
d3.selectAll('.edgelabelshadow').style('font', fontLink).attr('dy', dyLinkLabel);
d3.selectAll('.edgelabel').style('font', fontLink).attr('dy', dyLinkLabel);
const __this = this;
if (this.isSelectedNode) {
this.node.each(function (d) {
if (d.id === __this.selectedNodeData.id) {
d3.select(this).style('stroke-width', strokeWidthSelectedNode);
d3.select(this).style('stroke', colorSelectedNode);
}
});
}
}
public draw_graph() {
const linkedByOutgoing = {};
this.svg = d3.select('svg');
this.svg.selectAll('*').remove();
this.isSuppressTooltip = true;
filterMenuInitialized = false;
widthView = document.getElementById('svgcanvas').clientWidth;
heightView = document.getElementById('svgcanvas').clientHeight;
if (widthView === 0 && heightView === 0) {
const rect = document.getElementById('svgcanvas').getBoundingClientRect();
widthView = rect.width;
heightView = rect.height;
}
const colorScheme = d3.scaleOrdinal(d3.schemeCategory20);
const defaultAlphaDecay = 1 - Math.pow(0.001, 1 / 300);
const alphaDecay = this.atoms.result.atoms.length < 50 ? 0.008 : defaultAlphaDecay;
if (simulation) { simulation.stop(); }
simulation = d3.forceSimulation()
.force('link', d3.forceLink().id(function (d) { return d.id; }).distance(100))
.force('charge', d3.forceManyBody().strength(simForceStrength).distanceMax(250))
.force('center', d3.forceCenter(widthView / 2, heightView / 2))
.force('collide', d3.forceCollide().radius(function (d) {
let r = (d.name === '') ? radiusNodeNameless : radiusNode;
r = VisualizerComponent.scaleRadius(r, d.av.sti);
return r;
}))
.alphaDecay(alphaDecay);
this.svg.append('rect')
.attr('width', widthView)
.attr('height', heightView)
.style('fill', 'none')
.style('pointer-events', 'all');
const g = this.svg.append('g').attr('class', 'svg-grp');
function zoomHandler() {
this.zoomScale = d3.event.transform.k;
g.attr('transform', d3.event.transform);
}
this.svg.on('contextmenu', d3.contextMenu(this.menus.mainMenu));
g.append('svg:defs')
.selectAll('marker')
.data(['marker'])
.enter()
.append('svg:marker')
.attr('id', 'markerEnd')
.attr('viewBox', '0 -5 10 10')
.attr('refX', 8)
.attr('markerWidth', 5)
.attr('markerHeight', 5)
.attr('markerUnits', 'userSpaceOnUse')
.attr('orient', 'auto')
.append('svg:path')
.attr('d', 'M0,-3.5L10,0L0,3.5')
.style('fill', colorMarker)
.style('stroke', 'none');
g.append('svg:defs')
.selectAll('marker')
.data(['marker'])
.enter()
.append('svg:marker')
.attr('id', 'markerStart')
.attr('viewBox', '0 -5 10 10')
.attr('refX', 2)
.attr('markerWidth', 5)
.attr('markerHeight', 5)
.attr('markerUnits', 'userSpaceOnUse')
.attr('orient', 'auto')
.append('svg:path')
.attr('d', 'M0,0L10,-3.5L10,3.5Z')
.style('fill', colorMarker)
.style('stroke', 'none');
const __this = this;
this.link = g.append('g')
.attr('class', 'links')
.selectAll('path')
.data(this.parsedJson.links)
.enter()
.append('path')
.attr('class', 'lines')
.style('fill', 'none')
.style('stroke-width', strokeWidthLink)
.style('stroke-linecap', 'round')
.attr('marker-end', 'url(#markerEnd)')
;
this.textPath = g.append('g')
.attr('class', 'edgepath-grp')
.selectAll('.edgepath')
.data(this.parsedJson.links)
.enter()
.append('path')
.attr('class', 'edgepath')
.style('fill', 'none')
.style('stroke', 'black')
.attr('id', function (d, i) {
return 'edgepath' + i;
})
.style('user-select', 'none');
this.linkLabelShadow = g.append('g')
.attr('class', 'edgelabelshadow-grp')
.selectAll('.edgelabel')
.data(this.parsedJson.links)
.enter()
.append('text')
.style('user-select', 'none')
.style('font', fontLink)
.style('line-height', '150%')
.style('stroke-width', strokeWidthLabelShadow)
.attr('class', 'edgelabelshadow')
.attr('dy', dyLinkLabel)
.attr('id', function (d, i) {
return 'edgelabelshadow' + i;
});
this.linkLabel = g.append('g')
.attr('class', 'edgelabel-grp')
.selectAll('.edgelabel')
.data(this.parsedJson.links)
.enter()
.append('text')
.style('pointer-events', 'none')
.style('user-select', 'none')
.style('font', fontLink)
.style('line-height', '150%')
.attr('class', 'edgelabel')
.attr('dy', dyLinkLabel)
.attr('id', function (d, i) {
return 'edgelabel' + i;
});
this.linkLabelShadow.append('textPath')
.attr('xlink:xlink:href', function (d, i) {
return '#edgepath' + i;
})
.style('text-anchor', 'middle')
.style('user-select', 'none')
.attr('startOffset', '50%')
.text(function (d) {
return d.name;
})
.on('mouseover', (d) => linkMouseOver.call(this, d))
.on('mouseout', (d) => linkMouseOut.call(this, d));
this.linkLabel.append('textPath')
.attr('xlink:xlink:href', function (d, i) {
return '#edgepath' + i;
})
.style('text-anchor', 'middle')
.style('pointer-events', 'none')
.style('user-select', 'none')
.attr('startOffset', '50%')
.text(function (d) {
return d.name;
});
this.node = g.append('g')
.attr('class', 'nodes')
.selectAll('circle')
.data(this.parsedJson.nodes)
.enter().append('circle')
.on('contextmenu', d3.contextMenu(this.menus.nodeMenu))
.attr('r', (d) => {
let r = (d.name === '') ? radiusNodeNameless : radiusNode;
r = VisualizerComponent.scaleRadius(r, d.av.sti);
return r;
})
.style('fill', function (d) {
d.color = d.color ? d.color : colorScheme(d.group);
return d.color;
})
.style('opacity', opacityNode)
.on('mouseover', (d) => nodeMouseOver.call(this, d))
.on('mouseout', (d) => nodeMouseOut.call(this, d))
.call(d3.drag().subject((d) => d)
.on('start', (d) => nodeDragStarted.call(this, d))
.on('drag', (d) => nodeDragging.call(this, d))
.on('end', (d) => nodeDragEnded.call(this, d)));
this.nodeLabel = g.append('g')
.attr('class', 'nodelabel-grp')
.selectAll('.mytext')
.data(this.parsedJson.nodes)
.enter()
.append('text')
.text(function (d) {
const len = d.name.length;
if (len === 0) {
return '';
} else if (len > maxNodeLabelLength) {
return d.name.substr(0, maxNodeLabelLength - 3) + '...';
} else { return d.name; }
})
.style('font-family', fontfamilyNode)
.style('font-weight', fontweightNode)
.style('font-size', '1px')
.each(getSizeNodeLabel)
.style('font-size', function () {
return Math.min(d3.select(this).attr('data-scale'), maxfontsizeNode) + 'px';
})
.attr('class', 'node-labels')
.attr('text-anchor', 'middle')
.style('pointer-events', 'none')
.style('user-select', 'none')
.style('fill', '#fff')
.style('opacity', opacityNodeLabel);
this.node.on('click', (d) => nodeSingleClick.call(this, d));
this.node.on('dblclick', (d) => nodeDoubleClick.call(this, d));
d3.select(window).on('resize', () => graphResize.call(this));
d3.select(window).on('keydown', () => graphKeydown.call(this));
d3.select(window).on('mousemove', () => graphMousemove.call(this));
this.svg.call(this.d3zoom
.scaleExtent([1 / 2, 4])
.duration(defaultTransitionDuration)
.on('zoom', () => zoomHandler.call(this)))
.on('dblclick.zoom', null);
if (this.zoomScale !== 1) {
const view = d3.select('svg');
view.transition().duration(defaultTransitionDuration).call(this.d3zoom.scaleTo, this.zoomScale);
}
simulation
.nodes(this.parsedJson.nodes)
.stop();
simulation.force('link')
.links(this.parsedJson.links);
this.link.each(function (d) {
if (linkedByOutgoing[d.source.id + ',' + d.target.id]) {
linkedByOutgoing[d.source.id + ',' + d.target.id] += ',' + d.id;
} else {
linkedByOutgoing[d.source.id + ',' + d.target.id] = d.id.toString();
}
if (linkedByOutgoing[d.target.id + ',' + d.source.id]) {
linkedByOutgoing[d.target.id + ',' + d.source.id] += ',' + d.id;
} else {
linkedByOutgoing[d.target.id + ',' + d.source.id] = d.id.toString();
}
});
for (var i = 0; i < 300; ++i) {
simulation.tick();
}
graphTick.call(this)
simulation.on('tick', () => graphTick.call(this));
simulation.restart();
function nodeDragStarted(d) {
if (!d3.event.active) { simulation.alphaTarget(0.3).restart(); }
d.fx = d.x;
d.fy = d.y;
d3.event.sourceEvent.stopPropagation();
}
function nodeDragging(d) {
if (isNodesConstrainedToClientArea) {
d.fx = Math.max(nodePositionMargin, Math.min(widthView - nodePositionMargin, d3.event.x));
d.fy = Math.max(nodePositionMargin, Math.min(heightView - nodePositionMargin, d3.event.y));
} else {
d.fx = d3.event.x;
d.fy = d3.event.y;
}
}
function nodeDragEnded(d) {
if (!d3.event.active) { simulation.alphaTarget(0); }
if (isSimulationRunning === false) { this.pauseSimulation(); }
if (!d3.event.sourceEvent.ctrlKey) {
d.fx = d.fy = null;
}
if (d3.event.sourceEvent.shiftKey) {
simulation.force('charge', d3.forceManyBody().strength(function (o) {
return d.id === o.id ? simForceStrengthHighNodeCharge : simForceStrength;
}));
d.charge = simForceStrengthHighNodeCharge;
if (isSimulationRunning) { simulation.restart(); }
}
}
function graphTick() {
if (versionIE > 0) {
this.link.each(function() { this.parentNode.insertBefore(this, this); } );
}
const offsetRadsL1 = 0.13, offsetRadsL2 = 0.26, offsetRadsL3 = 0.39,
radiusFactorL1 = 1.75, radiusFactorL2 = 1.0, radiusFactorL3 = 0.70;
this.link.attr('d', function(d) {
const arrOutLinks = getOutgoingLinks(d);
if (arrOutLinks.length % 2) {
switch (d.id) {
case arrOutLinks[0]: return straightPath(d, true);
case arrOutLinks[1]: return arcPath(d, true, offsetRadsL1, radiusFactorL1, true);
case arrOutLinks[2]: return arcPath(d, true, offsetRadsL1, radiusFactorL1, false);
case arrOutLinks[3]: return arcPath(d, true, offsetRadsL2, radiusFactorL2, true);
case arrOutLinks[4]: return arcPath(d, true, offsetRadsL2, radiusFactorL2, false);
case arrOutLinks[5]: return arcPath(d, true, offsetRadsL3, radiusFactorL3, true);
case arrOutLinks[6]: return arcPath(d, true, offsetRadsL3, radiusFactorL3, false);
default: return straightPath(d, true);
}
} else {
switch (d.id) {
case arrOutLinks[0]: return arcPath(d, true, offsetRadsL1, radiusFactorL1, true);
case arrOutLinks[1]: return arcPath(d, true, offsetRadsL1, radiusFactorL1, false);
case arrOutLinks[2]: return arcPath(d, true, offsetRadsL2, radiusFactorL2, true);
case arrOutLinks[3]: return arcPath(d, true, offsetRadsL2, radiusFactorL2, false);
case arrOutLinks[4]: return arcPath(d, true, offsetRadsL3, radiusFactorL3, true);
case arrOutLinks[5]: return arcPath(d, true, offsetRadsL3, radiusFactorL3, false);
default: return straightPath(d, true);
}
}
});
this.textPath.attr('d', function(d) {
const isLeftHand = d.source.x < d.target.x;
const arrOutLinks = getOutgoingLinks(d);
if (arrOutLinks.length % 2) {
switch (d.id) {
case arrOutLinks[0]: return straightPath(d, isLeftHand);
case arrOutLinks[1]: return arcPath(d, isLeftHand, offsetRadsL1, radiusFactorL1, true);
case arrOutLinks[2]: return arcPath(d, isLeftHand, offsetRadsL1, radiusFactorL1, false);
case arrOutLinks[3]: return arcPath(d, isLeftHand, offsetRadsL2, radiusFactorL2, true);
case arrOutLinks[4]: return arcPath(d, isLeftHand, offsetRadsL2, radiusFactorL2, false);
case arrOutLinks[5]: return arcPath(d, isLeftHand, offsetRadsL3, radiusFactorL3, true);
case arrOutLinks[6]: return arcPath(d, isLeftHand, offsetRadsL3, radiusFactorL3, false);
default: return straightPath(d, isLeftHand);
}
} else {
switch (d.id) {
case arrOutLinks[0]: return arcPath(d, isLeftHand, offsetRadsL1, radiusFactorL1, true);
case arrOutLinks[1]: return arcPath(d, isLeftHand, offsetRadsL1, radiusFactorL1, false);
case arrOutLinks[2]: return arcPath(d, isLeftHand, offsetRadsL2, radiusFactorL2, true);
case arrOutLinks[3]: return arcPath(d, isLeftHand, offsetRadsL2, radiusFactorL2, false);
case arrOutLinks[4]: return arcPath(d, isLeftHand, offsetRadsL3, radiusFactorL3, true);
case arrOutLinks[5]: return arcPath(d, isLeftHand, offsetRadsL3, radiusFactorL3, false);
default: return straightPath(d, isLeftHand);
}
}
});
this.node
.attr('cx', function (d) {
if (isNodesConstrainedToClientArea) {
return d.x = Math.max(nodePositionMargin, Math.min(widthView - nodePositionMargin, d.x));
} else {
return d.x;
}
})
.attr('cy', function (d) {
if (isNodesConstrainedToClientArea) {
return d.y = Math.max(nodePositionMargin, Math.min(heightView - nodePositionMargin, d.y));
} else {
return d.y;
}
});
this.nodeLabel
.attr('x', function (d) { return d.x; })
.attr('y', function (d) { return d.y; })
.attr('dy', '0.35em');
}
function nodeMouseOver(d) {
if (d3.event.buttons !== 0) {
this.divTooltip.transition().duration(0).style('opacity', 0);
return;
}
if (this.isSuppressTooltip) { return; }
if (d3.select(d3.event.currentTarget).style('opacity') === 0) { return; }
this.divTooltip.html(buildNodeTooltip(d, this.isDetailedTooltips));
const evt = d3.event, wTT = this.divTooltip.node().firstChild.clientWidth, hTT = this.divTooltip.node().firstChild.clientHeight;
const xTT = evt.pageX < (window.innerWidth - wTT) - this.marginTT ? evt.pageX + 12 : (evt.pageX - 12) - wTT;
const yTT = evt.pageY < window.innerHeight - hTT ? evt.pageY - 12 : (evt.pageY + 12) - hTT;
this.divTooltip.style('left', xTT + 'px').style('top', yTT + 'px');
this.divTooltip.transition()
.style('opacity', 0.90)
.style('left', xTT + 'px')
.style('top', yTT + 'px');
drawNodeDecorators.call(this, d, true);
}
function nodeMouseOut(d) {
this.divTooltip.transition().duration(0).style('opacity', 0);
drawNodeDecorators.call(this, d, false);
}
function linkMouseOver(d) {
if (d3.event.buttons !== 0) {
this.divTooltip.transition().duration(0).style('opacity', 0);
return;
}
if (this.isSuppressTooltip) { return; }
if (d3.select(d3.event.currentTarget).style('opacity') === 0) { return; }
this.divTooltip.html(buildLinkTooltip(d, this.isDetailedTooltips));
const evt = d3.event, wTT = this.divTooltip.node().firstChild.clientWidth, hTT = this.divTooltip.node().firstChild.clientHeight;
const xTT = evt.pageX < (window.innerWidth - wTT) - this.marginTT ? evt.pageX + 12 : (evt.pageX - 12) - wTT;
const yTT = evt.pageY < window.innerHeight - hTT ? evt.pageY - 12 : (evt.pageY + 12) - hTT;
this.divTooltip.style('left', xTT + 'px').style('top', yTT + 'px');
this.divTooltip.transition()
.style('opacity', 0.90)
.style('left', xTT + 'px')
.style('top', yTT + 'px');
drawLinkDecorators.call(this, d, true);
}
function linkMouseOut(d) {
this.divTooltip.transition().duration(0).style('opacity', 0);
drawLinkDecorators.call(this, d, false);
}
function nodeSingleClick(d) {
this.divTooltip.transition().duration(0).style('opacity', 0);
if (this.isSelectedNode === true && this.selectedNodeData === d) {
this.removeNodeDecorators();
this.isSelectedNode = false;
this.selectedNodeData = null;
filterMenuInitialized = false;
if (isSimulationRunning === false) { this.pauseSimulation(); }
return;
}
this.selectedNodeData = d;
this.isSelectedNode = true;
drawNodeDecorators.call(this, d, false);
if (isSimulationRunning === false) { this.pauseSimulation(); }
}
function nodeDoubleClick(d) {
for (let i = 0; i < this.parsedJson.nodes.length; i++) {
this.parsedJson.nodes[i]["color"] = salientProcessingColor[i];
}
for (let i = 0; i < this.parsedJson.links.length; i++) {
this.parsedJson.links[i]["name"] = salientProcessingLinkNames[i];
}
this.isDrilledNodes = true;
this.selectedNodeData = d;
this.isSelectedNode = true;
const linkedByIndex = {};
this.link.each(function (k) {
linkedByIndex[k.source.index + ',' + k.target.index] = true;
linkedByIndex[k.target.index + ',' + k.source.index] = true;
});
const neighbourLinks = [];
const neighlink = [];
const neigh = [];
if (isXtraLevelNeighbors) {
this.node.each(function (k) {
if (neighboring(d, k) && k.name === '') {
neighbourLinks.push(k);
}
});
neighbourLinks.forEach((elem, i) => {
neighlink.push(neighbourLinks[i].id);
this.node.each(function (l) {
if (neighboring(neighbourLinks[i], l)) {
neigh.push(l.id);
}
});
});
}
function neighboring(a, b) {
return linkedByIndex[a.index + ',' + b.index];
}
this.node.style('opacity', function (o) {
if (isXtraLevelNeighbors) {
return neighboring(d, o) || (d.id === o.id) || (neigh.indexOf(o.id) !== -1) ? opacityNode : opacityHidden;
} else {
return neighboring(d, o) || (d.id === o.id) ? opacityNode : opacityHidden;
}
});
this.nodeLabel.style('opacity', function (o) {
if (isXtraLevelNeighbors) {
return neighboring(d, o) || (d.id === o.id) || (neigh.indexOf(o.id) !== -1) ? opacityNodeLabel : opacityHidden;
} else {
return neighboring(d, o) || (d.id === o.id) ? opacityNodeLabel : opacityHidden;
}
});
this.link.style('opacity', function (o) {
if (isXtraLevelNeighbors) {
return o.source === d || o.target === d || ((neigh.indexOf(o.target.id) !== -1) &&
(neighlink.indexOf(o.source.id) !== -1)) || ((neigh.indexOf(o.source.id) !== -1) &&
(neighlink.indexOf(o.target.id) !== -1)) ? opacityLink : opacityHidden;
} else {
return o.source === d || o.target === d ? opacityLink : opacityHidden;
}
});
this.textPath.style('opacity', function (o) {
if (isXtraLevelNeighbors) {
return o.source === d || o.target === d || ((neigh.indexOf(o.target.id) !== -1) &&
(neighlink.indexOf(o.source.id) !== -1)) || ((neigh.indexOf(o.source.id) !== -1) &&
(neighlink.indexOf(o.target.id) !== -1)) ? opacityLink : opacityHidden;
} else {
return o.source === d || o.target === d ? opacityLink : opacityHidden;
}
});
this.linkLabelShadow.style('opacity', function (o) {
if (isXtraLevelNeighbors) {
return o.source === d || o.target === d || ((neigh.indexOf(o.target.id) !== -1) &&
(neighlink.indexOf(o.source.id) !== -1)) || ((neigh.indexOf(o.source.id) !== -1) &&
(neighlink.indexOf(o.target.id) !== -1)) ? 1 : opacityLinkLabelHidden;
} else {
return o.source === d || o.target === d ? 1 : opacityLinkLabelHidden;
}
});
this.linkLabel.style('opacity', function (o) {
if (isXtraLevelNeighbors) {
return o.source === d || o.target === d || ((neigh.indexOf(o.target.id) !== -1) &&
(neighlink.indexOf(o.source.id) !== -1)) || ((neigh.indexOf(o.source.id) !== -1) &&
(neighlink.indexOf(o.target.id) !== -1)) ? opacityLinkLabel : opacityHidden;
} else {
return o.source === d || o.target === d ? opacityLinkLabel : opacityHidden;
}
});
if (isPruneFilteredNodes) {
const nodesFiltered = [], linksFiltered = [];
const isRunning = isSimulationRunning;
this.node.each(function (o) {
if (d3.select(this).style('opacity') === opacityHidden.toString()) {
} else {
nodesFiltered.push(o);
}
});
this.link.each(function (o) {
if (d3.select(this).style('opacity') === opacityHidden.toString()) {
} else {
linksFiltered.push(o);
}
});
this.parsedJson.nodes = nodesFiltered;
this.parsedJson.links = linksFiltered;
this.draw_graph();
drawNodeDecorators.call(this, d, false);
if (d3.event.shiftKey) {
simulation.force('charge', d3.forceManyBody().strength(function (o) {
return d.id === o.id ? simForceStrengthHighNodeCharge : simForceStrength;
}));
d.charge = simForceStrengthHighNodeCharge;
if (isRunning) { simulation.restart(); }
}
} else {
}
}
function graphResize() {
if (document.getElementById('svgcanvas') === null) { return; }
const view = d3.select('rect');
widthView = document.getElementById('svgcanvas').clientWidth;
heightView = document.getElementById('svgcanvas').clientHeight;
if (widthView === 0 && heightView === 0) {
const rect = document.getElementById('svgcanvas').getBoundingClientRect();
widthView = rect.width;
heightView = rect.height;
}
view.attr('width', widthView).attr('height', heightView);
if (isSimulationRunning === true) {
simulation.force('center', d3.forceCenter(widthView / 2, heightView / 2));
} else {
this.playSimulation();
simulation.force('center', d3.forceCenter(widthView / 2, heightView / 2));
}
}
function graphKeydown() {
const e = d3.event;
if (document.getElementById('svgcanvas') === null) { return; }
switch (e.keyCode) {
case 27:
this.divTooltip.transition().duration(0).style('opacity', 0);
d3.select('.d3-context-menu').style('display', 'none');
break;
case 19:
if (!simulation) { break; }
if (e.shiftKey) {
this.restartSimulation();
} else {
if (isSimulationRunning === true) {
this.pauseSimulation();
} else {
this.playSimulation();
}
}
break;
case 107:
this.zoomIn(defaultTransitionDuration);
break;
case 109:
this.zoomOut(defaultTransitionDuration);
break;
case 106:
this.zoomReset(defaultTransitionDuration);
break;
case 86:
if (e.altKey && e.ctrlKey) {
alert('Version ' + version);
}
break;
}
}
function graphMousemove() {
this.isSuppressTooltip = false;
}
function drawNodeDecorators(d, isOver) {
const __this = this;
d3.selectAll('circle')
.style('stroke-width', function (o) {
if (isOver) {
if (o.id === d.id) {
return strokeWidthHoverNode;
} else if (__this.isSelectedNode && (o.id === __this.selectedNodeData.id)) {
return strokeWidthSelectedNode;
} else {
return strokeWidthNode;
}
} else {
if (__this.isSelectedNode && (o.id === __this.selectedNodeData.id)) {
return strokeWidthSelectedNode;
} else {
return strokeWidthNode;
}
}})
.style('stroke', function (o) {
if (isOver) {
if (o.id === d.id) {
return colorHoverNode;
} else if (__this.isSelectedNode && (o.id === __this.selectedNodeData.id)) {
return colorSelectedNode;
} else {
return '#fff';
}
} else {
if (__this.isSelectedNode && (o.id === __this.selectedNodeData.id)) {
return colorSelectedNode;
} else {
return '#fff';
}
}
});
}
function drawLinkDecorators(d, isOver) {
d3.selectAll('.lines')
.style('stroke-width', function (o: any) {
if (isOver) {
return o.id === d.id ? strokeWidthHoverLink : strokeWidthLink;
} else {
return strokeWidthLink;
}})
.style('stroke', function (o) {
if (isOver) {
if (o.id === d.id) {
return colorHoverLink;
} else {
return '#000';
}
} else {
return '#000';
}
});
}
function buildNodeTooltip(d, verbose) {
let headText = '';
if (verbose) {
headText = (d.name === '') ? d.type : d.type + '<hr>' + d.name;
} else {
headText = (d.name === '') ? d.type + ' (' + d.id + ')' : d.type + ' (' + d.id + ')' + '<hr>' + d.name;
}
return getTooltipHTML(d, headText, verbose);
}
function buildLinkTooltip(d, verbose) {
const headText = verbose ? d.name : d.name + ' (' + d.id + ')';
return getTooltipHTML(d, headText, verbose);
}
function getTooltipHTML(d, headText, verbose) {
if (verbose) {
const translate = VisualizerComponent.this()._translate;
let incoming = d.incoming && d.incoming.length ? d.incoming.join(', ') : '';
if (incoming.length > maxTooltipInOutLength) { incoming = incoming.substr(0, maxTooltipInOutLength - 3) + '...'; }
let outgoing = d.outgoing && d.outgoing.length ? d.outgoing.join(', ') : '';
if (outgoing.length > maxTooltipInOutLength) { outgoing = outgoing.substr(0, maxTooltipInOutLength - 3) + '...'; }
return '<div class=\'html-detailed-tooltip\'> <table class=\'ui celled striped table\'> <thead> <tr> <th colspan=\'2\'>' +
headText + '</th> </tr> </thead> <tbody> <tr> <td class=\'collapsing\'> <span>' + translate.instant('Handle') +
'</span> </td> <td>' + d.id + '</td> </tr> <tr> <td> <span>' + translate.instant('Incoming') + '</span> </td> <td>' +
incoming + '</td> </tr> <tr> <td> <span>' + translate.instant('Outgoing') + '</span> </td> <td>' + outgoing +
'</td> </tr> <tr> <td> <span>LTI</span> </td> <td>' + d.av.lti + '</td> </tr> <tr> <td> <span>STI</span> </td> <td>' +
d.av.sti + '</td> </tr> <tr> <td> <span>VLTI</span> </td> <td>' + d.av.vlti + '</td> </tr> <tr> <td> <span>' +
translate.instant('Confidence') + '</span> </td> <td>' + d.tv.details.confidence + '</td> </tr> <tr> <td> <span>' +
translate.instant('Strength') + '</span> </td> <td>' + d.tv.details.strength + '</td> </tr> </tbody> </table> </div>';
} else {
return '<div class=\'html-tooltip\'> <table class=\'ui celled striped table\'> <tbody> <tr> <td nowrap>' + headText +
'</td> </tr> </tbody> </table> </div>';
}
}
function getOutgoingLinks(d) {
const strOutLinks: string = linkedByOutgoing[d.target.id + ',' + d.source.id];
const arrOutLinks = [];
if (strOutLinks) {
arrOutLinks.push.apply(arrOutLinks, strOutLinks.split(',').map(function (str) { return Number(str); }));
}
return arrOutLinks;
}
function getSourceNodeCircumPt(d, offsetRads, isDefaultSweep) {
const r = d.source.name === '' ? radiusNodeNameless : radiusNode;
const radius = VisualizerComponent.scaleRadius(r, d.source.av.sti) + strokeWidthNode + 1;
const dx = d.source.x - d.target.x;
const dy = d.source.y - d.target.y;
const offset = isDefaultSweep ? offsetRads : -offsetRads;
const gamma = Math.atan2(dy, dx) + offset;
const tx = d.source.x - (Math.cos(gamma) * radius);
const ty = d.source.y - (Math.sin(gamma) * radius);
return [tx, ty];
}
function getTargetNodeCircumPt(d, offsetRads, isDefaultSweep) {
const r = d.target.name === '' ? radiusNodeNameless : radiusNode;
const radius = VisualizerComponent.scaleRadius(r, d.target.av.sti) + strokeWidthNode + 1;
const dx = d.target.x - d.source.x;
const dy = d.target.y - d.source.y;
const offset = isDefaultSweep ? -offsetRads : offsetRads;
const gamma = Math.atan2(dy, dx) + offset;
const tx = d.target.x - (Math.cos(gamma) * radius);
const ty = d.target.y - (Math.sin(gamma) * radius);
return [tx, ty];
}
function arcPath(d, isLeftHand, offsetRads, radiusFactor, isDefaultSweep) {
const start = isLeftHand ? { x: getSourceNodeCircumPt(d, offsetRads, isDefaultSweep)[0],
y: getSourceNodeCircumPt(d, offsetRads, isDefaultSweep)[1] } :
{ x: getTargetNodeCircumPt(d, offsetRads, isDefaultSweep)[0],
y: getTargetNodeCircumPt(d, offsetRads, isDefaultSweep)[1] };
const end = isLeftHand ? { x: getTargetNodeCircumPt(d, offsetRads, isDefaultSweep)[0],
y: getTargetNodeCircumPt(d, offsetRads, isDefaultSweep)[1] } :
{ x: getSourceNodeCircumPt(d, offsetRads, isDefaultSweep)[0],
y: getSourceNodeCircumPt(d, offsetRads, isDefaultSweep)[1] };
const dx = end.x - start.x,
dy = end.y - start.y,
dr = Math.sqrt(dx * dx + dy * dy) * radiusFactor;
let sweep = isLeftHand ? 0 : 1;
if (!isDefaultSweep) {
sweep = isLeftHand ? 1 : 0;
}
return 'M' + start.x + ',' + start.y + 'A' + dr + ',' + dr + ' 0 0,' + sweep + ' ' + end.x + ',' + end.y;
}
function straightPath(d, isLeftHand) {
const start = isLeftHand ? { x: getSourceNodeCircumPt(d, 0, true)[0], y: getSourceNodeCircumPt(d, 0, true)[1] } :
{ x: getTargetNodeCircumPt(d, 0, true)[0], y: getTargetNodeCircumPt(d, 0, true)[1] };
const end = isLeftHand ? { x: getTargetNodeCircumPt(d, 0, true)[0], y: getTargetNodeCircumPt(d, 0, true)[1] } :
{ x: getSourceNodeCircumPt(d, 0, true)[0], y: getSourceNodeCircumPt(d, 0, true)[1] };
const dx = end.x - start.x,
dy = end.y - start.y,
sweep = isLeftHand ? 1 : 0;
return 'M ' + start.x + ' ' + start.y + ' L ' + end.x + ' ' + end.y;
}
function getSizeNodeLabel(d) {
if (d.name === '') { return; }
const d3text = d3.select(this);
let radius = radiusNode;
radius = VisualizerComponent.scaleRadius(radius, d.av.sti);
const offset = Number(d3text.attr('dy'));
const textWidth = this.getComputedTextLength();
const availWidth = radius * 2 * nodeLabelPadding;
const dataScale = availWidth / textWidth;
d3text.attr('data-scale', dataScale);
}
}
public initContextMenus() {
const __this = this;
const mainMenu = [{
title: function (d) {
if (!__this.isSelectedNode) {
return __this._translate.instant('RecenterPanning');
} else {
const menutext = __this._translate.instant('PanNodeToCenter');
return menutext;
}
},
action: function(elm, d, i) {
if (!__this.isSelectedNode) {
__this.panToCenter.call(__this);
} else {
__this.panNodeToCenter.call(__this, __this.selectedNodeData);
}
}
},
{
title: __this._translate.instant('UnpinAll'),
action: function (elm, d, i) {
__this.node.each(function (o) {
o.fx = null;
o.fy = null;
});
}
}, {
title: __this._translate.instant('ResetChargeAll'),
action: function (elm, d, i) {
simulation.force('charge', d3.forceManyBody().strength(function (o) {
o.charge = simForceStrength;
return simForceStrength;
}));
if (isSimulationRunning) {
simulation.restart();
}
}
}];
const nodeMenu = [
{
title: function (d) {
__this.divTooltip.style('opacity', 0);
if (d.fx == null) {
return __this._translate.instant('Pin');
} else {
return __this._translate.instant('Unpin');
}
},
action: function (elm, d, i) {
if (d.fx == null) {
d.fx = d.x;
d.fy = d.y;
if (d3.event.shiftKey) {
simulation.force('charge', d3.forceManyBody().strength(function (o) {
return d.id === o.id ? simForceStrengthHighNodeCharge : simForceStrength;
}));
d.charge = simForceStrengthHighNodeCharge;
}
simulation.alphaTarget(0.1).restart();
} else {
d.fx = d.fy = null;
}
}
}, {
title: function (d) {
__this.divTooltip.style('opacity', 0);
if (d.charge && d.charge === simForceStrengthHighNodeCharge) {
return __this._translate.instant('RestoreCharge');
} else {
return __this._translate.instant('ApplyHighCharge');
}
},
action: function (elm, d, i) {
if (d.charge && d.charge === simForceStrengthHighNodeCharge) {
simulation.force('charge', d3.forceManyBody().strength(function (o) {
return simForceStrength;
}));
d.charge = simForceStrength;
if (isSimulationRunning) {
simulation.restart();
}
} else {
simulation.force('charge', d3.forceManyBody().strength(function (o) {
return d.id === o.id ? simForceStrengthHighNodeCharge : simForceStrength;
}));
d.charge = simForceStrengthHighNodeCharge;
if (d3.event.ctrlKey) {
d.fx = d.x;
d.fy = d.y;
}
simulation.alphaTarget(0.1).restart();
}
}
},
{
title: __this._translate.instant('PanToCenter'),
action: function(elm, d, i) {
__this.panNodeToCenter.call(__this, d);
},
}
];
return { mainMenu, nodeMenu };
}
public reinitContextMenus() {
if (this.menus) { this.menus = this.initContextMenus(); }
if (this.svg) { this.svg.on('contextmenu', d3.contextMenu(this.menus.mainMenu)); }
if (this.node) { this.node.on('contextmenu', d3.contextMenu(this.menus.nodeMenu)); }
}
public isCurrentLang(lang: string) {
return lang === this._translate.currentLang;
}
public selectLang(lang: string) {
this._translate.use(lang);
TranslateConfig.setCurrentLang(lang);
if (this.menus) { this.reinitContextMenus(); }
}
public setLanguage(lang) {
const key = lang.value.value;
this.selectLang(key);
}
}