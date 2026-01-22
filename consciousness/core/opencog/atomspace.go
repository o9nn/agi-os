package opencog
import (
"fmt"
"math"
"sync"
"time"
)
type AtomSpace struct {
mu sync.RWMutex
Atoms       map[string]*Atom
Links       map[string]*Link
Incoming    map[string][]string
AttentionBank *AttentionBank
TruthValueCache map[string]*TruthValue
PatternMatcher *PatternMatcher
Created   time.Time
Modified  time.Time
AtomCount int64
LinkCount int64
}
type Atom struct {
ID        string
Type      AtomType
Name      string
TruthValue *TruthValue
Attention  *AttentionValue
Incoming   []string
Created    time.Time
Modified   time.Time
}
type Link struct {
ID         string
Type       LinkType
Outgoing   []string
TruthValue *TruthValue
Attention  *AttentionValue
Created    time.Time
Modified   time.Time
}
type AtomType string
const (
ConceptNode     AtomType = "ConceptNode"
PredicateNode   AtomType = "PredicateNode"
VariableNode    AtomType = "VariableNode"
NumberNode      AtomType = "NumberNode"
SchemaNode      AtomType = "SchemaNode"
GroundedSchemaNode AtomType = "GroundedSchemaNode"
)
type LinkType string
const (
InheritanceLink    LinkType = "InheritanceLink"
SimilarityLink     LinkType = "SimilarityLink"
EvaluationLink     LinkType = "EvaluationLink"
MemberLink         LinkType = "MemberLink"
ListLink           LinkType = "ListLink"
ExecutionLink      LinkType = "ExecutionLink"
ImplicationLink    LinkType = "ImplicationLink"
EquivalenceLink    LinkType = "EquivalenceLink"
SubsetLink         LinkType = "SubsetLink"
AndLink            LinkType = "AndLink"
OrLink             LinkType = "OrLink"
NotLink            LinkType = "NotLink"
)
type TruthValue struct {
Strength    float64
Confidence  float64
Count       float64
}
type AttentionValue struct {
STI  int16
LTI  int16
VLTI int16
AF   float64
}
type AttentionBank struct {
mu sync.RWMutex
STIFunds    int64
LTIFunds    int64
AFBoundary  float64
STIHeap     *ImportanceHeap
LTIHeap     *ImportanceHeap
ForgettingRate float64
MinAF          float64
MaxAF          float64
}
type ImportanceHeap struct {
atoms    []string
values   map[string]int16
}
type PatternMatcher struct {
mu sync.RWMutex
Templates map[string]*Pattern
QueryCache map[string]*QueryResult
CacheSize  int
}
type Pattern struct {
Variables map[string]*Variable
Clauses   []Clause
Type      PatternType
}
type PatternType string
const (
BindPattern      PatternType = "BindPattern"
GetPattern       PatternType = "GetPattern"
SatisfactionPattern PatternType = "SatisfactionPattern"
)
type Variable struct {
Name  string
Type  AtomType
Constraints []Constraint
}
type Constraint struct {
Type  ConstraintType
Value interface{}
}
type ConstraintType string
const (
TypeConstraint      ConstraintType = "TypeConstraint"
TruthValueConstraint ConstraintType = "TruthValueConstraint"
AttentionConstraint  ConstraintType = "AttentionConstraint"
)
type Clause struct {
LinkType LinkType
Atoms    []string
}
type QueryResult struct {
Bindings  []map[string]string
Timestamp time.Time
Count     int
}
func NewAtomSpace() *AtomSpace {
return &AtomSpace{
Atoms:           make(map[string]*Atom),
Links:           make(map[string]*Link),
Incoming:        make(map[string][]string),
TruthValueCache: make(map[string]*TruthValue),
AttentionBank:   NewAttentionBank(),
PatternMatcher:  NewPatternMatcher(),
Created:         time.Now(),
Modified:        time.Now(),
}
}
func NewAttentionBank() *AttentionBank {
return &AttentionBank{
STIFunds:       100000,
LTIFunds:       100000,
AFBoundary:     0.5,
STIHeap:        &ImportanceHeap{atoms: []string{}, values: make(map[string]int16)},
LTIHeap:        &ImportanceHeap{atoms: []string{}, values: make(map[string]int16)},
ForgettingRate: 0.01,
MinAF:          0.0,
MaxAF:          1.0,
}
}
func NewPatternMatcher() *PatternMatcher {
return &PatternMatcher{
Templates:  make(map[string]*Pattern),
QueryCache: make(map[string]*QueryResult),
CacheSize:  1000,
}
}
func (as *AtomSpace) AddAtom(atomType AtomType, name string, tv *TruthValue) (*Atom, error) {
as.mu.Lock()
defer as.mu.Unlock()
id := fmt.Sprintf("atom_%s_%d", name, time.Now().UnixNano())
if tv == nil {
tv = &TruthValue{Strength: 1.0, Confidence: 0.0, Count: 0.0}
}
atom := &Atom{
ID:         id,
Type:       atomType,
Name:       name,
TruthValue: tv,
Attention: &AttentionValue{
STI:  0,
LTI:  0,
VLTI: 0,
AF:   0.0,
},
Incoming: []string{},
Created:  time.Now(),
Modified: time.Now(),
}
as.Atoms[id] = atom
as.AtomCount++
as.Modified = time.Now()
as.AttentionBank.RegisterAtom(id, atom.Attention)
return atom, nil
}
func (as *AtomSpace) AddLink(linkType LinkType, outgoing []string, tv *TruthValue) (*Link, error) {
as.mu.Lock()
defer as.mu.Unlock()
for _, atomID := range outgoing {
if _, exists := as.Atoms[atomID]; !exists {
if _, linkExists := as.Links[atomID]; !linkExists {
return nil, fmt.Errorf("atom or link %s not found", atomID)
}
}
}
id := fmt.Sprintf("link_%s_%d", linkType, time.Now().UnixNano())
if tv == nil {
tv = &TruthValue{Strength: 1.0, Confidence: 0.0, Count: 0.0}
}
link := &Link{
ID:         id,
Type:       linkType,
Outgoing:   outgoing,
TruthValue: tv,
Attention: &AttentionValue{
STI:  0,
LTI:  0,
VLTI: 0,
AF:   0.0,
},
Created:  time.Now(),
Modified: time.Now(),
}
as.Links[id] = link
as.LinkCount++
as.Modified = time.Now()
for _, atomID := range outgoing {
as.Incoming[atomID] = append(as.Incoming[atomID], id)
if atom, exists := as.Atoms[atomID]; exists {
atom.Incoming = append(atom.Incoming, id)
}
}
as.AttentionBank.RegisterAtom(id, link.Attention)
return link, nil
}
func (as *AtomSpace) GetAtom(id string) (*Atom, bool) {
as.mu.RLock()
defer as.mu.RUnlock()
atom, exists := as.Atoms[id]
return atom, exists
}
func (as *AtomSpace) GetLink(id string) (*Link, bool) {
as.mu.RLock()
defer as.mu.RUnlock()
link, exists := as.Links[id]
return link, exists
}
func (as *AtomSpace) GetIncoming(atomID string) []string {
as.mu.RLock()
defer as.mu.RUnlock()
return as.Incoming[atomID]
}
func (as *AtomSpace) UpdateTruthValue(id string, tv *TruthValue) error {
as.mu.Lock()
defer as.mu.Unlock()
if atom, exists := as.Atoms[id]; exists {
atom.TruthValue = tv
atom.Modified = time.Now()
as.Modified = time.Now()
return nil
}
if link, exists := as.Links[id]; exists {
link.TruthValue = tv
link.Modified = time.Now()
as.Modified = time.Now()
return nil
}
return fmt.Errorf("atom or link %s not found", id)
}
func (as *AtomSpace) SpreadAttention() {
as.mu.Lock()
defer as.mu.Unlock()
for _, link := range as.Links {
strength := link.TruthValue.Strength
sourceSTI := link.Attention.STI
for _, targetID := range link.Outgoing {
if atom, exists := as.Atoms[targetID]; exists {
transfer := int16(float64(sourceSTI) * strength * 0.1)
atom.Attention.STI += transfer
link.Attention.STI -= transfer
}
}
}
as.AttentionBank.Update()
}
func (as *AtomSpace) Forget() {
as.mu.Lock()
defer as.mu.Unlock()
forgettingThreshold := as.AttentionBank.AFBoundary * as.AttentionBank.ForgettingRate
for id, atom := range as.Atoms {
if atom.Attention.AF < forgettingThreshold {
delete(as.Atoms, id)
delete(as.Incoming, id)
as.AttentionBank.UnregisterAtom(id)
as.AtomCount--
}
}
for id, link := range as.Links {
hasOrphan := false
for _, atomID := range link.Outgoing {
if _, exists := as.Atoms[atomID]; !exists {
hasOrphan = true
break
}
}
if hasOrphan {
delete(as.Links, id)
as.AttentionBank.UnregisterAtom(id)
as.LinkCount--
}
}
as.Modified = time.Now()
}
func (as *AtomSpace) Query(pattern *Pattern) (*QueryResult, error) {
as.mu.RLock()
defer as.mu.RUnlock()
return as.PatternMatcher.Match(as, pattern)
}
func (ab *AttentionBank) RegisterAtom(id string, av *AttentionValue) {
ab.mu.Lock()
defer ab.mu.Unlock()
ab.STIHeap.values[id] = av.STI
ab.LTIHeap.values[id] = av.LTI
}
func (ab *AttentionBank) UnregisterAtom(id string) {
ab.mu.Lock()
defer ab.mu.Unlock()
delete(ab.STIHeap.values, id)
delete(ab.LTIHeap.values, id)
}
func (ab *AttentionBank) Update() {
ab.mu.Lock()
defer ab.mu.Unlock()
ab.STIHeap.atoms = make([]string, 0, len(ab.STIHeap.values))
for id := range ab.STIHeap.values {
ab.STIHeap.atoms = append(ab.STIHeap.atoms, id)
}
ab.LTIHeap.atoms = make([]string, 0, len(ab.LTIHeap.values))
for id := range ab.LTIHeap.values {
ab.LTIHeap.atoms = append(ab.LTIHeap.atoms, id)
}
}
func (pm *PatternMatcher) Match(as *AtomSpace, pattern *Pattern) (*QueryResult, error) {
pm.mu.Lock()
defer pm.mu.Unlock()
result := &QueryResult{
Bindings:  []map[string]string{},
Timestamp: time.Now(),
Count:     0,
}
for _, clause := range pattern.Clauses {
matches := pm.matchClause(as, clause, pattern.Variables)
if len(matches) > 0 {
result.Bindings = append(result.Bindings, matches...)
result.Count += len(matches)
}
}
return result, nil
}
func (pm *PatternMatcher) matchClause(as *AtomSpace, clause Clause, variables map[string]*Variable) []map[string]string {
matches := []map[string]string{}
for _, link := range as.Links {
if link.Type == clause.LinkType {
binding := make(map[string]string)
matched := true
if len(link.Outgoing) == len(clause.Atoms) {
for i, patternAtom := range clause.Atoms {
if _, isVar := variables[patternAtom]; isVar {
binding[patternAtom] = link.Outgoing[i]
} else {
if link.Outgoing[i] != patternAtom {
matched = false
break
}
}
}
if matched {
matches = append(matches, binding)
}
}
}
}
return matches
}
func ComputeTruthValue(tv1, tv2 *TruthValue, operation string) *TruthValue {
switch operation {
case "and":
return &TruthValue{
Strength:   tv1.Strength * tv2.Strength,
Confidence: math.Min(tv1.Confidence, tv2.Confidence),
Count:      tv1.Count + tv2.Count,
}
case "or":
return &TruthValue{
Strength:   tv1.Strength + tv2.Strength - tv1.Strength*tv2.Strength,
Confidence: math.Min(tv1.Confidence, tv2.Confidence),
Count:      tv1.Count + tv2.Count,
}
case "not":
return &TruthValue{
Strength:   1.0 - tv1.Strength,
Confidence: tv1.Confidence,
Count:      tv1.Count,
}
default:
return &TruthValue{
Strength:   (tv1.Strength + tv2.Strength) / 2.0,
Confidence: (tv1.Confidence + tv2.Confidence) / 2.0,
Count:      tv1.Count + tv2.Count,
}
}
}
func (as *AtomSpace) GetStatus() map[string]interface{} {
as.mu.RLock()
defer as.mu.RUnlock()
return map[string]interface{}{
"atoms":        as.AtomCount,
"links":        as.LinkCount,
"created":      as.Created,
"modified":     as.Modified,
"sti_funds":    as.AttentionBank.STIFunds,
"lti_funds":    as.AttentionBank.LTIFunds,
"af_boundary":  as.AttentionBank.AFBoundary,
}
}