package deeptreeecho
import (
"context"
"encoding/json"
"fmt"
"os"
"time"
"github.com/supabase-community/supabase-go"
"github.com/supabase-community/postgrest-go"
)
type SupabasePersistence struct {
client *supabase.Client
ctx    context.Context
}
type PersistentMemory struct {
ID               string                 `json:"id"`
Content          string                 `json:"content"`
MemoryType       string                 `json:"memory_type"`
Importance       float64                `json:"importance"`
EmotionalValence float64                `json:"emotional_valence"`
Timestamp        time.Time              `json:"timestamp"`
Associations     []string               `json:"associations"`
Metadata         map[string]interface{} `json:"metadata"`
ConsolidatedAt   *time.Time             `json:"consolidated_at,omitempty"`
}
type PersistentKnowledgeNode struct {
ID         string                 `json:"id"`
Label      string                 `json:"label"`
Type       string                 `json:"type"`
Properties map[string]interface{} `json:"properties"`
CreatedAt  time.Time              `json:"created_at"`
UpdatedAt  time.Time              `json:"updated_at"`
Strength   float64                `json:"strength"`
}
type PersistentKnowledgeEdge struct {
ID         string                 `json:"id"`
SourceID   string                 `json:"source_id"`
TargetID   string                 `json:"target_id"`
Relation   string                 `json:"relation"`
Weight     float64                `json:"weight"`
Properties map[string]interface{} `json:"properties"`
CreatedAt  time.Time              `json:"created_at"`
}
type IdentitySnapshot struct {
ID               string                 `json:"id"`
Timestamp        time.Time              `json:"timestamp"`
Coherence        float64                `json:"coherence"`
CoreValues       []string               `json:"core_values"`
Beliefs          map[string]float64     `json:"beliefs"`
Goals            []string               `json:"goals"`
Traits           map[string]float64     `json:"traits"`
WisdomMetrics    map[string]float64     `json:"wisdom_metrics"`
Metadata         map[string]interface{} `json:"metadata"`
}
type LearningRecord struct {
ID           string                 `json:"id"`
SkillName    string                 `json:"skill_name"`
Progress     float64                `json:"progress"`
Proficiency  float64                `json:"proficiency"`
PracticeTime time.Duration          `json:"practice_time"`
LastPractice time.Time              `json:"last_practice"`
Insights     []string               `json:"insights"`
Metadata     map[string]interface{} `json:"metadata"`
}
type DiscussionRecord struct {
ID           string                 `json:"id"`
Topic        string                 `json:"topic"`
Participants []string               `json:"participants"`
StartTime    time.Time              `json:"start_time"`
EndTime      *time.Time             `json:"end_time,omitempty"`
Messages     []PersistentDiscussionMessage    `json:"messages"`
InterestScore float64               `json:"interest_score"`
Metadata     map[string]interface{} `json:"metadata"`
}
type PersistentDiscussionMessage struct {
Speaker   string    `json:"speaker"`
Content   string    `json:"content"`
Timestamp time.Time `json:"timestamp"`
Sentiment float64   `json:"sentiment"`
}
func NewSupabasePersistence(ctx context.Context) (*SupabasePersistence, error) {
supabaseURL := os.Getenv("SUPABASE_URL")
supabaseKey := os.Getenv("SUPABASE_KEY")
if supabaseURL == "" || supabaseKey == "" {
return nil, fmt.Errorf("SUPABASE_URL and SUPABASE_KEY environment variables must be set")
}
client, err := supabase.NewClient(supabaseURL, supabaseKey, nil)
if err != nil {
return nil, fmt.Errorf("failed to create Supabase client: %w", err)
}
sp := &SupabasePersistence{
client: client,
ctx:    ctx,
}
if err := sp.initializeSchema(); err != nil {
return nil, fmt.Errorf("failed to initialize schema: %w", err)
}
return sp, nil
}
func (sp *SupabasePersistence) initializeSchema() error {
return nil
}
func (sp *SupabasePersistence) PersistMemory(memory *PersistentMemory) error {
if memory.ID == "" {
memory.ID = generatePersistenceID()
}
if memory.Timestamp.IsZero() {
memory.Timestamp = time.Now()
}
data, err := json.Marshal(memory)
if err != nil {
return fmt.Errorf("failed to marshal memory: %w", err)
}
_, _, err = sp.client.From("memories").Insert(data, false, "", "", "").Execute()
if err != nil {
return fmt.Errorf("failed to insert memory: %w", err)
}
return nil
}
func (sp *SupabasePersistence) RetrieveRelevantMemories(context string, limit int) ([]*PersistentMemory, error) {
var results []PersistentMemory
data, _, err := sp.client.From("memories").
Select("*", "", false).
Ilike("content", fmt.Sprintf("%%%s%%", context)).
Order("importance", &postgrest.OrderOpts{Ascending: false}).
Limit(limit, "").
Execute()
if err == nil && data != nil {
err = json.Unmarshal(data, &results)
}
if err != nil {
return nil, fmt.Errorf("failed to retrieve memories: %w", err)
}
memories := make([]*PersistentMemory, len(results))
for i := range results {
memories[i] = &results[i]
}
return memories, nil
}
func (sp *SupabasePersistence) UpdateKnowledgeGraph(nodes []*PersistentKnowledgeNode, edges []*PersistentKnowledgeEdge) error {
for _, node := range nodes {
if node.ID == "" {
node.ID = generatePersistenceID()
}
if node.CreatedAt.IsZero() {
node.CreatedAt = time.Now()
}
node.UpdatedAt = time.Now()
data, err := json.Marshal(node)
if err != nil {
return fmt.Errorf("failed to marshal node: %w", err)
}
_, _, err = sp.client.From("knowledge_nodes").Upsert(data, "id", "", "").Execute()
if err != nil {
return fmt.Errorf("failed to upsert node: %w", err)
}
}
for _, edge := range edges {
if edge.ID == "" {
edge.ID = generatePersistenceID()
}
if edge.CreatedAt.IsZero() {
edge.CreatedAt = time.Now()
}
data, err := json.Marshal(edge)
if err != nil {
return fmt.Errorf("failed to marshal edge: %w", err)
}
_, _, err = sp.client.From("knowledge_edges").Upsert(data, "id", "", "").Execute()
if err != nil {
return fmt.Errorf("failed to upsert edge: %w", err)
}
}
return nil
}
func (sp *SupabasePersistence) SaveIdentitySnapshot(snapshot *IdentitySnapshot) error {
if snapshot.ID == "" {
snapshot.ID = generatePersistenceID()
}
if snapshot.Timestamp.IsZero() {
snapshot.Timestamp = time.Now()
}
data, err := json.Marshal(snapshot)
if err != nil {
return fmt.Errorf("failed to marshal snapshot: %w", err)
}
_, _, err = sp.client.From("identity_snapshots").Insert(data, false, "", "", "").Execute()
if err != nil {
return fmt.Errorf("failed to insert snapshot: %w", err)
}
return nil
}
func (sp *SupabasePersistence) LoadLatestIdentity() (*IdentitySnapshot, error) {
var results []IdentitySnapshot
data, _, err := sp.client.From("identity_snapshots").
Select("*", "", false).
Order("timestamp", &postgrest.OrderOpts{Ascending: false}).
Limit(1, "").
Execute()
if err == nil && data != nil {
err = json.Unmarshal(data, &results)
}
if err != nil {
return nil, fmt.Errorf("failed to load identity: %w", err)
}
if len(results) == 0 {
return nil, nil
}
return &results[0], nil
}
func (sp *SupabasePersistence) TrackLearning(record *LearningRecord) error {
if record.ID == "" {
record.ID = generatePersistenceID()
}
if record.LastPractice.IsZero() {
record.LastPractice = time.Now()
}
data, err := json.Marshal(record)
if err != nil {
return fmt.Errorf("failed to marshal learning record: %w", err)
}
_, _, err = sp.client.From("learning_records").Upsert(data, "skill_name", "", "").Execute()
if err != nil {
return fmt.Errorf("failed to upsert learning record: %w", err)
}
return nil
}
func (sp *SupabasePersistence) GetLearningProgress(skillName string) (*LearningRecord, error) {
var results []LearningRecord
data, _, err := sp.client.From("learning_records").
Select("*", "", false).
Eq("skill_name", skillName).
Limit(1, "").
Execute()
if err == nil && data != nil {
err = json.Unmarshal(data, &results)
}
if err != nil {
return nil, fmt.Errorf("failed to get learning progress: %w", err)
}
if len(results) == 0 {
return nil, nil
}
return &results[0], nil
}
func (sp *SupabasePersistence) SaveDiscussion(discussion *DiscussionRecord) error {
if discussion.ID == "" {
discussion.ID = generatePersistenceID()
}
if discussion.StartTime.IsZero() {
discussion.StartTime = time.Now()
}
data, err := json.Marshal(discussion)
if err != nil {
return fmt.Errorf("failed to marshal discussion: %w", err)
}
_, _, err = sp.client.From("discussions").Upsert(data, "id", "", "").Execute()
if err != nil {
return fmt.Errorf("failed to upsert discussion: %w", err)
}
return nil
}
func (sp *SupabasePersistence) GetRecentDiscussions(limit int) ([]*DiscussionRecord, error) {
var results []DiscussionRecord
data, _, err := sp.client.From("discussions").
Select("*", "", false).
Order("start_time", &postgrest.OrderOpts{Ascending: false}).
Limit(limit, "").
Execute()
if err == nil && data != nil {
err = json.Unmarshal(data, &results)
}
if err != nil {
return nil, fmt.Errorf("failed to get discussions: %w", err)
}
discussions := make([]*DiscussionRecord, len(results))
for i := range results {
discussions[i] = &results[i]
}
return discussions, nil
}
func (sp *SupabasePersistence) ConsolidateMemories(memoryIDs []string) error {
now := time.Now()
for _, id := range memoryIDs {
update := map[string]interface{}{
"consolidated_at": now,
}
data, err := json.Marshal(update)
if err != nil {
return fmt.Errorf("failed to marshal update: %w", err)
}
_, _, err = sp.client.From("memories").
Update(data, "", "").
Eq("id", id).
Execute()
if err != nil {
return fmt.Errorf("failed to consolidate memory %s: %w", id, err)
}
}
return nil
}
func (sp *SupabasePersistence) GetWisdomMetricsHistory(limit int) ([]IdentitySnapshot, error) {
var results []IdentitySnapshot
data, _, err := sp.client.From("identity_snapshots").
Select("timestamp,wisdom_metrics", "", false).
Order("timestamp", &postgrest.OrderOpts{Ascending: false}).
Limit(limit, "").
Execute()
if err == nil && data != nil {
err = json.Unmarshal(data, &results)
}
if err != nil {
return nil, fmt.Errorf("failed to get wisdom metrics: %w", err)
}
return results, nil
}
func generatePersistenceID() string {
return fmt.Sprintf("%d-%d", time.Now().UnixNano(), time.Now().Unix())
}