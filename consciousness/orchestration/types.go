package orchestration
import (
"context"
"time"
"github.com/EchoCog/echollama/api"
)
type Agent struct {
ID          string                 `json:"id"`
Name        string                 `json:"name"`
Description string                 `json:"description"`
Models      []string               `json:"models"`
Config      map[string]interface{} `json:"config"`
Type        AgentType              `json:"type"`
State       *AgentState            `json:"state,omitempty"`
Tools       []string               `json:"tools,omitempty"`
CreatedAt   time.Time              `json:"created_at"`
UpdatedAt   time.Time              `json:"updated_at"`
}
type AgentType string
const (
AgentTypeGeneral     AgentType = "general"
AgentTypeSpecialist  AgentType = "specialist"
AgentTypeOrchestrator AgentType = "orchestrator"
AgentTypeReflective  AgentType = "reflective"
)
type AgentState struct {
Memory         map[string]interface{} `json:"memory,omitempty"`
Context        []ContextItem          `json:"context,omitempty"`
Goals          []string               `json:"goals,omitempty"`
Capabilities   []string               `json:"capabilities,omitempty"`
LastInteraction time.Time             `json:"last_interaction"`
}
type ContextItem struct {
Key       string      `json:"key"`
Value     interface{} `json:"value"`
Timestamp time.Time   `json:"timestamp"`
Relevance float64     `json:"relevance"`
}
type Task struct {
ID          string                 `json:"id"`
Type        string                 `json:"type"`
Input       string                 `json:"input"`
Output      string                 `json:"output,omitempty"`
Status      string                 `json:"status"`
AgentID     string                 `json:"agent_id"`
ModelName   string                 `json:"model_name,omitempty"`
Parameters  map[string]interface{} `json:"parameters,omitempty"`
CreatedAt   time.Time              `json:"created_at"`
CompletedAt *time.Time             `json:"completed_at,omitempty"`
Error       string                 `json:"error,omitempty"`
}
const (
TaskStatusPending   = "pending"
TaskStatusRunning   = "running"
TaskStatusCompleted = "completed"
TaskStatusFailed    = "failed"
)
const (
TaskTypeGenerate = "generate"
TaskTypeChat     = "chat"
TaskTypeEmbed    = "embed"
TaskTypeCustom   = "custom"
TaskTypeTool     = "tool"
TaskTypeReflect  = "reflect"
TaskTypePlugin   = "plugin"
)
type ToolCall struct {
Name       string                 `json:"name"`
Parameters map[string]interface{} `json:"parameters"`
Timeout    time.Duration          `json:"timeout,omitempty"`
}
type ToolResult struct {
Success bool        `json:"success"`
Output  interface{} `json:"output"`
Error   string      `json:"error,omitempty"`
}
type Plugin interface {
Name() string
Description() string
Execute(ctx context.Context, input string, params map[string]interface{}) (interface{}, error)
}
type PluginRegistry struct {
plugins map[string]Plugin
}
type Tool interface {
Name() string
Description() string
Call(ctx context.Context, params map[string]interface{}) (*ToolResult, error)
}
type OrchestrationRequest struct {
AgentID     string                 `json:"agent_id"`
Tasks       []TaskRequest          `json:"tasks"`
Sequential  bool                   `json:"sequential"`
Parameters  map[string]interface{} `json:"parameters,omitempty"`
Stream      *bool                  `json:"stream,omitempty"`
KeepAlive   *api.Duration          `json:"keep_alive,omitempty"`
}
type TaskRequest struct {
Type       string                 `json:"type"`
Input      string                 `json:"input"`
ModelName  string                 `json:"model_name,omitempty"`
Parameters map[string]interface{} `json:"parameters,omitempty"`
}
type OrchestrationResponse struct {
ID        string `json:"id"`
AgentID   string `json:"agent_id"`
Status    string `json:"status"`
Tasks     []Task `json:"tasks"`
Results   []TaskResult `json:"results,omitempty"`
Error     string `json:"error,omitempty"`
CreatedAt time.Time `json:"created_at"`
}
type TaskResult struct {
TaskID    string `json:"task_id"`
Output    string `json:"output"`
ModelUsed string `json:"model_used,omitempty"`
Metrics   TaskMetrics `json:"metrics,omitempty"`
}
type TaskMetrics struct {
Duration     time.Duration `json:"duration"`
TokensUsed   int           `json:"tokens_used,omitempty"`
PromptTokens int           `json:"prompt_tokens,omitempty"`
OutputTokens int           `json:"output_tokens,omitempty"`
}
type AgentManager interface {
CreateAgent(ctx context.Context, agent *Agent) error
GetAgent(ctx context.Context, id string) (*Agent, error)
ListAgents(ctx context.Context) ([]*Agent, error)
UpdateAgent(ctx context.Context, agent *Agent) error
DeleteAgent(ctx context.Context, id string) error
}
type TaskExecutor interface {
ExecuteTask(ctx context.Context, task *Task, agent *Agent) (*TaskResult, error)
ExecuteTasks(ctx context.Context, tasks []*Task, agent *Agent, sequential bool) ([]*TaskResult, error)
}
type Orchestrator interface {
AgentManager
TaskExecutor
OrchestrateTasks(ctx context.Context, req *OrchestrationRequest) (*OrchestrationResponse, error)
}
type Message struct {
ID          string                 `json:"id"`
FromAgentID string                 `json:"from_agent_id"`
ToAgentID   string                 `json:"to_agent_id"`
Content     string                 `json:"content"`
Type        MessageType            `json:"type"`
Context     map[string]interface{} `json:"context,omitempty"`
Timestamp   time.Time              `json:"timestamp"`
Response    *Message               `json:"response,omitempty"`
}
type MessageType string
const (
MessageTypeRequest     MessageType = "request"
MessageTypeResponse    MessageType = "response"
MessageTypeNotification MessageType = "notification"
MessageTypeTask        MessageType = "task"
MessageTypeReflection  MessageType = "reflection"
MessageTypeBroadcast   MessageType = "broadcast"
)
type Conversation struct {
ID           string                 `json:"id"`
Participants []string               `json:"participants"`
Messages     []Message              `json:"messages"`
Status       ConversationStatus     `json:"status"`
Topic        string                 `json:"topic,omitempty"`
Metadata     map[string]interface{} `json:"metadata,omitempty"`
CreatedAt    time.Time              `json:"created_at"`
UpdatedAt    time.Time              `json:"updated_at"`
}
type ConversationStatus string
const (
ConversationStatusActive   ConversationStatus = "active"
ConversationStatusPaused   ConversationStatus = "paused"
ConversationStatusClosed   ConversationStatus = "closed"
ConversationStatusArchived ConversationStatus = "archived"
)
type ConversationManager interface {
StartConversation(ctx context.Context, participants []string, topic string) (*Conversation, error)
SendMessage(ctx context.Context, conversationID string, message *Message) error
GetConversation(ctx context.Context, id string) (*Conversation, error)
ListConversations(ctx context.Context, agentID string) ([]*Conversation, error)
CloseConversation(ctx context.Context, id string) error
}
type ConversationWorkflow struct {
ID           string                   `json:"id"`
Name         string                   `json:"name"`
Description  string                   `json:"description"`
Participants []string                 `json:"participants"`
Steps        []ConversationStep       `json:"steps"`
Status       ConversationStatus       `json:"status"`
Result       *ConversationWorkflowResult `json:"result,omitempty"`
CreatedAt    time.Time                `json:"created_at"`
}
type ConversationStep struct {
ID             string                 `json:"id"`
Name           string                 `json:"name"`
FromAgentID    string                 `json:"from_agent_id"`
ToAgentID      string                 `json:"to_agent_id"`
MessageTemplate string                `json:"message_template"`
ExpectedResponse string               `json:"expected_response,omitempty"`
Timeout         time.Duration          `json:"timeout,omitempty"`
Parameters      map[string]interface{} `json:"parameters,omitempty"`
}
type ConversationWorkflowResult struct {
Success        bool                        `json:"success"`
StepResults    []ConversationStepResult    `json:"step_results"`
FinalOutcome   string                      `json:"final_outcome"`
Insights       []string                    `json:"insights,omitempty"`
Duration       time.Duration               `json:"duration"`
Error          string                      `json:"error,omitempty"`
}
type ConversationStepResult struct {
StepID       string        `json:"step_id"`
Message      *Message      `json:"message"`
Response     *Message      `json:"response,omitempty"`
Success      bool          `json:"success"`
Duration     time.Duration `json:"duration"`
Error        string        `json:"error,omitempty"`
}