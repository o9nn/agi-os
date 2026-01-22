package api
import (
"encoding/json"
"fmt"
"log/slog"
"math"
"os"
"reflect"
"strconv"
"strings"
"time"
"github.com/EchoCog/echollama/envconfig"
"github.com/EchoCog/echollama/types/model"
)
type StatusError struct {
StatusCode   int
Status       string
ErrorMessage string `json:"error"`
}
func (e StatusError) Error() string {
switch {
case e.Status != "" && e.ErrorMessage != "":
return fmt.Sprintf("%s: %s", e.Status, e.ErrorMessage)
case e.Status != "":
return e.Status
case e.ErrorMessage != "":
return e.ErrorMessage
default:
return "something went wrong, please see the ollama server logs for details"
}
}
type ImageData []byte
type GenerateRequest struct {
Model string `json:"model"`
Prompt string `json:"prompt"`
Suffix string `json:"suffix"`
System string `json:"system"`
Template string `json:"template"`
Context []int `json:"context,omitempty"`
Stream *bool `json:"stream,omitempty"`
Raw bool `json:"raw,omitempty"`
Format json.RawMessage `json:"format,omitempty"`
KeepAlive *Duration `json:"keep_alive,omitempty"`
Images []ImageData `json:"images,omitempty"`
Options map[string]any `json:"options"`
Think *ThinkValue `json:"think,omitempty"`
}
type ChatRequest struct {
Model string `json:"model"`
Messages []Message `json:"messages"`
Stream *bool `json:"stream,omitempty"`
Format json.RawMessage `json:"format,omitempty"`
KeepAlive *Duration `json:"keep_alive,omitempty"`
Tools `json:"tools,omitempty"`
Options map[string]any `json:"options"`
Think *ThinkValue `json:"think,omitempty"`
}
type Tools []Tool
func (t Tools) String() string {
bts, _ := json.Marshal(t)
return string(bts)
}
func (t Tool) String() string {
bts, _ := json.Marshal(t)
return string(bts)
}
type Message struct {
Role    string `json:"role"`
Content string `json:"content"`
Thinking  string      `json:"thinking,omitempty"`
Images    []ImageData `json:"images,omitempty"`
ToolCalls []ToolCall  `json:"tool_calls,omitempty"`
ToolName  string      `json:"tool_name,omitempty"`
}
func (m *Message) UnmarshalJSON(b []byte) error {
type Alias Message
var a Alias
if err := json.Unmarshal(b, &a); err != nil {
return err
}
*m = Message(a)
m.Role = strings.ToLower(m.Role)
return nil
}
type ToolCall struct {
Function ToolCallFunction `json:"function"`
}
type ToolCallFunction struct {
Index     int                       `json:"index,omitempty"`
Name      string                    `json:"name"`
Arguments ToolCallFunctionArguments `json:"arguments"`
}
type ToolCallFunctionArguments map[string]any
func (t *ToolCallFunctionArguments) String() string {
bts, _ := json.Marshal(t)
return string(bts)
}
type Tool struct {
Type     string       `json:"type"`
Items    any          `json:"items,omitempty"`
Function ToolFunction `json:"function"`
}
type PropertyType []string
func (pt *PropertyType) UnmarshalJSON(data []byte) error {
var s string
if err := json.Unmarshal(data, &s); err == nil {
*pt = []string{s}
return nil
}
var a []string
if err := json.Unmarshal(data, &a); err != nil {
return err
}
*pt = a
return nil
}
func (pt PropertyType) MarshalJSON() ([]byte, error) {
if len(pt) == 1 {
return json.Marshal(pt[0])
}
return json.Marshal([]string(pt))
}
func (pt PropertyType) String() string {
if len(pt) == 0 {
return ""
}
if len(pt) == 1 {
return pt[0]
}
return fmt.Sprintf("%v", []string(pt))
}
type ToolProperty struct {
AnyOf       []ToolProperty `json:"anyOf,omitempty"`
Type        PropertyType   `json:"type"`
Items       any            `json:"items,omitempty"`
Description string         `json:"description"`
Enum        []any          `json:"enum,omitempty"`
}
func (tp ToolProperty) ToTypeScriptType() string {
if len(tp.AnyOf) > 0 {
var types []string
for _, anyOf := range tp.AnyOf {
types = append(types, anyOf.ToTypeScriptType())
}
return strings.Join(types, " | ")
}
if len(tp.Type) == 0 {
return "any"
}
if len(tp.Type) == 1 {
return mapToTypeScriptType(tp.Type[0])
}
var types []string
for _, t := range tp.Type {
types = append(types, mapToTypeScriptType(t))
}
return strings.Join(types, " | ")
}
func mapToTypeScriptType(jsonType string) string {
switch jsonType {
case "string":
return "string"
case "number", "integer":
return "number"
case "boolean":
return "boolean"
case "array":
return "any[]"
case "object":
return "Record<string, any>"
case "null":
return "null"
default:
return "any"
}
}
type ToolFunction struct {
Name        string `json:"name"`
Description string `json:"description"`
Parameters  struct {
Type       string                  `json:"type"`
Defs       any                     `json:"$defs,omitempty"`
Items      any                     `json:"items,omitempty"`
Required   []string                `json:"required"`
Properties map[string]ToolProperty `json:"properties"`
} `json:"parameters"`
}
func (t *ToolFunction) String() string {
bts, _ := json.Marshal(t)
return string(bts)
}
type ChatResponse struct {
Model      string    `json:"model"`
CreatedAt  time.Time `json:"created_at"`
Message    Message   `json:"message"`
DoneReason string    `json:"done_reason,omitempty"`
Done bool `json:"done"`
Metrics
}
type Metrics struct {
TotalDuration      time.Duration `json:"total_duration,omitempty"`
LoadDuration       time.Duration `json:"load_duration,omitempty"`
PromptEvalCount    int           `json:"prompt_eval_count,omitempty"`
PromptEvalDuration time.Duration `json:"prompt_eval_duration,omitempty"`
EvalCount          int           `json:"eval_count,omitempty"`
EvalDuration       time.Duration `json:"eval_duration,omitempty"`
}
type Options struct {
Runner
NumKeep          int      `json:"num_keep,omitempty"`
Seed             int      `json:"seed,omitempty"`
NumPredict       int      `json:"num_predict,omitempty"`
TopK             int      `json:"top_k,omitempty"`
TopP             float32  `json:"top_p,omitempty"`
MinP             float32  `json:"min_p,omitempty"`
TypicalP         float32  `json:"typical_p,omitempty"`
RepeatLastN      int      `json:"repeat_last_n,omitempty"`
Temperature      float32  `json:"temperature,omitempty"`
RepeatPenalty    float32  `json:"repeat_penalty,omitempty"`
PresencePenalty  float32  `json:"presence_penalty,omitempty"`
FrequencyPenalty float32  `json:"frequency_penalty,omitempty"`
Stop             []string `json:"stop,omitempty"`
}
type Runner struct {
NumCtx    int   `json:"num_ctx,omitempty"`
NumBatch  int   `json:"num_batch,omitempty"`
NumGPU    int   `json:"num_gpu,omitempty"`
MainGPU   int   `json:"main_gpu,omitempty"`
UseMMap   *bool `json:"use_mmap,omitempty"`
NumThread int   `json:"num_thread,omitempty"`
}
type EmbedRequest struct {
Model string `json:"model"`
Input any `json:"input"`
KeepAlive *Duration `json:"keep_alive,omitempty"`
Truncate *bool `json:"truncate,omitempty"`
Options map[string]any `json:"options"`
}
type EmbedResponse struct {
Model      string      `json:"model"`
Embeddings [][]float32 `json:"embeddings"`
TotalDuration   time.Duration `json:"total_duration,omitempty"`
LoadDuration    time.Duration `json:"load_duration,omitempty"`
PromptEvalCount int           `json:"prompt_eval_count,omitempty"`
}
type EmbeddingRequest struct {
Model string `json:"model"`
Prompt string `json:"prompt"`
KeepAlive *Duration `json:"keep_alive,omitempty"`
Options map[string]any `json:"options"`
}
type EmbeddingResponse struct {
Embedding []float64 `json:"embedding"`
}
type CreateRequest struct {
Model    string `json:"model"`
Stream   *bool  `json:"stream,omitempty"`
Quantize string `json:"quantize,omitempty"`
From       string            `json:"from,omitempty"`
Files      map[string]string `json:"files,omitempty"`
Adapters   map[string]string `json:"adapters,omitempty"`
Template   string            `json:"template,omitempty"`
License    any               `json:"license,omitempty"`
System     string            `json:"system,omitempty"`
Parameters map[string]any    `json:"parameters,omitempty"`
Messages   []Message         `json:"messages,omitempty"`
Name string `json:"name"`
Quantization string `json:"quantization,omitempty"`
}
type DeleteRequest struct {
Model string `json:"model"`
Name string `json:"name"`
}
type ShowRequest struct {
Model  string `json:"model"`
System string `json:"system"`
Template string `json:"template"`
Verbose  bool   `json:"verbose"`
Options map[string]any `json:"options"`
Name string `json:"name"`
}
type ShowResponse struct {
License       string             `json:"license,omitempty"`
Modelfile     string             `json:"modelfile,omitempty"`
Parameters    string             `json:"parameters,omitempty"`
Template      string             `json:"template,omitempty"`
System        string             `json:"system,omitempty"`
Details       ModelDetails       `json:"details,omitempty"`
Messages      []Message          `json:"messages,omitempty"`
ModelInfo     map[string]any     `json:"model_info,omitempty"`
ProjectorInfo map[string]any     `json:"projector_info,omitempty"`
Tensors       []Tensor           `json:"tensors,omitempty"`
Capabilities  []model.Capability `json:"capabilities,omitempty"`
ModifiedAt    time.Time          `json:"modified_at,omitempty"`
}
type CopyRequest struct {
Source      string `json:"source"`
Destination string `json:"destination"`
}
type PullRequest struct {
Model    string `json:"model"`
Insecure bool   `json:"insecure,omitempty"`
Username string `json:"username"`
Password string `json:"password"`
Stream   *bool  `json:"stream,omitempty"`
Name string `json:"name"`
}
type ProgressResponse struct {
Status    string `json:"status"`
Digest    string `json:"digest,omitempty"`
Total     int64  `json:"total,omitempty"`
Completed int64  `json:"completed,omitempty"`
}
type PushRequest struct {
Model    string `json:"model"`
Insecure bool   `json:"insecure,omitempty"`
Username string `json:"username"`
Password string `json:"password"`
Stream   *bool  `json:"stream,omitempty"`
Name string `json:"name"`
}
type ListResponse struct {
Models []ListModelResponse `json:"models"`
}
type ProcessResponse struct {
Models []ProcessModelResponse `json:"models"`
}
type ListModelResponse struct {
Name       string       `json:"name"`
Model      string       `json:"model"`
ModifiedAt time.Time    `json:"modified_at"`
Size       int64        `json:"size"`
Digest     string       `json:"digest"`
Details    ModelDetails `json:"details,omitempty"`
}
type ProcessModelResponse struct {
Name          string       `json:"name"`
Model         string       `json:"model"`
Size          int64        `json:"size"`
Digest        string       `json:"digest"`
Details       ModelDetails `json:"details,omitempty"`
ExpiresAt     time.Time    `json:"expires_at"`
SizeVRAM      int64        `json:"size_vram"`
ContextLength int          `json:"context_length"`
}
type TokenResponse struct {
Token string `json:"token"`
}
type GenerateResponse struct {
Model string `json:"model"`
CreatedAt time.Time `json:"created_at"`
Response string `json:"response"`
Thinking string `json:"thinking,omitempty"`
Done bool `json:"done"`
DoneReason string `json:"done_reason,omitempty"`
Context []int `json:"context,omitempty"`
Tokens []int `json:"tokens,omitempty"`
Metrics
ToolCalls []ToolCall `json:"tool_calls,omitempty"`
}
type ModelDetails struct {
ParentModel       string   `json:"parent_model"`
Format            string   `json:"format"`
Family            string   `json:"family"`
Families          []string `json:"families"`
ParameterSize     string   `json:"parameter_size"`
QuantizationLevel string   `json:"quantization_level"`
}
type Tensor struct {
Name  string   `json:"name"`
Type  string   `json:"type"`
Shape []uint64 `json:"shape"`
}
func (m *Metrics) Summary() {
if m.TotalDuration > 0 {
fmt.Fprintf(os.Stderr, "total duration:       %v\n", m.TotalDuration)
}
if m.LoadDuration > 0 {
fmt.Fprintf(os.Stderr, "load duration:        %v\n", m.LoadDuration)
}
if m.PromptEvalCount > 0 {
fmt.Fprintf(os.Stderr, "prompt eval count:    %d token(s)\n", m.PromptEvalCount)
}
if m.PromptEvalDuration > 0 {
fmt.Fprintf(os.Stderr, "prompt eval duration: %s\n", m.PromptEvalDuration)
fmt.Fprintf(os.Stderr, "prompt eval rate:     %.2f tokens/s\n", float64(m.PromptEvalCount)/m.PromptEvalDuration.Seconds())
}
if m.EvalCount > 0 {
fmt.Fprintf(os.Stderr, "eval count:           %d token(s)\n", m.EvalCount)
}
if m.EvalDuration > 0 {
fmt.Fprintf(os.Stderr, "eval duration:        %s\n", m.EvalDuration)
fmt.Fprintf(os.Stderr, "eval rate:            %.2f tokens/s\n", float64(m.EvalCount)/m.EvalDuration.Seconds())
}
}
func (opts *Options) FromMap(m map[string]any) error {
valueOpts := reflect.ValueOf(opts).Elem()
typeOpts := reflect.TypeOf(opts).Elem()
jsonOpts := make(map[string]reflect.StructField)
for _, field := range reflect.VisibleFields(typeOpts) {
jsonTag := strings.Split(field.Tag.Get("json"), ",")[0]
if jsonTag != "" {
jsonOpts[jsonTag] = field
}
}
for key, val := range m {
opt, ok := jsonOpts[key]
if !ok {
slog.Warn("invalid option provided", "option", key)
continue
}
field := valueOpts.FieldByName(opt.Name)
if field.IsValid() && field.CanSet() {
if val == nil {
continue
}
switch field.Kind() {
case reflect.Int:
switch t := val.(type) {
case int64:
field.SetInt(t)
case float64:
field.SetInt(int64(t))
default:
return fmt.Errorf("option %q must be of type integer", key)
}
case reflect.Bool:
val, ok := val.(bool)
if !ok {
return fmt.Errorf("option %q must be of type boolean", key)
}
field.SetBool(val)
case reflect.Float32:
val, ok := val.(float64)
if !ok {
return fmt.Errorf("option %q must be of type float32", key)
}
field.SetFloat(val)
case reflect.String:
val, ok := val.(string)
if !ok {
return fmt.Errorf("option %q must be of type string", key)
}
field.SetString(val)
case reflect.Slice:
val, ok := val.([]any)
if !ok {
return fmt.Errorf("option %q must be of type array", key)
}
slice := make([]string, len(val))
for i, item := range val {
str, ok := item.(string)
if !ok {
return fmt.Errorf("option %q must be of an array of strings", key)
}
slice[i] = str
}
field.Set(reflect.ValueOf(slice))
case reflect.Pointer:
var b bool
if field.Type() == reflect.TypeOf(&b) {
val, ok := val.(bool)
if !ok {
return fmt.Errorf("option %q must be of type boolean", key)
}
field.Set(reflect.ValueOf(&val))
} else {
return fmt.Errorf("unknown type loading config params: %v %v", field.Kind(), field.Type())
}
default:
return fmt.Errorf("unknown type loading config params: %v", field.Kind())
}
}
}
return nil
}
func DefaultOptions() Options {
return Options{
NumPredict: -1,
NumKeep:          4,
Temperature:      0.8,
TopK:             40,
TopP:             0.9,
TypicalP:         1.0,
RepeatLastN:      64,
RepeatPenalty:    1.1,
PresencePenalty:  0.0,
FrequencyPenalty: 0.0,
Seed:             -1,
Runner: Runner{
NumCtx:    int(envconfig.ContextLength()),
NumBatch:  512,
NumGPU:    -1,
NumThread: 0,
UseMMap:   nil,
},
}
}
type ThinkValue struct {
Value interface{}
}
func (t *ThinkValue) IsValid() bool {
if t == nil || t.Value == nil {
return true
}
switch v := t.Value.(type) {
case bool:
return true
case string:
return v == "high" || v == "medium" || v == "low"
default:
return false
}
}
func (t *ThinkValue) IsBool() bool {
if t == nil || t.Value == nil {
return false
}
_, ok := t.Value.(bool)
return ok
}
func (t *ThinkValue) IsString() bool {
if t == nil || t.Value == nil {
return false
}
_, ok := t.Value.(string)
return ok
}
func (t *ThinkValue) AsBool() bool {
if t == nil || t.Value == nil {
return false
}
switch v := t.Value.(type) {
case bool:
return v
case string:
return v == "high" || v == "medium" || v == "low"
default:
return false
}
}
func (t *ThinkValue) AsString() string {
if t == nil || t.Value == nil {
return ""
}
switch v := t.Value.(type) {
case string:
return v
case bool:
if v {
return "medium"
}
return ""
default:
return ""
}
}
func (t *ThinkValue) UnmarshalJSON(data []byte) error {
var b bool
if err := json.Unmarshal(data, &b); err == nil {
t.Value = b
return nil
}
var s string
if err := json.Unmarshal(data, &s); err == nil {
if s != "high" && s != "medium" && s != "low" {
return fmt.Errorf("invalid think value: %q (must be \"high\", \"medium\", \"low\", true, or false)", s)
}
t.Value = s
return nil
}
return fmt.Errorf("think must be a boolean or string (\"high\", \"medium\", \"low\")")
}
func (t *ThinkValue) MarshalJSON() ([]byte, error) {
if t == nil || t.Value == nil {
return []byte("null"), nil
}
return json.Marshal(t.Value)
}
type Duration struct {
time.Duration
}
func (d Duration) MarshalJSON() ([]byte, error) {
if d.Duration < 0 {
return []byte("-1"), nil
}
return []byte("\"" + d.Duration.String() + "\""), nil
}
func (d *Duration) UnmarshalJSON(b []byte) (err error) {
var v any
if err := json.Unmarshal(b, &v); err != nil {
return err
}
d.Duration = 5 * time.Minute
switch t := v.(type) {
case float64:
if t < 0 {
d.Duration = time.Duration(math.MaxInt64)
} else {
d.Duration = time.Duration(int(t) * int(time.Second))
}
case string:
d.Duration, err = time.ParseDuration(t)
if err != nil {
return err
}
if d.Duration < 0 {
d.Duration = time.Duration(math.MaxInt64)
}
default:
return fmt.Errorf("Unsupported type: '%s'", reflect.TypeOf(v))
}
return nil
}
func FormatParams(params map[string][]string) (map[string]any, error) {
opts := Options{}
valueOpts := reflect.ValueOf(&opts).Elem()
typeOpts := reflect.TypeOf(opts)
jsonOpts := make(map[string]reflect.StructField)
for _, field := range reflect.VisibleFields(typeOpts) {
jsonTag := strings.Split(field.Tag.Get("json"), ",")[0]
if jsonTag != "" {
jsonOpts[jsonTag] = field
}
}
out := make(map[string]any)
for key, vals := range params {
if opt, ok := jsonOpts[key]; !ok {
return nil, fmt.Errorf("unknown parameter '%s'", key)
} else {
field := valueOpts.FieldByName(opt.Name)
if field.IsValid() && field.CanSet() {
switch field.Kind() {
case reflect.Float32:
floatVal, err := strconv.ParseFloat(vals[0], 32)
if err != nil {
return nil, fmt.Errorf("invalid float value %s", vals)
}
out[key] = float32(floatVal)
case reflect.Int:
intVal, err := strconv.ParseInt(vals[0], 10, 64)
if err != nil {
return nil, fmt.Errorf("invalid int value %s", vals)
}
out[key] = intVal
case reflect.Bool:
boolVal, err := strconv.ParseBool(vals[0])
if err != nil {
return nil, fmt.Errorf("invalid bool value %s", vals)
}
out[key] = boolVal
case reflect.String:
out[key] = vals[0]
case reflect.Slice:
out[key] = vals
case reflect.Pointer:
var b bool
if field.Type() == reflect.TypeOf(&b) {
boolVal, err := strconv.ParseBool(vals[0])
if err != nil {
return nil, fmt.Errorf("invalid bool value %s", vals)
}
out[key] = &boolVal
} else {
return nil, fmt.Errorf("unknown type %s for %s", field.Kind(), key)
}
default:
return nil, fmt.Errorf("unknown type %s for %s", field.Kind(), key)
}
}
}
}
return out, nil
}
type CreateAgentRequest struct {
Name        string                 `json:"name"`
Description string                 `json:"description"`
Models      []string               `json:"models"`
Config      map[string]interface{} `json:"config,omitempty"`
}
type AgentResponse struct {
ID          string                 `json:"id"`
Name        string                 `json:"name"`
Description string                 `json:"description"`
Models      []string               `json:"models"`
Config      map[string]interface{} `json:"config"`
CreatedAt   time.Time              `json:"created_at"`
UpdatedAt   time.Time              `json:"updated_at"`
}
type ListAgentsResponse struct {
Agents []AgentResponse `json:"agents"`
}
type OrchestrationRequest struct {
AgentID     string                 `json:"agent_id"`
Tasks       []OrchestrationTask    `json:"tasks"`
Sequential  bool                   `json:"sequential"`
Parameters  map[string]interface{} `json:"parameters,omitempty"`
Stream      *bool                  `json:"stream,omitempty"`
KeepAlive   *Duration              `json:"keep_alive,omitempty"`
}
type OrchestrationTask struct {
Type       string                 `json:"type"`
Input      string                 `json:"input"`
ModelName  string                 `json:"model_name,omitempty"`
Parameters map[string]interface{} `json:"parameters,omitempty"`
}
type OrchestrationResponse struct {
ID        string                    `json:"id"`
AgentID   string                    `json:"agent_id"`
Status    string                    `json:"status"`
Tasks     []OrchestrationTaskResult `json:"tasks"`
Results   []OrchestrationResult     `json:"results,omitempty"`
Error     string                    `json:"error,omitempty"`
CreatedAt time.Time                 `json:"created_at"`
}
type OrchestrationTaskResult struct {
ID          string                 `json:"id"`
Type        string                 `json:"type"`
Input       string                 `json:"input"`
Output      string                 `json:"output,omitempty"`
Status      string                 `json:"status"`
ModelName   string                 `json:"model_name,omitempty"`
Parameters  map[string]interface{} `json:"parameters,omitempty"`
CreatedAt   time.Time              `json:"created_at"`
CompletedAt *time.Time             `json:"completed_at,omitempty"`
Error       string                 `json:"error,omitempty"`
}
type OrchestrationResult struct {
TaskID    string                    `json:"task_id"`
Output    string                    `json:"output"`
ModelUsed string                    `json:"model_used,omitempty"`
Metrics   OrchestrationTaskMetrics  `json:"metrics,omitempty"`
}
type OrchestrationTaskMetrics struct {
Duration     Duration `json:"duration"`
TokensUsed   int      `json:"tokens_used,omitempty"`
PromptTokens int      `json:"prompt_tokens,omitempty"`
OutputTokens int      `json:"output_tokens,omitempty"`
}
type WorkflowRequest struct {
AgentID string         `json:"agent_id"`
Steps   []WorkflowStep `json:"steps"`
}
type WorkflowStep struct {
Name      string `json:"name"`
Type      string `json:"type"`
Input     string `json:"input"`
ModelName string `json:"model_name,omitempty"`
}
type WorkflowResponse struct {
Steps   []WorkflowStepResult `json:"steps"`
Success bool                 `json:"success"`
Error   string               `json:"error,omitempty"`
}
type WorkflowStepResult struct {
Name      string `json:"name"`
Type      string `json:"type"`
Input     string `json:"input"`
Output    string `json:"output"`
ModelUsed string `json:"model_used"`
Success   bool   `json:"success"`
Error     string `json:"error,omitempty"`
}