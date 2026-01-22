export type JSONRPCBatchRequest = (JSONRPCNotification | JSONRPCRequest)[]
export type JSONRPCBatchResponse = (JSONRPCError | JSONRPCResponse)[]
export type JSONRPCMessage =
  | JSONRPCBatchRequest
  | JSONRPCBatchResponse
  | JSONRPCError
  | JSONRPCNotification
  | JSONRPCRequest
  | JSONRPCResponse
export const LATEST_PROTOCOL_VERSION = '2025-03-26'
export const JSONRPC_VERSION = '2.0'
export type Cursor = string
export interface JSONRPCNotification extends Notification {
  jsonrpc: typeof JSONRPC_VERSION
}
export interface JSONRPCRequest extends Request {
  id: RequestId
  jsonrpc: typeof JSONRPC_VERSION
}
export interface JSONRPCResponse {
  id: RequestId
  jsonrpc: typeof JSONRPC_VERSION
  result: Result
}
export interface Notification {
  method: string
  params?: {
    [key: string]: unknown
    _meta?: { [key: string]: unknown }
  }
}
export type ProgressToken = number | string
export interface Request {
  method: string
  params?: {
    [key: string]: unknown
    _meta?: {
      progressToken?: ProgressToken
    }
  }
}
export type RequestId = number | string
export interface Result {
  [key: string]: unknown
  _meta?: { [key: string]: unknown }
}
export const PARSE_ERROR = -32700
export const INVALID_REQUEST = -32600
export const METHOD_NOT_FOUND = -32601
export const INVALID_PARAMS = -32602
export const INTERNAL_ERROR = -32603
export interface Annotations {
  audience?: Role[]
  priority?: number
}
export interface AudioContent {
  annotations?: Annotations
  data: string
  mimeType: string
  type: 'audio'
}
export interface BlobResourceContents extends ResourceContents {
  blob: string
}
export interface CallToolRequest extends Request {
  method: 'tools/call'
  params: {
    arguments?: { [key: string]: unknown }
    name: string
  }
}
export interface CallToolResult extends Result {
  content: (AudioContent | EmbeddedResource | ImageContent | TextContent)[]
  isError?: boolean
}
export interface CancelledNotification extends Notification {
  method: 'notifications/cancelled'
  params: {
    reason?: string
    requestId: RequestId
  }
}
export interface ClientCapabilities {
  experimental?: { [key: string]: object }
  roots?: {
    listChanged?: boolean
  }
  sampling?: object
}
export type ClientNotification =
  | CancelledNotification
  | InitializedNotification
  | ProgressNotification
  | RootsListChangedNotification
export type ClientRequest =
  | CallToolRequest
  | CompleteRequest
  | GetPromptRequest
  | InitializeRequest
  | ListPromptsRequest
  | ListResourcesRequest
  | ListToolsRequest
  | PingRequest
  | ReadResourceRequest
  | SetLevelRequest
  | SubscribeRequest
  | UnsubscribeRequest
export type ClientResult = CreateMessageResult | EmptyResult | ListRootsResult
export interface CompleteRequest extends Request {
  method: 'completion/complete'
  params: {
    argument: {
      name: string
      value: string
    }
    ref: PromptReference | ResourceReference
  }
}
export interface CompleteResult extends Result {
  completion: {
    hasMore?: boolean
    total?: number
    values: string[]
  }
}
export interface CreateMessageRequest extends Request {
  method: 'sampling/createMessage'
  params: {
    includeContext?: 'allServers' | 'none' | 'thisServer'
    maxTokens: number
    messages: SamplingMessage[]
    metadata?: object
    modelPreferences?: ModelPreferences
    stopSequences?: string[]
    systemPrompt?: string
    temperature?: number
  }
}
export interface CreateMessageResult extends Result, SamplingMessage {
  model: string
  stopReason?: 'endTurn' | 'maxTokens' | 'stopSequence' | string
}
export interface EmbeddedResource {
  annotations?: Annotations
  resource: BlobResourceContents | TextResourceContents
  type: 'resource'
}
export type EmptyResult = Result
export interface GetPromptRequest extends Request {
  method: 'prompts/get'
  params: {
    arguments?: { [key: string]: string }
    name: string
  }
}
export interface GetPromptResult extends Result {
  description?: string
  messages: PromptMessage[]
}
export interface ImageContent {
  annotations?: Annotations
  data: string
  mimeType: string
  type: 'image'
}
export interface Implementation {
  name: string
  version: string
}
export interface InitializedNotification extends Notification {
  method: 'notifications/initialized'
}
export interface InitializeRequest extends Request {
  method: 'initialize'
  params: {
    capabilities: ClientCapabilities
    clientInfo: Implementation
    protocolVersion: string
  }
}
export interface InitializeResult extends Result {
  capabilities: ServerCapabilities
  instructions?: string
  protocolVersion: string
  serverInfo: Implementation
}
export interface JSONRPCError {
  error: {
    code: number
    data?: unknown
    message: string
  }
  id: RequestId
  jsonrpc: typeof JSONRPC_VERSION
}
export interface ListPromptsRequest extends PaginatedRequest {
  method: 'prompts/list'
}
export interface ListPromptsResult extends PaginatedResult {
  prompts: Prompt[]
}
export interface ListResourcesRequest extends PaginatedRequest {
  method: 'resources/list'
}
export interface ListResourcesResult extends PaginatedResult {
  resources: Resource[]
}
export interface ListResourceTemplatesRequest extends PaginatedRequest {
  method: 'resources/templates/list'
}
export interface ListResourceTemplatesResult extends PaginatedResult {
  resourceTemplates: ResourceTemplate[]
}
export interface ListRootsRequest extends Request {
  method: 'roots/list'
}
export interface ListRootsResult extends Result {
  roots: Root[]
}
export interface ListToolsRequest extends PaginatedRequest {
  method: 'tools/list'
}
export interface ListToolsResult extends PaginatedResult {
  tools: Tool[]
}
export type LoggingLevel =
  | 'alert'
  | 'critical'
  | 'debug'
  | 'emergency'
  | 'error'
  | 'info'
  | 'notice'
  | 'warning'
export interface LoggingMessageNotification extends Notification {
  method: 'notifications/message'
  params: {
    data: unknown
    level: LoggingLevel
    logger?: string
  }
}
export interface ModelHint {
  name?: string
}
export interface ModelPreferences {
  costPriority?: number
  hints?: ModelHint[]
  intelligencePriority?: number
  speedPriority?: number
}
export interface PaginatedRequest extends Request {
  params?: {
    cursor?: Cursor
  }
}
export interface PaginatedResult extends Result {
  nextCursor?: Cursor
}
export interface PingRequest extends Request {
  method: 'ping'
}
export interface ProgressNotification extends Notification {
  method: 'notifications/progress'
  params: {
    message?: string
    progress: number
    progressToken: ProgressToken
    total?: number
  }
}
export interface Prompt {
  arguments?: PromptArgument[]
  description?: string
  name: string
}
export interface PromptArgument {
  description?: string
  name: string
  required?: boolean
}
export interface PromptListChangedNotification extends Notification {
  method: 'notifications/prompts/list_changed'
}
export interface PromptMessage {
  content: AudioContent | EmbeddedResource | ImageContent | TextContent
  role: Role
}
export interface PromptReference {
  name: string
  type: 'ref/prompt'
}
export interface ReadResourceRequest extends Request {
  method: 'resources/read'
  params: {
    uri: string
  }
}
export interface ReadResourceResult extends Result {
  contents: (BlobResourceContents | TextResourceContents)[]
}
export interface Resource {
  annotations?: Annotations
  description?: string
  mimeType?: string
  name: string
  uri: string
}
export interface ResourceContents {
  mimeType?: string
  uri: string
}
export interface ResourceListChangedNotification extends Notification {
  method: 'notifications/resources/list_changed'
}
export interface ResourceReference {
  type: 'ref/resource'
  uri: string
}
export interface ResourceTemplate {
  annotations?: Annotations
  description?: string
  mimeType?: string
  name: string
  uriTemplate: string
}
export interface ResourceUpdatedNotification extends Notification {
  method: 'notifications/resources/updated'
  params: {
    uri: string
  }
}
export type Role = 'assistant' | 'user'
export interface Root {
  name?: string
  uri: string
}
export interface RootsListChangedNotification extends Notification {
  method: 'notifications/roots/list_changed'
}
export interface SamplingMessage {
  content: AudioContent | ImageContent | TextContent
  role: Role
}
export interface ServerCapabilities {
  completions?: object
  experimental?: { [key: string]: object }
  logging?: object
  prompts?: {
    listChanged?: boolean
  }
  resources?: {
    listChanged?: boolean
    subscribe?: boolean
  }
  tools?: {
    listChanged?: boolean
  }
}
export type ServerNotification =
  | CancelledNotification
  | LoggingMessageNotification
  | ProgressNotification
  | PromptListChangedNotification
  | ResourceListChangedNotification
  | ResourceUpdatedNotification
  | ToolListChangedNotification
export type ServerRequest =
  | CreateMessageRequest
  | ListRootsRequest
  | PingRequest
export type ServerResult =
  | CallToolResult
  | CompleteResult
  | EmptyResult
  | GetPromptResult
  | InitializeResult
  | ListPromptsResult
  | ListResourcesResult
  | ListToolsResult
  | ReadResourceResult
export interface SetLevelRequest extends Request {
  method: 'logging/setLevel'
  params: {
    level: LoggingLevel
  }
}
export interface SubscribeRequest extends Request {
  method: 'resources/subscribe'
  params: {
    uri: string
  }
}
export interface TextContent {
  annotations?: Annotations
  text: string
  type: 'text'
}
export interface TextResourceContents extends ResourceContents {
  text: string
}
export interface Tool {
  annotations?: ToolAnnotations
  description?: string
  inputSchema: {
    properties?: { [key: string]: object }
    required?: string[]
    type: 'object'
  }
  name: string
}
export interface ToolAnnotations {
  destructiveHint?: boolean
  idempotentHint?: boolean
  openWorldHint?: boolean
  readOnlyHint?: boolean
  title?: string
}
export interface ToolListChangedNotification extends Notification {
  method: 'notifications/tools/list_changed'
}
export interface UnsubscribeRequest extends Request {
  method: 'resources/unsubscribe'
  params: {
    uri: string
  }
}