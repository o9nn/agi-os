export namespace MessageToFrontend {}
export namespace MessageToBackend {
  export interface LogEntry {
    type: 'log'
    data: [
      channel: string,
      level: string,
      stack_trace: string | StackFrame[],
      ...args: any[],
    ]
  }
  export interface UIReady {
    type: 'UIReady'
  }
  export interface UIReadyFrontendReady {
    type: 'UIReadyFrontendReady'
  }
  export type AllTypes = LogEntry | UIReady | UIReadyFrontendReady
}