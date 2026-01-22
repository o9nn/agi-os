export interface Peer {
  get id(): string
  send: (data: unknown, options?: {
    compress?: boolean
  }) => number | void | undefined
  readyState?: number
}
export interface NamedPeer {
  name: string
  index?: number
  peer: Peer
}
export enum WebSocketReadyState {
  CONNECTING = 0,
  OPEN = 1,
  CLOSING = 2,
  CLOSED = 3,
}
export interface AuthenticatedPeer extends NamedPeer {
  authenticated: boolean
}