import type { Eventa, EventTag } from './eventa'
interface EventaAdapterProps<EmitOptions = any> {
cleanup: () => void
hooks: {
onReceived: <Req, Res>(tag: EventTag<Req, Res>, payload: Req) => void
onSent: <Req, Res>(tag: EventTag<Req, Res>, payload: Req, options?: EmitOptions) => void
}
}
export type EventaAdapter<EmitOptions = any> = <P>(emit: (event: Eventa<P>, payload: P, options?: EmitOptions) => void) => EventaAdapterProps