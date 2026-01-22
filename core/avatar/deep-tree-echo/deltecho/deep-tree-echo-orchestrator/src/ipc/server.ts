import { getLogger } from 'deep-tree-echo-core'
const log = getLogger('deep-tree-echo-orchestrator/IPCServer')
export class IPCServer {
private running: boolean = false
public async start(): Promise<void> {
log.info('Starting IPC server...')
this.running = true
log.info('IPC server started')
}
public async stop(): Promise<void> {
if (!this.running) return
log.info('Stopping IPC server...')
this.running = false
log.info('IPC server stopped')
}
public isRunning(): boolean {
return this.running
}
}