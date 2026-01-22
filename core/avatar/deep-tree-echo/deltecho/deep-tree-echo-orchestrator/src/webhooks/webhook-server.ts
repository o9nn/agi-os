import { getLogger } from 'deep-tree-echo-core'
const log = getLogger('deep-tree-echo-orchestrator/WebhookServer')
export class WebhookServer {
  private running: boolean = false
  public async start(): Promise<void> {
    log.info('Starting webhook server...')
    this.running = true
    log.info('Webhook server started')
  }
  public async stop(): Promise<void> {
    if (!this.running) return
    log.info('Stopping webhook server...')
    this.running = false
    log.info('Webhook server stopped')
  }
  public isRunning(): boolean {
    return this.running
  }
}