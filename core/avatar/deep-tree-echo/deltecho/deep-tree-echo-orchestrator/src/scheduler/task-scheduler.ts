import { getLogger } from 'deep-tree-echo-core'
const log = getLogger('deep-tree-echo-orchestrator/TaskScheduler')
export class TaskScheduler {
  private running: boolean = false
  public async start(): Promise<void> {
    log.info('Starting task scheduler...')
    this.running = true
    log.info('Task scheduler started')
  }
  public async stop(): Promise<void> {
    if (!this.running) return
    log.info('Stopping task scheduler...')
    this.running = false
    log.info('Task scheduler stopped')
  }
  public scheduleTask(cronExpression: string, handler: () => Promise<void>): void {
    log.info(`Scheduled task with expression: ${cronExpression}`)
  }
  public isRunning(): boolean {
    return this.running
  }
}