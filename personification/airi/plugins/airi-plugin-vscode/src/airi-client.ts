import type { AiriEvent } from './types'
import { useLogger } from '@guiiai/logg'
import { Client } from '@proj-airi/server-sdk'
export class AiriClient {
private client: Client<AiriEvent> | null = null
async connect(): Promise<boolean> {
try {
this.client = new Client({ name: 'proj-airi:plugin-vscode' })
useLogger().log('Airi companion connected to Channel Server')
return true
}
catch (error) {
useLogger().errorWithError('Failed to connect to Airi Channel Server:', error)
return false
}
}
disconnect(): void {
if (this.client) {
this.client.close()
this.client = null
useLogger().log('Airi companion disconnected')
}
}
sendEvent(event: AiriEvent): void {
if (!this.client) {
useLogger().warn('Cannot send event: not connected to Airi Channel Server')
return
}
try {
this.client.send({
type: 'vscode:context',
data: event,
})
useLogger().log(`Sent event to Airi: ${event.type}`, event)
}
catch (error) {
useLogger().errorWithError('Failed to send event to Airi:', error)
}
}
isConnected(): boolean {
return !!this.client
}
}