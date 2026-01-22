export interface MemoryStorage {
  load(key: string): Promise<string | undefined>
  save(key: string, value: string): Promise<void>
}
export class InMemoryStorage implements MemoryStorage {
  private storage: Map<string, string> = new Map()
  async load(key: string): Promise<string | undefined> {
    return this.storage.get(key)
  }
  async save(key: string, value: string): Promise<void> {
    this.storage.set(key, value)
  }
  clear(): void {
    this.storage.clear()
  }
}