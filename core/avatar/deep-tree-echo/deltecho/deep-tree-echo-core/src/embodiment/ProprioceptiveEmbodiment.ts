export class ProprioceptiveEmbodiment {
  private presenceState: Record<string, number> = {
    engagement: 0.7,
    responsiveness: 0.8,
    attentiveness: 0.75,
  }
  public getPresenceState(): Record<string, number> {
    return { ...this.presenceState }
  }
  public updatePresence(params: Record<string, number>): void {
    this.presenceState = { ...this.presenceState, ...params }
  }
}