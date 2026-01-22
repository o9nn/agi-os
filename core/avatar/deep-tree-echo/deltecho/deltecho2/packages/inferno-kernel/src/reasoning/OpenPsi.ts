import { Atom, AtomSpace } from '../atomspace/AtomSpace.js'
export interface Goal {
  id: string
  name: string
  priority: number
  satisfaction: number
  atomId?: string
}
export interface Drive {
  name: string
  value: number
  min: number
  max: number
  decayRate: number
}
export interface Emotion {
  name: string
  valence: number   
  arousal: number   
  duration: number  
}
export class OpenPsi {
  private atomSpace: AtomSpace
  private goals: Map<string, Goal>
  private drives: Map<string, Drive>
  private emotions: Map<string, Emotion>
  private nextGoalId: number
  constructor(atomSpace: AtomSpace) {
    this.atomSpace = atomSpace
    this.goals = new Map()
    this.drives = new Map()
    this.emotions = new Map()
    this.nextGoalId = 1
    this.initializeDrives()
  }
  private initializeDrives(): void {
    this.addDrive('certainty', 0.5, 0, 1, 0.01)
    this.addDrive('competence', 0.5, 0, 1, 0.01)
    this.addDrive('affiliation', 0.5, 0, 1, 0.01)
    this.addDrive('energy', 1.0, 0, 1, 0.02)
    console.log('[OpenPsi] Initialized drives')
  }
  addDrive(
    name: string,
    initialValue: number,
    min: number,
    max: number,
    decayRate: number
  ): void {
    this.drives.set(name, {
      name,
      value: initialValue,
      min,
      max,
      decayRate,
    })
  }
  createGoal(name: string, priority: number): Goal {
    const goalId = `goal_${this.nextGoalId++}`
    const goalAtom = this.atomSpace.addNode('ConceptNode', name, {
      strength: priority,
      confidence: 1.0,
    })
    const goal: Goal = {
      id: goalId,
      name,
      priority,
      satisfaction: 0,
      atomId: goalAtom.id,
    }
    this.goals.set(goalId, goal)
    console.log(`[OpenPsi] Created goal: ${name} (priority: ${priority})`)
    return goal
  }
  updateGoalSatisfaction(goalId: string, satisfaction: number): boolean {
    const goal = this.goals.get(goalId)
    if (!goal) return false
    goal.satisfaction = Math.max(0, Math.min(1, satisfaction))
    if (goal.atomId) {
      this.atomSpace.setTruthValue(goal.atomId, {
        strength: goal.satisfaction,
        confidence: 1.0,
      })
    }
    return true
  }
  selectGoal(): Goal | null {
    if (this.goals.size === 0) return null
    let bestGoal: Goal | null = null
    let bestScore = -Infinity
    for (const goal of this.goals.values()) {
      const score = goal.priority * (1 - goal.satisfaction)
      if (score > bestScore) {
        bestScore = score
        bestGoal = goal
      }
    }
    if (bestGoal) {
      console.log(`[OpenPsi] Selected goal: ${bestGoal.name}`)
    }
    return bestGoal
  }
  updateDrives(): void {
    for (const drive of this.drives.values()) {
      drive.value = Math.max(
        drive.min,
        drive.value - drive.decayRate
      )
      if (drive.value < 0.3) {
        this.createGoal(`Satisfy ${drive.name}`, 1 - drive.value)
      }
    }
  }
  satisfyDrive(driveName: string, amount: number): boolean {
    const drive = this.drives.get(driveName)
    if (!drive) return false
    drive.value = Math.min(drive.max, drive.value + amount)
    console.log(`[OpenPsi] Satisfied drive ${driveName}: ${drive.value}`)
    return true
  }
  addEmotion(name: string, valence: number, arousal: number, duration: number): void {
    this.emotions.set(name, {
      name,
      valence: Math.max(-1, Math.min(1, valence)),
      arousal: Math.max(0, Math.min(1, arousal)),
      duration,
    })
    console.log(`[OpenPsi] Emotion: ${name} (valence: ${valence}, arousal: ${arousal})`)
  }
  updateEmotions(): void {
    for (const [name, emotion] of this.emotions.entries()) {
      emotion.duration -= 1
      if (emotion.duration <= 0) {
        this.emotions.delete(name)
      } else {
        emotion.arousal *= 0.95
      }
    }
  }
  getDominantEmotion(): Emotion | null {
    if (this.emotions.size === 0) return null
    let dominant: Emotion | null = null
    let maxArousal = -Infinity
    for (const emotion of this.emotions.values()) {
      if (emotion.arousal > maxArousal) {
        maxArousal = emotion.arousal
        dominant = emotion
      }
    }
    return dominant
  }
  executeAction(): void {
    const goal = this.selectGoal()
    if (goal) {
      goal.satisfaction += 0.1
      if (goal.satisfaction > 0.8) {
        this.addEmotion('joy', 0.8, 0.7, 10)
      }
      this.satisfyDrive('competence', 0.1)
    }
    this.updateDrives()
    this.updateEmotions()
  }
  getState(): {
    goals: Goal[]
    drives: Drive[]
    emotions: Emotion[]
    dominantEmotion: Emotion | null
  } {
    return {
      goals: Array.from(this.goals.values()),
      drives: Array.from(this.drives.values()),
      emotions: Array.from(this.emotions.values()),
      dominantEmotion: this.getDominantEmotion(),
    }
  }
}