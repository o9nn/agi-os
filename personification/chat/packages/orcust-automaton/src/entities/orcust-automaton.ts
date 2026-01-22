import type { GameEntity } from 'yuka'
import { ArriveBehavior, StateMachine, Vehicle } from 'yuka'
import { IdleState } from '../states/idle'
import { WalkState } from '../states/walk'
export class OrcustAutomaton extends Vehicle {
public currentTarget: GameEntity | null = null
public currentTime = 0
public stateMachine: StateMachine<OrcustAutomaton>
private onStoreChange?: () => void
constructor() {
super()
this.maxTurnRate = Math.PI * 0.5
this.maxSpeed = 1.5
const arriveBehavior = new ArriveBehavior()
arriveBehavior.active = false
this.steering.add(arriveBehavior)
this.stateMachine = new StateMachine(this)
this.stateMachine.add('idle', new IdleState())
this.stateMachine.add('walk', new WalkState())
this.stateMachine.changeTo('idle')
}
public setCurrentTarget(target: GameEntity) {
this.currentTarget = target
this.stateMachine.changeTo('walk')
}
public subscribe(onStoreChange: () => void) {
this.onStoreChange = onStoreChange
}
public toggleArriveBehavior(active: boolean) {
const arriveBehavior = this.steering.behaviors.at(0) as ArriveBehavior
arriveBehavior.active = active
this.onStoreChange?.()
}
public unsubscribe() {
this.onStoreChange = undefined
}
public update(delta: number) {
super.update(delta)
this.stateMachine.update()
return this
}
}