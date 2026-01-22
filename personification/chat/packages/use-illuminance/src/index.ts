import { useSingleton } from 'foxact/use-singleton'
import { useSyncExternalStore } from 'react'
declare global {
interface Window {
AmbientLightSensor?: AmbientLightSensor
}
}
interface SensorErrorEvent extends Event {
readonly error: Error
}
interface SensorOptions {
frequency?: number
}
declare class Sensor extends EventTarget {
readonly activated: boolean
readonly hasReading: boolean
onactivate: (this: this, ev: Event) => unknown
onerror: (this: this, ev: SensorErrorEvent) => unknown
onreading: (this: this, ev: Event) => unknown
readonly timestamp?: number
addEventListener(
type: 'activate' | 'reading',
listener: (this: this, ev: Event) => unknown,
useCapture?: boolean,
): void
addEventListener(
type: 'error',
listener: (this: this, ev: SensorErrorEvent) => unknown,
useCapture?: boolean
): void
start(): void
stop(): void
}
declare class AmbientLightSensor extends Sensor {
readonly illuminance?: number
constructor(options?: SensorOptions)
}
declare global {
interface Window {
AmbientLightSensor?: AmbientLightSensor
}
}
export const useIlluminance = () => {
const sensor = useSingleton(() => {
if ('AmbientLightSensor' in window) {
return new AmbientLightSensor()
}
})
const subscribe = (onStoreChange: () => void) => {
if (!sensor.current)
return () => {}
sensor.current.addEventListener('reading', onStoreChange)
sensor.current.start()
return () => {
sensor.current!.removeEventListener('reading', onStoreChange)
sensor.current!.stop()
}
}
const getSnapshot = () => {
if (!sensor.current)
return
return sensor.current.illuminance
}
return useSyncExternalStore(subscribe, getSnapshot)
}