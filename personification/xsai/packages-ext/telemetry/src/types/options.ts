import type { AttributeValue } from '@opentelemetry/api'
export type TelemetryMetadata = Record<string, AttributeValue>
export interface TelemetryOptions {
metadata?: TelemetryMetadata
}
export type WithTelemetry<T> = T & {
telemetry?: TelemetryOptions
}