import type { App, ComponentInternalInstance, DefineComponent } from 'vue'
export interface ComponentPropText {
  type: 'string'
  value?: string
}
export interface ComponentPropBool {
  type: 'boolean'
  value?: boolean
}
export interface ComponentPropNumber {
  type: 'number'
  value?: number
}
export interface ComponentPropUnknown {
  type: 'unknown'
  value?: unknown
}
export interface ComponentPropArray {
  type: 'array'
  value?: unknown[]
}
export type ComponentProp = (ComponentPropText | ComponentPropBool | ComponentPropNumber | ComponentPropArray | ComponentPropUnknown) & {
  title: string
  key: string
}
function willTurnIntoNumber(value: unknown): boolean {
  if (value === Number) {
    return true
  }
  if (typeof value === 'object' && value !== null && 'type' in value) {
    if (typeof (value as { type: unknown }).type === 'function' && (value as { type: unknown }).type === Number) {
      return true
    }
  }
  return false
}
function willTurnIntoBoolean(value: unknown): boolean {
  if (value === Boolean) {
    return true
  }
  if (typeof value === 'object' && value !== null && 'type' in value) {
    if (typeof (value as { type: unknown }).type === 'function' && (value as { type: unknown }).type === Boolean) {
      return true
    }
  }
  return false
}
function willTurnIntoString(value: unknown): boolean {
  if (value === String) {
    return true
  }
  if (typeof value === 'object' && value !== null && 'type' in value) {
    if (typeof (value as { type: unknown }).type === 'function' && (value as { type: unknown }).type === String) {
      return true
    }
  }
  return false
}
function willTurnIntoArray(value: unknown): boolean {
  if (value === Array) {
    return true
  }
  if (typeof value === 'object' && value !== null && 'type' in value) {
    if (typeof (value as { type: unknown }).type === 'function' && (value as { type: unknown }).type === Array) {
      return true
    }
  }
  return false
}
function inferType(
  propDef:
    | {
      type: Function
    }
    | typeof String
    | typeof Number
    | typeof Boolean
    | typeof Array
    | unknown,
) {
  let type: 'unknown' | 'string' | 'number' | 'boolean' | 'array' = 'unknown'
  if (willTurnIntoString(propDef)) {
    type = 'string'
  }
  else if (willTurnIntoNumber(propDef)) {
    type = 'number'
  }
  else if (willTurnIntoBoolean(propDef)) {
    type = 'boolean'
  }
  else if (willTurnIntoArray(propDef)) {
    type = 'array'
  }
  return type
}
export function resolveProps(component: DefineComponent | App<any>): ComponentProp[] {
  if (component._component && component._component.props && typeof component._component.props === 'object') {
    return Object.entries(component._component.props).map(([key, propDef]) => {
      return {
        key,
        title: key,
        type: inferType(propDef),
      }
    })
  }
  else if ((component as unknown as ComponentInternalInstance).props && typeof (component as unknown as ComponentInternalInstance).props === 'object') {
    return Object.entries((component as unknown as ComponentInternalInstance).props).map(([key, propDef]) => {
      return {
        key,
        title: key,
        type: inferType(propDef),
      }
    })
  }
  else {
    return []
  }
}