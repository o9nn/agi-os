import type { Ref, WatchOptions } from 'vue'
import { watch } from 'vue'
import { get, set } from './object'
export function makePropWatcher<T, E>(
  propGetter: () => T,
  target: Ref<E>,
  propertyPath: string,
  newPlainObjectFunction: () => E & { dispose?: () => void },
  watchOptions: WatchOptions = {},
) {
  return watch(propGetter, (newValue) => {
    if (!target.value) {
      return
    }
    if (newValue === undefined) {
      const plainObject = newPlainObjectFunction()
      set(target.value, propertyPath, get(plainObject, propertyPath))
      plainObject.dispose?.()
    }
    else {
      set(target.value, propertyPath, propGetter())
    }
  }, watchOptions)
}
export function makePropWatchers<E>(
  propGettersAndPropertyPaths: (string | (() => any))[][],
  target: Ref<E>,
  newPlainObjectFunction: () => E & { dispose?: () => void },
) {
  return propGettersAndPropertyPaths.map(([propGetterFn, path]) => makePropWatcher(
    propGetterFn as () => any,
    target,
    path as string,
    newPlainObjectFunction,
  ))
}
export function makePropWatchersUsingAllProps<E>(
  props: { [key: PropertyKey]: any },
  target: Ref<E>,
  newPlainObjectFunction: () => E & { dispose?: () => void },
) {
  return Object.keys(props).map(key => makePropWatcher(
    () => props[key],
    target,
    key,
    newPlainObjectFunction,
  ))
}