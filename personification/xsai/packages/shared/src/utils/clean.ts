export const clean = <T extends Record<string, undefined | unknown>>(obj: T) =>
  Object.fromEntries(
    Object.entries(obj).filter(([, v]) => v !== undefined),
  ) as Record<keyof T, Exclude<T[keyof T], unknown>>