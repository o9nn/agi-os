import { camelCase, isArray, transform, isObject } from "lodash";
export const camelize = (
  obj: Record<string, unknown>
): Record<string, unknown> =>
  transform(
    obj,
    (
      result: Record<string, unknown>,
      value: unknown,
      key: string,
      target: unknown
    ) => {
      const camelKey = isArray(target) ? key : camelCase(key);
      result[camelKey] = isObject(value)
        ? camelize(value as Record<string, unknown>)
        : value;
    }
  );