export function pushAll<const T>(array: T[], items: readonly NoInfer<T>[] | ReadonlySet<NoInfer<T>>): T[] {
for (const item of items)
array.push(item);
return array;
}