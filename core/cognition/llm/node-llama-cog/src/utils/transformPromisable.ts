export function transformPromisable<T, R>(value: Promisable<T>, transformer: (value: T) => Promisable<R>): Promisable<R> {
    if (value instanceof Promise)
        return value.then(transformer);
    return transformer(value);
}
export function transformPromisables<const Types extends readonly any[], R>(
    values: {[Index in keyof Types]: Promisable<Types[Index]>},
    transformer: (values: {[Index in keyof Types]: Types[Index]}) => Promisable<R>
): Promisable<R> {
    if (values.some((value) => value instanceof Promise))
        return Promise.all(values).then(transformer);
    return transformer(values);
}
export function promisableLoop<R>({
    condition,
    callback,
    afterthought = () => void 0,
    returnValue
}: {
    condition: () => Promisable<boolean>,
    callback: () => Promisable<void>,
    afterthought?: () => Promisable<void>,
    returnValue: () => Promisable<R>
}): Promisable<R> {
    function iterate(): Promisable<R> {
        while (true) {
            const shouldContinue = condition();
            if (shouldContinue instanceof Promise)
                return shouldContinue
                    .then((shouldContinue): Promisable<R> => {
                        if (shouldContinue) {
                            const value = callback();
                            if (value instanceof Promise)
                                return value.then(() => transformPromisable(afterthought(), iterate));
                            return transformPromisable(afterthought(), iterate);
                        }
                        return returnValue();
                    });
            if (shouldContinue) {
                const value = callback();
                if (value instanceof Promise)
                    return value.then(() => transformPromisable(afterthought(), iterate));
                const afterthoughtValue = afterthought();
                if (afterthoughtValue instanceof Promise)
                    return afterthoughtValue.then(iterate);
                continue;
            }
            return returnValue();
        }
    }
    return iterate();
}
export function transformPromisablesInOrder<
    const T extends (() => Promisable<any>)[],
    const R = {readonly [Index in keyof T]: Awaited<ReturnType<T[Index]>>;}
>(
    getters: T,
    transformer: (
        values: {readonly [Index in keyof T]: Awaited<ReturnType<T[Index]>>;}
    ) => Promisable<R> = ((values) => values as R)
): Promisable<R> {
    let i = 0;
    const res: any[] = [];
    let skipPushingValue = true;
    function iterate(currentValue: any): Promisable<R> {
        if (skipPushingValue)
            skipPushingValue = false;
        else
            res.push(currentValue);
        while (i < getters.length) {
            const getter = getters[i];
            if (getter == null)
                break;
            i++;
            const value = getter();
            if (value instanceof Promise)
                return value.then(iterate);
            res.push(value);
        }
        return transformer(res as {[Index in keyof T]: Awaited<ReturnType<T[Index]>>;});
    }
    return iterate(undefined);
}
export type Promisable<T> = T | Promise<T>;