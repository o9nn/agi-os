export function getFirstValidResult<const T extends (() => any)[]>(options: T): ReturnType<T[number]> {
    for (let i = 0; i < options.length; i++) {
        if (i === options.length - 1)
            return options[i]!();
        try {
            return options[i]!();
        } catch (err) {
        }
    }
    throw new Error("All options failed");
}