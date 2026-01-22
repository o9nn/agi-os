import {getConsoleLogPrefix} from "../../utils/getConsoleLogPrefix.js";
export async function asyncEvery(promises: Promise<boolean>[]): Promise<boolean> {
    try {
        return (await Promise.all(promises)).every(Boolean);
    } catch (err) {
        console.error(getConsoleLogPrefix(false, false), err);
        return false;
    }
}