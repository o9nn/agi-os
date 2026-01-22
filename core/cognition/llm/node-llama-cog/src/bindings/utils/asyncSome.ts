import {getConsoleLogPrefix} from "../../utils/getConsoleLogPrefix.js";
export async function asyncSome(promises: Promise<boolean>[]): Promise<boolean> {
if (promises.length === 0)
return Promise.resolve(false);
return new Promise((resolve) => {
let fulfilled = 0;
for (const promise of promises) {
promise
.then((result) => {
if (result)
return void resolve(true);
fulfilled++;
if (fulfilled === promises.length)
resolve(false);
})
.catch((err) => {
console.error(getConsoleLogPrefix(false, false), err);
fulfilled++;
if (fulfilled === promises.length)
resolve(false);
});
}
});
}