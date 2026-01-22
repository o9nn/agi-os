import process from "process";
import fs from "fs-extra";
import {tempDownloadDirectory} from "../config.js";
export async function clearTempFolder() {
if (process.platform === "win32") {
try {
await fs.remove(tempDownloadDirectory);
} catch (err) {
}
return;
}
await fs.remove(tempDownloadDirectory);
}