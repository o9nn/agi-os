import {fileURLToPath} from "node:url";
import path from "node:path";
import {app, shell, BrowserWindow} from "electron";
import {registerLlmRpc} from "./rpc/llmRpc.ts";
const __dirname = path.dirname(fileURLToPath(import.meta.url));
process.env.APP_ROOT = path.join(__dirname, "..");
export const VITE_DEV_SERVER_URL = process.env["VITE_DEV_SERVER_URL"];
export const MAIN_DIST = path.join(process.env.APP_ROOT, "dist-electron");
export const RENDERER_DIST = path.join(process.env.APP_ROOT, "dist");
process.env.VITE_PUBLIC = VITE_DEV_SERVER_URL
? path.join(process.env.APP_ROOT, "public")
: RENDERER_DIST;
let win: BrowserWindow | null;
function createWindow() {
win = new BrowserWindow({
icon: path.join(process.env.VITE_PUBLIC, "electron-vite.svg"),
webPreferences: {
preload: path.join(__dirname, "preload.mjs"),
scrollBounce: true
},
width: 1000,
height: 700
});
registerLlmRpc(win);
win.webContents.setWindowOpenHandler(({url}) => {
if (url.startsWith("file://"))
return {action: "allow"};
void shell.openExternal(url);
return {action: "deny"};
});
win.webContents.on("did-finish-load", () => {
win?.webContents.send("main-process-message", (new Date()).toLocaleString());
});
if (VITE_DEV_SERVER_URL)
void win.loadURL(VITE_DEV_SERVER_URL);
else
void win.loadFile(path.join(RENDERER_DIST, "index.html"));
}
app.on("window-all-closed", () => {
if (process.platform !== "darwin") {
app.quit();
win = null;
}
});
app.on("activate", () => {
if (BrowserWindow.getAllWindows().length === 0) {
createWindow();
}
});
app.whenReady().then(createWindow);