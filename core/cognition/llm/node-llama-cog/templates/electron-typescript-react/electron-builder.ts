import path from "node:path";
import {$} from "zx";
import type {Configuration} from "electron-builder";
const appId = "node-llama-cpp.electron.example";
const productName = "node-llama-cpp Electron example";
const executableName = "node-llama-cpp-electron-example";
const appxIdentityName = "node.llama.cpp.electron.example";
export default {
appId: appId,
asar: true,
productName: productName,
executableName: executableName,
directories: {
output: "release"
},
icon: "./public/app-icon.png",
async afterPack(context) {
if (context.electronPlatformName === "darwin") {
const appPath = path.join(context.appOutDir, `${context.packager.appInfo.productFilename}.app`);
await $`codesign --force --deep --sign - ${appPath}`;
}
},
files: [
"dist",
"dist-electron",
"!node_modules/node-llama-cpp/bins*",
"node_modules/node-llama-cpp/bins/${os}-${arch}**",
"!node_modules/node-llama-cpp/llama/localBuilds*",
"node_modules/node-llama-cpp/llama/localBuilds/${os}-${arch}**",
"!node_modules/@node-llama-cpp*",
"node_modules/@node-llama-cpp/${os}-${arch}*/bins*"
],
asarUnpack: [
"node_modules/node-llama-cpp/bins",
"node_modules/node-llama-cpp/llama/localBuilds",
"node_modules/@node-llama-cpp/*"
],
mac: {
target: [{
target: "dmg",
arch: [
"arm64",
"x64"
]
}, {
target: "zip",
arch: [
"arm64",
"x64"
]
}],
artifactName: "${name}.macOS.${version}.${arch}.${ext}"
},
win: {
target: [{
target: "nsis",
arch: [
"x64",
"arm64"
]
}],
artifactName: "${name}.Windows.${version}.${arch}.${ext}"
},
appx: {
identityName: appxIdentityName,
artifactName: "${name}.Windows.${version}.${arch}.${ext}"
},
nsis: {
oneClick: true,
perMachine: false,
allowToChangeInstallationDirectory: false,
deleteAppDataOnUninstall: true
},
linux: {
target: [{
target: "AppImage",
arch: [
"x64",
"arm64"
]
}, {
target: "snap",
arch: [
"x64"
]
}, {
target: "deb",
arch: [
"x64",
"arm64"
]
}, {
target: "tar.gz",
arch: [
"x64",
"arm64"
]
}],
category: "Utility",
artifactName: "${name}.Linux.${version}.${arch}.${ext}"
}
} as Configuration;