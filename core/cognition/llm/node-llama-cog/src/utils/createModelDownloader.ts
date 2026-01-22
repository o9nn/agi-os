import process from "process";
import path from "path";
import {DownloadEngineMultiDownload, DownloadEngineNodejs, downloadFile, downloadSequence} from "ipull";
import fs from "fs-extra";
import chalk from "chalk";
import {createSplitPartFilename, resolveSplitGgufParts} from "../gguf/utils/resolveSplitGgufParts.js";
import {getFilenameForBinarySplitGgufPartUrls, resolveBinarySplitGgufPartUrls} from "../gguf/utils/resolveBinarySplitGgufPartUrls.js";
import {cliModelsDirectory, isCI} from "../config.js";
import {safeEventCallback} from "./safeEventCallback.js";
import {ModelFileAccessTokens, resolveModelFileAccessTokensTryHeaders} from "./modelFileAccessTokens.js";
import {ModelDownloadEndpoints} from "./modelDownloadEndpoints.js";
import {pushAll} from "./pushAll.js";
import {resolveModelDestination} from "./resolveModelDestination.js";
import {getAuthorizationHeader, resolveParsedModelUri} from "./parseModelUri.js";
import withOra from "./withOra.js";
export type ModelDownloaderOptions = ({
modelUri: string
} | {
modelUrl: string
}) & {
dirPath?: string,
fileName?: string,
headers?: Record<string, string>,
showCliProgress?: boolean,
onProgress?: (status: {totalSize: number, downloadedSize: number}) => void,
skipExisting?: boolean,
deleteTempFileOnCancel?: boolean,
parallelDownloads?: number,
tokens?: ModelFileAccessTokens,
endpoints?: ModelDownloadEndpoints,
_showUriResolvingProgress?: boolean
};
export function createModelDownloader(options: ModelDownloaderOptions) {
return ModelDownloader._create(options);
}
export async function combineModelDownloaders(
downloaders: (ModelDownloader | Promise<ModelDownloader>)[],
options?: CombinedModelDownloaderOptions
) {
const downloader = CombinedModelDownloader._create(await Promise.all(downloaders), options);
await downloader._init();
return downloader;
}
export class ModelDownloader {
private readonly _modelUrl: string;
private readonly _dirPath: string;
private readonly _fileName?: string;
private readonly _headers?: Record<string, string>;
private readonly _showCliProgress: boolean;
private readonly _onProgress?: ModelDownloaderOptions["onProgress"];
private readonly _tokens?: ModelFileAccessTokens;
private readonly _endpoints?: ModelDownloadEndpoints;
public readonly _deleteTempFileOnCancel: boolean;
private readonly _skipExisting: boolean;
private readonly _parallelDownloads: number;
public _specificFileDownloaders: DownloadEngineNodejs[] = [];
private _downloader?: DownloadEngineMultiDownload | DownloadEngineNodejs;
private _entrypointFilename?: string;
private _splitBinaryParts?: number;
private _totalFiles?: number;
private _tryHeaders: Record<string, string>[] = [];
private constructor(options: ModelDownloaderOptions, {resolvedModelUrl, resolvedFileName}: {
resolvedModelUrl: string,
resolvedFileName?: string
}) {
const {
dirPath = cliModelsDirectory, headers, showCliProgress = false, onProgress, deleteTempFileOnCancel = true,
skipExisting = true, parallelDownloads = 4, tokens, endpoints
} = options;
this._modelUrl = resolvedModelUrl;
this._dirPath = path.resolve(process.cwd(), dirPath);
this._fileName = resolvedFileName;
this._headers = headers;
this._showCliProgress = showCliProgress;
this._onProgress = safeEventCallback(onProgress);
this._deleteTempFileOnCancel = deleteTempFileOnCancel;
this._skipExisting = skipExisting;
this._parallelDownloads = parallelDownloads;
this._tokens = tokens;
this._endpoints = endpoints;
this._onDownloadProgress = this._onDownloadProgress.bind(this);
}
public get entrypointFilename() {
return this._entrypointFilename!;
}
public get entrypointFilePath() {
return path.join(this._dirPath, this.entrypointFilename);
}
public get splitBinaryParts() {
return this._splitBinaryParts;
}
public get totalFiles() {
return this._totalFiles!;
}
public get totalSize() {
return this._specificFileDownloaders
.map((downloader) => downloader.status.totalBytes)
.reduce((acc, totalBytes) => acc + totalBytes, 0);
}
public get downloadedSize() {
return this._specificFileDownloaders
.map((downloader) => downloader.status.transferredBytes)
.reduce((acc, transferredBytes) => acc + transferredBytes, 0);
}
public async download({
signal
}: {
signal?: AbortSignal
} = {}) {
if (signal?.aborted)
throw signal.reason;
const onAbort = () => {
signal?.removeEventListener("abort", onAbort);
this.cancel();
};
if (signal != null)
signal.addEventListener("abort", onAbort);
try {
if (this._onProgress)
this._downloader!.on("progress", this._onDownloadProgress);
await this._downloader!.download();
} catch (err) {
if (signal?.aborted)
throw signal.reason;
throw err;
} finally {
if (this._onProgress)
this._downloader!.off("progress", this._onDownloadProgress);
if (signal != null)
signal.removeEventListener("abort", onAbort);
}
return this.entrypointFilePath;
}
public async cancel({
deleteTempFile = this._deleteTempFileOnCancel
}: {
deleteTempFile?: boolean
} = {}) {
for (const downloader of this._specificFileDownloaders)
await downloader.close({deleteTempFile});
if (this._downloader !== this._specificFileDownloaders[0])
await this._downloader?.close({deleteTempFile});
}
private _onDownloadProgress() {
this._onProgress?.({
totalSize: this.totalSize,
downloadedSize: this.downloadedSize
});
}
private async resolveTryHeaders() {
if (this._tokens == null)
return;
pushAll(
this._tryHeaders,
await resolveModelFileAccessTokensTryHeaders(this._modelUrl, this._tokens, this._endpoints, this._headers)
);
}
public async _init() {
await this.resolveTryHeaders();
const binarySplitPartUrls = resolveBinarySplitGgufPartUrls(this._modelUrl);
await fs.ensureDir(this._dirPath);
if (binarySplitPartUrls instanceof Array) {
this._downloader = await downloadFile({
partURLs: binarySplitPartUrls,
directory: this._dirPath,
fileName: this._fileName ?? getFilenameForBinarySplitGgufPartUrls(binarySplitPartUrls),
cliProgress: this._showCliProgress,
cliStyle: isCI ? "ci" : "fancy",
headers: this._headers ?? {},
tryHeaders: this._tryHeaders.slice(),
skipExisting: this._skipExisting
});
this._specificFileDownloaders.push(this._downloader);
this._entrypointFilename = this._downloader.fileName;
this._splitBinaryParts = binarySplitPartUrls.length;
this._totalFiles = 1;
if (this._downloader.fileName == null || this._downloader.fileName === "")
throw new Error("Failed to get the file name from the given URL");
return;
}
const splitGgufPartUrls = resolveSplitGgufParts(this._modelUrl);
if (splitGgufPartUrls.length === 1) {
this._downloader = await downloadFile({
url: splitGgufPartUrls[0]!,
directory: this._dirPath,
fileName: this._fileName ?? undefined,
cliProgress: this._showCliProgress,
cliStyle: isCI ? "ci" : "fancy",
headers: this._headers ?? {},
tryHeaders: this._tryHeaders.slice(),
skipExisting: this._skipExisting
});
this._specificFileDownloaders.push(this._downloader);
this._entrypointFilename = this._downloader.fileName;
this._totalFiles = 1;
if (this._downloader.fileName == null || this._downloader.fileName === "")
throw new Error("Failed to get the file name from the given URL");
return;
}
const partDownloads = splitGgufPartUrls.map((url, index) => downloadFile({
url,
directory: this._dirPath,
fileName: this._fileName != null
? createSplitPartFilename(this._fileName, index + 1, splitGgufPartUrls.length)
: undefined,
headers: this._headers ?? {},
tryHeaders: this._tryHeaders.slice(),
skipExisting: this._skipExisting
}));
this._downloader = await downloadSequence(
{
cliProgress: this._showCliProgress,
cliStyle: isCI ? "ci" : "fancy",
parallelDownloads: this._parallelDownloads
},
...partDownloads
);
const firstDownload = await partDownloads[0]!;
this._specificFileDownloaders = await Promise.all(partDownloads);
this._entrypointFilename = firstDownload.fileName;
this._totalFiles = partDownloads.length;
if (this._entrypointFilename == null || this._entrypointFilename === "")
throw new Error("Failed to get the file name from the given URL");
return;
}
public static async _create(options: ModelDownloaderOptions) {
const {
modelUri, modelUrl, dirPath = cliModelsDirectory, fileName, _showUriResolvingProgress = false
} = options as ModelDownloaderOptions & {
modelUri?: string,
modelUrl?: string
};
const resolvedModelUri = modelUri || modelUrl;
if (resolvedModelUri == null || dirPath == null)
throw new Error("modelUri and dirPath cannot be null");
async function getModelUrlAndFilename(): Promise<{
resolvedModelUrl: string,
resolvedFileName?: string
}> {
const resolvedModelDestination = resolveModelDestination(resolvedModelUri!, undefined, options.endpoints);
if (resolvedModelDestination.type == "file")
return {
resolvedModelUrl: path.resolve(dirPath, resolvedModelDestination.path),
resolvedFileName: fileName
};
else if (resolvedModelDestination.type === "url")
return {
resolvedModelUrl: resolvedModelDestination.url,
resolvedFileName: fileName
};
else if (resolvedModelDestination.parsedUri.type === "resolved")
return {
resolvedModelUrl: resolvedModelDestination.parsedUri.resolvedUrl,
resolvedFileName: fileName || resolvedModelDestination.parsedUri.fullFilename
};
const resolvedUri = _showUriResolvingProgress
? await withOra({
loading: chalk.blue("Resolving model URI"),
success: chalk.blue("Resolved model URI"),
fail: chalk.blue("Failed to resolve model URI"),
noSuccessLiveStatus: true
}, () => {
return resolveParsedModelUri(resolvedModelDestination.parsedUri, {
tokens: options.tokens,
endpoints: options.endpoints,
authorizationHeader: getAuthorizationHeader(options.headers)
});
})
: await resolveParsedModelUri(resolvedModelDestination.parsedUri, {
tokens: options.tokens,
endpoints: options.endpoints,
authorizationHeader: getAuthorizationHeader(options.headers)
});
return {
resolvedModelUrl: resolvedUri.resolvedUrl,
resolvedFileName: fileName || resolvedUri.fullFilename
};
}
const modelDownloader = new ModelDownloader(options, await getModelUrlAndFilename());
await modelDownloader._init();
return modelDownloader;
}
}
export type CombinedModelDownloaderOptions = {
showCliProgress?: boolean,
onProgress?: (status: {totalSize: number, downloadedSize: number}) => void,
parallelDownloads?: number
};
export class CombinedModelDownloader {
private readonly _downloaders: readonly ModelDownloader[];
private readonly _showCliProgress: boolean;
private readonly _onProgress?: CombinedModelDownloaderOptions["onProgress"];
private readonly _parallelDownloads: number;
private readonly _lock = {};
private _downloader?: DownloadEngineMultiDownload;
private constructor(downloaders: ModelDownloader[], options?: CombinedModelDownloaderOptions) {
const {
showCliProgress = false,
onProgress,
parallelDownloads = 4
} = options ?? {};
this._downloaders = Object.freeze(downloaders);
this._showCliProgress = showCliProgress;
this._onProgress = onProgress;
this._parallelDownloads = parallelDownloads;
this._onDownloadProgress = this._onDownloadProgress.bind(this);
}
public async cancel() {
for (const modelDownloader of this._downloaders) {
if (modelDownloader._specificFileDownloaders.every(
(downloader) => downloader.status.downloadStatus === "Finished"
))
continue;
for (const downloader of modelDownloader._specificFileDownloaders)
await downloader.close({
deleteTempFile: modelDownloader._deleteTempFileOnCancel
});
}
}
public async download({
signal
}: {
signal?: AbortSignal
} = {}) {
if (signal?.aborted)
throw signal.reason;
const onAbort = () => {
signal?.removeEventListener("abort", onAbort);
this.cancel();
};
if (signal != null)
signal.addEventListener("abort", onAbort);
try {
if (this._onProgress)
this._downloader!.on("progress", this._onDownloadProgress);
await this._downloader!.download();
} catch (err) {
if (signal?.aborted)
throw signal.reason;
throw err;
} finally {
if (this._onProgress)
this._downloader!.off("progress", this._onDownloadProgress);
if (signal != null)
signal.removeEventListener("abort", onAbort);
}
return this.entrypointFilePaths;
}
public get modelDownloaders(): readonly ModelDownloader[] {
return this._downloaders;
}
public get entrypointFilenames() {
return this._downloaders.map((downloader) => downloader.entrypointFilename);
}
public get entrypointFilePaths() {
return this._downloaders.map((downloader) => downloader.entrypointFilePath);
}
public get totalFiles() {
return this._downloaders
.map((downloader) => downloader.totalFiles)
.reduce((acc, totalFiles) => acc + totalFiles, 0);
}
public get totalSize() {
return this._downloaders
.map((downloader) => downloader.totalSize)
.reduce((acc, totalBytes) => acc + totalBytes, 0);
}
public get downloadedSize() {
return this._downloaders
.map((downloader) => downloader.downloadedSize)
.reduce((acc, transferredBytes) => acc + transferredBytes, 0);
}
private _onDownloadProgress() {
this._onProgress?.({
totalSize: this.totalSize,
downloadedSize: this.downloadedSize
});
}
public async _init() {
this._downloader = await downloadSequence(
{
cliProgress: this._showCliProgress,
cliStyle: isCI ? "ci" : "fancy",
parallelDownloads: this._parallelDownloads
},
...this._downloaders.flatMap((downloader) => downloader._specificFileDownloaders)
);
}
public static _create(downloaders: ModelDownloader[], options?: CombinedModelDownloaderOptions) {
return new CombinedModelDownloader(downloaders, options);
}
}