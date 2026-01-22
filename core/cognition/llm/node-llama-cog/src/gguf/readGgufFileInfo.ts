import retry from "async-retry";
import {isUrl} from "../utils/isUrl.js";
import {ModelFileAccessTokens} from "../utils/modelFileAccessTokens.js";
import {getAuthorizationHeader, isModelUri, parseModelUri, resolveParsedModelUri} from "../utils/parseModelUri.js";
import {Writable} from "../utils/utilTypes.js";
import {ModelDownloadEndpoints} from "../utils/modelDownloadEndpoints.js";
import {parseGguf} from "./parser/parseGguf.js";
import {GgufNetworkFetchFileReader} from "./fileReaders/GgufNetworkFetchFileReader.js";
import {GgufFsFileReader} from "./fileReaders/GgufFsFileReader.js";
import {ggufDefaultFetchRetryOptions} from "./consts.js";
import {normalizeGgufDownloadUrl} from "./utils/normalizeGgufDownloadUrl.js";
import {resolveSplitGgufParts} from "./utils/resolveSplitGgufParts.js";
import {GgufFileInfo} from "./types/GgufFileInfoTypes.js";
import {GgufTensorInfo} from "./types/GgufTensorInfoTypes.js";
export async function readGgufFileInfo(pathOrUri: string, {
readTensorInfo = true,
sourceType,
ignoreKeys = [],
logWarnings = true,
fetchRetryOptions = ggufDefaultFetchRetryOptions,
fetchHeaders = {},
spliceSplitFiles = true,
signal,
tokens,
endpoints
}: {
readTensorInfo?: boolean,
sourceType?: "network" | "filesystem",
ignoreKeys?: string[],
logWarnings?: boolean,
fetchRetryOptions?: retry.Options,
fetchHeaders?: Record<string, string>,
spliceSplitFiles?: boolean,
signal?: AbortSignal,
tokens?: ModelFileAccessTokens,
endpoints?: ModelDownloadEndpoints
} = {}) {
const useNetworkReader = sourceType === "network" || (sourceType == null && (isUrl(pathOrUri) || isModelUri(pathOrUri)));
async function createFileReader(pathOrUri: string) {
if (useNetworkReader) {
const parsedModelUri = await resolveParsedModelUri(parseModelUri(pathOrUri, undefined, endpoints), {
tokens, endpoints, signal,
authorizationHeader: getAuthorizationHeader(fetchHeaders)
});
return new GgufNetworkFetchFileReader({
url: parsedModelUri?.resolvedUrl ?? normalizeGgufDownloadUrl(pathOrUri, endpoints),
retryOptions: fetchRetryOptions,
headers: fetchHeaders,
signal,
tokens,
endpoints
});
} else if (sourceType === "filesystem" || sourceType == null) {
return new GgufFsFileReader({
filePath: pathOrUri,
signal
});
}
void (sourceType satisfies never);
throw new Error(`Unsupported sourceType: ${sourceType}`);
}
async function readSingleFile(pathOrUri: string, splitPartNumber: number = 1) {
const fileReader = await createFileReader(pathOrUri);
const res = await parseGguf({
fileReader,
ignoreKeys,
readTensorInfo,
logWarnings
});
if (splitPartNumber > 1) {
for (const tensor of res.tensorInfo ?? [])
(tensor as Writable<GgufTensorInfo>).filePart = splitPartNumber;
}
return res;
}
if (!spliceSplitFiles)
return await readSingleFile(pathOrUri);
const allSplitPartPaths = resolveSplitGgufParts(pathOrUri);
if (allSplitPartPaths.length === 1)
return await readSingleFile(allSplitPartPaths[0]!);
const [first, ...rest] = await Promise.all(
allSplitPartPaths.map((partPath, index) => readSingleFile(partPath, index + 1))
);
if (first == null)
throw new Error("First part of the split GGUF file is missing");
return {
version: first.version,
tensorCount: first.tensorCount,
metadata: first.metadata,
architectureMetadata: first.architectureMetadata,
tensorInfo: first.tensorInfo,
metadataSize: first.metadataSize,
splicedParts: allSplitPartPaths.length,
totalTensorInfoSize: first.totalTensorInfoSize == null
? undefined
: (first.totalTensorInfoSize + rest.reduce((acc, part) => (acc + (part.totalTensorInfoSize ?? 0)), 0)),
totalTensorCount: Number(first.totalTensorCount) + rest.reduce((acc, part) => acc + Number(part.totalTensorCount), 0),
totalMetadataSize: first.totalMetadataSize + rest.reduce((acc, part) => acc + part.totalMetadataSize, 0),
fullTensorInfo: first.fullTensorInfo == null
? undefined
: [first, ...rest].flatMap((part) => (part.fullTensorInfo ?? [])),
tensorInfoSize: first.tensorInfoSize
} satisfies GgufFileInfo;
}