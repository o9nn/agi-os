import path from "path";
import fs from "fs-extra";
import chalk from "chalk";
import {cliModelsDirectory} from "../config.js";
import {getReadablePath} from "../cli/utils/getReadablePath.js";
import {resolveSplitGgufParts} from "../gguf/utils/resolveSplitGgufParts.js";
import {resolveModelDestination} from "./resolveModelDestination.js";
import {ModelFileAccessTokens} from "./modelFileAccessTokens.js";
import {ModelDownloadEndpoints} from "./modelDownloadEndpoints.js";
import {createModelDownloader} from "./createModelDownloader.js";
import {genericFilePartNumber} from "./parseModelUri.js";
import {isFilePartText} from "./parseModelFileName.js";
import {pushAll} from "./pushAll.js";
export type ResolveModelFileOptions = {
    directory?: string,
    download?: "auto" | false,
    verify?: boolean,
    fileName?: string,
    headers?: Record<string, string>,
    cli?: boolean,
    onProgress?: (status: {totalSize: number, downloadedSize: number}) => void,
    deleteTempFileOnCancel?: boolean,
    parallel?: number,
    tokens?: ModelFileAccessTokens,
    endpoints?: ModelDownloadEndpoints,
    signal?: AbortSignal
};
export async function resolveModelFile(
    uriOrPath: string,
    optionsOrDirectory?: ResolveModelFileOptions | string
): Promise<string> {
    const {
        directory,
        download = "auto",
        verify = false,
        fileName,
        headers,
        cli = true,
        onProgress,
        deleteTempFileOnCancel = true,
        parallel = 4,
        tokens,
        endpoints,
        signal
    } = typeof optionsOrDirectory === "string"
        ? {directory: optionsOrDirectory}
        : (optionsOrDirectory ?? {});
    const resolvedDirectory = directory || cliModelsDirectory;
    const resolvedCli = cli == null ? true : cli;
    let resolvedVerify = verify ?? false;
    if (download === false)
        resolvedVerify = false;
    const resolvedModelDestination = resolveModelDestination(uriOrPath, undefined, endpoints);
    if (resolvedModelDestination.type === "file") {
        const resolvedFilePath = path.resolve(resolvedDirectory, uriOrPath);
        if (await fs.pathExists(resolvedFilePath))
            return resolvedFilePath;
        throw new Error(`No model file found at "${resolvedFilePath}"`);
    }
    const expectedFileNames: string[] = fileName != null
        ? [fileName]
        : [];
    if (expectedFileNames.length === 0 && resolvedModelDestination.type === "uri") {
        if (resolvedModelDestination.parsedUri.type === "resolved")
            expectedFileNames.push(resolvedModelDestination.parsedUri.fullFilename);
        else
            pushAll(expectedFileNames, resolvedModelDestination.parsedUri.possibleFullFilenames);
    } else if (expectedFileNames.length === 0 && resolvedModelDestination.type === "url") {
        const enforcedParsedUrl = resolveModelDestination(uriOrPath, true, endpoints);
        if (enforcedParsedUrl != null && enforcedParsedUrl.type === "uri") {
            if (enforcedParsedUrl.parsedUri.type === "resolved")
                expectedFileNames.push(enforcedParsedUrl.parsedUri.fullFilename);
            else
                pushAll(expectedFileNames, enforcedParsedUrl.parsedUri.possibleFullFilenames);
        }
    }
    const foundExpectedFilePath = await findMatchingFilesInDirectory(resolvedDirectory, expectedFileNames);
    if (foundExpectedFilePath != null && !resolvedVerify) {
        const allGgufParts = resolveSplitGgufParts(foundExpectedFilePath);
        if (allGgufParts.length === 1 && allGgufParts[0] === foundExpectedFilePath)
            return foundExpectedFilePath;
        const allPartsExist = await Promise.all(allGgufParts.map((part) => fs.pathExists(part)));
        if (allGgufParts.length > 0) {
            if (allPartsExist.every((exists) => exists))
                return allGgufParts[0]!;
            else if (download === false)
                throw new Error(`Not all split parts of the model file "${allGgufParts[0]}" are present in the same directory`);
        }
    }
    if (download === false) {
        if (expectedFileNames.length === 1)
            throw new Error(`No model file found at "${path.join(resolvedDirectory, expectedFileNames[0]!)}" and download is disabled`);
        throw new Error(`No model file found for "${uriOrPath}" at "${resolvedDirectory}" and download is disabled`);
    }
    if (signal?.aborted)
        throw signal.reason;
    const downloader = await createModelDownloader({
        modelUri: resolvedModelDestination.type === "uri"
            ? resolvedModelDestination.uri
            : resolvedModelDestination.url,
        dirPath: resolvedDirectory,
        headers,
        showCliProgress: resolvedCli,
        deleteTempFileOnCancel,
        skipExisting: true,
        fileName: fileName || undefined,
        parallelDownloads: parallel,
        onProgress,
        tokens,
        endpoints
    });
    if (foundExpectedFilePath != null && downloader.totalFiles === 1 && await fs.pathExists(downloader.entrypointFilePath)) {
        const fileStats = await fs.stat(foundExpectedFilePath);
        if (downloader.totalSize === fileStats.size) {
            await downloader.cancel({deleteTempFile: false});
            return foundExpectedFilePath;
        }
    }
    if (resolvedCli)
        console.info(`Downloading to ${chalk.yellow(getReadablePath(resolvedDirectory))}${
            downloader.splitBinaryParts != null
                ? chalk.gray(` (combining ${downloader.splitBinaryParts} parts into a single file)`)
                : ""
        }`);
    await downloader.download({signal});
    if (resolvedCli)
        console.info(`Downloaded to ${chalk.yellow(getReadablePath(downloader.entrypointFilePath))}`);
    return downloader.entrypointFilePath;
}
async function findMatchingFilesInDirectory(dirPath: string, fileNames: (string | `${string}${typeof genericFilePartNumber}${string}`)[]) {
    let directoryFileNames: string[] | undefined = undefined;
    if (!(await fs.pathExists(dirPath)) || !(await fs.stat(dirPath)).isDirectory())
        return undefined;
    for (const expectedFileName of fileNames) {
        if (expectedFileName.includes(genericFilePartNumber)) {
            const [firstPart, ...restParts] = expectedFileName.split(genericFilePartNumber);
            const resolvedFirstPart = firstPart || "";
            const resolvedLastParts = restParts.join(genericFilePartNumber);
            if (directoryFileNames == null)
                directoryFileNames = (await fs.readdir(dirPath, {withFileTypes: true}))
                    .filter((item) => item.isFile())
                    .map((item) => item.name);
            for (const directoryFileName of directoryFileNames) {
                if (directoryFileName.startsWith(resolvedFirstPart) && directoryFileName.endsWith(resolvedLastParts)) {
                    const numberPart = directoryFileName.slice(resolvedFirstPart.length, -resolvedLastParts.length);
                    if (isFilePartText(numberPart))
                        return path.join(dirPath, directoryFileName);
                }
            }
            continue;
        }
        const testPath = path.join(dirPath, expectedFileName);
        if (await fs.pathExists(testPath))
            return testPath;
    }
    return undefined;
}