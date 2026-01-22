import type {GgufReadOffset} from "../utils/GgufReadOffset.js";
import type {GgufFileReader} from "../fileReaders/GgufFileReader.js";
import type {MergeOptionalUnionTypes} from "../../utils/mergeUnionTypes.js";
import type {GgufArchitectureType, GgufMetadata} from "./GgufMetadataTypes.js";
import type {GgufTensorInfo} from "./GgufTensorInfoTypes.js";
export type MetadataValue = string | number | bigint | boolean | MetadataValue[];
export type MetadataKeyValueRecord = Record<string, MetadataValue>;
export type MetadataNestedObject = {
    [key: string]: MetadataValue | MetadataNestedObject
};
export type GgufFileInfo = {
    readonly version: 2 | 3 | number,
    readonly tensorCount: number | bigint,
    readonly metadata: GgufMetadata,
    readonly metadataSize: number,
    readonly architectureMetadata: MergeOptionalUnionTypes<Exclude<GgufMetadata[GgufArchitectureType], undefined>>,
    readonly tensorInfo?: GgufTensorInfo[],
    readonly tensorInfoSize?: number,
    readonly splicedParts: number,
    readonly totalTensorCount: number | bigint,
    readonly totalMetadataSize: number,
    readonly fullTensorInfo?: GgufTensorInfo[],
    readonly totalTensorInfoSize?: number
};
export const enum GgufValueType {
    Uint8 = 0,
    Int8 = 1,
    Uint16 = 2,
    Int16 = 3,
    Uint32 = 4,
    Int32 = 5,
    Float32 = 6,
    Bool = 7,
    String = 8,
    Array = 9,
    Uint64 = 10,
    Int64 = 11,
    Float64 = 12
}
export type GgufVersionParserOptions = {
    fileReader: GgufFileReader,
    readTensorInfo?: boolean,
    ignoreKeys?: string[],
    version: number,
    readOffset: GgufReadOffset,
    logWarnings: boolean
};
export type GgufVersionParserResult = {
    tensorCount: number | bigint,
    metadata: GgufMetadata,
    tensorInfo?: GgufTensorInfo[],
    metadataSize: number,
    tensorInfoSize?: number,
    tensorDataOffset?: number
};