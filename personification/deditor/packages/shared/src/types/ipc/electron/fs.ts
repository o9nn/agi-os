import type { Mode, OpenMode, PathLike, WriteFileOptions } from 'node:fs'
export interface FsMethods {
exists: (params: { path: PathLike }) => boolean
readFile: (params: { path: PathLike, options?: ({ encoding?: null | undefined, flag?: OpenMode | undefined }) }) => ArrayBufferLike
writeFile: (params: { path: PathLike, data: ArrayBufferLike, options?: WriteFileOptions }) => void
mkdir: (params: {
path: PathLike
recursive?: boolean | undefined
mode?: Mode | undefined
}) => void
}