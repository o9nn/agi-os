import type { PmxObject } from 'babylon-mmd/esm/Loader/Parser/pmxObject'
import type { LoadingManager } from 'three'
import { PmdReader } from 'babylon-mmd/esm/Loader/Parser/pmdReader'
import {
FileLoader,
Loader,
} from 'three'
export class PMDLoader extends Loader<PmxObject> {
constructor(manager?: LoadingManager) {
super(manager)
}
public load(
url: string,
onLoad: (object: PmxObject) => void,
onProgress?: (event: ProgressEvent) => void,
onError?: (event: ErrorEvent) => void,
): void {
const loader = new FileLoader(this.manager)
loader.setResponseType('arraybuffer')
loader.setPath(this.path)
loader.setRequestHeader(this.requestHeader)
loader.setWithCredentials(this.withCredentials)
loader.load(
url,
buffer => void PmdReader.ParseAsync(buffer as ArrayBuffer)
.then(onLoad)
.catch(onError),
onProgress,
onError as (error: unknown) => void,
)
}
public async loadAsync(
url: string,
onProgress?: (event: ProgressEvent) => void,
): Promise<PmxObject> {
return super.loadAsync(url, onProgress)
}
}