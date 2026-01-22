import process from "process";
import {LlamaGpuType} from "../types.js";
import {getGpuTypesToUseForOption} from "./getGpuTypesToUseForOption.js";
import {getPlatform} from "./getPlatform.js";
export async function getLlamaGpuTypes(include: "supported" | "allValid"): Promise<LlamaGpuType[]> {
const platform = getPlatform();
const arch = process.arch;
if (include === "supported") {
const gpuTypes = new Set(await getGpuTypesToUseForOption("auto"));
if (platform === "win" && arch !== "x64")
gpuTypes.delete("vulkan");
return [...gpuTypes];
}
const res: LlamaGpuType[] = [];
if (platform === "mac" && arch === "arm64")
res.push("metal");
else
res.push("cuda");
res.push("vulkan");
res.push(false);
return res;
}