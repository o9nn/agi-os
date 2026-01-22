import {isUrl} from "./isUrl.js";
export type ModelDownloadEndpoints = {
huggingFace?: string
};
export function resolveHuggingFaceEndpoint(endpoints?: ModelDownloadEndpoints) {
const ensureLastSlash = (url: string) => (
url.endsWith("/")
? url
: url + "/"
);
if (endpoints?.huggingFace != null && isUrl(endpoints?.huggingFace, false))
return ensureLastSlash(endpoints?.huggingFace);
const modelEndpoint = process.env.MODEL_ENDPOINT;
if (modelEndpoint != null && isUrl(modelEndpoint, false))
return ensureLastSlash(modelEndpoint);
const hfEndpoint = process.env.HF_ENDPOINT;
if (hfEndpoint != null && isUrl(hfEndpoint, false))
return ensureLastSlash(hfEndpoint);
return "https://huggingface.co/";
}
export function isHuggingFaceUrl(url: string, endpoints?: ModelDownloadEndpoints) {
const parsedUrl = new URL(url);
const hfEndpoint = resolveHuggingFaceEndpoint(endpoints);
const hfEndpointDomain = (new URL(hfEndpoint)).hostname;
if (parsedUrl.hostname === hfEndpointDomain)
return true;
return (
(hfEndpoint === "https://huggingface.co/" || hfEndpoint === "https://hf.co/") &&
(parsedUrl.hostname === "huggingface.co" || parsedUrl.hostname === "hf.co")
);
}