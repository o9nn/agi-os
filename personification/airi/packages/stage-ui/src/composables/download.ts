export function useDownload(data: Blob, filename: string) {
function download() {
const url = URL.createObjectURL(data)
const downloadLink = document.createElement('a')
downloadLink.href = url
downloadLink.download = filename
downloadLink.click()
setTimeout(() => {
URL.revokeObjectURL(url)
}, 5000)
}
return {
download,
}
}