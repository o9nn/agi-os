#!/usr/bin/env bash
function log {
echo "$@" 1>&2
}
function usage {
log "Usage: $0 [[--url] <url>] [--repo <repo>] [--file <file>] [--outdir <dir> [-h|--help]"
exit 1
}
function has_cmd {
if ! [ -x "$(command -v $1)" ]; then
return 1
fi
}
if has_cmd wget; then
cmd="wget -q -c -O %s/%s %s"
elif has_cmd curl; then
cmd="curl -C - -f --output-dir %s -o %s -L %s"
else
log "[E] curl or wget not found"
exit 1
fi
url=""
repo=""
file=""
outdir="."
while [[ $# -gt 0 ]]; do
case "$1" in
--url)
url="$2"
shift 2
;;
--repo)
repo="$2"
shift 2
;;
--file)
file="$2"
shift 2
;;
--outdir)
outdir="$2"
shift 2
;;
-h|--help)
usage
;;
*)
url="$1"
shift
;;
esac
done
if [ -n "$repo" ] && [ -n "$file" ]; then
url="https://huggingface.co/$repo/resolve/main/$file"
fi
if [ -z "$url" ]; then
log "[E] missing --url"
usage
fi
is_url=false
if [[ ${
if [[ ${url:0:22} == "https://huggingface.co" ]]; then
is_url=true
fi
fi
if [ "$is_url" = false ]; then
log "[E] invalid URL, must start with https://huggingface.co"
exit 0
fi
url=${url/blob\/main/resolve\/main}
basename=$(basename $url)
log "[+] attempting to download $basename"
if [ -n "$cmd" ]; then
cmd=$(printf "$cmd" "$outdir" "$basename" "$url")
log "[+] $cmd"
if $cmd; then
echo $outdir/$basename
exit 0
fi
fi
log "[-] failed to download"
exit 1