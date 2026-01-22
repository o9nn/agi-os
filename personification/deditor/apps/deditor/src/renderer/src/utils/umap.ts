export function selectNEpochs(count: number) {
if (count <= 2500) {
return 500
}
else if (count <= 5000) {
return 400
}
else if (count <= 7500) {
return 300
}
else {
return 200
}
}