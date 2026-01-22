export function isDeepEqualData(obj1: any, obj2: any): boolean {
if (obj1 === obj2)
return true
if (obj1 == null || obj2 == null)
return false
if (typeof obj1 !== 'object' && typeof obj2 !== 'object')
return obj1 === obj2
if (obj1.constructor !== obj2.constructor)
return false
if (obj1 instanceof Date && obj2 instanceof Date) {
return obj1.getTime() === obj2.getTime()
}
if (Array.isArray(obj1)) {
if (obj1.length !== obj2.length)
return false
for (let i = 0; i < obj1.length; i++) {
if (!isDeepEqualData(obj1[i], obj2[i]))
return false
}
return true
}
const keys1 = Object.keys(obj1)
const keys2 = Object.keys(obj2)
if (keys1.length !== keys2.length)
return false
for (const key of keys1) {
if (!keys2.includes(key))
return false
if (!isDeepEqualData(obj1[key], obj2[key]))
return false
}
return true
}