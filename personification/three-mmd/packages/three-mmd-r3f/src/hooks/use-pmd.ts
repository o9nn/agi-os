import { PMDLoader } from '@moeru/three-mmd'
import { useLoader } from '@react-three/fiber'
const usePMD = (path: string) => useLoader(PMDLoader, path)
usePMD.preload = (path: string) =>
useLoader.preload(PMDLoader, path)
usePMD.clear = (path: string) =>
useLoader.clear(PMDLoader, path)
export { usePMD }