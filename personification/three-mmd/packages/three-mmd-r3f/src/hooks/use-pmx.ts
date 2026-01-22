import { PMXLoader } from '@moeru/three-mmd'
import { useLoader } from '@react-three/fiber'
const usePMX = (path: string) => useLoader(PMXLoader, path)
usePMX.preload = (path: string) =>
  useLoader.preload(PMXLoader, path)
usePMX.clear = (path: string) =>
  useLoader.clear(PMXLoader, path)
export { usePMX }