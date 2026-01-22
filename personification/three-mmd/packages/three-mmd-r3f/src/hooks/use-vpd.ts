import { VPDLoader } from '@moeru/three-mmd'
import { useLoader } from '@react-three/fiber'
const useVPD = (path: string) => useLoader(VPDLoader, path)
useVPD.preload = (path: string) =>
  useLoader.preload(VPDLoader, path)
useVPD.clear = (path: string) =>
  useLoader.clear(VPDLoader, path)
export { useVPD }