import { SpaceCard as SpaceCardImported, LFS as LFSImported } from './index'
const SpaceCard = SpaceCardImported.rspack as typeof SpaceCardImported.rspack
const LFS = LFSImported.rspack as typeof LFSImported.rspack
const exports = {
  SpaceCard: SpaceCard as typeof SpaceCardImported.rspack,
  LFS: LFS as typeof LFSImported.rspack,
}
export { SpaceCard, LFS }
export { exports as 'module.exports' }
export type * from './plugins/lfs'
export type * from './plugins/space-card'