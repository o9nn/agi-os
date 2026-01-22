export interface UMAPOptions {
  metric?: 'euclidean' | 'cosine'
  knn_method?: 'hnsw' | 'nndescent' | 'vptree'
  initialize_method?: 'spectral' | 'random' | 'none'
  local_connectivity?: number
  bandwidth?: number
  mix_ratio?: number
  spread?: number
  min_dist?: number
  a?: number
  b?: number
  repulsion_strength?: number
  n_epochs?: number
  learning_rate?: number
  negative_sample_rate?: number
  n_neighbors?: number
  seed?: number
}
export interface UMAP {
  get epoch(): number
  get input_dim(): number
  get output_dim(): number
  get embedding(): Float32Array
  run: (epoch_limit?: number) => void
  destroy: () => void
}
export function createUMAP(
  count: number,
  input_dim: number,
  output_dim: number,
  data: Float32Array,
  options?: UMAPOptions,
): Promise<UMAP>
export interface KNNOptions {
  metric?: 'euclidean' | 'cosine'
  method?: 'hnsw' | 'nndescent' | 'vptree'
}
export interface KNNQueryResult {
  indices: Int32Array
  distnaces: Float32Array
}
export interface KNN {
  query_by_index: (index: number, k: number) => KNNQueryResult
  query_by_vector: (data: Float32Array, k: number) => KNNQueryResult
  destroy: () => void
}
export function createKNN(count: number, input_dim: number, data: Float32Array, options?: KNNOptions): Promise<KNN>