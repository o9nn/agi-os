import '@tanstack/vue-table'
declare module '@tanstack/vue-table' {
interface ColumnMeta<TData, TValue> {
filterable?: boolean
}
}