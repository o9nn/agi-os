import { sqliteTable, text } from 'drizzle-orm/sqlite-core'
import type { Data } from '../utils/ccv3/types'
export const charactersTable = sqliteTable('characters_table', {
  avatar: text(),
  data: text({ mode: 'json' }).$type<Data>().notNull(),
  id: text().primaryKey(),
  name: text().notNull(),
})