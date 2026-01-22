import { drizzle } from 'drizzle-orm/sqlite-proxy'
import { SQLocalDrizzle } from 'sqlocal/drizzle'
import init from './generated/0000_init.sql?raw'
const { batchDriver, driver, sql } = new SQLocalDrizzle('moetalk.db')
await sql(init.replace('CREATE TABLE', 'CREATE TABLE IF NOT EXISTS'))
export const db = drizzle(driver, batchDriver)