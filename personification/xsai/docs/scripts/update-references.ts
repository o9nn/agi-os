import { findWorkspaceDir } from '@pnpm/find-workspace-dir'
import { glob, readFile, writeFile } from 'node:fs/promises'
import { resolve } from 'node:path'
import { cwd } from 'node:process'
const pages = []
const workspaceDir = await findWorkspaceDir(cwd())
const packagesTop = glob(`${workspaceDir}/packages-toppackage.json`)
for await (const pkg of packagesTop) {
const { name, private: priv } = JSON.parse(await readFile(pkg, 'utf8')) as { name: string, private?: boolean }
if (priv !== true)
pages.push(`[${name}](https://doc.deno.land/https://esm.sh/${name})`)
}
const packagesExt = glob(`${workspaceDir}/packages-extpackage.json`)
for await (const pkg of packagesExt) {
const { name, private: priv } = JSON.parse(await readFile(pkg, 'utf8')) as { name: string, private?: boolean }
if (priv !== true)
pages.push(`[${name}](https://doc.deno.land/https://esm.sh/${name})`)
}
const packages = glob(`${workspaceDir}/packagespackage.json`)
for await (const pkg of packages) {
const { name, private: priv } = JSON.parse(await readFile(pkg, 'utf8')) as { name: string, private?: boolean }
if (priv !== true)
pages.push(`[${name}](https://doc.deno.land/https://esm.sh/${name})`)
}
const json = JSON.stringify({
defaultOpen: false,
pages: pages.toReversed(),
title: 'Packages',
}, null, 2)
await writeFile(resolve(import.meta.dirname, '../content/docs/references/meta.json'), `${json}\n`)