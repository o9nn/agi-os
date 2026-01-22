import type { Configuration } from 'electron-builder'
import process from 'node:process'
import { readFile } from 'node:fs/promises'
import { resolve } from 'node:path'
import { execa } from 'execa'
import * as yaml from 'yaml'
import packageJSON from '../package.json' assert { type: 'json' }
export async function getVersion(options: { release: boolean, autoTag: boolean, tag: string[] }) {
  if (!options.release || !options.tag) {
    const res = await execa('git', ['log', '-1', '--pretty=format:"%H"'])
    const date = new Date().toISOString().split('T')[0].replace(/-/g, '')
    return `nightly-${date}-${String(res.stdout.replace(/"/g, '')).trim().substring(0, 7)}`
  }
  let version = packageJSON.version
  if (options.tag[0] !== 'true') {
    version = String(options.tag[0]).replace(/^v/, '').trim()
  }
  else {
    version = ''
  }
  if (version) {
    return version
  }
  if (!options.autoTag) {
    throw new Error('Tag cannot be empty when --release is specified')
  }
  try {
    const res = await execa('git', ['describe', '--tags', '--abbrev=0'])
    return String(res.stdout).replace(/^v/, '').trim()
  }
  catch {
    console.warn('No git tags found, falling back to package.json version')
    return packageJSON.version
  }
}
export async function getElectronBuilderConfig() {
  return yaml.parse(await readFile(resolve(import.meta.dirname, '..', 'electron-builder.yml'), 'utf-8')) as Configuration
}
export async function getFilename(target: string, options: { release: boolean, autoTag: boolean, tag: string[] }) {
  const electronBuilder = await getElectronBuilderConfig()
  const version = await getVersion(options)
  if (!target) {
    throw new Error('<Target> is required')
  }
  switch (target) {
    case 'x86_64-pc-windows-msvc':
      return electronBuilder.nsis?.artifactName
        ?.replace('${productName}', electronBuilder.productName!)
        .replace('${version}', version)
        .replace('${arch}', 'x64')
        .replace('${ext}', 'exe') ?? `${electronBuilder.productName}-${version}-windows-x64-setup.exe`
      break
    case 'x86_64-unknown-linux-gnu':
      return electronBuilder.linux?.artifactName
        ?.replace('${productName}', electronBuilder.productName!)
        .replace('${version}', version)
        .replace('${arch}', 'x64')
        .replace('${ext}', 'AppImage') ?? `${electronBuilder.productName}-${version}-linux-x64.AppImage`
      break
    case 'aarch64-unknown-linux-gnu':
      return electronBuilder.linux?.artifactName
        ?.replace('${productName}', electronBuilder.productName!)
        .replace('${version}', version)
        .replace('${arch}', 'arm64')
        .replace('${ext}', 'AppImage') ?? `${electronBuilder.productName}-${version}-linux-arm64.AppImage`
      break
    case 'aarch64-apple-darwin':
      return electronBuilder.dmg?.artifactName
        ?.replace('${productName}', electronBuilder.productName!)
        .replace('${version}', version)
        .replace('${arch}', 'arm64')
        .replace('${ext}', 'dmg') ?? `${electronBuilder.productName}-${version}-darwin-arm64.dmg`
      break
    case 'x86_64-apple-darwin':
      return electronBuilder.dmg?.artifactName
        ?.replace('${productName}', electronBuilder.productName!)
        .replace('${version}', version)
        .replace('${arch}', 'x64')
        .replace('${ext}', 'dmg') ?? `${electronBuilder.productName}-${version}-darwin-x64.dmg`
    default:
      console.error('Target is not supported')
      process.exit(1)
  }
}