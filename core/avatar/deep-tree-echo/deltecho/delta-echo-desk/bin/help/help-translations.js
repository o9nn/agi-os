#!/usr/bin/env node
import { readdir, readFile, writeFile } from 'fs/promises'
import { existsSync } from 'fs'
const translations = {
  search: 'Search',
  search_no_result_for_x: 'No results for "%s"',
  search_result_for_x: 'Result for "%s"',
  menu_help: 'Help',
  app_name: 'Delta Chat',
}
async function main() {
  const files = await readdir('static/help/')
  const fallback = JSON.parse(
    await readFile(`_locales/_untranslated_en.json`, 'utf-8')
  )
  const allHelpLabel = {}
  for (const file of files) {
    if (existsSync(`_locales/${file}.json`)) {
      const json = JSON.parse(await readFile(`_locales/${file}.json`, 'utf-8'))
      const helpLabel = { ...translations }
      for (const key in translations) {
        if (json[key] || fallback[key]) {
          helpLabel[key] = json[key]?.message || fallback[key].message
        }
      }
      helpLabel['placeholder'] = helpLabel['search']
      delete helpLabel['search']
      helpLabel['one_result'] = helpLabel['search_result_for_x'].replace(
        '%s',
        '[SEARCH_TERM]'
      )
      delete helpLabel['search_result_for_x']
      helpLabel['zero_results'] = helpLabel['search_no_result_for_x'].replace(
        '%s',
        '[SEARCH_TERM]'
      )
      delete helpLabel['search_no_result_for_x']
      helpLabel['clear_search'] = '×'
      allHelpLabel[file] = helpLabel
    }
  }
  await writeFile(
    `static/help/pagefind/locales.json`,
    JSON.stringify(allHelpLabel, null, 2)
  )
}
main()