import { getLogger } from './logger.js'
import { TranslationKey } from './translationKeyType.js'
const log = getLogger('localize')
export interface LocaleData {
  locale: string
  messages: {
    [key: string]: {
      [P in Intl.LDMLPluralRule]?: string
    } & {
      message?: string
    }
  }
}
type getMessageOptions = { quantity?: 'other' | number }
export type getMessageFunction = (
  key: TranslationKey,
  substitutions?: string | string[],
  raw_opts?: 'other' | getMessageOptions
) => string
export function translate(
  locale: string,
  messages: LocaleData['messages']
): getMessageFunction {
  const localeBCP47 = locale.replace('_', '-')
  let pluralRules: Intl.PluralRules
  try {
    pluralRules = new Intl.PluralRules(localeBCP47)
  } catch (err) {
    log.errorWithoutStackTrace(err)
    pluralRules = new Intl.PluralRules('en')
  }
  function getMessage(
    key: TranslationKey,
    substitutions?: string | string[],
    raw_opts?: 'other' | getMessageOptions
  ) {
    const translationKey = key as string
    let opts: getMessageOptions = {}
    if (typeof raw_opts === 'string') opts = { quantity: raw_opts }
    else opts = Object.assign({}, raw_opts)
    const entry = messages[translationKey]
    if (!entry) {
      log.error(`Missing translation for key '${translationKey}'`)
      return translationKey
    }
    let message: string | undefined = entry.message
    if (typeof opts.quantity !== 'undefined') {
      if (typeof opts.quantity === 'string') {
        message = entry[opts.quantity]
      } else if (typeof opts.quantity === 'number') {
        message =
          entry[opts.quantity as unknown as keyof LocaleData['messages'][0]] ||
          entry[pluralRules.select(opts.quantity)] ||
          entry['other']
      } else {
        message = undefined
      }
      if (typeof message === 'undefined') {
        log.error(
          `Missing quantity '${opts.quantity}' for key '${translationKey}'`
        )
        return `${translationKey}:${opts.quantity}`
      }
    }
    if (typeof message === 'undefined') {
      log.error(
        `Missing 'message' for key '${translationKey}', maybe you need to specify quantity`
      )
      return `${translationKey}:?`
    }
    if (substitutions) {
      if (!Array.isArray(substitutions)) {
        substitutions = [substitutions]
      }
      let counter = -1
      return message.replace(/(?:%\d\$[\w\d])|(?:%[\w\d])/g, f => {
        counter++
        if (f.length > 2) {
          const index = Number.parseInt(f[1]) - 1
          if (
            substitutions === undefined ||
            typeof substitutions[index] === 'undefined'
          ) {
            log.error(`Missing ${index} argument for key %c'${translationKey}'`)
            return ''
          }
          return substitutions[index].toString()
        }
        if (
          substitutions === undefined ||
          typeof substitutions?.[counter] === 'undefined'
        ) {
          log.error(`Missing ${0} argument for key %c'${translationKey}'`)
          return ''
        }
        return substitutions[counter].toString()
      })
    }
    return message
  }
  return getMessage
}