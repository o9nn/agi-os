import type { ReaderLike } from 'clustr'
import { readGraphemeClusters } from 'clustr'
export const TTS_FLUSH_INSTRUCTION = '\u200B'
const keptPunctuations = new Set('?？!！')
const hardPunctuations = new Set('.。?？!！…⋯～~\n\t\r')
const softPunctuations = new Set(',，、–—:：;；《》「」')
export interface TTSInputChunk {
  text: string
  words: number
  reason: 'boost' | 'limit' | 'hard' | 'flush'
}
export interface TTSInputChunkOptions {
  boost?: number
  minimumWords?: number
  maximumWords?: number
}
export async function* chunkTTSInput(input: string | ReaderLike, options?: TTSInputChunkOptions): AsyncGenerator<TTSInputChunk, void, unknown> {
  const {
    boost = 2,
    minimumWords = 4,
    maximumWords = 12,
  } = options ?? {}
  const iterator = readGraphemeClusters(
    typeof input === 'string'
      ? new ReadableStream({
          start(controller) {
            controller.enqueue(new TextEncoder().encode(input))
            controller.close()
          },
        }).getReader()
      : input,
  )
  const segmenter = new Intl.Segmenter(undefined, { granularity: 'word' }) 
  let yieldCount = 0
  let buffer = ''
  let chunk = ''
  let chunkWordsCount = 0
  let previousValue: string | undefined
  let current = await iterator.next()
  while (!current.done) {
    let value = current.value
    if (value.length > 1) {
      previousValue = value
      current = await iterator.next()
      continue
    }
    const flush = value === TTS_FLUSH_INSTRUCTION
    const hard = hardPunctuations.has(value)
    const soft = softPunctuations.has(value)
    const kept = keptPunctuations.has(value)
    let next: IteratorResult<string, any> | undefined
    let afterNext: IteratorResult<string, any> | undefined
    if (flush || hard || soft) {
      switch (value) {
        case '.':
        case ',': {
          if (previousValue !== undefined && /\d/.test(previousValue)) {
            next = await iterator.next()
            if (!next.done && next.value && /\d/.test(next.value)) {
              buffer += value
              current = next
              next = undefined
              continue
            }
          }
          else if (value === '.') {
            next = await iterator.next()
            if (!next.done && next.value && next.value === '.') {
              afterNext = await iterator.next()
              if (!afterNext.done && afterNext.value && afterNext.value === '.') {
                value = '…'
                next = undefined
                afterNext = undefined
              }
            }
          }
        }
      }
      if (buffer.length === 0) {
        previousValue = value
        current = await iterator.next()
        continue
      }
      const words = [...segmenter.segment(buffer)].filter(w => w.isWordLike)
      if (chunkWordsCount > minimumWords && chunkWordsCount + words.length > maximumWords) {
        const text = kept ? chunk.trim() + value : chunk.trim()
        yield {
          text,
          words: chunkWordsCount,
          reason: 'limit',
        }
        yieldCount++
        chunk = ''
        chunkWordsCount = 0
      }
      chunk += buffer + value
      chunkWordsCount += words.length
      buffer = ''
      if (flush || hard || chunkWordsCount > maximumWords || yieldCount < boost) {
        const text = chunk.trim()
        yield {
          text,
          words: chunkWordsCount,
          reason: flush ? 'flush' : hard ? 'hard' : chunkWordsCount > maximumWords ? 'limit' : 'boost',
        }
        yieldCount++
        chunk = ''
        chunkWordsCount = 0
      }
      previousValue = value
      if (next !== undefined) {
        if (afterNext !== undefined) {
          current = afterNext
          next = undefined
          afterNext = undefined
        }
        else {
          current = next
          next = undefined
        }
      }
      else {
        current = await iterator.next()
      }
      continue
    }
    buffer += value
    previousValue = value
    next = await iterator.next()
    current = next
  }
  console.debug('while loop ends, chunk/buffer:', chunk, buffer)
  if (chunk.length > 0 || buffer.length > 0) {
    const text = (chunk + buffer).trim()
    yield {
      text,
      words: chunkWordsCount + [...segmenter.segment(buffer)].filter(w => w.isWordLike).length,
      reason: 'flush',
    }
  }
}
export async function chunkEmitter(reader: ReaderLike, handler: (chunk: string) => Promise<void> | void) {
  try {
    for await (const chunk of chunkTTSInput(reader)) {
      console.debug('chunk to be pushed: ', chunk)
      await handler(chunk.text)
    }
  }
  catch (e) {
    console.error('Error chunking stream to TTS queue:', e)
  }
}