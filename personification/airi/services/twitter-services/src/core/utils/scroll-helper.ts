import type { Page } from 'playwright'
import { SELECTORS } from '../../parsers/selectors'
import { logger } from '../../utils/logger'
export async function scrollToLoadMoreTweets(
page: Page,
targetCount: number,
selector = SELECTORS.TIMELINE.TWEET,
): Promise<void> {
try {
let previousTweetCount = 0
let currentTweetCount = await countVisibleElements(page, selector)
let scrollAttempts = 0
const maxScrollAttempts = 10
logger.main.debug(`Initial tweet count: ${currentTweetCount}, target count: ${targetCount}`)
while (currentTweetCount < targetCount && scrollAttempts < maxScrollAttempts) {
await page.mouse.wheel(0, 800)
await page.waitForTimeout(1000)
previousTweetCount = currentTweetCount
currentTweetCount = await countVisibleElements(page, selector)
if (currentTweetCount === previousTweetCount) {
scrollAttempts++
}
else {
scrollAttempts = 0
}
logger.main.debug(`Scrolled to load more content: ${currentTweetCount}/${targetCount}`)
}
}
catch (error) {
logger.main.errorWithError('Error while scrolling to load more content:', (error as Error).message)
}
}
export async function countVisibleElements(page: Page, selector: string): Promise<number> {
const elements = await page.$$(selector)
return elements.length
}