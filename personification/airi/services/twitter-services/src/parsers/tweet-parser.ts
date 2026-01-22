import type { ElementHandle, Page } from 'playwright'
import type { Tweet } from '../core/services/tweet'
import { logger } from '../utils/logger'
import { SELECTORS } from './selectors'
export class TweetParser {
static async parseTimelineTweets(page: Page): Promise<Tweet[]> {
try {
const tweetElements = await page.$$(SELECTORS.TIMELINE.TWEET)
logger.parser.log(`Found ${tweetElements.length} tweet elements`)
const tweets: Tweet[] = []
for (const tweetElement of tweetElements) {
const tweet = await this.extractTweetData(page, tweetElement)
if (tweet) {
tweets.push(tweet)
}
}
return tweets
}
catch (error) {
logger.parser.error('Error parsing timeline tweets:', (error as Error).message)
return []
}
}
static async extractTweetData(page: Page, tweetElement: ElementHandle): Promise<Tweet | null> {
try {
const id = await this.extractTweetId(tweetElement)
const textElement = await tweetElement.$(SELECTORS.TIMELINE.TWEET_TEXT)
const text = textElement ? await textElement.textContent() : ''
const author = await this.extractAuthorInfo(tweetElement)
const timeElement = await tweetElement.$('time')
const timestamp = timeElement ? await timeElement.getAttribute('datetime') : new Date().toISOString()
const stats = await this.extractTweetStats(tweetElement)
const mediaUrls = await this.extractMediaUrls(tweetElement)
const tweet: Tweet = {
id,
text: text || '',
author,
timestamp: timestamp || new Date().toISOString(),
...stats,
}
if (mediaUrls.length > 0) {
tweet.mediaUrls = mediaUrls
}
return tweet
}
catch (error) {
logger.parser.error('Error extracting tweet data:', (error as Error).message)
return null
}
}
private static async extractTweetId(tweetElement: ElementHandle): Promise<string> {
try {
const statusLink = await tweetElement.$('a[href*="/status/"]')
if (statusLink) {
const href = await statusLink.getAttribute('href')
if (href) {
const match = href.match(/\/status\/(\d+)/)
if (match && match[1]) {
return match[1]
}
}
}
return `tweet-${Date.now()}-${Math.floor(Math.random() * 1000)}`
}
catch (error) {
logger.parser.error('Error extracting tweet ID:', (error as Error).message)
return `tweet-${Date.now()}`
}
}
private static async extractAuthorInfo(tweetElement: ElementHandle): Promise<Tweet['author']> {
try {
const authorElement = await tweetElement.$('[data-testid="User-Name"]')
if (!authorElement) {
return {
username: 'unknown',
displayName: 'Unknown User',
}
}
const displayNameElement = await authorElement.$('span:first-child')
const displayName = displayNameElement ? await displayNameElement.textContent() || 'Unknown User' : 'Unknown User'
const usernameElement = await authorElement.$('a[href^="/"]')
let username = usernameElement ? await usernameElement.getAttribute('href') : 'unknown'
username = username?.replace('/', '') || 'unknown'
const avatarElement = await tweetElement.$('img[src*="/profile_images/"]')
const avatarUrl = avatarElement ? await avatarElement.getAttribute('src') : undefined
return {
username,
displayName,
...(avatarUrl && { avatarUrl }),
}
}
catch (error) {
logger.parser.error('Error extracting author info:', (error as Error).message)
return {
username: 'unknown',
displayName: 'Unknown User',
}
}
}
private static async extractTweetStats(tweetElement: ElementHandle): Promise<{
likeCount?: number
retweetCount?: number
replyCount?: number
}> {
const stats: {
likeCount?: number
retweetCount?: number
replyCount?: number
} = {}
try {
const likeElement = await tweetElement.$(SELECTORS.TIMELINE.LIKE_BUTTON)
if (likeElement) {
const likeCountElement = await likeElement.$('span span')
const likeCountText = likeCountElement ? await likeCountElement.textContent() : null
stats.likeCount = this.parseCount(likeCountText)
}
const retweetElement = await tweetElement.$(SELECTORS.TIMELINE.RETWEET_BUTTON)
if (retweetElement) {
const retweetCountElement = await retweetElement.$('span span')
const retweetCountText = retweetCountElement ? await retweetCountElement.textContent() : null
stats.retweetCount = this.parseCount(retweetCountText)
}
const replyElement = await tweetElement.$(SELECTORS.TIMELINE.REPLY_BUTTON)
if (replyElement) {
const replyCountElement = await replyElement.$('span span')
const replyCountText = replyCountElement ? await replyCountElement.textContent() : null
stats.replyCount = this.parseCount(replyCountText)
}
return stats
}
catch (error) {
logger.parser.error('Error extracting tweet stats:', (error as Error).message)
return stats
}
}
private static async extractMediaUrls(tweetElement: ElementHandle): Promise<string[]> {
try {
const mediaElements = await tweetElement.$$('img[src*="pbs.twimg.com/media/"]')
const mediaUrls: string[] = []
for (const mediaElement of mediaElements) {
const src = await mediaElement.getAttribute('src')
if (src) {
mediaUrls.push(src)
}
}
return mediaUrls
}
catch (error) {
logger.parser.error('Error extracting media URLs:', (error as Error).message)
return []
}
}
private static parseCount(countText: string | null): number | undefined {
if (!countText)
return undefined
try {
countText = countText.trim()
if (!countText)
return undefined
if (countText.includes('K')) {
return Math.round(Number.parseFloat(countText.replace('K', '')) * 1000)
}
else if (countText.includes('M')) {
return Math.round(Number.parseFloat(countText.replace('M', '')) * 1000000)
}
return Number.parseInt(countText, 10) || undefined
}
catch {
return undefined
}
}
}