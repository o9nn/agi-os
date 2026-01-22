import type { TwitterService } from '../../types/services'
import type { Context } from '../browser/context'
import { TWITTER_BASE_URL, TWITTER_HOME_URL, TWITTER_SEARCH_URL } from '../../constants'
import { SELECTORS } from '../../parsers/selectors'
import { TweetParser } from '../../parsers/tweet-parser'
import { scrollToLoadMoreTweets } from '../utils/scroll-helper'
export interface Tweet {
id: string
text: string
author: {
username: string
displayName: string
avatarUrl?: string
}
timestamp: string
likeCount?: number
retweetCount?: number
replyCount?: number
mediaUrls?: string[]
}
export interface SearchOptions {
count?: number
filter?: 'latest' | 'photos' | 'videos' | 'top'
}
export interface PostOptions {
media?: string[]
inReplyTo?: string
}
export interface TweetDetail extends Tweet {
replies?: Tweet[]
quotedTweet?: Tweet
}
export function useTwitterTweetServices(ctx: Context): TwitterService {
async function searchTweets(query: string, options: SearchOptions = {}): Promise<Tweet[]> {
try {
const page = ctx.page
const searchUrl = new URL(TWITTER_SEARCH_URL)
searchUrl.searchParams.append('q', query)
if (options.filter) {
searchUrl.searchParams.append('f', options.filter)
}
await page.goto(searchUrl.toString())
await page.waitForSelector(SELECTORS.TIMELINE.TWEET)
if (options.filter) {
switch (options.filter) {
case 'latest':
await page.click(SELECTORS.SEARCH.LATEST_TAB)
break
case 'top':
await page.click(SELECTORS.SEARCH.TOP_TAB)
break
case 'photos':
case 'videos':
await page.click(SELECTORS.SEARCH.SEARCH_FILTERS)
await page.click(`[role="menuitem"]:has-text("${options.filter === 'photos' ? 'Photos' : 'Videos'}")`)
break
}
}
await page.waitForSelector(SELECTORS.TIMELINE.TWEET)
let tweets = await TweetParser.parseTimelineTweets(page)
if (options.count && options.count > 0) {
tweets = tweets.slice(0, options.count)
}
return tweets
}
catch (error: unknown) {
console.error('Error searching tweets:', error)
throw new Error(`Failed to search tweets: ${error instanceof Error ? error.message : String(error)}`)
}
}
async function likeTweet(tweetId: string): Promise<boolean> {
try {
const page = ctx.page
await page.goto(`${TWITTER_BASE_URL}/i/status/${tweetId}`)
await page.waitForSelector(SELECTORS.TIMELINE.TWEET)
const likeButton = await page.$(SELECTORS.TIMELINE.LIKE_BUTTON)
if (!likeButton) {
throw new Error('Like button not found')
}
const isAlreadyLiked = await page.$eval(
SELECTORS.TIMELINE.LIKE_BUTTON,
el => el.getAttribute('aria-pressed') === 'true',
)
if (!isAlreadyLiked) {
await likeButton.click()
await page.waitForFunction(
`document.querySelector('${SELECTORS.TIMELINE.LIKE_BUTTON}')?.getAttribute('aria-pressed') === 'true'`,
{ timeout: 5000 },
)
}
return true
}
catch (error: unknown) {
console.error('Error liking tweet:', error)
throw new Error(`Failed to like tweet: ${error instanceof Error ? error.message : String(error)}`)
}
}
async function retweet(tweetId: string): Promise<boolean> {
try {
const page = ctx.page
await page.goto(`${TWITTER_BASE_URL}/i/status/${tweetId}`)
await page.waitForSelector(SELECTORS.TIMELINE.TWEET)
const retweetButton = await page.$(SELECTORS.TIMELINE.RETWEET_BUTTON)
if (!retweetButton) {
throw new Error('Retweet button not found')
}
await retweetButton.click()
await page.waitForSelector('[data-testid="retweetConfirm"]')
await page.click('[data-testid="retweetConfirm"]')
await page.waitForFunction(
`document.querySelector('${SELECTORS.TIMELINE.RETWEET_BUTTON}')?.getAttribute('aria-pressed') === 'true'`,
{ timeout: 5000 },
)
return true
}
catch (error: unknown) {
console.error('Error retweeting:', error)
throw new Error(`Failed to retweet: ${error instanceof Error ? error.message : String(error)}`)
}
}
async function postTweet(content: string, options: PostOptions = {}): Promise<string> {
try {
const page = ctx.page
await page.goto(TWITTER_HOME_URL)
await page.waitForSelector(SELECTORS.COMPOSE.TWEET_INPUT)
await page.click(SELECTORS.COMPOSE.TWEET_INPUT)
await page.type(SELECTORS.COMPOSE.TWEET_INPUT, content)
if (options.media && options.media.length > 0) {
await page.click(SELECTORS.COMPOSE.MEDIA_BUTTON)
await page.setInputFiles('input[type="file"][multiple]', options.media)
await page.waitForSelector('[data-testid="attachments"]')
}
if (options.inReplyTo) {
await page.goto(`${TWITTER_BASE_URL}/i/status/${options.inReplyTo}`)
await page.waitForSelector(SELECTORS.TIMELINE.REPLY_BUTTON)
await page.click(SELECTORS.TIMELINE.REPLY_BUTTON)
await page.waitForSelector(SELECTORS.COMPOSE.TWEET_INPUT)
await page.click(SELECTORS.COMPOSE.TWEET_INPUT)
await page.type(SELECTORS.COMPOSE.TWEET_INPUT, content)
if (options.media && options.media.length > 0) {
await page.click(SELECTORS.COMPOSE.MEDIA_BUTTON)
await page.setInputFiles('input[type="file"][multiple]', options.media)
await page.waitForSelector('[data-testid="attachments"]')
}
}
await page.click(SELECTORS.COMPOSE.TWEET_BUTTON)
await page.waitForSelector('[data-testid="toast"]', { timeout: 10000 })
let tweetId = ''
try {
const toastLink = await page.$('[data-testid="toast"] a[href*="/status/"]')
if (toastLink) {
const href = await toastLink.getAttribute('href')
if (href) {
const match = href.match(/\/status\/(\d+)/)
tweetId = match?.[1] || ''
}
}
if (!tweetId) {
const url = await page.url()
const match = url.match(/\/status\/(\d+)/)
tweetId = match?.[1] || ''
}
}
catch {
tweetId = `temp-${Date.now()}`
}
return tweetId
}
catch (error: unknown) {
console.error('Error posting tweet:', error)
throw new Error(`Failed to post tweet: ${error instanceof Error ? error.message : String(error)}`)
}
}
async function getTweetDetails(tweetId: string): Promise<TweetDetail> {
try {
const page = ctx.page
await page.goto(`${TWITTER_BASE_URL}/i/status/${tweetId}`)
await page.waitForSelector(SELECTORS.TIMELINE.TWEET)
const tweetElement = await page.$(SELECTORS.TIMELINE.TWEET)
if (!tweetElement) {
throw new Error('Tweet element not found')
}
const mainTweet = await TweetParser.extractTweetData(page, tweetElement)
if (!mainTweet) {
throw new Error('Failed to extract tweet data')
}
let quotedTweet: Tweet | undefined
const quotedTweetElement = await page.$('[data-testid="quotedTweet"]')
if (quotedTweetElement) {
const extractedQuotedTweet = await TweetParser.extractTweetData(page, quotedTweetElement)
if (extractedQuotedTweet) {
quotedTweet = extractedQuotedTweet
}
}
const replySelector = '[data-testid="tweet"][aria-labelledby*="reply"]'
await scrollToLoadMoreTweets(page, 10, replySelector)
const replyElements = await page.$$(replySelector)
const replies: Tweet[] = []
for (const replyElement of replyElements) {
const extractedReply = await TweetParser.extractTweetData(page, replyElement)
if (extractedReply) {
replies.push(extractedReply)
}
}
const tweetDetail: TweetDetail = {
...mainTweet,
replies: replies.length > 0 ? replies : undefined,
quotedTweet,
}
return tweetDetail
}
catch (error: unknown) {
console.error('Error getting tweet details:', error)
throw new Error(`Failed to get tweet details: ${error instanceof Error ? error.message : String(error)}`)
}
}
return {
searchTweets,
likeTweet,
retweet,
postTweet,
getTweetDetails,
}
}