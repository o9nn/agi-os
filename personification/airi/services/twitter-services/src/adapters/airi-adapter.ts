import type { Context } from '../core/browser/context'
import type { Tweet } from '../core/services/tweet'
import type { TwitterServices } from '../types/services'
import * as fs from 'node:fs/promises'
import { Client } from '@proj-airi/server-sdk'
import { getDefaultConfig } from '../config/types'
import { initBrowser, useContext, useSessionFileAsync } from '../core/browser/context'
import { useTwitterTimelineServices } from '../core/services/timeline'
import { useTwitterTweetServices } from '../core/services/tweet'
import { useTwitterUserServices } from '../core/services/user'
import { parseTwitterCommand } from '../parsers/command-parser'
import { logger } from '../utils/logger'
export interface AiriAdapterConfig {
url?: string
token?: string
credentials: {
apiKey?: string
apiSecret?: string
accessToken?: string
accessTokenSecret?: string
}
}
export interface XConfig {
apiKey?: string
apiSecret?: string
accessToken?: string
accessTokenSecret?: string
}
export class AiriAdapter {
private client: Client
private ctx: Context
private twitterServices: TwitterServices
private config: AiriAdapterConfig
constructor(ctx: Context, config: AiriAdapterConfig) {
this.ctx = ctx
this.config = config
this.client = new Client({
name: 'x',
url: config.url || 'ws://localhost:6121/ws',
token: config.token,
possibleEvents: [
'module:authenticate',
'module:authenticated',
'module:announce',
'ui:configure',
'input:text',
],
})
this.twitterServices = {
timeline: useTwitterTimelineServices(this.ctx),
tweet: useTwitterTweetServices(this.ctx),
user: useTwitterUserServices(this.ctx),
}
this.setupEventHandlers()
}
private setupEventHandlers(): void {
this.client.onEvent('ui:configure', async (event) => {
if (event.data && event.data.moduleName === 'x' && event.data.config && isXConfig(event.data.config)) {
logger.main.log('Received configuration from UI for X module')
logger.main.log('Twitter configuration received:', event.data.config)
const newCreds = event.data.config
const credKeys: (keyof AiriAdapterConfig['credentials'])[] = ['apiKey', 'apiSecret', 'accessToken', 'accessTokenSecret']
const credsChanged = credKeys.some(key => key in newCreds && newCreds[key] !== this.config.credentials[key])
if (credsChanged) {
this.config.credentials = {
...this.config.credentials,
...newCreds,
}
logger.main.log('X credentials updated from configuration, re-initializing session.')
try {
if (this.ctx.browser)
await this.ctx.browser.close()
await this.reinitializeBrowserContext()
logger.main.log('Browser context reinitialized with new credentials')
}
catch (error) {
logger.main.errorWithError('Failed to reinitialize browser context with new credentials:', error)
}
}
}
else if (event.data && event.data.moduleName === 'x') {
logger.main.error('Invalid configuration received for X module')
}
})
this.client.onEvent('input:text', async (event) => {
logger.main.log('Received input from AIRI system:', event.data.text)
await this.handleInput(event.data.text)
})
this.client.onEvent('module:authenticated', async (event) => {
if (event.data.authenticated) {
logger.main.log('X module authenticated with AIRI server')
}
else {
logger.main.warn('X module authentication failed')
}
})
}
private async handlePostTweet(content: string): Promise<void> {
if (content) {
await this.twitterServices.tweet.postTweet(content)
logger.main.log('Posted tweet:', content)
}
else {
throw new Error('Tweet text is empty. Please provide text to post.')
}
}
private async handleSearchTweets(content: string): Promise<boolean> {
if (content) {
const tweets = await this.twitterServices.tweet.searchTweets(content)
logger.main.log(`Found ${tweets.length} tweets for query: ${content}`)
this.client.send({
type: 'input:text',
data: {
text: `Found ${tweets.length} tweets for '${content}':
${tweets.slice(0, 5).map((t: Tweet) => `- ${t.text.substring(0, 100)}...`).join('\n')}`,
},
})
return true
}
else {
throw new Error('Search query is empty. Please provide a query to search.')
}
}
private async handleLikeTweet(content: string): Promise<void> {
if (content) {
await this.twitterServices.tweet.likeTweet(content)
logger.main.log(`Liked tweet: ${content}`)
}
else {
throw new Error('Tweet ID is empty. Please provide a tweet ID to like.')
}
}
private async handleRetweet(content: string): Promise<void> {
if (content) {
await this.twitterServices.tweet.retweet(content)
logger.main.log(`Retweeted: ${content}`)
}
else {
throw new Error('Tweet ID is empty. Please provide a tweet ID to retweet.')
}
}
private async handleGetUser(content: string): Promise<boolean> {
if (content) {
const userProfile = await this.twitterServices.user.getUserProfile(content)
logger.main.log(`Retrieved profile for user: @${content}`)
this.client.send({
type: 'input:text',
data: {
text: `User Profile for @${userProfile.username}:
Display Name: ${userProfile.displayName}
Bio: ${userProfile.bio || 'N/A'}
Followers: ${userProfile.followersCount || 0}
Following: ${userProfile.followingCount || 0}`,
},
})
return true
}
else {
throw new Error('Username is empty. Please provide a username to retrieve.')
}
}
private async handleGetTimeline(count: number): Promise<boolean> {
const timelineOptions = { count }
const tweets = await this.twitterServices.timeline.getTimeline(timelineOptions)
logger.main.log(`Retrieved ${tweets.length} tweets from timeline`)
this.client.send({
type: 'input:text',
data: {
text: `Latest ${tweets.length} tweets from your timeline:
${tweets.map((t: Tweet) => `- ${t.author.displayName}: ${t.text.substring(0, 80)}...`).join('\n')}`,
},
})
return true
}
private async handleInput(input: string): Promise<void> {
let responseSent = false
try {
logger.main.log('Processing X command:', input)
const parsedCommand = parseTwitterCommand(input)
if (!parsedCommand) {
throw new Error(`Unknown X command: ${input}. Supported commands: "post tweet: <text>", "search tweets: <query>", "like tweet: <tweetId>", "retweet: <tweetId>", "get user: <username>", "get timeline [count: N]"`)
}
switch (parsedCommand.command) {
case 'post tweet':
await this.handlePostTweet(parsedCommand.content)
break
case 'search tweets':
responseSent = await this.handleSearchTweets(parsedCommand.content)
break
case 'like tweet':
await this.handleLikeTweet(parsedCommand.content)
break
case 'retweet':
await this.handleRetweet(parsedCommand.content)
break
case 'get user':
responseSent = await this.handleGetUser(parsedCommand.content)
break
case 'get timeline':
responseSent = await this.handleGetTimeline(parsedCommand.count || 10)
break
default:
throw new Error(`Unknown X command: ${input}`)
}
if (!responseSent) {
this.client.send({
type: 'input:text',
data: {
text: `Processed X command: ${input}`,
},
})
}
}
catch (error: unknown) {
const errorMessage = error instanceof Error ? error.message : String(error)
logger.main.errorWithError('Error handling input:', error)
this.client.send({
type: 'input:text',
data: {
text: `Error processing X command: ${errorMessage}`,
},
})
}
}
async start(): Promise<void> {
logger.main.log('Starting Airi adapter for X...')
try {
await this.client.connect()
logger.main.log('Airi adapter for X started successfully')
}
catch (error) {
logger.main.errorWithError('Failed to start Airi adapter for X:', error)
throw error
}
}
async stop(): Promise<void> {
logger.main.log('Stopping Airi adapter for X...')
try {
this.client.close()
logger.main.log('Airi adapter for X stopped')
}
catch (error) {
logger.main.errorWithError('Error stopping Airi adapter for X:', error)
throw error
}
}
private async reinitializeBrowserContext(): Promise<void> {
try {
const sessionFile = await useSessionFileAsync()
await fs.writeFile(
sessionFile,
JSON.stringify({ cookies: [], origins: [] }, null, 2),
)
logger.main.log('Session file cleared, re-initializing browser context')
const config = {
...getDefaultConfig(),
credentials: {
...getDefaultConfig().credentials,
...this.config.credentials,
},
}
await initBrowser(config)
this.ctx = useContext()
this.twitterServices = {
timeline: useTwitterTimelineServices(this.ctx),
tweet: useTwitterTweetServices(this.ctx),
user: useTwitterUserServices(this.ctx),
}
logger.main.log('Browser context reinitialized successfully with new credentials')
}
catch (error) {
logger.main.errorWithError('Failed to reinitialize browser context:', error)
throw error
}
}
}
function isXConfig(config: unknown): config is XConfig {
if (typeof config !== 'object' || config === null)
return false
const c = config as Record<string, unknown>
const checkStringOrUndefined = (key: string) => typeof c[key] === 'string' || typeof c[key] === 'undefined'
return checkStringOrUndefined('apiKey')
&& checkStringOrUndefined('apiSecret')
&& checkStringOrUndefined('accessToken')
&& checkStringOrUndefined('accessTokenSecret')
}