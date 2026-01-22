import type { Page } from 'playwright'
import type { UserLink, UserProfile, UserStats } from '../core/services/user'
import { logger } from '../utils/logger'
import { SELECTORS } from './selectors'
export class ProfileParser {
  static async parseUserProfile(page: Page): Promise<UserProfile> {
    try {
      const displayNameElement = await page.$(SELECTORS.PROFILE.DISPLAY_NAME)
      const displayName = await displayNameElement?.textContent() || 'Unknown User'
      let username = ''
      const url = page.url()
      const urlUsername = this.extractUsernameFromUrl(url)
      if (urlUsername) {
        username = urlUsername
      }
      else {
        const usernameElement = await page.$('[data-testid="UserName"] span:has-text("@")')
        const usernameText = await usernameElement?.textContent()
        username = usernameText?.replace('@', '') || 'unknown'
      }
      const bioElement = await page.$(SELECTORS.PROFILE.BIO)
      const bio = await bioElement?.textContent()
      const avatarUrl = await this.extractAvatarUrl(page)
      const bannerUrl = await this.extractBannerUrl(page)
      const stats = await this.extractUserStats(page)
      const joinDate = await this.extractJoinDate(page)
      const profile: UserProfile = {
        username,
        displayName,
      }
      if (bio)
        profile.bio = bio
      if (avatarUrl)
        profile.avatarUrl = avatarUrl
      if (bannerUrl)
        profile.bannerUrl = bannerUrl
      if (stats.followers)
        profile.followersCount = stats.followers
      if (stats.following)
        profile.followingCount = stats.following
      if (stats.tweets)
        profile.tweetCount = stats.tweets
      if (joinDate)
        profile.joinDate = joinDate
      const isVerified = await page.$('[data-testid="icon-verified"]') !== null
      if (isVerified)
        profile.isVerified = true
      return profile
    }
    catch (error) {
      logger.parser.error('Error parsing user profile:', (error as Error).message)
      return {
        username: 'unknown',
        displayName: 'Unknown User',
      }
    }
  }
  private static extractUsernameFromUrl(url: string): string | null {
    try {
      const match = url.match(/twitter\.com\/([^/]+)/)
      if (match && match[1] && !['home', 'explore', 'notifications', 'messages'].includes(match[1])) {
        return match[1]
      }
      return null
    }
    catch {
      return null
    }
  }
  private static async extractUserStats(page: Page): Promise<UserStats> {
    const stats: UserStats = {
      followers: 0,
      following: 0,
      tweets: 0,
    }
    try {
      const statsContainer = await page.$(SELECTORS.PROFILE.STATS)
      if (!statsContainer)
        return stats
      const statItems = await statsContainer.$$('a')
      for (const statItem of statItems) {
        const text = await statItem.textContent() || ''
        if (text.includes('Following')) {
          const countText = text.replace(/Following.*/, '').trim()
          stats.following = this.parseStatNumber(countText)
        }
        else if (text.includes('Followers')) {
          const countText = text.replace(/Followers.*/, '').trim()
          stats.followers = this.parseStatNumber(countText)
        }
        else if (text.includes('posts') || text.includes('Posts')) {
          const countText = text.replace(/posts|Posts.*/, '').trim()
          stats.tweets = this.parseStatNumber(countText)
        }
      }
      return stats
    }
    catch (error) {
      logger.parser.error('Error extracting user stats:', (error as Error).message)
      return stats
    }
  }
  private static async extractAvatarUrl(page: Page): Promise<string | undefined> {
    try {
      const avatarElement = await page.$('img[src*="profile_images"]')
      const src = await avatarElement?.getAttribute('src')
      return src || undefined
    }
    catch (error) {
      logger.parser.error('Error extracting avatar URL:', (error as Error).message)
      return undefined
    }
  }
  private static async extractBannerUrl(page: Page): Promise<string | undefined> {
    try {
      const bannerElement = await page.$('img[src*="profile_banners"]')
      const src = await bannerElement?.getAttribute('src')
      return src || undefined
    }
    catch (error) {
      logger.parser.error('Error extracting banner URL:', (error as Error).message)
      return undefined
    }
  }
  private static async extractJoinDate(page: Page): Promise<string | undefined> {
    try {
      const joinedText = await page.$('span:has-text("Joined")')
      if (!joinedText)
        return undefined
      const fullText = await joinedText.textContent()
      if (fullText && fullText.includes('Joined')) {
        const datePart = fullText.replace('Joined', '').trim()
        return datePart || undefined
      }
      return undefined
    }
    catch (error) {
      logger.parser.error('Error extracting join date:', (error as Error).message)
      return undefined
    }
  }
  private static async extractUserLinks(page: Page): Promise<UserLink[]> {
    const links: UserLink[] = []
    try {
      const linkElements = await page.$$('a[href^="https"]:not([href*="x.com"])')
      for (const linkElement of linkElements) {
        const href = await linkElement.getAttribute('href')
        const title = await linkElement.textContent()
        if (href && title) {
          links.push({
            type: 'url',
            url: href,
            title,
          })
        }
      }
      const locationElement = await page.$('span:has-text("Location")')
      if (locationElement) {
        const locationText = await locationElement.textContent()
        if (locationText) {
          links.push({
            type: 'location',
            url: '',
            title: locationText.replace('Location', '').trim(),
          })
        }
      }
      return links
    }
    catch (error) {
      logger.parser.error('Error extracting user links:', (error as Error).message)
      return links
    }
  }
  private static parseStatNumber(text: string): number {
    try {
      text = text.trim()
      if (!text)
        return 0
      if (text.includes('K')) {
        return Math.round(Number.parseFloat(text.replace('K', '')) * 1000)
      }
      else if (text.includes('M')) {
        return Math.round(Number.parseFloat(text.replace('M', '')) * 1000000)
      }
      const normalized = text.replace(/,/g, '')
      return Number.parseInt(normalized, 10) || 0
    }
    catch {
      return 0
    }
  }
}