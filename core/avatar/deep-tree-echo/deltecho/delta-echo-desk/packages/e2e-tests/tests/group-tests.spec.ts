import { test, expect } from '@playwright/test'
import {
  groupName,
  getUser,
  createProfiles,
  switchToProfile,
  User,
  loadExistingProfiles,
  deleteAllProfiles,
  reloadPage,
  clickThroughTestIds,
} from '../playwright-helper'
test.describe.configure({ mode: 'serial' })
let existingProfiles: User[] = []
const numberOfProfiles = 3
test.beforeAll(async ({ browser }) => {
  const context = await browser.newContext()
  const page = await context.newPage()
  await reloadPage(page)
  existingProfiles = (await loadExistingProfiles(page)) ?? existingProfiles
  test.setTimeout(120_000)
  await createProfiles(
    numberOfProfiles,
    existingProfiles,
    page,
    context,
    browser.browserType().name()
  )
  await context.close()
})
test.beforeEach(async ({ page }) => {
  await reloadPage(page)
})
test.afterAll(async ({ browser }) => {
  const context = await browser.newContext()
  const page = await context.newPage()
  await reloadPage(page)
  await deleteAllProfiles(page, existingProfiles)
  await context.close()
})
test('start chat with user', async ({ page, context, browserName }) => {
  if (browserName.toLowerCase().indexOf('chrom') > -1) {
    await context.grantPermissions(['clipboard-read', 'clipboard-write'])
  }
  const userA = getUser(0, existingProfiles)
  const userB = getUser(1, existingProfiles)
  await switchToProfile(page, userA.id)
  await clickThroughTestIds(page, [
    'qr-scan-button',
    'copy-qr-code',
    'confirm-qr-code',
  ])
  await switchToProfile(page, userB.id)
  await clickThroughTestIds(page, ['qr-scan-button', 'show-qr-scan', 'paste'])
  const confirmDialog = page.getByTestId('confirm-start-chat')
  await expect(confirmDialog).toContainText(userA.name)
  await page.getByTestId('confirm-start-chat').getByTestId('confirm').click()
  await expect(
    page.locator('.chat-list .chat-list-item').filter({ hasText: userA.name })
  ).toHaveCount(1)
  console.log(`Chat with ${userA.name} created!`)
  await page
    .locator('.chat-list .chat-list-item')
    .filter({ hasText: userA.name })
    .click()
  const messageText = `Hello ${userA.name}!`
  await page.locator('#composer-textarea').fill(messageText)
  await page.locator('button.send-button').click()
  const sentMessageText = page
    .locator(`.message.outgoing`)
    .last()
    .locator('.msg-body .text')
  await expect(sentMessageText).toHaveText(messageText)
})
test('create group', async ({ page, context, browserName }) => {
  if (browserName.toLowerCase().indexOf('chrom') > -1) {
    await context.grantPermissions(['clipboard-read', 'clipboard-write'])
  }
  const userA = existingProfiles[0]
  const userB = existingProfiles[1]
  const userC = existingProfiles[2]
  await switchToProfile(page, userA.id)
  const chatUserB = page
    .locator('.chat-list .chat-list-item')
    .filter({ hasText: userB.name })
  await expect(chatUserB).toBeVisible()
  await page.locator('#new-chat-button').click()
  await page.locator('#newgroup button').click()
  await page.locator('.group-name-input').fill(groupName)
  await page.locator('#addmember button').click()
  const addMemberDialog = page.getByTestId('add-member-dialog')
  console.log('userB', userB)
  await page
    .locator('.contact-list-item')
    .filter({ hasText: userB.name })
    .click()
  await addMemberDialog.getByTestId('ok').click()
  await page.getByTestId('group-create-button').click()
  const chatListItem = page
    .locator('.chat-list .chat-list-item')
    .filter({ hasText: groupName })
  await expect(chatListItem).toBeVisible()
  await page.locator('#composer-textarea').fill(`Hello group members!`)
  await page.locator('button.send-button').click()
  const badgeNumber = page
    .getByTestId(`account-item-${userB.id}`)
    .locator('.styles_module_accountBadgeIcon')
  await expect(badgeNumber).toHaveText('1')
  await page.getByTestId('chat-info-button').click()
  await page.locator('#showqrcode button').click()
  await clickThroughTestIds(page, [
    'copy-qr-code',
    'confirm-qr-code',
    'view-group-dialog-header-close',
  ])
  await switchToProfile(page, userC.id)
  await clickThroughTestIds(page, ['qr-scan-button', 'show-qr-scan', 'paste'])
  const confirmDialog = page.getByTestId('confirm-join-group')
  await expect(confirmDialog).toBeVisible()
  await expect(confirmDialog).toContainText(groupName)
  await page.getByTestId('confirm-join-group').getByTestId('confirm').click()
  await expect(page.locator('#message-list li').nth(1)).toContainText(
    userA.address
  )
  await expect(page.locator('.verified-icon-info-msg')).toBeVisible()
  const badge = page
    .getByTestId(`account-item-${userB.id}`)
    .locator('.styles_module_accountBadgeIcon')
    .getByText('2')
  await expect(badge).toBeVisible()
})