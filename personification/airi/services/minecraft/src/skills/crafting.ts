import type { Block } from 'prismarine-block'
import type { Item } from 'prismarine-item'
import type { Recipe } from 'prismarine-recipe'
import type { Mineflayer } from '../libs/mineflayer'
import { useLogger } from '../utils/logger'
import { getItemId, getItemName } from '../utils/mcdata'
import { ensureCraftingTable } from './actions/ensure'
import { collectBlock, placeBlock } from './blocks'
import { goToNearestBlock, goToPosition, moveAway } from './movement'
import { getInventoryCounts, getNearestBlock, getNearestFreeSpace } from './world'
const logger = useLogger()
export async function craftRecipe(
  mineflayer: Mineflayer,
  incomingItemName: string,
  num = 1,
): Promise<boolean> {
  let itemName = incomingItemName.replace(' ', '_').toLowerCase()
  if (itemName.endsWith('plank'))
    itemName += 's' 
  const itemId = getItemId(itemName)
  if (itemId === null) {
    logger.log(`Invalid item name: ${itemName}`)
    return false
  }
  async function attemptCraft(
    recipes: Recipe[] | null,
    craftingTable: Block | null = null,
  ): Promise<boolean> {
    if (recipes && recipes.length > 0) {
      const recipe = recipes[0]
      try {
        await mineflayer.bot.craft(recipe, num, craftingTable ?? undefined)
        logger.log(
          `Successfully crafted ${num} ${itemName}${
            craftingTable ? ' using crafting table' : ''
          }.`,
        )
        return true
      }
      catch (err) {
        logger.log(`Failed to craft ${itemName}: ${(err as Error).message}`)
        return false
      }
    }
    return false
  }
  async function moveToAndCraft(craftingTable: Block): Promise<boolean> {
    logger.log(`Crafting table found, moving to it.`)
    const maxRetries = 2
    let attempts = 0
    let success = false
    while (attempts < maxRetries && !success) {
      try {
        await goToPosition(
          mineflayer,
          craftingTable.position.x,
          craftingTable.position.y,
          craftingTable.position.z,
          1,
        )
        const recipes = mineflayer.bot.recipesFor(itemId, null, 1, craftingTable)
        success = await attemptCraft(recipes, craftingTable)
      }
      catch (err) {
        logger.log(
          `Attempt ${attempts + 1} to move to crafting table failed: ${
            (err as Error).message
          }`,
        )
      }
      attempts++
    }
    return success
  }
  async function findAndUseCraftingTable(
    craftingTableRange: number,
  ): Promise<boolean> {
    let craftingTable = getNearestBlock(mineflayer, 'crafting_table', craftingTableRange)
    if (craftingTable) {
      return await moveToAndCraft(craftingTable)
    }
    logger.log(`No crafting table nearby, attempting to place one.`)
    const hasCraftingTable = await ensureCraftingTable(mineflayer)
    if (!hasCraftingTable) {
      logger.log(`Failed to ensure a crafting table to craft ${itemName}.`)
      return false
    }
    const pos = getNearestFreeSpace(mineflayer, 1, 10)
    if (pos) {
      moveAway(mineflayer, 4)
      logger.log(
        `Placing crafting table at position (${pos.x}, ${pos.y}, ${pos.z}).`,
      )
      await placeBlock(mineflayer, 'crafting_table', pos.x, pos.y, pos.z)
      craftingTable = getNearestBlock(mineflayer, 'crafting_table', craftingTableRange)
      if (craftingTable) {
        return await moveToAndCraft(craftingTable)
      }
    }
    else {
      logger.log('No suitable position found to place the crafting table.')
      moveAway(mineflayer, 5)
      return false
    }
    return false
  }
  logger.log(`Step 1: Try to craft without a crafting table`)
  const recipes = mineflayer.bot.recipesFor(itemId, null, 1, null)
  if (recipes && (await attemptCraft(recipes))) {
    return true
  }
  logger.log(`Step 2: Find and use a crafting table`)
  const craftingTableRange = 32
  if (await findAndUseCraftingTable(craftingTableRange)) {
    return true
  }
  return false
}
export async function smeltItem(mineflayer: Mineflayer, itemName: string, num = 1): Promise<boolean> {
  const foods = [
    'beef',
    'chicken',
    'cod',
    'mutton',
    'porkchop',
    'rabbit',
    'salmon',
    'tropical_fish',
  ]
  if (!itemName.includes('raw') && !foods.includes(itemName)) {
    logger.log(
      `Cannot smelt ${itemName}, must be a "raw" item, like "raw_iron".`,
    )
    return false
  } 
  let placedFurnace = false
  let furnaceBlock = getNearestBlock(mineflayer, 'furnace', 32)
  if (!furnaceBlock) {
    const hasFurnace = getInventoryCounts(mineflayer).furnace > 0
    if (hasFurnace) {
      const pos = getNearestFreeSpace(mineflayer, 1, 32)
      if (pos) {
        await placeBlock(mineflayer, 'furnace', pos.x, pos.y, pos.z)
      }
      else {
        logger.log('No suitable position found to place the furnace.')
        return false
      }
      furnaceBlock = getNearestBlock(mineflayer, 'furnace', 32)
      placedFurnace = true
    }
  }
  if (!furnaceBlock) {
    logger.log(`There is no furnace nearby and I have no furnace.`)
    return false
  }
  if (mineflayer.bot.entity.position.distanceTo(furnaceBlock.position) > 4) {
    await goToNearestBlock(mineflayer, 'furnace', 4, 32)
  }
  await mineflayer.bot.lookAt(furnaceBlock.position)
  logger.log('smelting...')
  const furnace = await mineflayer.bot.openFurnace(furnaceBlock)
  const inputItem = furnace.inputItem()
  if (
    inputItem
    && inputItem.type !== getItemId(itemName)
    && inputItem.count > 0
  ) {
    logger.log(
      `The furnace is currently smelting ${getItemName(
        inputItem.type,
      )}.`,
    )
    if (placedFurnace)
      await collectBlock(mineflayer, 'furnace', 1)
    return false
  }
  const invCounts = getInventoryCounts(mineflayer)
  if (!invCounts[itemName] || invCounts[itemName] < num) {
    logger.log(`I do not have enough ${itemName} to smelt.`)
    if (placedFurnace)
      await collectBlock(mineflayer, 'furnace', 1)
    return false
  }
  if (!furnace.fuelItem()) {
    const fuel = mineflayer.bot.inventory
      .items()
      .find(item => item.name === 'coal' || item.name === 'charcoal')
    const putFuel = Math.ceil(num / 8)
    if (!fuel || fuel.count < putFuel) {
      logger.log(
        `I do not have enough coal or charcoal to smelt ${num} ${itemName}, I need ${putFuel} coal or charcoal`,
      )
      if (placedFurnace)
        await collectBlock(mineflayer, 'furnace', 1)
      return false
    }
    await furnace.putFuel(fuel.type, null, putFuel)
    logger.log(
      `Added ${putFuel} ${getItemName(fuel.type)} to furnace fuel.`,
    )
  }
  const itemId = getItemId(itemName)
  if (itemId === null) {
    logger.log(`Invalid item name: ${itemName}`)
    return false
  }
  await furnace.putInput(itemId, null, num)
  let total = 0
  let collectedLast = true
  let smeltedItem: Item | null = null
  await new Promise(resolve => setTimeout(resolve, 200))
  while (total < num) {
    await new Promise(resolve => setTimeout(resolve, 10000))
    logger.log('checking...')
    let collected = false
    if (furnace.outputItem()) {
      smeltedItem = await furnace.takeOutput()
      if (smeltedItem) {
        total += smeltedItem.count
        collected = true
      }
    }
    if (!collected && !collectedLast) {
      break 
    }
    collectedLast = collected
  }
  await mineflayer.bot.closeWindow(furnace)
  if (placedFurnace) {
    await collectBlock(mineflayer, 'furnace', 1)
  }
  if (total === 0) {
    logger.log(`Failed to smelt ${itemName}.`)
    return false
  }
  if (total < num) {
    logger.log(
      `Only smelted ${total} ${getItemName(smeltedItem?.type || 0)}.`,
    )
    return false
  }
  logger.log(
    `Successfully smelted ${itemName}, got ${total} ${getItemName(
      smeltedItem?.type || 0,
    )}.`,
  )
  return true
}
export async function clearNearestFurnace(mineflayer: Mineflayer): Promise<boolean> {
  const furnaceBlock = getNearestBlock(mineflayer, 'furnace', 6)
  if (!furnaceBlock) {
    logger.log(`There is no furnace nearby.`)
    return false
  }
  logger.log('clearing furnace...')
  const furnace = await mineflayer.bot.openFurnace(furnaceBlock)
  logger.log('opened furnace...')
  let smeltedItem: Item | null = null
  let inputItem: Item | null = null
  let fuelItem: Item | null = null
  if (furnace.outputItem())
    smeltedItem = await furnace.takeOutput()
  if (furnace.inputItem())
    inputItem = await furnace.takeInput()
  if (furnace.fuelItem())
    fuelItem = await furnace.takeFuel()
  logger.log(smeltedItem, inputItem, fuelItem)
  const smeltedName = smeltedItem
    ? `${smeltedItem.count} ${smeltedItem.name}`
    : `0 smelted items`
  const inputName = inputItem
    ? `${inputItem.count} ${inputItem.name}`
    : `0 input items`
  const fuelName = fuelItem
    ? `${fuelItem.count} ${fuelItem.name}`
    : `0 fuel items`
  logger.log(
    `Cleared furnace, received ${smeltedName}, ${inputName}, and ${fuelName}.`,
  )
  await mineflayer.bot.closeWindow(furnace)
  return true
}