import process from 'node:process'
import { createAnthropic, createFeatherless, createOpenRouter, createTogetherAI } from './create'
export const anthropic = createAnthropic(process.env.ANTHROPIC_API_KEY ?? '')
export const featherless = createFeatherless(process.env.FEATHERLESS_API_KEY ?? '')
export const openrouter = createOpenRouter(process.env.OPENROUTER_API_KEY ?? '')
export const togetherai = createTogetherAI(process.env.TOGETHER_API_KEY ?? '')