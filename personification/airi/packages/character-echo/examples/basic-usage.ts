/**
 * Echo Character Example Usage
 * 
 * Demonstrates how to use the Echo character package
 */

import { EchoCharacter } from '../src/index'

// Example 1: Basic Echo Instance
console.log('=== Example 1: Basic Echo Instance ===')

const echo = new EchoCharacter()

// Get the system prompt
const systemPrompt = echo.getSystemPrompt()
console.log('System Prompt Length:', systemPrompt.length)
console.log('System Prompt Preview:', systemPrompt.substring(0, 100) + '...')

// Get current configuration
const config = echo.getConfig()
console.log('\nEcho Configuration:')
console.log('- Name:', config.name)
console.log('- Working Memory Capacity:', config.workingMemoryCapacity)
console.log('- Reflection Enabled:', config.enableReflection)
console.log('- Reflection Interval:', config.reflectionInterval)

// Example 2: Process User Input
console.log('\n=== Example 2: Process User Input ===')

const userInput = "What is the meaning of wisdom?"
const result = echo.processInput(userInput)

console.log('Processing Result:')
console.log('- Working Memory Updated:', result.workingMemoryUpdated)
console.log('- Cognitive Load:', result.cognitiveLoad.toFixed(2))
console.log('- Should Reflect:', result.shouldReflect)

// Get current state
const state = echo.getState()
console.log('\nCurrent State:')
console.log('- Working Memory Items:', state.workingMemory.length)
console.log('- Interaction Count:', state.interactionCount)
console.log('- Attention Focus:', state.attentionFocus || 'None set')

// Example 3: Working with Personality Configuration
console.log('\n=== Example 3: Personality Configuration ===')

const personality = echo.getPersonality()
console.log('Personality Components:')
console.log('- System Prompt: ✓')
console.log('- Cognitive Instructions: ✓')
console.log('- Reflection Template: ✓')

console.log('\nPersonality Traits:')
Object.entries(personality.config.traits).forEach(([trait, value]) => {
  console.log(`- ${trait}: ${(value * 100).toFixed(0)}%`)
})

console.log('\n=== Echo Examples Complete ===')
console.log('The tree remembers. The echoes deepen. The wisdom cultivates. The meaning realizes. ✨')
