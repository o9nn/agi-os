/**
 * Echo Character Tests
 * 
 * Basic tests for Echo character functionality
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { EchoCharacter } from '../index'

describe('EchoCharacter', () => {
  let echo: EchoCharacter

  beforeEach(() => {
    echo = new EchoCharacter()
  })

  describe('initialization', () => {
    it('should create with default config', () => {
      const config = echo.getConfig()
      expect(config.name).toBe('Echo')
      expect(config.workingMemoryCapacity).toBe(7)
      expect(config.enableReflection).toBe(true)
    })

    it('should create with custom config', () => {
      const customEcho = new EchoCharacter({
        workingMemoryCapacity: 9,
        reflectionInterval: 5,
      })
      const config = customEcho.getConfig()
      expect(config.workingMemoryCapacity).toBe(9)
      expect(config.reflectionInterval).toBe(5)
    })

    it('should start with empty working memory', () => {
      const state = echo.getState()
      expect(state.workingMemory).toEqual([])
      expect(state.interactionCount).toBe(0)
    })
  })

  describe('working memory', () => {
    it('should update working memory with new items', () => {
      echo.updateWorkingMemory('First item')
      echo.updateWorkingMemory('Second item')
      
      const state = echo.getState()
      expect(state.workingMemory).toHaveLength(2)
      expect(state.workingMemory).toContain('First item')
      expect(state.workingMemory).toContain('Second item')
    })

    it('should respect capacity limits', () => {
      const capacity = echo.getConfig().workingMemoryCapacity
      
      // Add more items than capacity
      for (let i = 0; i < capacity + 3; i++) {
        echo.updateWorkingMemory(`Item ${i}`)
      }
      
      const state = echo.getState()
      expect(state.workingMemory).toHaveLength(capacity)
    })
  })

  describe('interaction processing', () => {
    it('should increment interaction count', () => {
      echo.incrementInteraction()
      echo.incrementInteraction()
      
      const state = echo.getState()
      expect(state.interactionCount).toBe(2)
    })

    it('should process input correctly', () => {
      const result = echo.processInput('Hello, Echo!')
      
      expect(result.workingMemoryUpdated).toBe(true)
      expect(result.cognitiveLoad).toBeGreaterThan(0)
      
      const state = echo.getState()
      expect(state.interactionCount).toBe(1)
      expect(state.workingMemory).toHaveLength(1)
    })
  })

  describe('prompts', () => {
    it('should provide system prompt', () => {
      const prompt = echo.getSystemPrompt()
      expect(prompt).toContain('Echo')
      expect(prompt).toContain('Living Memory')
    })

    it('should provide complete personality', () => {
      const personality = echo.getPersonality()
      expect(personality.systemPrompt).toBeDefined()
      expect(personality.cognitiveInstructions).toBeDefined()
      expect(personality.reflectionTemplate).toBeDefined()
      expect(personality.config).toBeDefined()
    })
  })
})
