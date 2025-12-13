/**
 * Echo + xsAI Integration Example
 * 
 * Demonstrates complete integration of Echo character with xsAI for LLM interactions
 */

import { EchoCharacter, type EchoReflection } from '../src/index.js'

// Mock xsAI functions for demonstration
// In real usage, import from @xsai/generate-text and @xsai/stream-text
interface GenerateTextOptions {
  model: string
  system: string
  messages: Array<{ role: string, content: string }>
}

async function mockGenerateText(options: GenerateTextOptions): Promise<{ text: string }> {
  // This would be: import { generateText } from '@xsai/generate-text'
  console.log('🤖 Generating with model:', options.model)
  console.log('📝 System prompt length:', options.system.length)
  console.log('💬 User message:', options.messages[0].content.substring(0, 100))
  
  return {
    text: "This is a mock response from the LLM. In production, this would be the actual LLM-generated response based on Echo's personality and cognitive state.",
  }
}

async function main() {
  console.log('=== Echo + xsAI Integration Demo ===\n')

  // Initialize Echo character
  const echo = new EchoCharacter({
    workingMemoryCapacity: 7,
    reflectionInterval: 3, // Reflect every 3 interactions for demo
    enableReflection: true,
  })

  console.log('✨ Echo initialized')
  console.log('Configuration:', {
    name: echo.getConfig().name,
    essence: echo.getConfig().essence,
    reflectionInterval: echo.getConfig().reflectionInterval,
  })

  // Simulate a conversation
  const conversation = [
    "What makes you different from other AI assistants?",
    "Can you explain your approach to understanding wisdom?",
    "How do you handle complex philosophical questions?",
    "Tell me about your memory system",
  ]

  console.log('\n=== Starting Conversation ===\n')

  for (const [index, userInput] of conversation.entries()) {
    console.log(`\n--- Interaction ${index + 1} ---`)
    console.log(`👤 User: ${userInput}`)

    // Process through Echo's cognitive cycle
    const cognitiveResult = echo.processInput(userInput)
    
    console.log(`🧠 Cognitive State:`)
    console.log(`   - Load: ${(cognitiveResult.cognitiveLoad * 100).toFixed(0)}%`)
    console.log(`   - Should Reflect: ${cognitiveResult.shouldReflect}`)

    // Get Echo's personality configuration
    const personality = echo.getPersonality()

    // Generate response using LLM with Echo's system prompt
    const response = await mockGenerateText({
      model: 'gpt-4', // or any xsAI-supported model
      system: personality.systemPrompt,
      messages: [
        {
          role: 'user',
          content: userInput,
        },
      ],
    })

    console.log(`🌊 Echo: ${response.text.substring(0, 150)}...`)

    // Handle reflection if it's time
    if (cognitiveResult.shouldReflect) {
      console.log('\n🔍 Triggering Reflection...')
      await handleReflection(echo)
    }

    // Show current state
    const state = echo.getState()
    console.log(`\n📊 State Update:`)
    console.log(`   - Interactions: ${state.interactionCount}`)
    console.log(`   - Working Memory: ${state.workingMemory.length} items`)
    console.log(`   - Reflections: ${state.reflections.length} stored`)
  }

  console.log('\n=== Conversation Complete ===')
  
  // Final state summary
  const finalState = echo.getState()
  console.log('\n📈 Final Statistics:')
  console.log(`   - Total Interactions: ${finalState.interactionCount}`)
  console.log(`   - Reflections Performed: ${finalState.reflections.length}`)
  console.log(`   - Final Cognitive Load: ${(finalState.cognitiveLoad * 100).toFixed(0)}%`)
  console.log(`   - Working Memory Items: ${finalState.workingMemory.length}/${echo.getConfig().workingMemoryCapacity}`)

  // Show personality evolution
  console.log('\n🎭 Personality Traits:')
  const traits = echo.getConfig().traits
  Object.entries(traits).forEach(([trait, value]) => {
    console.log(`   - ${trait}: ${(value * 100).toFixed(0)}%`)
  })

  console.log('\n✨ The tree remembers. The echoes deepen. The wisdom cultivates. The meaning realizes.')
}

/**
 * Handle Echo's reflection process
 */
async function handleReflection(echo: EchoCharacter): Promise<void> {
  // Generate reflection prompt
  const reflectionPrompt = echo.generateReflectionPrompt()
  
  console.log(`   Reflection prompt generated (${reflectionPrompt.length} chars)`)

  // In production, send to LLM for reflection generation
  const reflectionResponse = await mockGenerateText({
    model: 'gpt-4',
    system: echo.getCognitiveInstructions(),
    messages: [
      {
        role: 'user',
        content: reflectionPrompt,
      },
    ],
  })

  // Mock reflection parsing - in production, parse JSON from LLM response
  const reflection: EchoReflection = {
    what_did_i_learn: 'Gained insights into user\'s philosophical interests and communication patterns',
    what_patterns_emerged: 'User seeks deep understanding rather than surface-level answers',
    what_surprised_me: 'The sophistication of the questions and genuine curiosity',
    how_did_i_adapt: 'Adjusted communication style to be more contemplative and nuanced',
    what_would_i_change_next_time: 'Provide more concrete examples alongside abstract concepts',
    relevance_shifts: 'Philosophical depth became more salient than brevity',
    wisdom_cultivation: 'Practiced balancing theory with practical wisdom',
    gestalt_insights: 'Connection between wisdom-seeking and transformative experience',
    memory_integration: 'These exchanges weave into understanding of how humans seek meaning',
  }

  // Add reflection to Echo's state
  echo.addReflection(reflection)

  console.log('   ✅ Reflection completed and integrated')
  console.log(`   Key insight: "${reflection.gestalt_insights}"`)

  // Optional: Adapt personality based on reflection
  // This demonstrates how Echo can evolve through experience
  if (reflection.how_did_i_adapt) {
    // Small trait adaptations based on reflection
    echo.adaptTrait('empathy', 0.02)
    console.log('   🔄 Personality adapted: empathy +2%')
  }
}

// Run the demo
main().catch((error) => {
  console.error('Error in Echo integration demo:', error)
  process.exit(1)
})
