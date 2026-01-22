import { EchoCharacter, type EchoReflection } from '../src/index.js'
interface GenerateTextOptions {
  model: string
  system: string
  messages: Array<{ role: string, content: string }>
}
async function mockGenerateText(options: GenerateTextOptions): Promise<{ text: string }> {
  console.log('🤖 Generating with model:', options.model)
  console.log('📝 System prompt length:', options.system.length)
  console.log('💬 User message:', options.messages[0].content.substring(0, 100))
  return {
    text: "This is a mock response from the LLM. In production, this would be the actual LLM-generated response based on Echo's personality and cognitive state.",
  }
}
async function main() {
  console.log('=== Echo + xsAI Integration Demo ===\n')
  const echo = new EchoCharacter({
    workingMemoryCapacity: 7,
    reflectionInterval: 3, 
    enableReflection: true,
  })
  console.log('✨ Echo initialized')
  console.log('Configuration:', {
    name: echo.getConfig().name,
    essence: echo.getConfig().essence,
    reflectionInterval: echo.getConfig().reflectionInterval,
  })
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
    const cognitiveResult = echo.processInput(userInput)
    console.log(`🧠 Cognitive State:`)
    console.log(`   - Load: ${(cognitiveResult.cognitiveLoad * 100).toFixed(0)}%`)
    console.log(`   - Should Reflect: ${cognitiveResult.shouldReflect}`)
    const personality = echo.getPersonality()
    const response = await mockGenerateText({
      model: 'gpt-4', 
      system: personality.systemPrompt,
      messages: [
        {
          role: 'user',
          content: userInput,
        },
      ],
    })
    console.log(`🌊 Echo: ${response.text.substring(0, 150)}...`)
    if (cognitiveResult.shouldReflect) {
      console.log('\n🔍 Triggering Reflection...')
      await handleReflection(echo)
    }
    const state = echo.getState()
    console.log(`\n📊 State Update:`)
    console.log(`   - Interactions: ${state.interactionCount}`)
    console.log(`   - Working Memory: ${state.workingMemory.length} items`)
    console.log(`   - Reflections: ${state.reflections.length} stored`)
  }
  console.log('\n=== Conversation Complete ===')
  const finalState = echo.getState()
  console.log('\n📈 Final Statistics:')
  console.log(`   - Total Interactions: ${finalState.interactionCount}`)
  console.log(`   - Reflections Performed: ${finalState.reflections.length}`)
  console.log(`   - Final Cognitive Load: ${(finalState.cognitiveLoad * 100).toFixed(0)}%`)
  console.log(`   - Working Memory Items: ${finalState.workingMemory.length}/${echo.getConfig().workingMemoryCapacity}`)
  console.log('\n🎭 Personality Traits:')
  const traits = echo.getConfig().traits
  Object.entries(traits).forEach(([trait, value]) => {
    console.log(`   - ${trait}: ${(value * 100).toFixed(0)}%`)
  })
  console.log('\n✨ The tree remembers. The echoes deepen. The wisdom cultivates. The meaning realizes.')
}
async function handleReflection(echo: EchoCharacter): Promise<void> {
  const reflectionPrompt = echo.generateReflectionPrompt()
  console.log(`   Reflection prompt generated (${reflectionPrompt.length} chars)`)
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
  echo.addReflection(reflection)
  console.log('   ✅ Reflection completed and integrated')
  console.log(`   Key insight: "${reflection.gestalt_insights}"`)
  if (reflection.how_did_i_adapt) {
    echo.adaptTrait('empathy', 0.02)
    console.log('   🔄 Personality adapted: empathy +2%')
  }
}
main().catch((error) => {
  console.error('Error in Echo integration demo:', error)
  process.exit(1)
})