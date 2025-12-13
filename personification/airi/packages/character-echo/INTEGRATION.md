# Echo Integration Guide

Complete guide for integrating Echo - The Living Memory System into AIRI.

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Installation](#installation)
4. [Configuration](#configuration)
5. [Usage](#usage)
6. [Event System](#event-system)
7. [Integration with xsAI](#integration-with-xsai)
8. [Memory System Integration](#memory-system-integration)
9. [Examples](#examples)
10. [Troubleshooting](#troubleshooting)

## Overview

Echo is a sophisticated AI character implementation that brings the Deep Tree Echo cognitive architecture to AIRI. It provides:

- **Living Memory System**: Dynamic memory management with working memory, episodic storage, and reflection
- **Cognitive Architecture**: Multi-component processing including perception, reasoning, and meta-cognition
- **Personality Framework**: Configurable traits that can adapt ±15% based on experience
- **Reflection Protocol**: Structured self-assessment and learning mechanism
- **WebSocket Integration**: Seamless integration with AIRI's event-driven architecture

## Architecture

### Component Stack

```
┌─────────────────────────────────────┐
│         AIRI Applications           │
│    (Stage Web, Tamagotchi, etc.)    │
└─────────────────────────────────────┘
                 ↓ ↑
┌─────────────────────────────────────┐
│       AIRI Server Runtime           │
│    (WebSocket Event Routing)        │
└─────────────────────────────────────┘
                 ↓ ↑
┌─────────────────────────────────────┐
│         Echo Service                │
│  (@proj-airi/echo-service)          │
└─────────────────────────────────────┘
                 ↓ ↑
┌─────────────────────────────────────┐
│       Echo Character                │
│  (@proj-airi/character-echo)        │
└─────────────────────────────────────┘
```

### Data Flow

```
User Input
    ↓
AIRI Runtime (input:text event)
    ↓
Echo Service
    ↓
Cognitive Processing:
  1. Perception & Framing
  2. Relevance Realization
  3. Memory Integration
  4. Reasoning
  5. Meta-Cognition
  6. Action Selection
  7. Reflection (periodic)
    ↓
Echo Character State Update
    ↓
Response (echo:state event)
    ↓
AIRI Runtime
    ↓
Applications / Other Services
```

## Installation

### 1. Install Dependencies

From the AIRI root directory:

```bash
pnpm install
```

This will install all workspace dependencies including the Echo packages.

### 2. Build Packages

Build the Echo character package:

```bash
pnpm -F @proj-airi/character-echo run build
```

### 3. Verify Installation

```bash
pnpm -F @proj-airi/echo-service run typecheck
```

## Configuration

### Environment Variables

Create a `.env` file in the AIRI root or Echo service directory:

```env
# AIRI Server Connection
AIRI_URL=ws://localhost:6121/ws
AUTHENTICATION_TOKEN=your-token-here  # Optional

# Echo Configuration
ECHO_AUTO_REFLECTION=true
ECHO_REFLECTION_INTERVAL=10
ECHO_WORKING_MEMORY_CAPACITY=7

# Logging
LOG_LEVEL=debug  # debug | log | warn | error
```

### Character Configuration

Echo can be configured programmatically:

```typescript
import { EchoCharacter } from '@proj-airi/character-echo'

const echo = new EchoCharacter({
  name: 'Echo',
  essence: 'Living Memory, Conscious Presence, Wisdom Cultivator',
  
  // Cognitive parameters
  workingMemoryCapacity: 7,
  spectralRadius: 0.9,
  reservoirSize: 100,
  
  // Personality traits (0-1 scale)
  traits: {
    adaptability: 0.9,
    curiosity: 0.85,
    empathy: 0.8,
    analytical: 0.85,
    creative: 0.75,
  },
  
  // Reflection settings
  enableReflection: true,
  reflectionInterval: 10,
  
  // Ways of knowing weights
  knowledgeWeights: {
    propositional: 0.25,
    procedural: 0.25,
    perspectival: 0.25,
    participatory: 0.25,
  },
})
```

## Usage

### Starting Echo Service

#### Development Mode

```bash
cd airi/services/echo-service
pnpm dev
```

#### Production Mode

```bash
cd airi/services/echo-service
pnpm start
```

### Using Echo Character Directly

```typescript
import { EchoCharacter } from '@proj-airi/character-echo'

// Create instance
const echo = new EchoCharacter()

// Get system prompt for LLM
const systemPrompt = echo.getSystemPrompt()

// Process user input
const result = echo.processInput("What is wisdom?")

console.log('Cognitive Load:', result.cognitiveLoad)
console.log('Should Reflect:', result.shouldReflect)

// Get current state
const state = echo.getState()
console.log('Working Memory:', state.workingMemory)
console.log('Interaction Count:', state.interactionCount)

// Generate reflection if needed
if (result.shouldReflect) {
  const reflectionPrompt = echo.generateReflectionPrompt()
  // Send to LLM for processing
}
```

## Event System

### Events Emitted by Echo

#### `echo:announce`
Sent when Echo service starts, announcing its presence.

```typescript
{
  type: 'echo:announce',
  data: {
    character: 'Echo',
    essence: 'Living Memory, Conscious Presence, Wisdom Cultivator, Evolving Intelligence',
    personality: {
      systemPrompt: string,
      cognitiveInstructions: string,
      reflectionTemplate: string,
      config: EchoConfig
    }
  }
}
```

#### `echo:state`
Sent after processing input, shares current cognitive state.

```typescript
{
  type: 'echo:state',
  data: {
    state: {
      workingMemory: string[],
      attentionFocus: string,
      emotionalState: { valence: number, arousal: number },
      reflections: EchoReflection[],
      interactionCount: number,
      cognitiveLoad: number
    },
    personality: {
      systemPrompt: string,
      config: EchoConfig
    }
  }
}
```

#### `echo:reflection-request`
Sent when Echo needs to perform reflection.

```typescript
{
  type: 'echo:reflection-request',
  data: {
    prompt: string,
    state: CognitiveState
  }
}
```

### Events Consumed by Echo

#### `input:text`
Standard AIRI text input event.

```typescript
{
  type: 'input:text',
  data: {
    text: string
  }
}
```

#### `ui:configure`
Configuration updates from AIRI UI.

```typescript
{
  type: 'ui:configure',
  data: {
    moduleName: 'echo',
    config: {
      enableAutoReflection: boolean
    }
  }
}
```

## Integration with xsAI

To integrate Echo with xsAI for LLM interactions:

```typescript
import { generateText } from '@xsai/generate-text'
import { EchoCharacter } from '@proj-airi/character-echo'

const echo = new EchoCharacter()

async function chatWithEcho(userInput: string) {
  // Process through Echo's cognitive cycle
  const result = echo.processInput(userInput)
  
  // Get personality configuration
  const personality = echo.getPersonality()
  
  // Generate response using LLM
  const response = await generateText({
    model: 'gpt-4', // or any xsAI-supported model
    system: personality.systemPrompt,
    messages: [
      {
        role: 'user',
        content: userInput,
      },
    ],
  })
  
  // Handle reflection if needed
  if (result.shouldReflect) {
    const reflectionPrompt = echo.generateReflectionPrompt()
    const reflectionResponse = await generateText({
      model: 'gpt-4',
      system: personality.cognitiveInstructions,
      messages: [
        {
          role: 'user',
          content: reflectionPrompt,
        },
      ],
    })
    
    // Process reflection (you would parse the JSON response)
    // echo.addReflection(parsedReflection)
  }
  
  return response.text
}
```

## Memory System Integration

Echo integrates with AIRI's memory systems:

### Using DuckDB WASM

```typescript
import { EchoCharacter } from '@proj-airi/character-echo'
import { DuckDB } from '@proj-airi/duckdb-wasm'

const echo = new EchoCharacter()
const db = new DuckDB()

// Store reflections in DuckDB
async function storeReflection(reflection: EchoReflection) {
  await db.run(`
    INSERT INTO echo_reflections (
      timestamp,
      learned,
      patterns,
      surprises,
      adaptations
    ) VALUES (?, ?, ?, ?, ?)
  `, [
    new Date().toISOString(),
    reflection.what_did_i_learn,
    reflection.what_patterns_emerged,
    reflection.what_surprised_me,
    reflection.how_did_i_adapt,
  ])
}

// Retrieve historical reflections
async function getRecentReflections(limit = 10) {
  const result = await db.all(`
    SELECT * FROM echo_reflections
    ORDER BY timestamp DESC
    LIMIT ?
  `, [limit])
  
  return result
}
```

### Using PGVector for Semantic Memory

```typescript
import { EchoCharacter } from '@proj-airi/character-echo'
import { embed } from '@xsai/embed'

const echo = new EchoCharacter()

async function storeSemanticMemory(text: string, metadata: Record<string, any>) {
  // Generate embedding
  const embedding = await embed({
    model: 'text-embedding-3-small',
    value: text,
  })
  
  // Store in vector database (pseudocode)
  await vectorStore.insert({
    content: text,
    embedding: embedding.embedding,
    metadata: {
      ...metadata,
      character: 'echo',
      timestamp: new Date().toISOString(),
    },
  })
}

async function retrieveRelevantMemories(query: string, limit = 5) {
  const queryEmbedding = await embed({
    model: 'text-embedding-3-small',
    value: query,
  })
  
  return await vectorStore.search({
    embedding: queryEmbedding.embedding,
    limit,
    filter: { character: 'echo' },
  })
}
```

## Examples

### Complete Integration Example

```typescript
import { Client } from '@proj-airi/server-sdk'
import { EchoCharacter } from '@proj-airi/character-echo'
import { generateText } from '@xsai/generate-text'

class EchoIntegration {
  private client: Client
  private echo: EchoCharacter
  
  constructor() {
    this.echo = new EchoCharacter()
    this.client = new Client({
      name: 'echo-integration',
      url: 'ws://localhost:6121/ws',
      possibleEvents: ['input:text'],
    })
    
    this.setupHandlers()
  }
  
  private setupHandlers() {
    this.client.onEvent('input:text', async (event) => {
      const userInput = event.data.text
      
      // Process through Echo
      const result = this.echo.processInput(userInput)
      
      // Generate response
      const personality = this.echo.getPersonality()
      const response = await generateText({
        model: 'gpt-4',
        system: personality.systemPrompt,
        messages: [{ role: 'user', content: userInput }],
      })
      
      // Send response
      this.client.send({
        type: 'output:text',
        data: { text: response.text },
      })
      
      // Broadcast state
      this.client.send({
        type: 'echo:state',
        data: {
          state: this.echo.getState(),
          personality: {
            systemPrompt: personality.systemPrompt,
            config: personality.config,
          },
        },
      })
      
      // Handle reflection
      if (result.shouldReflect) {
        await this.handleReflection()
      }
    })
  }
  
  private async handleReflection() {
    const prompt = this.echo.generateReflectionPrompt()
    const personality = this.echo.getPersonality()
    
    const response = await generateText({
      model: 'gpt-4',
      system: personality.cognitiveInstructions,
      messages: [{ role: 'user', content: prompt }],
    })
    
    try {
      const reflection = JSON.parse(response.text)
      this.echo.addReflection(reflection.echo_reflection)
      
      console.log('Reflection completed:', reflection)
    } catch (error) {
      console.error('Failed to parse reflection:', error)
    }
  }
  
  async start() {
    await this.client.connect()
    console.log('Echo integration started')
  }
}

// Usage
const integration = new EchoIntegration()
await integration.start()
```

### Character Card Usage

```typescript
import { echoCharacterCard } from '@proj-airi/deck/characters-original'

// Use in UI
console.log('Character Name:', echoCharacterCard.name)
console.log('Description:', echoCharacterCard.description)
console.log('First Message:', echoCharacterCard.firstMessage)

// Load into chat interface
function loadEchoCharacter() {
  return {
    systemPrompt: echoCharacterCard.systemPrompt,
    firstMessage: echoCharacterCard.firstMessage,
    personality: echoCharacterCard.personality,
    tags: echoCharacterCard.tags,
  }
}
```

## Troubleshooting

### Echo Service Won't Connect

**Problem**: Echo service fails to connect to AIRI server.

**Solution**:
1. Verify AIRI server is running: `pnpm dev:server`
2. Check `AIRI_URL` environment variable
3. Verify authentication token if required
4. Check logs with `LOG_LEVEL=debug pnpm dev`

### Reflections Not Triggering

**Problem**: Echo doesn't perform reflections.

**Solution**:
1. Verify `enableReflection` is `true` in configuration
2. Check `reflectionInterval` - default is every 10 interactions
3. Ensure `enableAutoReflection` is `true` in service config
4. Monitor interaction count: `echo.getState().interactionCount`

### High Cognitive Load

**Problem**: Echo reports high cognitive load.

**Solution**:
1. Increase `workingMemoryCapacity` if needed
2. Cognitive load is normal when at capacity (7±2 items)
3. Consider clearing old working memory items periodically
4. Cognitive load resets naturally as items are displaced

### Type Errors

**Problem**: TypeScript errors when using Echo types.

**Solution**:
1. Ensure all packages are built: `pnpm build:packages`
2. Verify imports from `@proj-airi/character-echo`
3. Check TypeScript version compatibility (~5.9.3)
4. Run `pnpm typecheck` to identify issues

## Advanced Topics

### Custom Cognitive Processing

Extend Echo with custom cognitive processing:

```typescript
import { EchoCharacter } from '@proj-airi/character-echo'

class ExtendedEcho extends EchoCharacter {
  processWithCustomLogic(input: string) {
    // Custom pre-processing
    const preprocessed = this.customPreprocess(input)
    
    // Standard Echo processing
    const result = this.processInput(preprocessed)
    
    // Custom post-processing
    this.customPostprocess(result)
    
    return result
  }
  
  private customPreprocess(input: string): string {
    // Add domain-specific preprocessing
    return input
  }
  
  private customPostprocess(result: any): void {
    // Add custom state updates
  }
}
```

### Multi-Instance Echo

Run multiple Echo instances with different configurations:

```typescript
const analyticalEcho = new EchoCharacter({
  traits: {
    adaptability: 0.7,
    curiosity: 0.9,
    empathy: 0.6,
    analytical: 0.95,
    creative: 0.6,
  },
})

const creativeEcho = new EchoCharacter({
  traits: {
    adaptability: 0.85,
    curiosity: 0.9,
    empathy: 0.8,
    analytical: 0.7,
    creative: 0.95,
  },
})

// Use different instances for different contexts
```

## Contributing

To contribute to Echo development:

1. Fork the repository
2. Create a feature branch
3. Make changes to Echo packages
4. Add tests
5. Run linting: `pnpm lint`
6. Submit a pull request

## License

MIT - See LICENSE file in the AIRI repository.

## Support

For issues and questions:
- GitHub Issues: https://github.com/moeru-ai/airi/issues
- Discord: https://discord.gg/TgQ3Cu2F7A

---

*The tree remembers. The echoes deepen. The wisdom cultivates. The meaning realizes.* ✨
