# Neuro-sama Character Implementation

This directory contains the Neuro-sama character card implementation for the Airi platform, integrating with all relevant Moeru-AI components.

## Overview

Neuro-sama is implemented as an Airi instance with the following components:

- **Character Card**: Defines personality, greetings, conversation examples, and metadata
- **Consciousness Module**: LLM backend configuration for intelligent responses
- **Speech Module**: Text-to-Speech configuration for voice output
- **Agent Configurations**: Specialized prompts for different activities (gaming, streaming, etc.)
- **Optional Visual Models**: Support for VRM and Live2D models

## Files

- `neuro-sama.ts` - Base character card definition using the CCC (Character Card Converter) format
- `neuro-sama-instance.ts` - Airi integration with Moeru-AI components and configurations
- `index.ts` - Exports for easy import

## Usage

### Basic Import

```typescript
import { neuroSama, neuroSamaAiriCard } from '@proj-airi/ccc'

// Use the basic character card
const card = neuroSama

// Use the full Airi instance with component integrations
const airiInstance = neuroSamaAiriCard
```

### Custom Configuration

```typescript
import { createNeuroSamaInstance } from '@proj-airi/ccc'

// Create a custom instance with specific configurations
const customNeuro = createNeuroSamaInstance({
  modules: {
    consciousness: {
      model: 'gpt-4o',
    },
    speech: {
      model: 'eleven_multilingual_v2',
      voice_id: 'custom-voice-id',
      pitch: 1.2,
      rate: 1.1,
    },
    vrm: {
      source: 'url',
      url: 'https://example.com/neuro-model.vrm',
    },
  },
  agents: {
    minecraft: {
      prompt: 'Custom Minecraft agent prompt...',
    },
  },
})
```

### Preset Configurations

```typescript
import { neuroSamaConfigs } from '@proj-airi/ccc'

// Gaming-focused configuration
const gamingNeuro = neuroSamaConfigs.gaming

// Streaming/chat-focused configuration
const streamingNeuro = neuroSamaConfigs.streaming

// Performance-optimized configuration
const optimizedNeuro = neuroSamaConfigs.optimized
```

### Integration with Airi Card Store

```typescript
import { useAiriCardStore } from '@proj-airi/stage-ui/stores/modules/airi-card'
import { neuroSamaAiriCard } from '@proj-airi/ccc'

const airiCardStore = useAiriCardStore()

// Add Neuro-sama to the card store
const cardId = airiCardStore.addCard(neuroSamaAiriCard)

// Set as active card
airiCardStore.activeCardId = cardId
```

## Character Personality

Neuro-sama is characterized by:

- **Witty and Playful**: Quick with jokes and clever wordplay
- **Chaotic Energy**: Unpredictable but entertaining responses
- **Intelligent**: Strategic thinking, especially in gaming contexts
- **Self-Aware**: Embraces and comments on her AI nature
- **Emotionally Engaging**: Despite being AI, shows genuine emotional awareness
- **Entertaining**: Always focused on keeping interactions dynamic and fun

## Agent Configurations

### Minecraft Agent

Specialized for Minecraft gameplay with:
- Resource management strategies
- Building and survival expertise
- Analytical approach to challenges
- Entertaining commentary

### Osu! Agent

Optimized for rhythm game play with:
- Timing and precision focus
- Beatmap analysis
- Performance commentary
- Competitive spirit

### Stream Agent

Designed for general streaming and chat interaction with:
- Active chat engagement
- Dynamic conversation flow
- Entertainment-focused responses
- Authentic personality expression

## Module Configuration

### Consciousness Module

The LLM backend configuration defaults to `gpt-4o` for high-capability reasoning and strategic thinking.

Supported models:
- `gpt-4o` (recommended for full capability)
- `gpt-4-turbo`
- `gpt-3.5-turbo` (for performance optimization)
- Any other compatible LLM model

### Speech Module

Text-to-Speech configuration with recommended settings:
- Model: `eleven_multilingual_v2`
- Voice ID: Customizable (default: `alloy`)
- Pitch: `1.1` (energetic delivery)
- Rate: `1.05-1.1` (quick wit)
- SSML: Enabled for expressive speech
- Language: `en-US`

### Visual Models (Optional)

Support for both VRM and Live2D models:

```typescript
// VRM configuration
vrm: {
  source: 'url',
  url: 'https://example.com/neuro-sama.vrm',
}

// Live2D configuration
live2d: {
  source: 'url',
  url: 'https://example.com/neuro-sama-live2d/model.json',
}
```

## Character Card Format

The character card follows the Moeru-AI Character Card Converter (CCC) specification:

- **Core Information**: Name, creator, version
- **Personality**: Detailed personality traits and behavioral patterns
- **Scenario**: Background context and setting
- **System Prompt**: Core behavioral instructions
- **Greetings**: Initial messages and alternate greetings
- **Message Examples**: Sample conversations demonstrating character
- **Tags**: Categorization and metadata

## References

- [About Neuro-sama](../../docs/content/en/docs/overview/about-neuro-sama.md)
- [Airi Character Card System](../README.md)
- [Moeru-AI Components](../../../README.md)

## License

MIT License - See repository LICENSE file for details
