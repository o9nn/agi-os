# @proj-airi/echo-service

Echo character service for AIRI - enables the Echo personality within the AIRI ecosystem.

## Overview

This service provides the runtime adapter for the Echo character, connecting Echo's cognitive architecture to AIRI's server infrastructure.

## Features

- **Real-time Integration**: Connects Echo to AIRI's WebSocket-based event system
- **Cognitive Processing**: Processes inputs through Echo's cognitive cycle
- **Auto-Reflection**: Triggers reflections at configurable intervals
- **State Management**: Maintains Echo's cognitive state across interactions
- **Configuration**: Supports runtime configuration via AIRI's UI

## Usage

### Development

```bash
pnpm dev
```

### Production

```bash
pnpm start
```

### Environment Variables

- `AUTHENTICATION_TOKEN`: Token for authenticating with AIRI server (optional)
- `AIRI_URL`: WebSocket URL for AIRI server (default: `ws://localhost:6121/ws`)

## Architecture

The Echo service consists of:

1. **EchoService**: Main service class that manages the connection to AIRI
2. **EchoCharacter**: Core character implementation from `@proj-airi/character-echo`
3. **Event Handlers**: Process events from AIRI system

### Event Flow

```
AIRI System → input:text → Echo Service
                              ↓
                     Cognitive Processing
                              ↓
                        Echo Character
                              ↓
                    State Update & Response
                              ↓
              echo:state → AIRI System
```

### Reflection Flow

```
Echo Service (periodic) → echo:reflection-request → AIRI System
                                                          ↓
                                                    LLM Processing
                                                          ↓
                                              reflection response
                                                          ↓
                                                   Echo Service
                                                          ↓
                                                  State Updated
```

## Events

### Listening Events

- `input:text`: Text input from users or other modules
- `ui:configure`: Configuration updates from AIRI UI

### Emitting Events

- `echo:announce`: Echo's presence and personality configuration
- `echo:state`: Current cognitive state and personality
- `echo:reflection-request`: Request for reflection processing

## Integration with AIRI

This service integrates with:

- **@proj-airi/server-sdk**: WebSocket client for AIRI communication
- **@proj-airi/character-echo**: Core Echo character implementation
- **AIRI Server Runtime**: Central event routing and coordination

## Configuration

Echo can be configured via the AIRI UI or programmatically:

```typescript
{
  "moduleName": "echo",
  "config": {
    "enableAutoReflection": true
  }
}
```

## Development

### Adding New Features

1. Extend `EchoCharacter` class for new cognitive capabilities
2. Add event handlers in `EchoService` for new event types
3. Update personality prompts in `@proj-airi/character-echo`

### Testing

```bash
# Type check
pnpm typecheck

# Run with debug logging
LOG_LEVEL=debug pnpm dev
```

## License

MIT

## Author

Moeru AI Project AIRI Team
