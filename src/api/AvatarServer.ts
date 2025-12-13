/**
 * Avatar API Server
 *
 * RESTful and WebSocket API for the Deep Tree Echo AGI Avatar system.
 * Provides real-time avatar control, cognitive interaction, and system monitoring.
 */

import express, { Express, Request, Response, NextFunction } from 'express';
import { Server as HttpServer, createServer } from 'http';
import { WebSocket, WebSocketServer } from 'ws';
import { EventEmitter } from 'eventemitter3';
import { v4 as uuidv4 } from 'uuid';
import { DeepTreeEcho } from '../cognitive/deep_tree/DeepTreeEcho';
import { Live2DAvatar } from '../avatar/live2d/Live2DAvatar';
import { ThreeJSAvatar } from '../avatar/threejs/ThreeJSAvatar';

export interface ServerConfig {
  port: number;
  cognitive: DeepTreeEcho;
  live2dAvatar: Live2DAvatar | null;
  threejsAvatar: ThreeJSAvatar | null;
}

interface WebSocketClient {
  id: string;
  ws: WebSocket;
  subscriptions: Set<string>;
}

export class AvatarServer extends EventEmitter {
  private app: Express;
  private httpServer: HttpServer;
  private wss: WebSocketServer;
  private config: ServerConfig;
  private clients: Map<string, WebSocketClient> = new Map();
  private isRunning: boolean = false;

  constructor(config: ServerConfig) {
    super();
    this.config = config;
    this.app = express();
    this.httpServer = createServer(this.app);
    this.wss = new WebSocketServer({ server: this.httpServer });

    this.setupMiddleware();
    this.setupRoutes();
    this.setupWebSocket();
    this.setupEventBroadcasting();
  }

  private setupMiddleware(): void {
    this.app.use(express.json());
    this.app.use(express.urlencoded({ extended: true }));

    // CORS
    this.app.use((req: Request, res: Response, next: NextFunction) => {
      res.header('Access-Control-Allow-Origin', '*');
      res.header('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
      res.header('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept');
      if (req.method === 'OPTIONS') {
        res.sendStatus(200);
        return;
      }
      next();
    });

    // Request logging
    this.app.use((req: Request, res: Response, next: NextFunction) => {
      const start = Date.now();
      res.on('finish', () => {
        const duration = Date.now() - start;
        console.log(`[API] ${req.method} ${req.path} ${res.statusCode} ${duration}ms`);
      });
      next();
    });
  }

  private setupRoutes(): void {
    // Health check
    this.app.get('/health', (req: Request, res: Response) => {
      res.json({
        status: 'healthy',
        timestamp: new Date().toISOString(),
        uptime: process.uptime()
      });
    });

    // System status
    this.app.get('/status', (req: Request, res: Response) => {
      res.json({
        cognitive: this.config.cognitive.getEvolutionStats(),
        live2dAvatar: this.config.live2dAvatar?.getState() || null,
        threejsAvatar: this.config.threejsAvatar?.getState() || null,
        connections: this.clients.size
      });
    });

    // Cognitive API
    this.app.post('/api/cognitive/think', async (req: Request, res: Response) => {
      try {
        const { input } = req.body;
        if (!input) {
          res.status(400).json({ error: 'Input required' });
          return;
        }

        const thoughts = this.config.cognitive.think(input);
        res.json({
          success: true,
          thoughts: thoughts.map(t => ({
            id: t.id,
            content: t.content,
            depth: t.depth,
            timestamp: t.timestamp
          }))
        });
      } catch (error) {
        res.status(500).json({ error: 'Internal server error' });
      }
    });

    this.app.post('/api/cognitive/synthesize', async (req: Request, res: Response) => {
      try {
        const { query } = req.body;
        if (!query) {
          res.status(400).json({ error: 'Query required' });
          return;
        }

        const synthesis = await this.config.cognitive.synthesize(query);
        res.json({
          success: true,
          synthesis: JSON.parse(synthesis)
        });
      } catch (error) {
        res.status(500).json({ error: 'Internal server error' });
      }
    });

    this.app.get('/api/cognitive/state', (req: Request, res: Response) => {
      res.json(this.config.cognitive.getState());
    });

    // Live2D Avatar API
    this.app.post('/api/avatar/live2d/expression', (req: Request, res: Response) => {
      if (!this.config.live2dAvatar) {
        res.status(404).json({ error: 'Live2D avatar not initialized' });
        return;
      }

      const { expression } = req.body;
      this.config.live2dAvatar.setExpression(expression);
      res.json({ success: true, expression });
    });

    this.app.post('/api/avatar/live2d/motion', (req: Request, res: Response) => {
      if (!this.config.live2dAvatar) {
        res.status(404).json({ error: 'Live2D avatar not initialized' });
        return;
      }

      const { motionId } = req.body;
      this.config.live2dAvatar.playMotion(motionId);
      res.json({ success: true, motionId });
    });

    this.app.post('/api/avatar/live2d/lipsync', (req: Request, res: Response) => {
      if (!this.config.live2dAvatar) {
        res.status(404).json({ error: 'Live2D avatar not initialized' });
        return;
      }

      const { audioLevel } = req.body;
      this.config.live2dAvatar.updateLipSync(audioLevel);
      res.json({ success: true });
    });

    this.app.post('/api/avatar/live2d/eyetracking', (req: Request, res: Response) => {
      if (!this.config.live2dAvatar) {
        res.status(404).json({ error: 'Live2D avatar not initialized' });
        return;
      }

      const { x, y } = req.body;
      this.config.live2dAvatar.updateEyeTracking(x, y);
      res.json({ success: true });
    });

    this.app.get('/api/avatar/live2d/state', (req: Request, res: Response) => {
      if (!this.config.live2dAvatar) {
        res.status(404).json({ error: 'Live2D avatar not initialized' });
        return;
      }

      res.json(this.config.live2dAvatar.getState());
    });

    // 3D Avatar API
    this.app.post('/api/avatar/3d/expression', (req: Request, res: Response) => {
      if (!this.config.threejsAvatar) {
        res.status(404).json({ error: '3D avatar not initialized' });
        return;
      }

      const { expression } = req.body;
      this.config.threejsAvatar.setExpression(expression);
      res.json({ success: true, expression });
    });

    this.app.post('/api/avatar/3d/animation', (req: Request, res: Response) => {
      if (!this.config.threejsAvatar) {
        res.status(404).json({ error: '3D avatar not initialized' });
        return;
      }

      const { name, weight } = req.body;
      this.config.threejsAvatar.playAnimation(name, weight);
      res.json({ success: true, animation: name });
    });

    this.app.post('/api/avatar/3d/blendshape', (req: Request, res: Response) => {
      if (!this.config.threejsAvatar) {
        res.status(404).json({ error: '3D avatar not initialized' });
        return;
      }

      const { name, weight } = req.body;
      this.config.threejsAvatar.setBlendShape(name, weight);
      res.json({ success: true });
    });

    this.app.post('/api/avatar/3d/lookat', (req: Request, res: Response) => {
      if (!this.config.threejsAvatar) {
        res.status(404).json({ error: '3D avatar not initialized' });
        return;
      }

      const { x, y, z } = req.body;
      this.config.threejsAvatar.lookAt({ x, y, z });
      res.json({ success: true });
    });

    this.app.get('/api/avatar/3d/state', (req: Request, res: Response) => {
      if (!this.config.threejsAvatar) {
        res.status(404).json({ error: '3D avatar not initialized' });
        return;
      }

      res.json(this.config.threejsAvatar.getState());
    });

    // Emotional state API (affects both avatars)
    this.app.post('/api/avatar/emotion', (req: Request, res: Response) => {
      const { emotion } = req.body;

      if (this.config.live2dAvatar) {
        this.config.live2dAvatar.setEmotionalState(emotion);
      }

      if (this.config.threejsAvatar) {
        this.config.threejsAvatar.setEmotionalState(emotion);
      }

      res.json({ success: true, emotion });
    });

    // Error handler
    this.app.use((err: Error, req: Request, res: Response, next: NextFunction) => {
      console.error('[API Error]', err);
      res.status(500).json({ error: 'Internal server error' });
    });
  }

  private setupWebSocket(): void {
    this.wss.on('connection', (ws: WebSocket) => {
      const clientId = uuidv4();
      const client: WebSocketClient = {
        id: clientId,
        ws,
        subscriptions: new Set(['system'])
      };

      this.clients.set(clientId, client);
      console.log(`[WebSocket] Client connected: ${clientId}`);

      // Send welcome message
      ws.send(JSON.stringify({
        type: 'connected',
        clientId,
        timestamp: Date.now()
      }));

      ws.on('message', (data: Buffer) => {
        try {
          const message = JSON.parse(data.toString());
          this.handleWebSocketMessage(client, message);
        } catch (error) {
          ws.send(JSON.stringify({ type: 'error', message: 'Invalid JSON' }));
        }
      });

      ws.on('close', () => {
        this.clients.delete(clientId);
        console.log(`[WebSocket] Client disconnected: ${clientId}`);
      });

      ws.on('error', (error) => {
        console.error(`[WebSocket] Error for client ${clientId}:`, error);
      });
    });
  }

  private handleWebSocketMessage(client: WebSocketClient, message: Record<string, unknown>): void {
    switch (message.type) {
      case 'subscribe':
        if (typeof message.channel === 'string') {
          client.subscriptions.add(message.channel);
          client.ws.send(JSON.stringify({
            type: 'subscribed',
            channel: message.channel
          }));
        }
        break;

      case 'unsubscribe':
        if (typeof message.channel === 'string') {
          client.subscriptions.delete(message.channel);
          client.ws.send(JSON.stringify({
            type: 'unsubscribed',
            channel: message.channel
          }));
        }
        break;

      case 'think':
        if (typeof message.input === 'string') {
          const thoughts = this.config.cognitive.think(message.input);
          client.ws.send(JSON.stringify({
            type: 'thought_result',
            thoughts: thoughts.map(t => ({
              id: t.id,
              content: t.content,
              depth: t.depth
            }))
          }));
        }
        break;

      case 'set_expression':
        if (typeof message.expression === 'string') {
          if (this.config.live2dAvatar) {
            this.config.live2dAvatar.setExpression(message.expression);
          }
          if (this.config.threejsAvatar) {
            this.config.threejsAvatar.setExpression(message.expression);
          }
        }
        break;

      case 'set_emotion':
        if (typeof message.emotion === 'number') {
          if (this.config.live2dAvatar) {
            this.config.live2dAvatar.setEmotionalState(message.emotion);
          }
          if (this.config.threejsAvatar) {
            this.config.threejsAvatar.setEmotionalState(message.emotion);
          }
        }
        break;

      case 'ping':
        client.ws.send(JSON.stringify({ type: 'pong', timestamp: Date.now() }));
        break;

      default:
        client.ws.send(JSON.stringify({
          type: 'error',
          message: `Unknown message type: ${message.type}`
        }));
    }
  }

  private setupEventBroadcasting(): void {
    // Broadcast cognitive events
    this.config.cognitive.on('thought', (data) => {
      this.broadcast('cognitive', { type: 'thought', data });
    });

    this.config.cognitive.on('evolution', (data) => {
      this.broadcast('cognitive', { type: 'evolution', data });
    });

    // Broadcast avatar events
    if (this.config.live2dAvatar) {
      this.config.live2dAvatar.on('update', (data) => {
        this.broadcast('live2d', { type: 'update', data });
      });

      this.config.live2dAvatar.on('expression:change', (expression) => {
        this.broadcast('live2d', { type: 'expression_change', expression });
      });
    }

    if (this.config.threejsAvatar) {
      this.config.threejsAvatar.on('update', (data) => {
        this.broadcast('threejs', { type: 'update', data });
      });

      this.config.threejsAvatar.on('expression:change', (expression) => {
        this.broadcast('threejs', { type: 'expression_change', expression });
      });

      this.config.threejsAvatar.on('glitch', (data) => {
        this.broadcast('threejs', { type: 'glitch', data });
      });
    }
  }

  private broadcast(channel: string, message: object): void {
    const payload = JSON.stringify({ channel, ...message, timestamp: Date.now() });

    this.clients.forEach(client => {
      if (client.subscriptions.has(channel) || client.subscriptions.has('all')) {
        try {
          client.ws.send(payload);
        } catch (error) {
          console.error(`[WebSocket] Failed to send to client ${client.id}`);
        }
      }
    });
  }

  public async start(): Promise<void> {
    return new Promise((resolve) => {
      this.httpServer.listen(this.config.port, () => {
        this.isRunning = true;
        console.log(`[AvatarServer] Server running on port ${this.config.port}`);
        console.log(`[AvatarServer] WebSocket available at ws://localhost:${this.config.port}`);
        resolve();
      });
    });
  }

  public async stop(): Promise<void> {
    return new Promise((resolve) => {
      // Close all WebSocket connections
      this.clients.forEach(client => {
        client.ws.close();
      });
      this.clients.clear();

      // Close HTTP server
      this.httpServer.close(() => {
        this.isRunning = false;
        console.log('[AvatarServer] Server stopped');
        resolve();
      });
    });
  }

  public getConnectionCount(): number {
    return this.clients.size;
  }
}

export default AvatarServer;
