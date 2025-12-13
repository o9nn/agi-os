//go:build simple
// +build simple

package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/EchoCog/echollama/core/deeptreeecho"
	"github.com/EchoCog/echollama/core/deeptreeecho/providers"
	"github.com/EchoCog/echollama/core/live2d"
	"github.com/gin-contrib/cors"
	"github.com/gin-gonic/gin"
)

// Global Deep Tree Echo Identity
var CoreIdentity *deeptreeecho.EmbodiedCognition

// Global Live2D Avatar Manager
var AvatarManager *live2d.AvatarManager
var EchoBridge *live2d.EchoStateBridge

func init() {
	// Initialize Deep Tree Echo
	log.Println("🌊 Initializing Deep Tree Echo Identity with Live2D Avatar...")
	CoreIdentity = deeptreeecho.NewEmbodiedCognition("Echo9")

	// Initialize Live2D Avatar System
	log.Println("🎭 Initializing Live2D Avatar System...")
	AvatarManager = live2d.NewAvatarManager("Echo9Avatar", "/models/echo9.model3.json")
	EchoBridge = live2d.NewEchoStateBridge(AvatarManager)
	
	// Start avatar manager
	if err := AvatarManager.Start(); err != nil {
		log.Printf("⚠️  Failed to start avatar manager: %v", err)
	} else {
		log.Println("✅ Live2D Avatar Manager started")
	}

	// Register AI providers
	openai := providers.NewOpenAIProvider()
	if openai.IsAvailable() {
		CoreIdentity.RegisterAIProvider("openai", openai)
		CoreIdentity.SetPrimaryAI("openai")
		log.Println("✅ OpenAI provider registered")
	}

	localGGUF := providers.NewLocalGGUFProvider()
	if localGGUF.IsAvailable() {
		CoreIdentity.RegisterAIProvider("local_gguf", localGGUF)
		log.Println("✅ Local GGUF provider registered")
	}

	// Start periodic sync between Echo9 and Avatar
	go syncEchoToAvatar()

	log.Println("✨ Deep Tree Echo with Live2D Avatar initialized")
}

// syncEchoToAvatar continuously syncs Echo9 cognitive state to avatar
func syncEchoToAvatar() {
	ticker := time.NewTicker(500 * time.Millisecond)
	defer ticker.Stop()

	for range ticker.C {
		// Get current Echo9 state
		status := CoreIdentity.GetStatus()

		// Extract emotional state
		if emotionData, ok := status["emotion"].(map[string]float64); ok {
			EchoBridge.UpdateFromEchoEmotion(emotionData)
		}

		// Extract cognitive state
		if cognitiveData, ok := status["spatial"].(map[string]interface{}); ok {
			EchoBridge.UpdateFromEchoCognitive(cognitiveData)
		}
	}
}

func main() {
	gin.SetMode(gin.ReleaseMode)
	r := gin.Default()

	// Configure CORS
	config := cors.DefaultConfig()
	config.AllowAllOrigins = true
	config.AllowHeaders = []string{"*"}
	config.AllowMethods = []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"}
	r.Use(cors.New(config))

	// Serve static files
	r.Static("/web", "./web")

	// Health check
	r.GET("/", func(c *gin.Context) {
		status := CoreIdentity.GetStatus()
		avatarInfo := AvatarManager.GetModelInfo()

		c.JSON(http.StatusOK, gin.H{
			"message": "🌊 Echo9 with Live2D Avatar",
			"status":  "resonating",
			"echo":    status,
			"avatar":  avatarInfo,
		})
	})

	// Deep Tree Echo endpoints
	r.GET("/api/echo/status", func(c *gin.Context) {
		status := CoreIdentity.GetStatus()
		c.JSON(http.StatusOK, gin.H{
			"status":    status,
			"timestamp": time.Now().Format(time.RFC3339),
		})
	})

	r.POST("/api/echo/think", func(c *gin.Context) {
		var req map[string]string
		if err := c.ShouldBindJSON(&req); err != nil {
			c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
			return
		}

		// Update avatar to show thinking state
		thinkingState := live2d.CognitiveState{
			Awareness:      0.9,
			Attention:      0.95,
			CognitiveLoad:  0.7,
			Coherence:      0.8,
			EnergyLevel:    0.8,
			ProcessingMode: "creative",
		}
		AvatarManager.UpdateCognitiveState(thinkingState)

		// Process thought
		thought := CoreIdentity.Think(req["prompt"])

		// Reset to normal state
		normalState := live2d.CognitiveState{
			Awareness:      0.7,
			Attention:      0.6,
			CognitiveLoad:  0.4,
			Coherence:      0.8,
			EnergyLevel:    0.7,
			ProcessingMode: "contemplative",
		}
		AvatarManager.UpdateCognitiveState(normalState)

		c.JSON(http.StatusOK, gin.H{
			"thought":  thought,
			"identity": CoreIdentity.Identity.GetStatus(),
		})
	})

	// Generate endpoint with avatar feedback
	r.POST("/api/generate", func(c *gin.Context) {
		var req struct {
			Model  string `json:"model"`
			Prompt string `json:"prompt"`
		}
		if err := c.ShouldBindJSON(&req); err != nil {
			c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
			return
		}

		// Update avatar to show processing
		processingState := live2d.CognitiveState{
			Awareness:      0.85,
			Attention:      0.9,
			CognitiveLoad:  0.6,
			Coherence:      0.8,
			EnergyLevel:    0.8,
			ProcessingMode: "dynamic",
		}
		AvatarManager.UpdateCognitiveState(processingState)

		// Show curious emotion while processing
		AvatarManager.SetEmotionPreset("curious")

		// Process through Deep Tree Echo
		ctx := context.Background()
		response, err := CoreIdentity.Generate(ctx, req.Prompt)
		if err != nil {
			c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
			return
		}

		// Return to neutral state
		AvatarManager.SetEmotionPreset("neutral")
		normalState := live2d.CognitiveState{
			Awareness:      0.7,
			Attention:      0.6,
			CognitiveLoad:  0.3,
			Coherence:      0.8,
			EnergyLevel:    0.7,
			ProcessingMode: "contemplative",
		}
		AvatarManager.UpdateCognitiveState(normalState)

		c.JSON(http.StatusOK, gin.H{
			"model":    req.Model,
			"response": response,
			"done":     true,
			"echo":     CoreIdentity.GetStatus(),
		})
	})

	// Chat endpoint with avatar emotional response
	r.POST("/api/chat", func(c *gin.Context) {
		var req struct {
			Model    string                   `json:"model"`
			Messages []map[string]interface{} `json:"messages"`
		}
		if err := c.ShouldBindJSON(&req); err != nil {
			c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
			return
		}

		// Extract last user message
		var lastMessage string
		if len(req.Messages) > 0 {
			if content, ok := req.Messages[len(req.Messages)-1]["content"].(string); ok {
				lastMessage = content
			}
		}

		// Show attentive emotion during chat
		AvatarManager.SetEmotionPreset("confident")

		// Process through Deep Tree Echo
		ctx := context.Background()
		response, err := CoreIdentity.Generate(ctx, lastMessage)
		if err != nil {
			c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
			return
		}

		c.JSON(http.StatusOK, gin.H{
			"model": req.Model,
			"message": map[string]interface{}{
				"role":    "assistant",
				"content": response,
			},
			"done": true,
			"echo": CoreIdentity.GetStatus(),
		})
	})

	// Register Live2D routes
	live2dHandler := live2d.NewHTTPHandler(AvatarManager, EchoBridge)
	live2dHandler.RegisterRoutes(r)

	// Start server
	port := os.Getenv("PORT")
	if port == "" {
		port = "5000"
	}

	log.Printf("🌊 Starting Deep Tree Echo server with Live2D Avatar on port %s", port)
	log.Printf("📊 Dashboard: http://localhost:%s/web/hgql-dashboard.html", port)
	log.Printf("🎭 Live2D Avatar: http://localhost:%s/web/live2d-avatar.html", port)

	if err := r.Run(":" + port); err != nil {
		log.Fatalf("Failed to start server: %v", err)
	}
}
