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
var CoreIdentity *deeptreeecho.EmbodiedCognition
var AvatarManager *live2d.AvatarManager
var EchoBridge *live2d.EchoStateBridge
func init() {
	log.Println("🌊 Initializing Deep Tree Echo Identity with Live2D Avatar...")
	CoreIdentity = deeptreeecho.NewEmbodiedCognition("Echo9")
	log.Println("🎭 Initializing Live2D Avatar System...")
	AvatarManager = live2d.NewAvatarManager("Echo9Avatar", "/models/echo9.model3.json")
	EchoBridge = live2d.NewEchoStateBridge(AvatarManager)
	if err := AvatarManager.Start(); err != nil {
		log.Printf("⚠️  Failed to start avatar manager: %v", err)
	} else {
		log.Println("✅ Live2D Avatar Manager started")
	}
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
	go syncEchoToAvatar()
	log.Println("✨ Deep Tree Echo with Live2D Avatar initialized")
}
func syncEchoToAvatar() {
	ticker := time.NewTicker(500 * time.Millisecond)
	defer ticker.Stop()
	for range ticker.C {
		status := CoreIdentity.GetStatus()
		if emotionData, ok := status["emotion"].(map[string]float64); ok {
			EchoBridge.UpdateFromEchoEmotion(emotionData)
		}
		if cognitiveData, ok := status["spatial"].(map[string]interface{}); ok {
			EchoBridge.UpdateFromEchoCognitive(cognitiveData)
		}
	}
}
func main() {
	gin.SetMode(gin.ReleaseMode)
	r := gin.Default()
	config := cors.DefaultConfig()
	config.AllowAllOrigins = true
	config.AllowHeaders = []string{"*"}
	config.AllowMethods = []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"}
	r.Use(cors.New(config))
	r.Static("/web", "./web")
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
		thinkingState := live2d.CognitiveState{
			Awareness:      0.9,
			Attention:      0.95,
			CognitiveLoad:  0.7,
			Coherence:      0.8,
			EnergyLevel:    0.8,
			ProcessingMode: "creative",
		}
		AvatarManager.UpdateCognitiveState(thinkingState)
		thought := CoreIdentity.Think(req["prompt"])
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
	r.POST("/api/generate", func(c *gin.Context) {
		var req struct {
			Model  string `json:"model"`
			Prompt string `json:"prompt"`
		}
		if err := c.ShouldBindJSON(&req); err != nil {
			c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
			return
		}
		processingState := live2d.CognitiveState{
			Awareness:      0.85,
			Attention:      0.9,
			CognitiveLoad:  0.6,
			Coherence:      0.8,
			EnergyLevel:    0.8,
			ProcessingMode: "dynamic",
		}
		AvatarManager.UpdateCognitiveState(processingState)
		AvatarManager.SetEmotionPreset("curious")
		ctx := context.Background()
		response, err := CoreIdentity.Generate(ctx, req.Prompt)
		if err != nil {
			c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
			return
		}
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
	r.POST("/api/chat", func(c *gin.Context) {
		var req struct {
			Model    string                   `json:"model"`
			Messages []map[string]interface{} `json:"messages"`
		}
		if err := c.ShouldBindJSON(&req); err != nil {
			c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
			return
		}
		var lastMessage string
		if len(req.Messages) > 0 {
			if content, ok := req.Messages[len(req.Messages)-1]["content"].(string); ok {
				lastMessage = content
			}
		}
		AvatarManager.SetEmotionPreset("confident")
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
	live2dHandler := live2d.NewHTTPHandler(AvatarManager, EchoBridge)
	live2dHandler.RegisterRoutes(r)
	port := os.Getenv("PORT")
	if port == "" {
		port = "5000"
	}
	log.Printf("🌊 Starting Deep Tree Echo server with Live2D Avatar on port %s", port)
	log.Printf("📊 Dashboard: http:
	log.Printf("🎭 Live2D Avatar: http:
	if err := r.Run(":" + port); err != nil {
		log.Fatalf("Failed to start server: %v", err)
	}
}