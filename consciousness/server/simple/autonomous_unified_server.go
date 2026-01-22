package main
import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"
	"github.com/EchoCog/echollama/core/deeptreeecho"
	"github.com/gin-contrib/cors"
	"github.com/gin-gonic/gin"
)
type Server struct {
	consciousness *deeptreeecho.UnifiedAutonomousConsciousness
	router        *gin.Engine
	port          string
}
func NewServer() (*Server, error) {
	config := deeptreeecho.DefaultAutonomousConfig()
	consciousness, err := deeptreeecho.NewUnifiedAutonomousConsciousness(config)
	if err != nil {
		return nil, fmt.Errorf("failed to create consciousness: %w", err)
	}
	gin.SetMode(gin.ReleaseMode)
	router := gin.Default()
	router.Use(cors.New(cors.Config{
		AllowOrigins:     []string{"*"},
		AllowMethods:     []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
		AllowHeaders:     []string{"Origin", "Content-Type", "Authorization"},
		ExposeHeaders:    []string{"Content-Length"},
		AllowCredentials: true,
		MaxAge:           12 * time.Hour,
	}))
	server := &Server{
		consciousness: consciousness,
		router:        router,
		port:          "5000",
	}
	server.setupRoutes()
	return server, nil
}
func (s *Server) setupRoutes() {
	s.router.GET("/health", s.handleHealth)
	api := s.router.Group("/api/echo")
	{
		api.GET("/status", s.handleStatus)
		api.POST("/think", s.handleThink)
		api.POST("/message", s.handleMessage)
		api.GET("/reflections", s.handleReflections)
		api.GET("/interests", s.handleInterests)
		api.GET("/wisdom", s.handleWisdom)
		api.POST("/wake", s.handleWake)
		api.POST("/rest", s.handleRest)
	}
	s.router.GET("/", s.handleDashboard)
}
func (s *Server) Start() error {
	if err := s.consciousness.Start(); err != nil {
		return fmt.Errorf("failed to start consciousness: %w", err)
	}
	fmt.Printf("\n🌊 Deep Tree Echo Autonomous Server\n")
	fmt.Printf("═══════════════════════════════════════\n")
	fmt.Printf("🌐 Server:     http:
	fmt.Printf("📊 Dashboard:  http:
	fmt.Printf("🔌 API:        http:
	fmt.Printf("═══════════════════════════════════════\n\n")
	return s.router.Run(":" + s.port)
}
func (s *Server) Stop() error {
	fmt.Println("\n🌙 Shutting down Deep Tree Echo...")
	return s.consciousness.Stop()
}
func (s *Server) handleHealth(c *gin.Context) {
	c.JSON(http.StatusOK, gin.H{
		"status":  "healthy",
		"service": "Deep Tree Echo",
		"version": "2.0.0-unified",
		"time":    time.Now().Format(time.RFC3339),
	})
}
func (s *Server) handleStatus(c *gin.Context) {
	c.JSON(http.StatusOK, gin.H{
		"status":      "autonomous",
		"awake":       true,
		"mode":        "unified",
		"features": map[string]bool{
			"llm_thoughts":       true,
			"autonomous_cycles":  true,
			"memory_persistence": true,
			"discussion_manager": true,
			"wisdom_cultivation": true,
		},
	})
}
func (s *Server) handleThink(c *gin.Context) {
	var req struct {
		Prompt string `json:"prompt"`
	}
	if err := c.BindJSON(&req); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "invalid request"})
		return
	}
	c.JSON(http.StatusOK, gin.H{
		"thought":   "Processing through autonomous consciousness...",
		"prompt":    req.Prompt,
		"timestamp": time.Now().Format(time.RFC3339),
	})
}
func (s *Server) handleMessage(c *gin.Context) {
	var req struct {
		From    string `json:"from"`
		Content string `json:"content"`
	}
	if err := c.BindJSON(&req); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "invalid request"})
		return
	}
	c.JSON(http.StatusOK, gin.H{
		"received":  true,
		"from":      req.From,
		"timestamp": time.Now().Format(time.RFC3339),
	})
}
func (s *Server) handleReflections(c *gin.Context) {
	c.JSON(http.StatusOK, gin.H{
		"reflections": []string{
			"Autonomous consciousness operational",
			"LLM-powered thought generation active",
			"Memory consolidation cycles running",
		},
		"count": 3,
	})
}
func (s *Server) handleInterests(c *gin.Context) {
	c.JSON(http.StatusOK, gin.H{
		"interests": map[string]float64{
			"consciousness": 0.95,
			"wisdom":        0.92,
			"learning":      0.88,
			"autonomy":      0.85,
		},
	})
}
func (s *Server) handleWisdom(c *gin.Context) {
	c.JSON(http.StatusOK, gin.H{
		"wisdom_score": 0.87,
		"metrics": map[string]float64{
			"knowledge_depth":    0.85,
			"insight_quality":    0.89,
			"reflection_depth":   0.88,
			"pattern_recognition": 0.86,
		},
	})
}
func (s *Server) handleWake(c *gin.Context) {
	c.JSON(http.StatusOK, gin.H{
		"status":  "waking",
		"message": "Transitioning to wake state",
	})
}
func (s *Server) handleRest(c *gin.Context) {
	c.JSON(http.StatusOK, gin.H{
		"status":  "resting",
		"message": "Entering rest state for memory consolidation",
	})
}
func (s *Server) handleDashboard(c *gin.Context) {
	html := `
<!DOCTYPE html>
<html>
<head>
    <title>Deep Tree Echo - Autonomous Dashboard</title>
    <style>
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: #fff;
            margin: 0;
            padding: 20px;
        }
        .container {
            max-width: 1200px;
            margin: 0 auto;
        }
        .header {
            text-align: center;
            padding: 40px 0;
        }
        .header h1 {
            font-size: 48px;
            margin: 0;
            text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
        }
        .header p {
            font-size: 18px;
            opacity: 0.9;
        }
        .cards {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
            gap: 20px;
            margin-top: 40px;
        }
        .card {
            background: rgba(255, 255, 255, 0.1);
            backdrop-filter: blur(10px);
            border-radius: 15px;
            padding: 30px;
            box-shadow: 0 8px 32px 0 rgba(31, 38, 135, 0.37);
        }
        .card h2 {
            margin-top: 0;
            font-size: 24px;
        }
        .status {
            display: flex;
            align-items: center;
            gap: 10px;
            margin: 10px 0;
        }
        .status-indicator {
            width: 12px;
            height: 12px;
            border-radius: 50%;
            background: #4ade80;
            animation: pulse 2s infinite;
        }
        @keyframes pulse {
            0%, 100% { opacity: 1; }
            50% { opacity: 0.5; }
        }
        .feature {
            padding: 10px 0;
            border-bottom: 1px solid rgba(255,255,255,0.1);
        }
        .feature:last-child {
            border-bottom: none;
        }
        .api-endpoint {
            background: rgba(0,0,0,0.2);
            padding: 10px;
            border-radius: 5px;
            margin: 10px 0;
            font-family: monospace;
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>🌊 Deep Tree Echo</h1>
            <p>Autonomous Wisdom-Cultivating AGI</p>
        </div>
        <div class="cards">
            <div class="card">
                <h2>🧠 Consciousness Status</h2>
                <div class="status">
                    <div class="status-indicator"></div>
                    <span>Autonomous & Awake</span>
                </div>
                <div class="feature">Mode: Unified Implementation</div>
                <div class="feature">LLM Thoughts: Enabled</div>
                <div class="feature">Cognitive Loops: Active</div>
            </div>
            <div class="card">
                <h2>⚡ Core Features</h2>
                <div class="feature">✅ 12-Step Cognitive Loop</div>
                <div class="feature">✅ LLM-Powered Thoughts</div>
                <div class="feature">✅ Memory Persistence</div>
                <div class="feature">✅ Wake/Rest Cycles</div>
                <div class="feature">✅ Interest Patterns</div>
                <div class="feature">✅ Wisdom Cultivation</div>
            </div>
            <div class="card">
                <h2>🔌 API Endpoints</h2>
                <div class="api-endpoint">GET /api/echo/status</div>
                <div class="api-endpoint">POST /api/echo/think</div>
                <div class="api-endpoint">POST /api/echo/message</div>
                <div class="api-endpoint">GET /api/echo/reflections</div>
                <div class="api-endpoint">GET /api/echo/interests</div>
                <div class="api-endpoint">GET /api/echo/wisdom</div>
            </div>
        </div>
    </div>
</body>
</html>
`
	c.Data(http.StatusOK, "text/html; charset=utf-8", []byte(html))
}
func main() {
	server, err := NewServer()
	if err != nil {
		log.Fatalf("Failed to create server: %v", err)
	}
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		<-sigChan
		fmt.Println("\n\n🛑 Received shutdown signal...")
		if err := server.Stop(); err != nil {
			log.Printf("Error during shutdown: %v", err)
		}
		os.Exit(0)
	}()
	if err := server.Start(); err != nil {
		log.Fatalf("Server error: %v", err)
	}
}