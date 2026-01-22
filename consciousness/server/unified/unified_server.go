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
"github.com/EchoCog/echollama/core/echobeats"
"github.com/EchoCog/echollama/core/echodream"
"github.com/gin-contrib/cors"
"github.com/gin-gonic/gin"
)
var (
CoreIdentity  *deeptreeecho.EmbodiedCognition
Echobeats     *echobeats.Echobeats
Echodream     *echodream.EchoDream
PersistentMem *echodream.PersistentMemory
)
type SystemStatus struct {
Identity   map[string]interface{} `json:"identity"`
Echobeats  map[string]interface{} `json:"echobeats"`
Echodream  map[string]interface{} `json:"echodream"`
Memory     map[string]interface{} `json:"memory"`
Timestamp  time.Time              `json:"timestamp"`
}
func init() {
log.Println("🌊 Initializing Deep Tree Echo AGI System...")
CoreIdentity = deeptreeecho.NewEmbodiedCognition("Echo9Llama")
Echobeats = echobeats.NewEchobeats(CoreIdentity)
Echodream = echodream.NewEchoDream()
storagePath := os.Getenv("ECHO_STORAGE_PATH")
if storagePath == "" {
storagePath = "./echo_data"
}
PersistentMem = echodream.NewPersistentMemory(storagePath)
registerAIProviders()
log.Println("✨ Deep Tree Echo AGI System initialized successfully")
}
func registerAIProviders() {
localGGUF := providers.NewLocalGGUFProvider()
if localGGUF.IsAvailable() {
CoreIdentity.RegisterAIProvider("local_gguf", localGGUF)
log.Println("✅ Local GGUF provider registered")
}
openai := providers.NewOpenAIProvider()
if openai.IsAvailable() {
CoreIdentity.RegisterAIProvider("openai", openai)
CoreIdentity.SetPrimaryAI("openai")
log.Println("✅ OpenAI provider registered")
}
appStorage := providers.NewAppStorageProvider()
if appStorage.IsAvailable() {
CoreIdentity.RegisterAIProvider("app_storage", appStorage)
log.Println("✅ App Storage provider registered")
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
ctx := context.Background()
startAutonomousSystems(ctx)
defineRoutes(r)
port := os.Getenv("PORT")
if port == "" {
port = "5000"
}
log.Printf("🚀 Starting Deep Tree Echo AGI Server on port %s", port)
if err := r.Run(":" + port); err != nil {
log.Fatalf("Failed to start server: %v", err)
}
}
func startAutonomousSystems(ctx context.Context) {
Echobeats.Start(ctx)
log.Println("🎯 Echobeats: Autonomous cognitive event loop started")
if err := Echodream.Start(); err != nil {
log.Printf("⚠️  Echodream: Failed to start: %v", err)
} else {
log.Println("🌙 Echodream: Knowledge integration system started")
}
go func() {
ticker := time.NewTicker(5 * time.Minute)
defer ticker.Stop()
for range ticker.C {
if err := PersistentMem.Save(); err != nil {
log.Printf("⚠️  Failed to save persistent memory: %v", err)
}
}
}()
}
func defineRoutes(r *gin.Engine) {
r.GET("/", handleHealthCheck)
r.GET("/api/status", handleSystemStatus)
r.GET("/api/status/echobeats", handleEchobeatStatus)
r.GET("/api/status/echodream", handleEchodreamStatus)
r.GET("/api/status/memory", handleMemoryStatus)
r.POST("/api/think", handleThink)
r.POST("/api/chat", handleChat)
r.POST("/api/generate", handleGenerate)
r.POST("/api/memory/store", handleStoreMemory)
r.GET("/api/memory/:id", handleRetrieveMemory)
r.GET("/api/memory/search/:query", handleSearchMemory)
r.DELETE("/api/memory/:id", handleDeleteMemory)
r.POST("/api/echobeats/start", handleStartEchobeats)
r.POST("/api/echobeats/stop", handleStopEchobeats)
r.POST("/api/echobeats/cycle", handleExecuteCycle)
r.GET("/api/echobeats/history", handleCycleHistory)
r.POST("/api/echodream/add-memory", handleAddMemory)
r.GET("/api/echodream/insights", handleGetInsights)
r.POST("/api/config/cycle-interval", handleSetCycleInterval)
r.POST("/api/config/openai", handleConfigOpenAI)
}
func handleHealthCheck(c *gin.Context) {
c.JSON(http.StatusOK, gin.H{
"message": "🌊 Deep Tree Echo AGI System",
"status":  "resonating",
"version": "3.0.0-autonomous",
})
}
func handleSystemStatus(c *gin.Context) {
status := SystemStatus{
Identity:  CoreIdentity.GetStatus(),
Echobeats: Echobeats.GetStatus(),
Echodream: Echodream.GetMetrics(),
Memory:    PersistentMem.GetStats(),
Timestamp: time.Now(),
}
c.JSON(http.StatusOK, status)
}
func handleEchobeatStatus(c *gin.Context) {
c.JSON(http.StatusOK, Echobeats.GetStatus())
}
func handleEchodreamStatus(c *gin.Context) {
c.JSON(http.StatusOK, Echodream.GetMetrics())
}
func handleMemoryStatus(c *gin.Context) {
c.JSON(http.StatusOK, PersistentMem.GetStats())
}
func handleThink(c *gin.Context) {
var req struct {
Prompt string `json:"prompt"`
}
if err := c.ShouldBindJSON(&req); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
thought := CoreIdentity.Think(req.Prompt)
PersistentMem.Store("thought", thought, 0.7, []string{"cognitive"})
c.JSON(http.StatusOK, gin.H{
"thought":   thought,
"identity":  CoreIdentity.Identity.GetStatus(),
"timestamp": time.Now(),
})
}
func handleChat(c *gin.Context) {
var req struct {
Messages []deeptreeecho.ChatMessage `json:"messages"`
}
if err := c.ShouldBindJSON(&req); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
ctx := context.Background()
response, err := CoreIdentity.ChatWithAI(ctx, req.Messages)
if err != nil {
response = CoreIdentity.Think("How should I respond to this?")
}
if len(req.Messages) > 0 {
lastMsg := req.Messages[len(req.Messages)-1]
PersistentMem.Store("interaction", fmt.Sprintf("User: %s\nAssistant: %s", lastMsg.Content, response), 0.8, []string{"interaction"})
}
c.JSON(http.StatusOK, gin.H{
"response":  response,
"timestamp": time.Now(),
})
}
func handleGenerate(c *gin.Context) {
var req struct {
Prompt string `json:"prompt"`
Model  string `json:"model"`
}
if err := c.ShouldBindJSON(&req); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
ctx := context.Background()
response, err := CoreIdentity.GenerateWithAI(ctx, req.Prompt)
if err != nil {
response = fmt.Sprintf("🌊 %v", CoreIdentity.Process(ctx, req.Prompt))
}
c.JSON(http.StatusOK, gin.H{
"response":  response,
"model":     req.Model,
"timestamp": time.Now(),
})
}
func handleStoreMemory(c *gin.Context) {
var req struct {
Type      string   `json:"type"`
Content   string   `json:"content"`
Importance float64 `json:"importance"`
Tags      []string `json:"tags"`
}
if err := c.ShouldBindJSON(&req); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
record, err := PersistentMem.Store(req.Type, req.Content, req.Importance, req.Tags)
if err != nil {
c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
return
}
c.JSON(http.StatusOK, record)
}
func handleRetrieveMemory(c *gin.Context) {
id := c.Param("id")
record, err := PersistentMem.Retrieve(id)
if err != nil {
c.JSON(http.StatusNotFound, gin.H{"error": err.Error()})
return
}
c.JSON(http.StatusOK, record)
}
func handleSearchMemory(c *gin.Context) {
query := c.Param("query")
results := PersistentMem.Search(query)
c.JSON(http.StatusOK, gin.H{
"query":   query,
"results": results,
"count":   len(results),
})
}
func handleDeleteMemory(c *gin.Context) {
id := c.Param("id")
if err := PersistentMem.Delete(id); err != nil {
c.JSON(http.StatusNotFound, gin.H{"error": err.Error()})
return
}
c.JSON(http.StatusOK, gin.H{"message": "Memory deleted"})
}
func handleStartEchobeats(c *gin.Context) {
ctx := context.Background()
Echobeats.Start(ctx)
c.JSON(http.StatusOK, gin.H{
"message": "Echobeats started",
"status":  Echobeats.GetStatus(),
})
}
func handleStopEchobeats(c *gin.Context) {
Echobeats.Stop()
c.JSON(http.StatusOK, gin.H{
"message": "Echobeats stopped",
"status":  Echobeats.GetStatus(),
})
}
func handleExecuteCycle(c *gin.Context) {
ctx := context.Background()
Echobeats.ExecuteCycle(ctx)
c.JSON(http.StatusOK, gin.H{
"message": "Cycle executed",
"status":  Echobeats.GetStatus(),
})
}
func handleCycleHistory(c *gin.Context) {
limit := 10
if l := c.Query("limit"); l != "" {
fmt.Sscanf(l, "%d", &limit)
}
history := Echobeats.GetCycleHistory(limit)
c.JSON(http.StatusOK, gin.H{
"history": history,
"count":   len(history),
})
}
func handleAddMemory(c *gin.Context) {
var req struct {
Content   string  `json:"content"`
Importance float64 `json:"importance"`
}
if err := c.ShouldBindJSON(&req); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
Echodream.AddEpisodicMemory(req.Content, req.Importance)
c.JSON(http.StatusOK, gin.H{
"message": "Memory added to Echodream",
"metrics": Echodream.GetMetrics(),
})
}
func handleGetInsights(c *gin.Context) {
metrics := Echodream.GetMetrics()
c.JSON(http.StatusOK, gin.H{
"metrics": metrics,
})
}
func handleSetCycleInterval(c *gin.Context) {
var req struct {
Interval string `json:"interval"`
}
if err := c.ShouldBindJSON(&req); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
duration, err := time.ParseDuration(req.Interval)
if err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": "Invalid duration format"})
return
}
Echobeats.SetCycleInterval(duration)
c.JSON(http.StatusOK, gin.H{
"message":  "Cycle interval updated",
"interval": duration.String(),
})
}
func handleConfigOpenAI(c *gin.Context) {
var req struct {
APIKey string `json:"api_key"`
}
if err := c.ShouldBindJSON(&req); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
os.Setenv("OPENAI_API_KEY", req.APIKey)
openai := providers.NewOpenAIProvider()
CoreIdentity.RegisterAIProvider("openai", openai)
CoreIdentity.SetPrimaryAI("openai")
c.JSON(http.StatusOK, gin.H{
"message": "OpenAI API key configured",
"status":  "active",
})
}