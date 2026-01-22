package live2d
import (
"encoding/json"
"net/http"
"time"
"github.com/gin-gonic/gin"
)
type HTTPHandler struct {
avatarManager *AvatarManager
echoBridge    *EchoStateBridge
}
func NewHTTPHandler(avatarManager *AvatarManager, echoBridge *EchoStateBridge) *HTTPHandler {
return &HTTPHandler{
avatarManager: avatarManager,
echoBridge:    echoBridge,
}
}
func (h *HTTPHandler) RegisterRoutes(r *gin.Engine) {
live2d := r.Group("/api/live2d")
{
live2d.GET("/status", h.GetStatus)
live2d.GET("/model/info", h.GetModelInfo)
live2d.GET("/state", h.GetCurrentState)
live2d.POST("/state/emotional", h.UpdateEmotionalState)
live2d.POST("/state/cognitive", h.UpdateCognitiveState)
live2d.POST("/state/full", h.UpdateFullState)
live2d.POST("/emotion/preset/:name", h.SetEmotionPreset)
live2d.POST("/emotion/transition", h.TransitionEmotion)
live2d.GET("/parameters", h.GetCurrentParameters)
live2d.GET("/parameters/stream", h.StreamParameters)
live2d.POST("/echo/emotion", h.UpdateFromEchoEmotion)
live2d.POST("/echo/cognitive", h.UpdateFromEchoCognitive)
live2d.POST("/echo/reservoir", h.UpdateFromEchoReservoir)
}
}
func (h *HTTPHandler) GetStatus(c *gin.Context) {
c.JSON(http.StatusOK, gin.H{
"status":  "ok",
"running": h.avatarManager.IsRunning(),
"model":   h.avatarManager.GetModelInfo(),
})
}
func (h *HTTPHandler) GetModelInfo(c *gin.Context) {
c.JSON(http.StatusOK, h.avatarManager.GetModelInfo())
}
func (h *HTTPHandler) GetCurrentState(c *gin.Context) {
state := h.avatarManager.GetCurrentState()
c.JSON(http.StatusOK, state)
}
func (h *HTTPHandler) UpdateEmotionalState(c *gin.Context) {
var emotional EmotionalState
if err := c.ShouldBindJSON(&emotional); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
if err := h.avatarManager.UpdateEmotionalState(emotional); err != nil {
c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
return
}
c.JSON(http.StatusOK, gin.H{"status": "updated"})
}
func (h *HTTPHandler) UpdateCognitiveState(c *gin.Context) {
var cognitive CognitiveState
if err := c.ShouldBindJSON(&cognitive); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
if err := h.avatarManager.UpdateCognitiveState(cognitive); err != nil {
c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
return
}
c.JSON(http.StatusOK, gin.H{"status": "updated"})
}
func (h *HTTPHandler) UpdateFullState(c *gin.Context) {
var state AvatarState
if err := c.ShouldBindJSON(&state); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
if err := h.avatarManager.UpdateFullState(state); err != nil {
c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
return
}
c.JSON(http.StatusOK, gin.H{"status": "updated"})
}
func (h *HTTPHandler) SetEmotionPreset(c *gin.Context) {
presetName := c.Param("name")
if err := h.avatarManager.SetEmotionPreset(presetName); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
c.JSON(http.StatusOK, gin.H{
"status": "updated",
"preset": presetName,
})
}
func (h *HTTPHandler) TransitionEmotion(c *gin.Context) {
var req struct {
From       EmotionalState `json:"from"`
To         EmotionalState `json:"to"`
DurationMs int            `json:"duration_ms"`
}
if err := c.ShouldBindJSON(&req); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
duration := 1000
if req.DurationMs > 0 {
duration = req.DurationMs
}
go h.avatarManager.AnimateEmotionTransition(req.From, req.To,
time.Duration(duration)*time.Millisecond)
c.JSON(http.StatusOK, gin.H{
"status":   "transition_started",
"duration": duration,
})
}
func (h *HTTPHandler) GetCurrentParameters(c *gin.Context) {
params, err := h.avatarManager.GetCurrentParameters()
if err != nil {
c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
return
}
c.Data(http.StatusOK, "application/json", params)
}
func (h *HTTPHandler) StreamParameters(c *gin.Context) {
updateChan, err := h.avatarManager.Subscribe()
if err != nil {
c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
return
}
c.Header("Content-Type", "text/event-stream")
c.Header("Cache-Control", "no-cache")
c.Header("Connection", "keep-alive")
clientGone := c.Request.Context().Done()
for {
select {
case update, ok := <-updateChan:
if !ok {
return
}
data, err := json.Marshal(update)
if err != nil {
continue
}
c.SSEvent("parameters", string(data))
c.Writer.Flush()
case <-clientGone:
return
}
}
}
func (h *HTTPHandler) UpdateFromEchoEmotion(c *gin.Context) {
var echoEmotion map[string]float64
if err := c.ShouldBindJSON(&echoEmotion); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
if err := h.echoBridge.UpdateFromEchoEmotion(echoEmotion); err != nil {
c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
return
}
c.JSON(http.StatusOK, gin.H{"status": "updated"})
}
func (h *HTTPHandler) UpdateFromEchoCognitive(c *gin.Context) {
var echoCognitive map[string]interface{}
if err := c.ShouldBindJSON(&echoCognitive); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
if err := h.echoBridge.UpdateFromEchoCognitive(echoCognitive); err != nil {
c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
return
}
c.JSON(http.StatusOK, gin.H{"status": "updated"})
}
func (h *HTTPHandler) UpdateFromEchoReservoir(c *gin.Context) {
var reservoirState map[string]interface{}
if err := c.ShouldBindJSON(&reservoirState); err != nil {
c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
return
}
if err := h.echoBridge.UpdateFromEchoReservoir(reservoirState); err != nil {
c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
return
}
c.JSON(http.StatusOK, gin.H{"status": "updated"})
}