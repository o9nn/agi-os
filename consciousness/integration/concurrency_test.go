package integration
import (
	"context"
	"log/slog"
	"os"
	"strconv"
	"sync"
	"testing"
	"time"
	"github.com/stretchr/testify/require"
	"github.com/EchoCog/echollama/api"
	"github.com/EchoCog/echollama/format"
)
func TestMultiModelConcurrency(t *testing.T) {
	var (
		req = [2]api.GenerateRequest{
			{
				Model:     "llama3.2:1b",
				Prompt:    "why is the ocean blue?",
				Stream:    &stream,
				KeepAlive: &api.Duration{Duration: 10 * time.Second},
				Options: map[string]any{
					"seed":        42,
					"temperature": 0.0,
				},
			}, {
				Model:     "tinydolphin",
				Prompt:    "what is the origin of the us thanksgiving holiday?",
				Stream:    &stream,
				KeepAlive: &api.Duration{Duration: 10 * time.Second},
				Options: map[string]any{
					"seed":        42,
					"temperature": 0.0,
				},
			},
		}
		resp = [2][]string{
			{"sunlight"},
			{"england", "english", "massachusetts", "pilgrims", "british", "festival"},
		}
	)
	var wg sync.WaitGroup
	wg.Add(len(req))
	ctx, cancel := context.WithTimeout(context.Background(), time.Second*240)
	defer cancel()
	client, _, cleanup := InitServerConnection(ctx, t)
	defer cleanup()
	for i := 0; i < len(req); i++ {
		require.NoError(t, PullIfMissing(ctx, client, req[i].Model))
	}
	for i := 0; i < len(req); i++ {
		go func(i int) {
			defer wg.Done()
			DoGenerate(ctx, t, client, req[i], resp[i], 90*time.Second, 30*time.Second)
		}(i)
	}
	wg.Wait()
}
func TestIntegrationConcurrentPredict(t *testing.T) {
	req, resp := GenerateRequests()
	reqLimit := len(req)
	iterLimit := 5
	if s := os.Getenv("OLLAMA_MAX_VRAM"); s != "" {
		maxVram, err := strconv.ParseUint(s, 10, 64)
		require.NoError(t, err)
		if maxVram < 4*format.GibiByte {
			reqLimit = min(reqLimit, 2)
			iterLimit = 2
		}
	}
	ctx, cancel := context.WithTimeout(context.Background(), 9*time.Minute)
	defer cancel()
	client, _, cleanup := InitServerConnection(ctx, t)
	defer cleanup()
	DoGenerate(ctx, t, client, req[0], resp[0], 60*time.Second, 10*time.Second)
	var wg sync.WaitGroup
	wg.Add(reqLimit)
	for i := 0; i < reqLimit; i++ {
		go func(i int) {
			defer wg.Done()
			for j := 0; j < iterLimit; j++ {
				slog.Info("Starting", "req", i, "iter", j)
				DoGenerate(ctx, t, client, req[i], resp[i], 120*time.Second, 20*time.Second)
			}
		}(i)
	}
	wg.Wait()
}
func TestMultiModelStress(t *testing.T) {
	s := os.Getenv("OLLAMA_MAX_VRAM") 
	if s == "" {
		t.Skip("OLLAMA_MAX_VRAM not specified, can't pick the right models for the stress test")
	}
	maxVram, err := strconv.ParseUint(s, 10, 64)
	if err != nil {
		t.Fatal(err)
	}
	if maxVram < 2*format.GibiByte {
		t.Skip("VRAM less than 2G, skipping model stress tests")
	}
	type model struct {
		name string
		size uint64 
	}
	smallModels := []model{
		{
			name: "llama3.2:1b",
			size: 2876 * format.MebiByte,
		},
		{
			name: "phi",
			size: 2616 * format.MebiByte,
		},
		{
			name: "gemma:2b",
			size: 2364 * format.MebiByte,
		},
		{
			name: "stable-code:3b",
			size: 2608 * format.MebiByte,
		},
		{
			name: "starcoder2:3b",
			size: 2166 * format.MebiByte,
		},
	}
	mediumModels := []model{
		{
			name: "llama2",
			size: 5118 * format.MebiByte,
		},
		{
			name: "mistral",
			size: 4620 * format.MebiByte,
		},
		{
			name: "orca-mini:7b",
			size: 5118 * format.MebiByte,
		},
		{
			name: "dolphin-mistral",
			size: 4620 * format.MebiByte,
		},
		{
			name: "gemma:7b",
			size: 5000 * format.MebiByte,
		},
		{
			name: "codellama:7b",
			size: 5118 * format.MebiByte,
		},
	}
	var chosenModels []model
	switch {
	case maxVram < 10000*format.MebiByte:
		slog.Info("selecting small models")
		chosenModels = smallModels
	default:
		slog.Info("selecting medium models")
		chosenModels = mediumModels
	}
	req, resp := GenerateRequests()
	for i := range req {
		if i > len(chosenModels) {
			break
		}
		req[i].Model = chosenModels[i].name
	}
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Minute) 
	defer cancel()
	client, _, cleanup := InitServerConnection(ctx, t)
	defer cleanup()
	for _, r := range req {
		require.NoError(t, PullIfMissing(ctx, client, r.Model))
	}
	var wg sync.WaitGroup
	consumed := uint64(256 * format.MebiByte) 
	for i := 0; i < len(req); i++ {
		if i > 1 && consumed > maxVram {
			slog.Info("achieved target vram exhaustion", "count", i, "vram", format.HumanBytes2(maxVram), "models", format.HumanBytes2(consumed))
			break
		}
		consumed += chosenModels[i].size
		slog.Info("target vram", "count", i, "vram", format.HumanBytes2(maxVram), "models", format.HumanBytes2(consumed))
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			for j := 0; j < 3; j++ {
				slog.Info("Starting", "req", i, "iter", j, "model", req[i].Model)
				DoGenerate(ctx, t, client, req[i], resp[i], 120*time.Second, 5*time.Second)
			}
		}(i)
	}
	go func() {
		for {
			time.Sleep(2 * time.Second)
			select {
			case <-ctx.Done():
				return
			default:
				models, err := client.ListRunning(ctx)
				if err != nil {
					slog.Warn("failed to list running models", "error", err)
					continue
				}
				for _, m := range models.Models {
					slog.Info("loaded model snapshot", "model", m)
				}
			}
		}
	}()
	wg.Wait()
}