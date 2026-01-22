package llm
import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"
	"github.com/EchoCog/echollama/api"
	"golang.org/x/sync/semaphore"
)
func TestLLMServerCompletionFormat(t *testing.T) {
	ctx, cancel := context.WithCancel(t.Context())
	s := &llmServer{
		sem: semaphore.NewWeighted(1), 
	}
	checkInvalid := func(format string) {
		t.Helper()
		err := s.Completion(ctx, CompletionRequest{
			Options: new(api.Options),
			Format:  []byte(format),
		}, nil)
		want := fmt.Sprintf("invalid format: %q; expected \"json\" or a valid JSON Schema", format)
		if err == nil || !strings.Contains(err.Error(), want) {
			t.Fatalf("err = %v; want %q", err, want)
		}
	}
	checkInvalid("X")   
	checkInvalid(`"X"`) 
	cancel() 
	checkValid := func(err error) {
		t.Helper()
		if !errors.Is(err, context.Canceled) {
			t.Fatalf("Completion: err = %v; expected context.Canceled", err)
		}
	}
	valids := []string{
		``,
		`""`,
		`null`,
		`"json"`,
		`{"type":"object"}`,
	}
	for _, valid := range valids {
		err := s.Completion(ctx, CompletionRequest{
			Options: new(api.Options),
			Format:  []byte(valid),
		}, nil)
		checkValid(err)
	}
	err := s.Completion(ctx, CompletionRequest{
		Options: new(api.Options),
		Format:  nil, 
	}, nil)
	checkValid(err)
}