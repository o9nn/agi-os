package main
import (
	"context"
	"fmt"
	"log"
	"os"
	"github.com/EchoCog/echollama/api"
)
func main() {
	if len(os.Args) <= 1 {
		log.Fatal("usage: <image name>")
	}
	imgData, err := os.ReadFile(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}
	client, err := api.ClientFromEnvironment()
	if err != nil {
		log.Fatal(err)
	}
	req := &api.GenerateRequest{
		Model:  "llava",
		Prompt: "describe this image",
		Images: []api.ImageData{imgData},
	}
	ctx := context.Background()
	respFunc := func(resp api.GenerateResponse) error {
		fmt.Print(resp.Response)
		return nil
	}
	err = client.Generate(ctx, req, respFunc)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println()
}