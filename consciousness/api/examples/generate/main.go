package main
import (
"context"
"fmt"
"log"
"github.com/EchoCog/echollama/api"
)
func main() {
client, err := api.ClientFromEnvironment()
if err != nil {
log.Fatal(err)
}
req := &api.GenerateRequest{
Model:  "gemma2",
Prompt: "how many planets are there?",
Stream: new(bool),
}
ctx := context.Background()
respFunc := func(resp api.GenerateResponse) error {
fmt.Println(resp.Response)
return nil
}
err = client.Generate(ctx, req, respFunc)
if err != nil {
log.Fatal(err)
}
}