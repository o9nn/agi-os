package ggml
import "C"
import (
	_ "github.com/EchoCog/echollama/ml/backend/ggml/ggml/src/ggml-blas"
	_ "github.com/EchoCog/echollama/ml/backend/ggml/ggml/src/ggml-metal"
)