package input
import "github.com/EchoCog/echollama/ml"
type Multimodal struct {
Tensor ml.Tensor
Data any
}
type Input struct {
Token int32
Multimodal []Multimodal
MultimodalHash uint64
SameBatch int
}
type MultimodalIndex struct {
Index      int
Multimodal []Multimodal
}
type Batch struct {
Inputs ml.Tensor
Multimodal []MultimodalIndex
Positions []int32
Sequences []int
Outputs []int32
}