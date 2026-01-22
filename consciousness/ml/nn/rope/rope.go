package rope
import "github.com/EchoCog/echollama/ml"
type Options struct {
Type                  int
Factors               ml.Tensor
OriginalContextLength int
ExtrapolationFactor,
AttentionFactor,
BetaFast,
BetaSlow float32
}
func WithOriginalContextLength(n int) func(*Options) {
return func(opts *Options) {
opts.OriginalContextLength = n
}
}
func WithTypeNeoX() func(*Options) {
return func(opts *Options) {
opts.Type = 2
}
}
func WithFactors(factors ml.Tensor) func(*Options) {
return func(opts *Options) {
if factors != nil {
opts.Factors = factors
}
}
}
func WithExtrapolationFactor(extrapolationFactor float32) func(*Options) {
return func(opts *Options) {
opts.ExtrapolationFactor = extrapolationFactor
}
}
func WithAttentionFactor(attentionFactor float32) func(*Options) {
return func(opts *Options) {
opts.AttentionFactor = attentionFactor
}
}