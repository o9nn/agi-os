package sample
import (
	"container/heap"
	"math"
	"slices"
)
type tokenHeap []token
func (h tokenHeap) Len() int           { return len(h) }
func (h tokenHeap) Less(i, j int) bool { return h[i].value < h[j].value }
func (h tokenHeap) Swap(i, j int)      { h[i], h[j] = h[j], h[i] }
func (h *tokenHeap) Push(x any) {
	*h = append(*h, x.(token))
}
func (h *tokenHeap) Pop() any {
	old := *h
	n := len(old)
	x := old[n-1]
	*h = old[0 : n-1]
	return x
}
func temperature(ts []token, temp float32) {
	temp = max(temp, 1e-7)
	for i := range ts {
		ts[i].value = ts[i].value / temp
	}
}
func softmax(ts []token) {
	maxLogit := float32(math.Inf(-1))
	for _, t := range ts {
		if t.value > maxLogit {
			maxLogit = t.value
		}
	}
	var sum float32
	for i, v := range ts {
		ts[i].value = float32(math.Exp(float64(v.value - maxLogit)))
		sum += ts[i].value
	}
	for i := range ts {
		ts[i].value /= sum
	}
}
func topK(ts []token, k int) []token {
	if k >= len(ts) || k <= 0 {
		slices.SortFunc(ts, func(a, b token) int {
			switch {
			case a.value < b.value:
				return 1
			case a.value > b.value:
				return -1
			default:
				return 0
			}
		})
		return ts
	}
	h := make(tokenHeap, k)
	copy(h, ts[:k])
	heap.Init(&h)
	for i := k; i < len(ts); i++ {
		if ts[i].value > h[0].value {
			heap.Pop(&h)
			heap.Push(&h, ts[i])
		}
	}
	result := make([]token, len(h))
	for i := k - 1; i >= 0; i-- {
		result[i] = heap.Pop(&h).(token)
	}
	return result
}
func topP(ts []token, p float32) []token {
	if p == 1.0 {
		return ts
	}
	var sum float32
	for i, t := range ts {
		sum += t.value
		if sum > float32(p) {
			return ts[:i+1]
		}
	}
	return ts
}
func minP(ts []token, p float32) []token {
	maxProb := ts[0].value
	threshold := maxProb * p
	for i, t := range ts {
		if t.value < threshold {
			return ts[:i]
		}
	}
	return ts
}