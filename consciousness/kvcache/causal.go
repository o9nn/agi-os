package kvcache
import (
	"errors"
	"fmt"
	"log/slog"
	"math"
	"slices"
	"github.com/EchoCog/echollama/ml"
	"github.com/EchoCog/echollama/model/input"
)
type shiftFn func(ctx ml.Context, layer int, key, shift ml.Tensor) (ml.Tensor, error)
type Causal struct {
	DType ml.DType
	swaWindowSize int32
	swaMemorySize int32
	chunkSize int32
	opts CausalOptions
	maxBatch int
	config *ml.CacheConfig
	curReserve bool
	curLayer int
	curLoc int
	curBatchSize int
	curMask ml.Tensor
	curCellRange cellRange
	curSequences []int
	curPositions []int32
	cells []cacheCell
	cellRanges map[int]cellRange
	shiftFn      shiftFn
	backend      ml.Backend
	ctxs         map[int]ml.Context
	keys, values map[int]ml.Tensor
}
type cacheCell struct {
	pos       int32
	sequences []int
}
type cellRange struct {
	min int
	max int
}
func NewCausalCache(shift shiftFn) *Causal {
	return &Causal{
		shiftFn: shift,
		ctxs:    make(map[int]ml.Context),
		keys:    make(map[int]ml.Tensor),
		values:  make(map[int]ml.Tensor),
	}
}
func NewSWACache(windowSize int32, shift shiftFn) *Causal {
	return &Causal{
		swaWindowSize: windowSize,
		shiftFn:       shift,
		ctxs:          make(map[int]ml.Context),
		keys:          make(map[int]ml.Tensor),
		values:        make(map[int]ml.Tensor),
	}
}
func NewSWAMemCache(windowSize int32, memorySize int32, shift shiftFn) *Causal {
	return &Causal{
		swaWindowSize: windowSize,
		swaMemorySize: memorySize,
		shiftFn:       shift,
		ctxs:          make(map[int]ml.Context),
		keys:          make(map[int]ml.Tensor),
		values:        make(map[int]ml.Tensor),
	}
}
func NewChunkedAttentionCache(chunkSize int32, shift shiftFn) *Causal {
	return &Causal{
		chunkSize: chunkSize,
		shiftFn:   shift,
		ctxs:      make(map[int]ml.Context),
		keys:      make(map[int]ml.Tensor),
		values:    make(map[int]ml.Tensor),
	}
}
func (c *Causal) Init(backend ml.Backend, dtype ml.DType, maxSequences, capacity, maxBatch int) {
	if c.config == nil {
		var config ml.CacheConfig
		if cc, ok := backend.(ml.BackendCacheConfig); ok {
			config = cc.CacheConfig()
		}
		c.config = &config
	}
	if c.config.CachePadding == 0 {
		c.config.CachePadding = 1
	}
	if c.config.MaskBatchPadding == 0 {
		c.config.MaskBatchPadding = 1
	}
	if c.config.MaskDType == ml.DTypeOther {
		c.config.MaskDType = ml.DTypeF32
	}
	if c.swaWindowSize == 0 {
		c.swaWindowSize = math.MaxInt32
	}
	if c.swaMemorySize == 0 {
		c.swaMemorySize = c.swaWindowSize
	}
	if int(c.swaMemorySize) > capacity {
		c.swaMemorySize = math.MaxInt32
	}
	if c.swaMemorySize < c.swaWindowSize {
		panic(fmt.Errorf("sliding window memory (%v) must be at least as large as the window (%v)", c.swaMemorySize, c.swaWindowSize))
	}
	var cacheSize int
	if c.swaMemorySize == math.MaxInt32 {
		cacheSize = maxSequences * capacity
	} else {
		cacheSize = (maxSequences * int(c.swaMemorySize)) + maxBatch
	}
	cacheSize = roundUp(cacheSize, c.config.CachePadding)
	c.cells = make([]cacheCell, cacheSize)
	c.DType = dtype
	c.cellRanges = make(map[int]cellRange)
	c.backend = backend
	c.maxBatch = maxBatch
}
func (c *Causal) SetConfig(config ml.CacheConfig) {
	if c.config != nil {
		panic("config cannot be changed after being previously set, either by the model or backend")
	}
	c.config = &config
}
func (c *Causal) Close() {
	for _, ctx := range c.ctxs {
		ctx.Close()
	}
}
func (c *Causal) StartForward(ctx ml.Context, batch input.Batch, reserve bool) error {
	c.curReserve = reserve
	c.curBatchSize = len(batch.Positions)
	c.curSequences = batch.Sequences
	c.curPositions = batch.Positions
	c.opts.Except = nil
	if !c.curReserve {
		c.updateSlidingWindow()
		var err error
		c.curLoc, err = c.findStartLoc()
		if errors.Is(err, ErrKvCacheFull) {
			c.defrag()
			c.curLoc, err = c.findStartLoc()
		}
		if err != nil {
			slog.Warn("unable to find a kv cache slot", "cache", c)
			return err
		}
		for i, pos := range batch.Positions {
			seq := batch.Sequences[i]
			c.cells[c.curLoc+i] = cacheCell{pos: pos, sequences: []int{seq}}
			seqRange, ok := c.cellRanges[seq]
			if !ok {
				seqRange = newRange()
			}
			seqRange.min = min(seqRange.min, c.curLoc+i)
			c.curCellRange.min = min(c.curCellRange.min, c.curLoc+i)
			seqRange.max = max(seqRange.max, c.curLoc+i)
			c.curCellRange.max = max(c.curCellRange.max, c.curLoc+i)
			c.cellRanges[seq] = seqRange
		}
	} else {
		c.curLoc = 0
		c.curCellRange.min = 0
		c.curCellRange.max = len(c.cells) - 1
	}
	c.curMask = c.buildMask(ctx)
	return nil
}
func newRange() cellRange {
	return cellRange{
		min: math.MaxInt,
		max: 0,
	}
}
func (c *Causal) findStartLoc() (int, error) {
	var start, count int
	for i := range c.cells {
		if len(c.cells[i].sequences) == 0 {
			count++
			if count >= c.curBatchSize {
				return start, nil
			}
		} else {
			start = i + 1
			count = 0
		}
	}
	return 0, fmt.Errorf("%w (cache: %v batch: %v)", ErrKvCacheFull, len(c.cells), c.curBatchSize)
}
func (c *Causal) updateSlidingWindow() {
	c.curCellRange = newRange()
	if c.swaMemorySize == math.MaxInt32 {
		for _, seq := range c.curSequences {
			if seqRange, ok := c.cellRanges[seq]; ok {
				c.curCellRange.min = min(c.curCellRange.min, seqRange.min)
				c.curCellRange.max = max(c.curCellRange.max, seqRange.max)
			}
		}
		return
	}
	lowestPos := make(map[int]int32)
	for i := range c.curPositions {
		seq := c.curSequences[i]
		pos, ok := lowestPos[seq]
		if !ok {
			pos = c.curPositions[i]
		} else if c.curPositions[i] < pos {
			pos = c.curPositions[i]
		}
		lowestPos[seq] = pos
	}
	for seq, pos := range lowestPos {
		oldRange, ok := c.cellRanges[seq]
		if !ok {
			continue
		}
		newRange := newRange()
		for i := oldRange.min; i <= oldRange.max; i++ {
			if slices.Contains(c.cells[i].sequences, seq) {
				if c.cells[i].pos < pos-c.swaMemorySize {
					c.cells[i].sequences = slices.DeleteFunc(c.cells[i].sequences, func(s int) bool { return s == seq })
				} else {
					newRange.min = min(newRange.min, i)
					newRange.max = max(newRange.max, i)
				}
				if c.cells[i].pos >= pos-c.swaWindowSize {
					c.curCellRange.min = min(c.curCellRange.min, i)
					c.curCellRange.max = max(c.curCellRange.max, i)
				}
			}
		}
		c.cellRanges[seq] = newRange
	}
}
func roundDown(length, pad int) int {
	return (length / pad) * pad
}
func roundUp(length, pad int) int {
	return ((length + pad - 1) / pad) * pad
}
func (c *Causal) buildMask(ctx ml.Context) ml.Tensor {
	batchSize := roundUp(c.curBatchSize, c.config.MaskBatchPadding)
	c.curCellRange.min = roundDown(c.curCellRange.min, c.config.CachePadding)
	c.curCellRange.max = roundUp(c.curCellRange.max+1, c.config.CachePadding) - 1
	length := c.curCellRange.max - c.curCellRange.min + 1
	if c.curReserve {
		return ctx.Input().Empty(c.config.MaskDType, length, batchSize)
	}
	mask := make([]float32, batchSize*length)
	for i := range c.curBatchSize {
		enabled := !slices.Contains(c.opts.Except, i)
		for j := c.curCellRange.min; j <= c.curCellRange.max; j++ {
			if !slices.Contains(c.cells[j].sequences, c.curSequences[i]) ||
				(enabled && c.cells[j].pos > c.curPositions[i]) ||
				c.chunkSize > 0 && c.cells[j].pos < c.curPositions[i]-c.curPositions[i]%c.chunkSize ||
				c.cells[j].pos < c.curPositions[i]-c.swaWindowSize {
				mask[i*length+(j-c.curCellRange.min)] = float32(math.Inf(-1))
			}
		}
	}
	for i := c.curBatchSize * length; i < len(mask); i++ {
		mask[i] = float32(math.Inf(-1))
	}
	maskTensor := ctx.Input().FromFloatSlice(mask, length, batchSize)
	if c.config.MaskDType != ml.DTypeF32 {
		out := ctx.Input().Empty(c.config.MaskDType, maskTensor.Shape()...)
		ctx.Forward(maskTensor.Copy(ctx, out))
		maskTensor = out
	}
	return maskTensor
}
func (c *Causal) moveCells(ctx ml.Context, src, dst, length int) {
	for i, key := range c.keys {
		if key == nil {
			continue
		}
		kHeadDim := key.Dim(0)
		numKVHeads := key.Dim(1)
		rowSize := key.Stride(2)
		kSrcView := key.View(ctx, rowSize*src, kHeadDim*numKVHeads*length)
		kDstView := key.View(ctx, rowSize*dst, kHeadDim*numKVHeads*length)
		value := c.values[i]
		var vSrcView, vDstView ml.Tensor
		if c.config.PermutedV {
			vHeadDim := value.Dim(1)
			elemSize := value.Stride(0)
			vSrcView = value.View(ctx, elemSize*src, length, len(c.cells)*elemSize, vHeadDim*numKVHeads)
			vDstView = value.View(ctx, elemSize*dst, length, len(c.cells)*elemSize, vHeadDim*numKVHeads)
		} else {
			vHeadDim := value.Dim(0)
			rowSize := value.Stride(2)
			vSrcView = value.View(ctx, rowSize*src, vHeadDim*numKVHeads*length)
			vDstView = value.View(ctx, rowSize*dst, vHeadDim*numKVHeads*length)
		}
		ctx.Forward(
			kSrcView.Copy(ctx, kDstView),
			vSrcView.Copy(ctx, vDstView),
		)
	}
}
func (c *Causal) defrag() {
	slog.Debug("defragmenting kv cache")
	ctx := c.backend.NewContext()
	layers := 0
	for _, key := range c.keys {
		if key == nil {
			continue
		}
		layers++
	}
	maxMoves := (ctx.MaxGraphNodes() - 2*layers) / (6 * layers)
	moves := 0
	var pendingSrc, pendingDst, pendingLen int
	src := len(c.cells) - 1
	for dst := 0; dst < src; dst++ {
		if len(c.cells[dst].sequences) == 0 {
			for ; src > dst; src-- {
				if len(c.cells[src].sequences) != 0 {
					c.cells[dst] = c.cells[src]
					c.cells[src] = cacheCell{}
					if pendingLen > 0 {
						if src == pendingSrc-pendingLen && dst == pendingDst+pendingLen {
							pendingSrc = src
							pendingLen++
							break
						} else {
							c.moveCells(ctx, pendingSrc, pendingDst, pendingLen)
							moves++
						}
					}
					pendingSrc = src
					pendingDst = dst
					pendingLen = 1
					break
				}
			}
		}
		if moves >= maxMoves {
			ctx.Compute()
			ctx.Close()
			ctx = c.backend.NewContext()
			moves = 0
		}
	}
	if pendingLen > 0 {
		c.moveCells(ctx, pendingSrc, pendingDst, pendingLen)
		moves++
	}
	if moves > 0 {
		ctx.Compute()
	}
	ctx.Close()
	for seq := range c.cellRanges {
		seqRange := newRange()
		for i, cell := range c.cells {
			if slices.Contains(cell.sequences, seq) {
				if i < seqRange.min {
					seqRange.min = i
				}
				if i > seqRange.max {
					seqRange.max = i
				}
			}
		}
		c.cellRanges[seq] = seqRange
	}
	c.updateSlidingWindow()
}
func (c *Causal) SetLayer(layer int) {
	c.curLayer = layer
}
type CausalOptions struct {
	Except []int
}
func (c *Causal) SetCausal(ctx ml.Context, opts CausalOptions) {
	if !slices.Equal(c.opts.Except, opts.Except) {
		c.opts = opts
		if ctx != nil {
			c.curMask = c.buildMask(ctx)
		}
	}
}
func (c *Causal) Get(ctx ml.Context) (ml.Tensor, ml.Tensor, ml.Tensor) {
	key := c.keys[c.curLayer]
	value := c.values[c.curLayer]
	kHeadDim := key.Dim(0)
	numKVHeads := key.Dim(1)
	rowSize := key.Stride(2)
	cachedSize := c.curMask.Dim(0)
	key = key.View(ctx, rowSize*c.curCellRange.min,
		kHeadDim, key.Stride(1),
		numKVHeads, key.Stride(2),
		cachedSize,
	)
	if c.config.PermutedV {
		vHeadDim := value.Dim(1)
		elemSize := value.Stride(0)
		value = value.View(ctx, elemSize*c.curCellRange.min,
			cachedSize, value.Stride(1),
			vHeadDim, value.Stride(2),
			numKVHeads,
		)
	} else {
		vHeadDim := value.Dim(0)
		rowSize := value.Stride(2)
		value = value.View(ctx, rowSize*c.curCellRange.min,
			vHeadDim, value.Stride(1),
			numKVHeads, value.Stride(2),
			cachedSize,
		)
	}
	return key, value, c.curMask
}
func (c *Causal) Put(ctx ml.Context, key, value ml.Tensor) {
	kHeadDim := key.Dim(0)
	vHeadDim := value.Dim(0)
	numKVHeads := key.Dim(1)
	batchSize := key.Dim(2)
	if c.curBatchSize != batchSize {
		panic(fmt.Errorf("inconsistent batch sizes (layer: %v, batch size: %v layer batch size: %v)", c.curLayer, c.curBatchSize, batchSize))
	}
	if _, ok := c.ctxs[c.curLayer]; !ok {
		c.ctxs[c.curLayer] = c.backend.NewContextSize(2).Layer(c.curLayer)
	}
	if _, ok := c.keys[c.curLayer]; !ok {
		c.keys[c.curLayer] = c.ctxs[c.curLayer].Zeros(c.DType, kHeadDim, numKVHeads, len(c.cells))
	}
	if _, ok := c.values[c.curLayer]; !ok {
		if c.config.PermutedV {
			c.values[c.curLayer] = c.ctxs[c.curLayer].Zeros(c.DType, len(c.cells), vHeadDim, numKVHeads)
		} else {
			c.values[c.curLayer] = c.ctxs[c.curLayer].Zeros(c.DType, vHeadDim, numKVHeads, len(c.cells))
		}
	}
	rowSize := c.keys[c.curLayer].Stride(2)
	ctx.Forward(key.Copy(ctx, c.keys[c.curLayer].View(ctx, rowSize*c.curLoc, kHeadDim*numKVHeads*batchSize)))
	if c.config.PermutedV {
		elemSize := c.values[c.curLayer].Stride(0)
		value = value.Permute(ctx, 1, 2, 0, 3)
		ctx.Forward(value.Copy(ctx, c.values[c.curLayer].View(ctx, elemSize*c.curLoc, batchSize, len(c.cells)*elemSize, vHeadDim*numKVHeads)))
	} else {
		rowSize := c.values[c.curLayer].Stride(2)
		ctx.Forward(value.Copy(ctx, c.values[c.curLayer].View(ctx, rowSize*c.curLoc, vHeadDim*numKVHeads*batchSize)))
	}
}
func (c *Causal) CopyPrefix(srcSeq, dstSeq int, len int32) {
	seqRange := newRange()
	for i := range c.cells {
		if slices.Contains(c.cells[i].sequences, dstSeq) {
			c.cells[i].sequences = slices.DeleteFunc(c.cells[i].sequences, func(s int) bool { return s == dstSeq })
		}
		if slices.Contains(c.cells[i].sequences, srcSeq) && c.cells[i].pos < len {
			c.cells[i].sequences = append(c.cells[i].sequences, dstSeq)
			if i < seqRange.min {
				seqRange.min = i
			}
			if i > seqRange.max {
				seqRange.max = i
			}
		}
	}
	c.cellRanges[dstSeq] = seqRange
}
func (c *Causal) CanResume(seq int, pos int32) bool {
	if c.swaMemorySize == math.MaxInt32 {
		return true
	}
	seqRange, ok := c.cellRanges[seq]
	if !ok {
		return false
	}
	var last int32 = -1
	for i := seqRange.min; i <= seqRange.max; i++ {
		if slices.Contains(c.cells[i].sequences, seq) {
			last = max(last, c.cells[i].pos)
		}
	}
	if last == -1 {
		return false
	}
	lastWindowStart := max(0, last-c.swaMemorySize)
	posWindowStart := max(0, pos-c.swaWindowSize)
	return posWindowStart >= lastWindowStart
}
func (c *Causal) shift(seq int, beginIndex, offset int32) error {
	if c.shiftFn == nil {
		return ErrNotSupported
	}
	seqRange := c.cellRanges[seq]
	for start := seqRange.min; start <= seqRange.max; start += c.maxBatch {
		size := min(seqRange.max-start+1, c.maxBatch)
		offsets := make([]int32, size)
		var batchFirst, batchLast int
		batchFirst = -1
		for i := range offsets {
			cell := c.cells[start+i]
			if slices.Contains(cell.sequences, seq) && cell.pos >= beginIndex {
				offsets[i] = offset
				if batchFirst < 0 {
					batchFirst = i
				}
				batchLast = i
			}
		}
		if batchFirst < 0 {
			continue
		}
		offsets = offsets[batchFirst : batchLast+1]
		ctx := c.backend.NewContext()
		kShift := ctx.Input().FromIntSlice(offsets, len(offsets))
		for i, key := range c.keys {
			if key == nil {
				continue
			}
			kHeadDim := key.Dim(0)
			numKVHeads := key.Dim(1)
			rowSize := key.Stride(2)
			key = key.View(ctx, rowSize*(start+batchFirst),
				kHeadDim, key.Stride(1),
				numKVHeads, key.Stride(2),
				len(offsets),
			)
			roped, err := c.shiftFn(ctx, i, key, kShift)
			if err != nil {
				ctx.Close()
				return err
			}
			ctx.Forward(roped.Copy(ctx, key))
		}
		ctx.Compute()
		ctx.Close()
	}
	return nil
}
func (c *Causal) Remove(seq int, beginIndex, endIndex int32) error {
	var offset int32
	if endIndex != math.MaxInt32 {
		offset = beginIndex - endIndex
	}
	seqRange := newRange()
	for i := range c.cells {
		if slices.Contains(c.cells[i].sequences, seq) {
			if c.cells[i].pos >= beginIndex && c.cells[i].pos < endIndex {
				c.cells[i].sequences = slices.DeleteFunc(c.cells[i].sequences, func(s int) bool { return s == seq })
			} else {
				if c.cells[i].pos >= endIndex {
					if slices.ContainsFunc(c.cells[i].sequences, func(s int) bool { return s != seq }) {
						return errors.New("shifting cells shared by multiple sequences not supported")
					}
					c.cells[i].pos += offset
				}
				if i < seqRange.min {
					seqRange.min = i
				}
				if i > seqRange.max {
					seqRange.max = i
				}
			}
		}
	}
	if seqRange == newRange() {
		delete(c.cellRanges, seq)
		return nil
	}
	c.cellRanges[seq] = seqRange
	if endIndex != math.MaxInt32 {
		err := c.shift(seq, endIndex+offset, offset)
		if err != nil {
			return err
		}
	}
	return nil
}