package server
import (
	"fmt"
	"io"
	"log/slog"
	"maps"
	"os"
	"strings"
	"unsafe"
	fsggml "github.com/EchoCog/echollama/fs/ggml"
	"github.com/EchoCog/echollama/ml/backend/ggml"
)
type quantizer struct {
	*os.File
	offset     uint64
	from, to   *fsggml.Tensor
	progressFn func(n uint64)
}
func (q quantizer) WriteTo(w io.Writer) (int64, error) {
	quantize := q.from.Kind != q.to.Kind
	sr := io.NewSectionReader(q, int64(q.offset), int64(q.from.Size()))
	if !quantize {
		n, err := io.Copy(w, sr)
		q.progressFn(q.from.Size())
		return n, err
	}
	data, err := io.ReadAll(sr)
	if err != nil {
		slog.Warn("file read error", "tensor", q.from.Name, "file", q.Name(), "error", err)
		return 0, fmt.Errorf("unable to read tensor %s from %s: %s", q.from.Name, q.Name(), err)
	}
	var f32s []float32
	newType := fsggml.TensorType(q.to.Kind)
	if fsggml.TensorType(q.from.Kind) == fsggml.TensorTypeF32 {
		f32s = unsafe.Slice((*float32)(unsafe.Pointer(&data[0])), q.from.Elements())
	} else {
		f32s = ggml.ConvertToF32(data, q.from.Kind, q.from.Elements())
	}
	data = ggml.Quantize(newType, f32s, q.from.Shape)
	n, err := w.Write(data)
	q.progressFn(q.from.Size())
	return int64(n), err
}
type quantizeState struct {
	nAttnV    int  
	nFfnDown  int  
	iAttnV    int  
	iFfnDown  int  
	hasOutput bool 
}
func useMoreBits(iLayer, nLayers int) bool {
	return iLayer < (nLayers/8) || iLayer >= 7*nLayers/8 || (iLayer-nLayers/8)%3 == 2
}
func getTensorNewType(kv fsggml.KV, qs *quantizeState, newType fsggml.TensorType, name string, shape []uint64, ftype fsggml.FileType) fsggml.TensorType {
	nExperts := max(1, kv.Uint("expert_count", 0))
	if name == "output.weight" || name == "output_norm.weight" || (!qs.hasOutput && name == "token_embd.weight") {
		nx := shape[0]
		qk_k := newType.BlockSize()
		if nx%qk_k != 0 {
			newType = fsggml.TensorTypeQ8_0
		} else if newType != fsggml.TensorTypeQ8_0 {
			newType = fsggml.TensorTypeQ6_K
		}
	} else if strings.Contains(name, "attn_v.weight") {
		if (ftype == fsggml.FileTypeQ4_K_M) &&
			useMoreBits(qs.iAttnV, qs.nAttnV) {
			newType = fsggml.TensorTypeQ6_K
		} else if ftype == fsggml.FileTypeQ4_K_S && qs.iAttnV < 4 {
			newType = fsggml.TensorTypeQ5_K
		}
		if nExperts == 8 {
			newType = fsggml.TensorTypeQ8_0
		}
		qs.iAttnV++
	} else if strings.Contains(name, "attn_k.weight") {
		if nExperts == 8 {
			newType = fsggml.TensorTypeQ8_0
		}
	} else if strings.Contains(name, "ffn_down") {
		iLayer := qs.iFfnDown
		n_layer := qs.nFfnDown
		if ftype == fsggml.FileTypeQ4_K_M {
			if useMoreBits(iLayer, n_layer) {
				newType = fsggml.TensorTypeQ6_K
			}
		} else if ftype == fsggml.FileTypeQ4_K_S && iLayer < n_layer/8 {
			newType = fsggml.TensorTypeQ5_K
		}
		qs.iFfnDown++
	} else if strings.Contains(name, "attn_output.weight") {
		if nExperts == 8 {
			if ftype == fsggml.FileTypeQ4_K_S || ftype == fsggml.FileTypeQ4_K_M {
				newType = fsggml.TensorTypeQ5_K
			}
		}
	} else if strings.Contains(name, "attn_qkv.weight") {
		if ftype == fsggml.FileTypeQ4_K_M {
			newType = fsggml.TensorTypeQ5_K
		}
	}
	if newType.IsQuantized() {
		nx := shape[0]
		qk_k := newType.BlockSize()
		if nx%qk_k != 0 {
			originalType := newType
			switch newType {
			case fsggml.TensorTypeQ4_K:
				newType = fsggml.TensorTypeQ5_0
			case fsggml.TensorTypeQ5_K:
				newType = fsggml.TensorTypeQ5_1
			case fsggml.TensorTypeQ6_K:
				newType = fsggml.TensorTypeQ8_0
			}
			if nx%newType.BlockSize() != 0 {
				newType = fsggml.TensorTypeF16
			}
			slog.Warn(fmt.Sprintf("tensor cols %d are not divisible by %d, required for %s - using fallback quantization %s",
				nx, qk_k, originalType.String(), newType.String()))
		}
	}
	return newType
}
func quantize(in, out *os.File, orig *fsggml.GGML, newFileType fsggml.FileType, progressFn func(n uint64)) error {
	kv := maps.Clone(orig.KV())
	kv["general.file_type"] = newFileType
	qs := &quantizeState{}
	layerCount := 0
	for k, l := range orig.Tensors().GroupLayers() {
		if strings.HasPrefix(k, "blk.") {
			layerCount++
		}
		for _, tensor := range l {
			if strings.Contains(tensor.Name, "attn_v.weight") ||
				strings.Contains(tensor.Name, "attn_qkv.weight") ||
				strings.Contains(tensor.Name, "attn_kv_b.weight") {
				qs.nAttnV++
			} else if tensor.Name == "output.weight" {
				qs.hasOutput = true
			}
		}
	}
	qs.nFfnDown = layerCount
	origTensors := orig.Tensors().Items()
	outputTensors := make([]*fsggml.Tensor, len(origTensors))
	for i, tensor := range origTensors {
		tensor := tensor
		newType := newType(tensor, kv, qs, newFileType)
		newTensor := &fsggml.Tensor{
			Name:  tensor.Name,
			Shape: tensor.Shape,
			Kind:  uint32(newType),
		}
		outputTensors[i] = newTensor
		outputTensors[i].WriterTo = quantizer{
			File:       in,
			offset:     orig.Tensors().Offset + tensor.Offset,
			from:       tensor,
			to:         newTensor,
			progressFn: progressFn,
		}
	}
	return fsggml.WriteGGUF(out, kv, outputTensors)
}
func newType(t *fsggml.Tensor, kv fsggml.KV, qs *quantizeState, ftype fsggml.FileType) fsggml.TensorType {
	defaultType := ftype.ToTensorType()
	name := t.Name
	quantize := strings.HasSuffix(name, "weight")
	quantize = quantize && (!strings.Contains(name, "v.") || strings.Contains(name, "_v."))
	quantize = quantize && !strings.Contains(name, "mm.")
	quantize = quantize && (len(t.Shape) >= 2)
	quantize = quantize && !strings.Contains(name, "_norm.weight")
	quantize = quantize && !strings.Contains(name, "ffn_gate_inp.weight")
	quantize = quantize && (name != "position_embd.weight")
	quantize = quantize && (name != "token_types.weight")
	quantize = quantize && !strings.Contains(name, "ssm_conv1d.weight")
	quantize = quantize && !strings.Contains(name, "time_mix_first.weight")
	quantize = quantize && !strings.Contains(name, "time_mix_w1.weight")
	quantize = quantize && !strings.Contains(name, "time_mix_w2.weight")
	quantize = quantize && !strings.Contains(name, "time_mix_decay_w1.weight")
	quantize = quantize && !strings.Contains(name, "time_mix_decay_w2.weight")
	quantize = quantize && !strings.Contains(name, "time_mix_lerp_fused.weight")
	quantize = quantize && !strings.Contains(name, "attn_rel_b.weight")
	quantize = quantize && !strings.Contains(name, "per_layer_token_embd.weight")
	newType := fsggml.TensorType(t.Kind)
	if quantize {
		newType = getTensorNewType(kv, qs, defaultType, t.Name, t.Shape, ftype)
		if newType != defaultType {
			slog.Debug("tensor quantization adjusted for better quality", "name", t.Name, "requested", defaultType, "quantization", newType)
		}
	}
	return newType
}