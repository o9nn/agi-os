package server
import (
	"context"
	"errors"
	"fmt"
	"log/slog"
	"os"
	"reflect"
	"runtime"
	"slices"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"
	"github.com/EchoCog/echollama/api"
	"github.com/EchoCog/echollama/discover"
	"github.com/EchoCog/echollama/envconfig"
	"github.com/EchoCog/echollama/format"
	"github.com/EchoCog/echollama/fs/ggml"
	"github.com/EchoCog/echollama/llm"
	"github.com/EchoCog/echollama/types/model"
)
type LlmRequest struct {
	ctx             context.Context 
	model           *Model
	opts            api.Options
	origNumCtx      int 
	sessionDuration *api.Duration
	successCh       chan *runnerRef
	errCh           chan error
	schedAttempts   uint
}
type Scheduler struct {
	pendingReqCh  chan *LlmRequest
	finishedReqCh chan *LlmRequest
	expiredCh     chan *runnerRef
	unloadedCh    chan any
	loaded   map[string]*runnerRef
	loadedMu sync.Mutex
	loadFn       func(req *LlmRequest, f *ggml.GGML, gpus discover.GpuInfoList, numParallel int)
	newServerFn  func(gpus discover.GpuInfoList, model string, f *ggml.GGML, adapters []string, projectors []string, opts api.Options, numParallel int) (llm.LlamaServer, error)
	getGpuFn     func() discover.GpuInfoList
	getCpuFn     func() discover.GpuInfoList
	reschedDelay time.Duration
}
var defaultModelsPerGPU = 3
var defaultParallel = 1
var ErrMaxQueue = errors.New("server busy, please try again.  maximum pending requests exceeded")
func InitScheduler(ctx context.Context) *Scheduler {
	maxQueue := envconfig.MaxQueue()
	sched := &Scheduler{
		pendingReqCh:  make(chan *LlmRequest, maxQueue),
		finishedReqCh: make(chan *LlmRequest, maxQueue),
		expiredCh:     make(chan *runnerRef, maxQueue),
		unloadedCh:    make(chan any, maxQueue),
		loaded:        make(map[string]*runnerRef),
		newServerFn:   llm.NewLlamaServer,
		getGpuFn:      discover.GetGPUInfo,
		getCpuFn:      discover.GetCPUInfo,
		reschedDelay:  250 * time.Millisecond,
	}
	sched.loadFn = sched.load
	return sched
}
func (s *Scheduler) GetRunner(c context.Context, model *Model, opts api.Options, sessionDuration *api.Duration) (chan *runnerRef, chan error) {
	if opts.NumCtx < 4 {
		opts.NumCtx = 4
	}
	req := &LlmRequest{
		ctx:             c,
		model:           model,
		opts:            opts,
		sessionDuration: sessionDuration,
		successCh:       make(chan *runnerRef),
		errCh:           make(chan error, 1),
	}
	select {
	case s.pendingReqCh <- req:
	default:
		req.errCh <- ErrMaxQueue
	}
	return req.successCh, req.errCh
}
func (s *Scheduler) Run(ctx context.Context) {
	slog.Debug("starting llm scheduler")
	go func() {
		s.processPending(ctx)
	}()
	go func() {
		s.processCompleted(ctx)
	}()
}
func (s *Scheduler) processPending(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			slog.Debug("shutting down scheduler pending loop")
			return
		case pending := <-s.pendingReqCh:
			pending.schedAttempts++
			if pending.origNumCtx == 0 {
				pending.origNumCtx = pending.opts.NumCtx
			}
			if pending.ctx.Err() != nil {
				slog.Debug("pending request cancelled or timed out, skipping scheduling")
				continue
			}
			numParallel := int(envconfig.NumParallel())
			if slices.Contains(pending.model.Config.ModelFamilies, "mllama") && numParallel != 1 {
				numParallel = 1
				slog.Warn("mllama does not currently support parallel requests")
			}
			for {
				var runnerToExpire *runnerRef
				s.loadedMu.Lock()
				runner := s.loaded[pending.model.ModelPath]
				loadedCount := len(s.loaded)
				s.loadedMu.Unlock()
				if runner != nil {
					if runner.needsReload(ctx, pending) {
						slog.Debug("reloading", "runner", runner)
						runnerToExpire = runner
					} else {
						pending.useLoadedRunner(runner, s.finishedReqCh)
						break
					}
				} else if envconfig.MaxRunners() > 0 && loadedCount >= int(envconfig.MaxRunners()) {
					slog.Debug("max runners achieved, unloading one to make room", "runner_count", loadedCount)
					runnerToExpire = s.findRunnerToUnload()
				} else {
					var gpus discover.GpuInfoList
					if pending.opts.NumGPU == 0 {
						gpus = s.getCpuFn()
					} else {
						gpus = s.getGpuFn()
					}
					if envconfig.MaxRunners() <= 0 {
						allReliable := true
						for _, gpu := range gpus {
							if gpu.UnreliableFreeMemory {
								allReliable = false
								break
							}
						}
						if allReliable {
							os.Setenv("OLLAMA_MAX_LOADED_MODELS", strconv.Itoa(defaultModelsPerGPU*len(gpus)))
							slog.Debug("updating default concurrency", "OLLAMA_MAX_LOADED_MODELS", envconfig.MaxRunners(), "gpu_count", len(gpus))
						} else {
							os.Setenv("OLLAMA_MAX_LOADED_MODELS", strconv.Itoa(len(gpus)))
							slog.Info("one or more GPUs detected that are unable to accurately report free memory - disabling default concurrency")
						}
					}
					ggml, err := llm.LoadModel(pending.model.ModelPath, 1024)
					if err != nil {
						pending.errCh <- err
						break
					}
					if pending.model.CheckCapabilities(model.CapabilityCompletion) != nil {
						numParallel = 1
					}
					if len(gpus) == 1 && gpus[0].Library == "cpu" {
						if numParallel <= 0 {
							numParallel = defaultParallel
						}
						pending.opts.NumCtx = pending.origNumCtx * numParallel
						if loadedCount == 0 {
							slog.Debug("cpu mode with first model, loading")
							s.loadFn(pending, ggml, gpus, numParallel)
							break
						}
						runnerToExpire = s.maybeFindCPURunnerToUnload(pending, ggml, gpus)
						if runnerToExpire == nil {
							slog.Debug("cpu mode with available system memory or first model, loading")
							s.loadFn(pending, ggml, gpus, numParallel)
							break
						}
					} else if loadedCount == 0 {
						slog.Debug("loading first model", "model", pending.model.ModelPath)
						g := pickBestFullFitByLibrary(pending, ggml, gpus, &numParallel)
						if g != nil {
							gpus = g
						} else {
							gpus = pickBestPartialFitByLibrary(pending, ggml, gpus, &numParallel)
						}
						s.loadFn(pending, ggml, gpus, numParallel)
						break
					}
					if runnerToExpire == nil {
						availGpus := s.filterGPUsWithoutLoadingModels(gpus)
						s.updateFreeSpace(availGpus)
						fitGpus := pickBestFullFitByLibrary(pending, ggml, availGpus, &numParallel)
						if fitGpus != nil {
							slog.Debug("new model fits with existing models, loading")
							s.loadFn(pending, ggml, fitGpus, numParallel)
							break
						}
						if len(availGpus) < len(gpus) {
							go func() {
								slog.Debug("delaying scheduling while other models finish loading", "attempts", pending.schedAttempts, "model", pending.model.ModelPath)
								time.Sleep(s.reschedDelay)
								s.pendingReqCh <- pending
							}()
							break
						}
						runnerToExpire = s.findRunnerToUnload()
					}
				}
				if runnerToExpire == nil {
					slog.Error("runner to expire was nil!")
					continue
				}
				runnerToExpire.refMu.Lock()
				slog.Debug("resetting model to expire immediately to make room", "runner", runnerToExpire, "refCount", runnerToExpire.refCount)
				if runnerToExpire.expireTimer != nil {
					runnerToExpire.expireTimer.Stop()
					runnerToExpire.expireTimer = nil
				}
				runnerToExpire.sessionDuration = 0
				if runnerToExpire.refCount <= 0 {
					s.expiredCh <- runnerToExpire
				}
				runnerToExpire.refMu.Unlock()
				slog.Debug("waiting for pending requests to complete and unload to occur", "runner", runnerToExpire)
				select {
				case <-ctx.Done():
					slog.Debug("shutting down scheduler pending loop")
					return
				case <-s.unloadedCh:
					slog.Debug("unload completed", "runner", runnerToExpire)
					continue
				}
			}
		case <-s.unloadedCh:
			slog.Debug("ignoring unload event with no pending requests")
		}
	}
}
func (s *Scheduler) processCompleted(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			slog.Debug("shutting down scheduler completed loop")
			return
		case finished := <-s.finishedReqCh:
			s.loadedMu.Lock()
			runner := s.loaded[finished.model.ModelPath]
			s.loadedMu.Unlock()
			if runner == nil {
				slog.Error("finished request signal received after model unloaded", "modelPath", finished.model.ModelPath)
				continue
			}
			runner.refMu.Lock()
			runner.refCount--
			if runner.refCount <= 0 {
				if runner.sessionDuration <= 0 {
					slog.Debug("runner with zero duration has gone idle, expiring to unload", "runner", runner)
					if runner.expireTimer != nil {
						runner.expireTimer.Stop()
						runner.expireTimer = nil
					}
					s.expiredCh <- runner
				} else if runner.expireTimer == nil {
					slog.Debug("runner with non-zero duration has gone idle, adding timer", "runner", runner, "duration", runner.sessionDuration)
					runner.expireTimer = time.AfterFunc(runner.sessionDuration, func() {
						slog.Debug("timer expired, expiring to unload", "runner", runner)
						runner.refMu.Lock()
						defer runner.refMu.Unlock()
						if runner.expireTimer != nil {
							runner.expireTimer.Stop()
							runner.expireTimer = nil
						}
						s.expiredCh <- runner
					})
					runner.expiresAt = time.Now().Add(runner.sessionDuration)
				} else {
					slog.Debug("runner with non-zero duration has gone idle, resetting timer", "runner", runner, "duration", runner.sessionDuration)
					runner.expireTimer.Reset(runner.sessionDuration)
					runner.expiresAt = time.Now().Add(runner.sessionDuration)
				}
			}
			slog.Debug("after processing request finished event", "runner", runner, "refCount", runner.refCount)
			runner.refMu.Unlock()
		case runner := <-s.expiredCh:
			slog.Debug("runner expired event received", "runner", runner)
			runner.refMu.Lock()
			if runner.refCount > 0 {
				slog.Debug("expired event with positive ref count, retrying", "runner", runner, "refCount", runner.refCount)
				go func(runner *runnerRef) {
					time.Sleep(10 * time.Millisecond)
					s.expiredCh <- runner
				}(runner)
				runner.refMu.Unlock()
				continue
			}
			s.loadedMu.Lock()
			slog.Debug("got lock to unload expired event", "runner", runner)
			runnerToUnload := s.loaded[runner.modelPath]
			if runnerToUnload == nil {
				s.loadedMu.Unlock()
				runner.refMu.Unlock()
				slog.Debug("duplicate expired event, ignoring", "runner", runner)
			} else if runner.pid != runnerToUnload.pid {
				slog.Debug("orphaned runner shutting down", "orphan", runner, "loaded", runnerToUnload)
				runner.unload()
				s.loadedMu.Unlock()
				runner.refMu.Unlock()
			} else {
				slog.Debug("starting background wait for VRAM recovery", "runner", runner)
				finished := runner.waitForVRAMRecovery()
				runner.unload()
				delete(s.loaded, runner.modelPath)
				s.loadedMu.Unlock()
				slog.Debug("runner terminated and removed from list, blocking for VRAM recovery", "runner", runner)
				<-finished
				runner.refMu.Unlock()
				slog.Debug("sending an unloaded event", "runner", runner)
				s.unloadedCh <- struct{}{}
			}
		}
	}
}
func (pending *LlmRequest) useLoadedRunner(runner *runnerRef, finished chan *LlmRequest) {
	runner.refMu.Lock()
	defer runner.refMu.Unlock()
	runner.refCount++
	if runner.expireTimer != nil {
		runner.expireTimer.Stop()
		runner.expireTimer = nil
	}
	if pending.sessionDuration != nil {
		runner.sessionDuration = pending.sessionDuration.Duration
	}
	pending.successCh <- runner
	go func() {
		<-pending.ctx.Done()
		slog.Debug("context for request finished", "runner", runner)
		finished <- pending
	}()
}
func (s *Scheduler) load(req *LlmRequest, f *ggml.GGML, gpus discover.GpuInfoList, numParallel int) {
	if numParallel < 1 {
		numParallel = 1
	}
	sessionDuration := envconfig.KeepAlive()
	if req.sessionDuration != nil {
		sessionDuration = req.sessionDuration.Duration
	}
	llama, err := s.newServerFn(gpus, req.model.ModelPath, f, req.model.AdapterPaths, req.model.ProjectorPaths, req.opts, numParallel)
	if err != nil {
		if errors.Is(err, ggml.ErrUnsupportedFormat) || strings.Contains(err.Error(), "failed to load model") {
			err = fmt.Errorf("%v: this model may be incompatible with your version of Ollama. If you previously pulled this model, try updating it by running `ollama pull %s`", err, req.model.ShortName)
		}
		slog.Info("NewLlamaServer failed", "model", req.model.ModelPath, "error", err)
		req.errCh <- err
		return
	}
	runner := &runnerRef{
		model:           req.model,
		modelPath:       req.model.ModelPath,
		llama:           llama,
		Options:         &req.opts,
		sessionDuration: sessionDuration,
		gpus:            gpus,
		estimatedVRAM:   llama.EstimatedVRAM(),
		estimatedTotal:  llama.EstimatedTotal(),
		loading:         true,
		pid:             llama.Pid(),
	}
	runner.numParallel = numParallel
	runner.refMu.Lock() 
	s.loadedMu.Lock()
	if oldRunner, ok := s.loaded[req.model.ModelPath]; ok {
		slog.Warn("model was still loaded", "old_runner", oldRunner, "new_runner", runner)
		oldRunner.refMu.Lock()
		oldRunner.unload()
		oldRunner.refMu.Unlock()
	}
	s.loaded[req.model.ModelPath] = runner
	slog.Info("loaded runners", "count", len(s.loaded))
	s.loadedMu.Unlock()
	go func() {
		defer runner.refMu.Unlock()
		if err = llama.WaitUntilRunning(req.ctx); err != nil {
			slog.Error("error loading llama server", "error", err)
			req.errCh <- err
			slog.Debug("triggering expiration for failed load", "runner", runner)
			s.expiredCh <- runner
			return
		}
		slog.Debug("finished setting up", "runner", runner)
		if runner.pid < 0 {
			runner.pid = llama.Pid()
		}
		runner.refCount++
		runner.loading = false
		go func() {
			<-req.ctx.Done()
			slog.Debug("context for request finished")
			s.finishedReqCh <- req
		}()
		req.successCh <- runner
	}()
}
func (s *Scheduler) updateFreeSpace(allGpus discover.GpuInfoList) {
	type predKey struct {
		Library string
		ID      string
	}
	predMap := map[predKey]uint64{} 
	s.loadedMu.Lock()
	runners := make([]*runnerRef, 0, len(s.loaded))
	for _, r := range s.loaded {
		runners = append(runners, r)
	}
	s.loadedMu.Unlock()
	for _, r := range runners {
		r.refMu.Lock()
		if r.llama != nil {
			for _, gpu := range allGpus {
				predMap[predKey{gpu.Library, gpu.ID}] += r.llama.EstimatedVRAMByGPU(gpu.ID)
			}
		} else {
			slog.Warn("unexpected nil runner reference, memory prediction may be incorrect")
		}
		r.refMu.Unlock()
	}
	for i := range allGpus {
		if p, ok := predMap[predKey{allGpus[i].Library, allGpus[i].ID}]; ok {
			slog.Debug("gpu reported", "gpu", allGpus[i].ID, "library", allGpus[i].Library, "available", format.HumanBytes2(allGpus[i].FreeMemory))
			if p > allGpus[i].TotalMemory {
				slog.Warn("predicted usage exceeds VRAM", "gpu", allGpus[i].ID, "totalMemory", allGpus[i].TotalMemory, "predicted", p)
				allGpus[i].FreeMemory = 0
			} else if (allGpus[i].TotalMemory - p) < allGpus[i].FreeMemory { 
				allGpus[i].FreeMemory = allGpus[i].TotalMemory - p
			}
			slog.Info("updated VRAM based on existing loaded models", "gpu", allGpus[i].ID, "library", allGpus[i].Library, "total", format.HumanBytes2(allGpus[i].TotalMemory), "available", format.HumanBytes2(allGpus[i].FreeMemory))
		}
	}
}
func (s *Scheduler) filterGPUsWithoutLoadingModels(allGpus discover.GpuInfoList) discover.GpuInfoList {
	ret := append(discover.GpuInfoList{}, allGpus...)
	s.loadedMu.Lock()
	defer s.loadedMu.Unlock()
	for _, runner := range s.loaded {
		if runner.loading {
			slog.Debug("overlapping loads detected", "gpus", runner.gpus, "model", runner.modelPath)
			for _, busyGPU := range runner.gpus {
				for i := range ret {
					if ret[i].ID == busyGPU.ID {
						ret = append(ret[:i], ret[i+1:]...)
						break
					}
				}
			}
		}
	}
	return ret
}
type runnerRef struct {
	refMu    sync.Mutex
	refCount uint 
	llama          llm.LlamaServer
	pid            int
	loading        bool                 
	gpus           discover.GpuInfoList 
	estimatedVRAM  uint64
	estimatedTotal uint64
	sessionDuration time.Duration
	expireTimer     *time.Timer
	expiresAt       time.Time
	model       *Model
	modelPath   string
	numParallel int
	*api.Options
}
func (runner *runnerRef) unload() {
	if runner.expireTimer != nil {
		runner.expireTimer.Stop()
		runner.expireTimer = nil
	}
	if runner.llama != nil {
		runner.llama.Close()
	}
	runner.model = nil
	runner.llama = nil
	runner.Options = nil
	runner.gpus = nil
}
func (runner *runnerRef) needsReload(ctx context.Context, req *LlmRequest) bool {
	slog.Debug("evaluating already loaded", "model", req.model.ModelPath)
	runner.refMu.Lock()
	defer runner.refMu.Unlock()
	timeout := 10 * time.Second
	if runner.loading {
		timeout = 2 * time.Minute 
	}
	if runner.Options == nil {
		return true
	}
	optsExisting := runner.Options.Runner
	optsNew := req.opts.Runner
	if optsNew.NumGPU < 0 {
		optsExisting.NumGPU = -1
		optsNew.NumGPU = -1
	}
	optsExisting.NumCtx = optsExisting.NumCtx / runner.numParallel
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	if !reflect.DeepEqual(runner.model.AdapterPaths, req.model.AdapterPaths) || 
		!reflect.DeepEqual(runner.model.ProjectorPaths, req.model.ProjectorPaths) || 
		!reflect.DeepEqual(optsExisting, optsNew) || 
		runner.llama.Ping(ctx) != nil {
		return true
	}
	return false
}
func (runner *runnerRef) waitForVRAMRecovery() chan any {
	finished := make(chan any, 1)
	if len(runner.gpus) == 0 ||
		(len(runner.gpus) == 1 && (runner.gpus[0].Library == "cpu" || runner.gpus[0].Library == "metal")) ||
		(runtime.GOOS == "windows" && runner.gpus[0].Library != "cuda") {
		finished <- struct{}{}
		slog.Debug("no need to wait for VRAM recovery", "runner", runner)
		return finished
	}
	start := time.Now()
	gpusBefore := discover.GetGPUInfo()
	var totalMemoryBefore, freeMemoryBefore uint64
	for _, gpu := range gpusBefore {
		totalMemoryBefore += gpu.TotalMemory
		freeMemoryBefore += gpu.FreeMemory
	}
	go func() {
		expiresAt := start.Add(5 * time.Second) 
		ticker := time.NewTicker(250 * time.Millisecond)
		defer ticker.Stop()
		for {
			<-ticker.C
			if time.Now().After(expiresAt) {
				slog.Warn("gpu VRAM usage didn't recover within timeout", "seconds", time.Since(start).Seconds(), "runner", runner)
				finished <- struct{}{}
			}
			gpusNow := discover.GetGPUInfo()
			var totalMemoryNow, freeMemoryNow uint64
			for _, gpu := range gpusNow {
				totalMemoryNow += gpu.TotalMemory
				freeMemoryNow += gpu.FreeMemory
			}
			if float32(freeMemoryNow-freeMemoryBefore) > float32(runner.estimatedVRAM)*0.8 {
				slog.Debug(fmt.Sprintf("gpu VRAM free memory converged after %0.2f seconds", time.Since(start).Seconds()), "runner", runner)
				finished <- struct{}{}
				return
			}
		}
	}()
	return finished
}
func (runner *runnerRef) LogValue() slog.Value {
	if runner == nil {
		return slog.StringValue("nil")
	}
	attrs := []slog.Attr{}
	if runner.model != nil {
		attrs = append(attrs, slog.String("name", runner.model.Name))
	}
	if len(runner.gpus) > 0 {
		attrs = append(attrs,
			slog.String("inference", runner.gpus[0].Library),
			slog.Int("devices", len(runner.gpus)),
		)
	}
	attrs = append(attrs,
		slog.String("size", format.HumanBytes2(runner.estimatedTotal)),
		slog.String("vram", format.HumanBytes2(runner.estimatedVRAM)),
		slog.Int("parallel", runner.numParallel),
		slog.Int("pid", runner.pid),
		slog.String("model", runner.modelPath),
	)
	if runner.Options != nil {
		attrs = append(attrs, slog.Int("num_ctx", runner.Options.NumCtx))
	}
	return slog.GroupValue(attrs...)
}
type ByDurationAndName []*runnerRef
func (a ByDurationAndName) Len() int      { return len(a) }
func (a ByDurationAndName) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a ByDurationAndName) Less(i, j int) bool {
	d1 := uint64(a[i].sessionDuration)
	d2 := uint64(a[j].sessionDuration)
	if d1 != d2 {
		return d1 < d2
	}
	return a[i].modelPath < a[j].modelPath
}
func pickBestFullFitByLibrary(req *LlmRequest, f *ggml.GGML, gpus discover.GpuInfoList, numParallel *int) discover.GpuInfoList {
	var estimatedVRAM uint64
	var numParallelToTry []int
	if *numParallel <= 0 {
		numParallelToTry = append(numParallelToTry, defaultParallel, 1)
	} else {
		numParallelToTry = []int{*numParallel}
	}
	for _, gl := range gpus.ByLibrary() {
		var ok bool
		sgl := append(make(discover.GpuInfoList, 0, len(gl)), gl...)
		sort.Sort(sort.Reverse(discover.ByFreeMemory(sgl)))
		for _, p := range numParallelToTry {
			req.opts.NumCtx = req.origNumCtx * p
			if !envconfig.SchedSpread() {
				for _, g := range sgl {
					if ok, estimatedVRAM = llm.PredictServerFit([]discover.GpuInfo{g}, f, req.model.AdapterPaths, req.model.ProjectorPaths, req.opts, p); ok {
						slog.Info("new model will fit in available VRAM in single GPU, loading", "model", req.model.ModelPath, "gpu", g.ID, "parallel", p, "available", g.FreeMemory, "required", format.HumanBytes2(estimatedVRAM))
						*numParallel = p
						return []discover.GpuInfo{g}
					}
				}
			}
		}
		for _, p := range numParallelToTry {
			req.opts.NumCtx = req.origNumCtx * p
			if ok, estimatedVRAM = llm.PredictServerFit(sgl, f, req.model.AdapterPaths, req.model.ProjectorPaths, req.opts, p); ok {
				slog.Info("new model will fit in available VRAM, loading", "model", req.model.ModelPath, "library", sgl[0].Library, "parallel", p, "required", format.HumanBytes2(estimatedVRAM))
				*numParallel = p
				return sgl
			}
		}
	}
	return nil
}
func pickBestPartialFitByLibrary(req *LlmRequest, f *ggml.GGML, gpus discover.GpuInfoList, numParallel *int) discover.GpuInfoList {
	if *numParallel <= 0 {
		*numParallel = 1
		req.opts.NumCtx = req.origNumCtx
	}
	byLibrary := gpus.ByLibrary()
	if len(byLibrary) <= 1 {
		return gpus
	}
	var bestEstimate uint64
	var bestFit int
	for i, gl := range byLibrary {
		_, estimatedVRAM := llm.PredictServerFit(gl, f, req.model.AdapterPaths, req.model.ProjectorPaths, req.opts, *numParallel)
		if estimatedVRAM > bestEstimate {
			bestEstimate = estimatedVRAM
			bestFit = i
		}
	}
	return byLibrary[bestFit]
}
func (s *Scheduler) findRunnerToUnload() *runnerRef {
	s.loadedMu.Lock()
	runnerList := make([]*runnerRef, 0, len(s.loaded))
	for _, r := range s.loaded {
		runnerList = append(runnerList, r)
	}
	s.loadedMu.Unlock()
	if len(runnerList) == 0 {
		slog.Debug("no loaded runner to unload")
		return nil
	}
	sort.Sort(ByDurationAndName(runnerList))
	for _, runner := range runnerList {
		runner.refMu.Lock()
		rc := runner.refCount
		runner.refMu.Unlock()
		if rc == 0 {
			slog.Debug("found an idle runner to unload", "runner", runner)
			return runner
		}
	}
	slog.Debug("no idle runners, picking the shortest duration", "runner_count", len(runnerList), "runner", runnerList[0])
	return runnerList[0]
}
func (s *Scheduler) unloadAllRunners() {
	s.loadedMu.Lock()
	defer s.loadedMu.Unlock()
	for model, runner := range s.loaded {
		if runner.llama != nil {
			slog.Debug("shutting down runner", "model", model)
			runner.llama.Close()
		}
	}
}
func (s *Scheduler) expireRunner(model *Model) {
	s.loadedMu.Lock()
	runner, ok := s.loaded[model.ModelPath]
	s.loadedMu.Unlock()
	if ok {
		runner.refMu.Lock()
		runner.expiresAt = time.Now()
		if runner.expireTimer != nil {
			runner.expireTimer.Stop()
			runner.expireTimer = nil
		}
		runner.sessionDuration = 0
		if runner.refCount <= 0 {
			s.expiredCh <- runner
		}
		runner.refMu.Unlock()
	}
}
func (s *Scheduler) maybeFindCPURunnerToUnload(req *LlmRequest, f *ggml.GGML, gpus discover.GpuInfoList) *runnerRef {
	slog.Debug("evaluating if CPU model load will fit in available system memory")
	estimate := llm.EstimateGPULayers(gpus, f, req.model.ProjectorPaths, req.opts, req.opts.NumCtx/req.origNumCtx)
	if estimate.TotalSize <= gpus[0].FreeMemory {
		slog.Debug("cpu inference mode, model fits in available system memory", "model", format.HumanBytes2(estimate.TotalSize), "available", format.HumanBytes2(gpus[0].FreeMemory))
		return nil
	}
	return s.findRunnerToUnload()
}