import os from "os";
import path from "path";
import chalk from "chalk";
import {DisposedError, EventRelay, withLock} from "lifecycle-utils";
import {getConsoleLogPrefix} from "../utils/getConsoleLogPrefix.js";
import {LlamaModel, LlamaModelOptions} from "../evaluator/LlamaModel/LlamaModel.js";
import {DisposeGuard} from "../utils/DisposeGuard.js";
import {GbnfJsonDefList, GbnfJsonSchema} from "../utils/gbnfJson/types.js";
import {LlamaJsonSchemaGrammar} from "../evaluator/LlamaJsonSchemaGrammar.js";
import {LlamaGrammar, LlamaGrammarOptions} from "../evaluator/LlamaGrammar.js";
import {ThreadsSplitter} from "../utils/ThreadsSplitter.js";
import {getLlamaClasses, LlamaClasses} from "../utils/getLlamaClasses.js";
import {BindingModule} from "./AddonTypes.js";
import {
    BuildGpu, BuildMetadataFile, LlamaGpuType, LlamaLocks, LlamaLogLevel,
    LlamaLogLevelGreaterThan, LlamaLogLevelGreaterThanOrEqual, LlamaNuma
} from "./types.js";
import {MemoryOrchestrator, MemoryReservation} from "./utils/MemoryOrchestrator.js";
export const LlamaLogLevelToAddonLogLevel: ReadonlyMap<LlamaLogLevel, number> = new Map([
    [LlamaLogLevel.disabled, 0],
    [LlamaLogLevel.fatal, 1],
    [LlamaLogLevel.error, 2],
    [LlamaLogLevel.warn, 3],
    [LlamaLogLevel.info, 4],
    [LlamaLogLevel.log, 5],
    [LlamaLogLevel.debug, 6]
]);
const addonLogLevelToLlamaLogLevel: ReadonlyMap<number, LlamaLogLevel> = new Map(
    [...LlamaLogLevelToAddonLogLevel.entries()].map(([key, value]) => [value, key])
);
const defaultLogLevel = 5;
const defaultCPUMinThreadSplitterThreads = 4;
export class Llama {
     public readonly _bindings: BindingModule;
     public readonly _backendDisposeGuard = new DisposeGuard();
     public readonly _memoryLock = {};
     public readonly _consts: ReturnType<BindingModule["getConsts"]>;
     public readonly _vramOrchestrator: MemoryOrchestrator;
     public _vramPadding: MemoryReservation;
     public readonly _ramOrchestrator: MemoryOrchestrator;
     public readonly _ramPadding: MemoryReservation;
     public readonly _swapOrchestrator: MemoryOrchestrator;
     public readonly _debug: boolean;
     public readonly _threadsSplitter: ThreadsSplitter;
     public _hadErrorLogs: boolean = false;
     private readonly _gpu: LlamaGpuType;
     private readonly _numa: LlamaNuma;
     private readonly _buildType: "localBuild" | "prebuilt";
     private readonly _cmakeOptions: Readonly<Record<string, string>>;
     private readonly _supportsGpuOffloading: boolean;
     private readonly _supportsMmap: boolean;
     private readonly _gpuSupportsMmap: boolean;
     private readonly _supportsMlock: boolean;
     private readonly _mathCores: number;
     private readonly _llamaCppRelease: {
        readonly repo: string,
        readonly release: string
    };
     private _logger: ((level: LlamaLogLevel, message: string) => void);
     private _logLevel: LlamaLogLevel;
     private _pendingLog: string | null = null;
     private _pendingLogLevel: LlamaLogLevel | null = null;
     private _logDispatchQueuedMicrotasks: number = 0;
     private _previousLog: string | null = null;
     private _previousLogLevel: LlamaLogLevel | null = null;
     private _nextLogNeedNewLine: boolean = false;
     private _disposed: boolean = false;
    private _classes?: LlamaClasses;
    public readonly onDispose = new EventRelay<void>();
    private constructor({
        bindings, bindingPath, extBackendsPath, logLevel, logger, buildType, cmakeOptions, llamaCppRelease, debug, numa, buildGpu,
        maxThreads, vramOrchestrator, vramPadding, ramOrchestrator, ramPadding, swapOrchestrator, skipLlamaInit
    }: {
        bindings: BindingModule,
        bindingPath: string,
        extBackendsPath?: string,
        logLevel: LlamaLogLevel,
        logger: (level: LlamaLogLevel, message: string) => void,
        buildType: "localBuild" | "prebuilt",
        cmakeOptions: Record<string, string>,
        llamaCppRelease: {
            repo: string,
            release: string
        },
        debug: boolean,
        numa?: LlamaNuma,
        buildGpu: BuildGpu,
        maxThreads?: number,
        vramOrchestrator: MemoryOrchestrator,
        vramPadding: MemoryReservation,
        ramOrchestrator: MemoryOrchestrator,
        ramPadding: MemoryReservation,
        swapOrchestrator: MemoryOrchestrator,
        skipLlamaInit: boolean
    }) {
        this._dispatchPendingLogMicrotask = this._dispatchPendingLogMicrotask.bind(this);
        this._onAddonLog = this._onAddonLog.bind(this);
        this._bindings = bindings;
        this._debug = debug;
        this._numa = numa ?? false;
        this._logLevel = this._debug
            ? LlamaLogLevel.debug
            : (logLevel ?? LlamaLogLevel.debug);
        const previouslyLoaded = bindings.markLoaded();
        if (!this._debug && (!skipLlamaInit || !previouslyLoaded)) {
            this._bindings.setLogger(this._onAddonLog);
            this._bindings.setLoggerLogLevel(LlamaLogLevelToAddonLogLevel.get(this._logLevel) ?? defaultLogLevel);
        }
        bindings.loadBackends();
        let loadedGpu = bindings.getGpuType();
        if (loadedGpu == null || (loadedGpu === false && buildGpu !== false)) {
            const backendsPath = path.dirname(bindingPath);
            const fallbackBackendsDir = path.join(extBackendsPath ?? backendsPath, "fallback");
            bindings.loadBackends(backendsPath);
            loadedGpu = bindings.getGpuType();
            if (loadedGpu == null || (loadedGpu === false && buildGpu !== false))
                bindings.loadBackends(fallbackBackendsDir);
        }
        bindings.ensureGpuDeviceIsSupported();
        if (this._numa !== false)
            bindings.setNuma(numa);
        this._gpu = bindings.getGpuType() ?? false;
        this._supportsGpuOffloading = bindings.getSupportsGpuOffloading();
        this._supportsMmap = bindings.getSupportsMmap();
        this._gpuSupportsMmap = bindings.getGpuSupportsMmap();
        this._supportsMlock = bindings.getSupportsMlock();
        this._mathCores = bindings.getMathCores();
        this._consts = bindings.getConsts();
        this._vramOrchestrator = vramOrchestrator;
        this._vramPadding = vramPadding;
        this._ramOrchestrator = ramOrchestrator;
        this._ramPadding = ramPadding;
        this._swapOrchestrator = swapOrchestrator;
        this._threadsSplitter = new ThreadsSplitter(
            maxThreads ?? (
                this._gpu === false
                    ? Math.max(defaultCPUMinThreadSplitterThreads, this._mathCores)
                    : 0
            )
        );
        this._logger = logger;
        this._buildType = buildType;
        this._cmakeOptions = Object.freeze({...cmakeOptions});
        this._llamaCppRelease = Object.freeze({
            repo: llamaCppRelease.repo,
            release: llamaCppRelease.release
        });
        this._onExit = this._onExit.bind(this);
        process.on("exit", this._onExit);
    }
    public async dispose() {
        if (this._disposed)
            return;
        this._disposed = true;
        this.onDispose.dispatchEvent();
        await this._backendDisposeGuard.acquireDisposeLock();
        await this._bindings.dispose();
    }
    public async [Symbol.asyncDispose]() {
        await this.dispose();
    }
    public get disposed() {
        return this._disposed;
    }
    public get classes() {
        if (this._classes == null)
            this._classes = getLlamaClasses();
        return this._classes;
    }
    public get gpu() {
        return this._gpu;
    }
    public get supportsGpuOffloading() {
        return this._supportsGpuOffloading;
    }
    public get supportsMmap() {
        return this._supportsMmap;
    }
    public get gpuSupportsMmap() {
        return this._gpuSupportsMmap;
    }
    public get supportsMlock() {
        return this._supportsMlock;
    }
    public get cpuMathCores() {
        return this._mathCores;
    }
    public get maxThreads() {
        return this._threadsSplitter.maxThreads;
    }
    public set maxThreads(value: number) {
        this._threadsSplitter.maxThreads = Math.floor(Math.max(0, value));
    }
    public get numa() {
        return this._numa;
    }
    public get logLevel() {
        return this._logLevel;
    }
    public set logLevel(value: LlamaLogLevel) {
        this._ensureNotDisposed();
        if (value === this._logLevel || this._debug)
            return;
        this._bindings.setLoggerLogLevel(LlamaLogLevelToAddonLogLevel.get(value) ?? defaultLogLevel);
        this._logLevel = value;
    }
    public get logger() {
        return this._logger;
    }
    public set logger(value: (level: LlamaLogLevel, message: string) => void) {
        this._logger = value;
        if (value !== Llama.defaultConsoleLogger)
            this._nextLogNeedNewLine = false;
    }
    public get buildType() {
        return this._buildType;
    }
    public get cmakeOptions() {
        return this._cmakeOptions;
    }
    public get llamaCppRelease() {
        return this._llamaCppRelease;
    }
    public get systemInfo() {
        this._ensureNotDisposed();
        return this._bindings.systemInfo();
    }
    public get vramPaddingSize() {
        return this._vramPadding.size;
    }
    public async getVramState() {
        this._ensureNotDisposed();
        const {total, used, unifiedSize} = this._bindings.getGpuVramInfo();
        return {
            total,
            used,
            free: Math.max(0, total - used),
            unifiedSize
        };
    }
    public async getSwapState(): Promise<{
        maxSize: number,
        allocated: number,
        used: number
    }> {
        this._ensureNotDisposed();
        const {total, maxSize, free} = this._bindings.getSwapInfo();
        return {
            maxSize: maxSize === -1
                ? Infinity
                : maxSize,
            allocated: total,
            used: total - free
        };
    }
    public async getGpuDeviceNames() {
        this._ensureNotDisposed();
        const {deviceNames} = this._bindings.getGpuDeviceInfo();
        return deviceNames;
    }
    public async loadModel(options: LlamaModelOptions) {
        this._ensureNotDisposed();
        return await withLock([this._memoryLock, LlamaLocks.loadToMemory], options.loadSignal, async () => {
            this._ensureNotDisposed();
            const preventDisposalHandle = this._backendDisposeGuard.createPreventDisposalHandle();
            try {
                return await LlamaModel._create(options, {_llama: this});
            } finally {
                preventDisposalHandle.dispose();
            }
        });
    }
    public async createGrammarForJsonSchema<
        const T extends GbnfJsonSchema<Defs>,
        const Defs extends GbnfJsonDefList<Defs> = Record<any, any>
    >(schema: Readonly<T> & GbnfJsonSchema<Defs>) {
        return new LlamaJsonSchemaGrammar<T, Defs>(this, schema);
    }
    public async getGrammarFor(type: Parameters<typeof LlamaGrammar.getFor>[1]) {
        return await LlamaGrammar.getFor(this, type);
    }
    public async createGrammar(options: LlamaGrammarOptions) {
        return new LlamaGrammar(this, options);
    }
    public async _init() {
        await this._bindings.init();
    }
    public _log(level: LlamaLogLevel, message: string) {
        this._onAddonLog(LlamaLogLevelToAddonLogLevel.get(level) ?? defaultLogLevel, message + "\n");
    }
    private _onAddonLog(level: number, message: string) {
        const llamaLogLevel = addonLogLevelToLlamaLogLevel.get(level) ?? LlamaLogLevel.fatal;
        if (this._pendingLog != null && this._pendingLogLevel != null && this._pendingLogLevel != llamaLogLevel) {
            this._callLogger(this._pendingLogLevel, this._pendingLog);
            this._pendingLog = null;
        }
        const sourceMessage = (this._pendingLog ?? "") + message;
        const lastNewLineIndex = sourceMessage.lastIndexOf("\n");
        const currentLog = lastNewLineIndex < 0
            ? sourceMessage
            : sourceMessage.slice(0, lastNewLineIndex);
        const nextLog = lastNewLineIndex < 0
            ? ""
            : sourceMessage.slice(lastNewLineIndex + 1);
        if (currentLog !== "")
            this._callLogger(llamaLogLevel, currentLog);
        if (nextLog !== "") {
            this._pendingLog = nextLog;
            this._pendingLogLevel = llamaLogLevel;
            queueMicrotask(this._dispatchPendingLogMicrotask);
            this._logDispatchQueuedMicrotasks++;
        } else
            this._pendingLog = null;
    }
    private _dispatchPendingLogMicrotask() {
        this._logDispatchQueuedMicrotasks--;
        if (this._logDispatchQueuedMicrotasks !== 0)
            return;
        if (this._pendingLog != null && this._pendingLogLevel != null) {
            this._callLogger(this._pendingLogLevel, this._pendingLog);
            this._pendingLog = null;
        }
    }
    private _callLogger(level: LlamaLogLevel, message: string) {
        if (logMessageIsOnlyDots(message) && this._logger === Llama.defaultConsoleLogger) {
            if (logMessageIsOnlyDots(this._previousLog) && level === this._previousLogLevel) {
                process.stdout.write(message);
            } else {
                this._nextLogNeedNewLine = true;
                process.stdout.write(prefixAndColorMessage(message, getColorForLogLevel(level)));
            }
        } else {
            if (this._nextLogNeedNewLine) {
                process.stdout.write("\n");
                this._nextLogNeedNewLine = false;
            }
            try {
                const transformedLogLevel = getTransformedLogLevel(level, message, this.gpu);
                if (LlamaLogLevelGreaterThanOrEqual(transformedLogLevel, this._logLevel))
                    this._logger(transformedLogLevel, message);
            } catch (err) {
            }
        }
        this._previousLog = message;
        this._previousLogLevel = level;
        if (!this._hadErrorLogs && LlamaLogLevelGreaterThan(level, LlamaLogLevel.error))
            this._hadErrorLogs = true;
    }
    private _onExit() {
        if (this._pendingLog != null && this._pendingLogLevel != null) {
            this._callLogger(this._pendingLogLevel, this._pendingLog);
            this._pendingLog = null;
        }
    }
    private _ensureNotDisposed() {
        if (this._disposed)
            throw new DisposedError();
    }
    public static async _create({
        bindings, bindingPath, extBackendsPath, buildType, buildMetadata, logLevel, logger, vramPadding, ramPadding, maxThreads,
        skipLlamaInit = false, debug, numa
    }: {
        bindings: BindingModule,
        bindingPath: string,
        extBackendsPath?: string,
        buildType: "localBuild" | "prebuilt",
        buildMetadata: BuildMetadataFile,
        logLevel: LlamaLogLevel,
        logger: (level: LlamaLogLevel, message: string) => void,
        maxThreads?: number,
        vramPadding: number | ((totalVram: number) => number),
        ramPadding: number | ((totalRam: number) => number),
        skipLlamaInit?: boolean,
        debug: boolean,
        numa?: LlamaNuma
    }) {
        const vramOrchestrator = new MemoryOrchestrator(() => {
            const {total, used, unifiedSize} = bindings.getGpuVramInfo();
            return {
                total,
                free: Math.max(0, total - used),
                unifiedSize
            };
        });
        const ramOrchestrator = new MemoryOrchestrator(() => {
            const used = process.memoryUsage().rss;
            const total = os.totalmem();
            return {
                total,
                free: Math.max(0, total - used),
                unifiedSize: total
            };
        });
        const swapOrchestrator = new MemoryOrchestrator(() => {
            const {total, maxSize, free} = bindings.getSwapInfo();
            const used = total - free;
            if (maxSize === -1)
                return {
                    total: Infinity,
                    free: Infinity,
                    unifiedSize: Infinity
                };
            return {
                total: maxSize,
                free: maxSize - used,
                unifiedSize: maxSize
            };
        });
        let resolvedRamPadding: MemoryReservation;
        if (ramPadding instanceof Function)
            resolvedRamPadding = ramOrchestrator.reserveMemory(ramPadding((await ramOrchestrator.getMemoryState()).total));
        else
            resolvedRamPadding = ramOrchestrator.reserveMemory(ramPadding);
        const llama = new Llama({
            bindings,
            bindingPath,
            extBackendsPath,
            buildType,
            cmakeOptions: buildMetadata.buildOptions.customCmakeOptions,
            llamaCppRelease: {
                repo: buildMetadata.buildOptions.llamaCpp.repo,
                release: buildMetadata.buildOptions.llamaCpp.release
            },
            logLevel,
            logger,
            debug,
            numa,
            buildGpu: buildMetadata.buildOptions.gpu,
            vramOrchestrator,
            maxThreads,
            vramPadding: vramOrchestrator.reserveMemory(0),
            ramOrchestrator,
            ramPadding: resolvedRamPadding,
            swapOrchestrator,
            skipLlamaInit
        });
        if (llama.gpu === false || vramPadding === 0) {
        } else if (vramPadding instanceof Function) {
            const currentVramPadding = llama._vramPadding;
            llama._vramPadding = vramOrchestrator.reserveMemory(vramPadding((await vramOrchestrator.getMemoryState()).total));
            currentVramPadding.dispose();
        } else {
            const currentVramPadding = llama._vramPadding;
            llama._vramPadding = vramOrchestrator.reserveMemory(vramPadding);
            currentVramPadding.dispose();
        }
        if (!skipLlamaInit)
            await llama._init();
        return llama;
    }
    public static defaultConsoleLogger(level: LlamaLogLevel, message: string) {
        switch (level) {
            case LlamaLogLevel.disabled:
                break;
            case LlamaLogLevel.fatal:
                console.warn(prefixAndColorMessage(message, getColorForLogLevel(level)));
                break;
            case LlamaLogLevel.error:
                console.warn(prefixAndColorMessage(message, getColorForLogLevel(level)));
                break;
            case LlamaLogLevel.warn:
                console.warn(prefixAndColorMessage(message, getColorForLogLevel(level)));
                break;
            case LlamaLogLevel.info:
                console.info(prefixAndColorMessage(message, getColorForLogLevel(level)));
                break;
            case LlamaLogLevel.log:
                console.info(prefixAndColorMessage(message, getColorForLogLevel(level)));
                break;
            case LlamaLogLevel.debug:
                console.debug(prefixAndColorMessage(message, getColorForLogLevel(level)));
                break;
            default:
                void (level satisfies never);
                console.warn(getConsoleLogPrefix() + getColorForLogLevel(LlamaLogLevel.warn)(`Unknown log level: ${level}`));
                console.log(prefixAndColorMessage(message, getColorForLogLevel(level)));
        }
    }
}
function getColorForLogLevel(level: LlamaLogLevel) {
    switch (level) {
        case LlamaLogLevel.disabled: return chalk.whiteBright;
        case LlamaLogLevel.fatal: return chalk.redBright;
        case LlamaLogLevel.error: return chalk.red;
        case LlamaLogLevel.warn: return chalk.yellow;
        case LlamaLogLevel.info: return chalk.whiteBright;
        case LlamaLogLevel.log: return chalk.white;
        case LlamaLogLevel.debug: return chalk.gray;
        default:
            void (level satisfies never);
            return chalk.whiteBright;
    }
}
function prefixAndColorMessage(message: string, color: (message: string) => string) {
    return getConsoleLogPrefix() + (
        message
            .split("\n")
            .map((line) => color(line))
            .join("\n" + getConsoleLogPrefix())
    );
}
function logMessageIsOnlyDots(message: string | null) {
    if (message == null)
        return false;
    for (let i = 0; i < message.length; i++) {
        if (message[i] !== ".")
            return false;
    }
    return true;
}
function getTransformedLogLevel(level: LlamaLogLevel, message: string, gpu: BuildGpu): LlamaLogLevel {
    if (level === LlamaLogLevel.warn && message.endsWith("the full capacity of the model will not be utilized"))
        return LlamaLogLevel.info;
    else if (level === LlamaLogLevel.warn && message.startsWith("ggml_metal_init: skipping kernel_") && message.endsWith("(not supported)"))
        return LlamaLogLevel.log;
    else if (level === LlamaLogLevel.warn && message.startsWith("ggml_cuda_init: GGML_CUDA_FORCE_") && message.endsWith(" no"))
        return LlamaLogLevel.log;
    else if (level === LlamaLogLevel.info && message.startsWith("load_backend: loaded "))
        return LlamaLogLevel.log;
    else if (level === LlamaLogLevel.warn && message.startsWith("make_cpu_buft_list: disabling extra buffer types"))
        return LlamaLogLevel.info;
    else if (level === LlamaLogLevel.warn && message.startsWith("llama_context: non-unified KV cache requires ggml_set_rows() - forcing unified KV cache"))
        return LlamaLogLevel.info;
    else if (level === LlamaLogLevel.warn && message.startsWith("llama_kv_cache_unified: LLAMA_SET_ROWS=0, using old ggml_cpy() method for backwards compatibility"))
        return LlamaLogLevel.info;
    else if (level === LlamaLogLevel.warn && message.startsWith("init: embeddings required but some input tokens were not marked as outputs -> overriding"))
        return LlamaLogLevel.info;
    else if (level === LlamaLogLevel.warn && message.startsWith("load: special_eog_ids contains both '<|return|>' and '<|call|>' tokens, removing '<|end|>' token from EOG list"))
        return LlamaLogLevel.info;
    else if (level === LlamaLogLevel.warn && message.startsWith("llama_init_from_model: model default pooling_type is [0], but [-1] was specified"))
        return LlamaLogLevel.info;
    else if (gpu === false && level === LlamaLogLevel.warn && message.startsWith("llama_adapter_lora_init_impl: lora for '") && message.endsWith("' cannot use buft 'CPU_REPACK', fallback to CPU"))
        return LlamaLogLevel.info;
    else if (gpu === "metal" && level === LlamaLogLevel.warn && message.startsWith("ggml_metal_device_init: tensor API disabled for"))
        return LlamaLogLevel.info;
    return level;
}