#ifndef _COMPILER_INTERFACE_INCLUDED_
#define _COMPILER_INTERFACE_INCLUDED_
#include "../Include/ResourceLimits.h"
#include "../Include/visibility.h"
#include "../MachineIndependent/Versions.h"
#include <cstring>
#include <vector>
#ifdef _WIN32
#define C_DECL __cdecl
#else
#define C_DECL
#endif
#ifdef __cplusplus
extern "C" {
#endif
GLSLANG_EXPORT int ShInitialize();
GLSLANG_EXPORT int ShFinalize();
typedef enum {
EShLangVertex,
EShLangTessControl,
EShLangTessEvaluation,
EShLangGeometry,
EShLangFragment,
EShLangCompute,
EShLangRayGen,
EShLangRayGenNV = EShLangRayGen,
EShLangIntersect,
EShLangIntersectNV = EShLangIntersect,
EShLangAnyHit,
EShLangAnyHitNV = EShLangAnyHit,
EShLangClosestHit,
EShLangClosestHitNV = EShLangClosestHit,
EShLangMiss,
EShLangMissNV = EShLangMiss,
EShLangCallable,
EShLangCallableNV = EShLangCallable,
EShLangTask,
EShLangTaskNV = EShLangTask,
EShLangMesh,
EShLangMeshNV = EShLangMesh,
LAST_ELEMENT_MARKER(EShLangCount),
} EShLanguage;
typedef enum : unsigned {
EShLangVertexMask         = (1 << EShLangVertex),
EShLangTessControlMask    = (1 << EShLangTessControl),
EShLangTessEvaluationMask = (1 << EShLangTessEvaluation),
EShLangGeometryMask       = (1 << EShLangGeometry),
EShLangFragmentMask       = (1 << EShLangFragment),
EShLangComputeMask        = (1 << EShLangCompute),
EShLangRayGenMask         = (1 << EShLangRayGen),
EShLangRayGenNVMask       = EShLangRayGenMask,
EShLangIntersectMask      = (1 << EShLangIntersect),
EShLangIntersectNVMask    = EShLangIntersectMask,
EShLangAnyHitMask         = (1 << EShLangAnyHit),
EShLangAnyHitNVMask       = EShLangAnyHitMask,
EShLangClosestHitMask     = (1 << EShLangClosestHit),
EShLangClosestHitNVMask   = EShLangClosestHitMask,
EShLangMissMask           = (1 << EShLangMiss),
EShLangMissNVMask         = EShLangMissMask,
EShLangCallableMask       = (1 << EShLangCallable),
EShLangCallableNVMask     = EShLangCallableMask,
EShLangTaskMask           = (1 << EShLangTask),
EShLangTaskNVMask         = EShLangTaskMask,
EShLangMeshMask           = (1 << EShLangMesh),
EShLangMeshNVMask         = EShLangMeshMask,
LAST_ELEMENT_MARKER(EShLanguageMaskCount),
} EShLanguageMask;
namespace glslang {
class TType;
typedef enum {
EShSourceNone,
EShSourceGlsl,
EShSourceHlsl,
LAST_ELEMENT_MARKER(EShSourceCount),
} EShSource;
typedef enum {
EShClientNone,
EShClientVulkan,
EShClientOpenGL,
LAST_ELEMENT_MARKER(EShClientCount),
} EShClient;
typedef enum {
EShTargetNone,
EShTargetSpv,
EshTargetSpv = EShTargetSpv,
LAST_ELEMENT_MARKER(EShTargetCount),
} EShTargetLanguage;
typedef enum {
EShTargetVulkan_1_0 = (1 << 22),
EShTargetVulkan_1_1 = (1 << 22) | (1 << 12),
EShTargetVulkan_1_2 = (1 << 22) | (2 << 12),
EShTargetVulkan_1_3 = (1 << 22) | (3 << 12),
EShTargetVulkan_1_4 = (1 << 22) | (4 << 12),
EShTargetOpenGL_450 = 450,
LAST_ELEMENT_MARKER(EShTargetClientVersionCount = 6),
} EShTargetClientVersion;
typedef EShTargetClientVersion EshTargetClientVersion;
typedef enum {
EShTargetSpv_1_0 = (1 << 16),
EShTargetSpv_1_1 = (1 << 16) | (1 << 8),
EShTargetSpv_1_2 = (1 << 16) | (2 << 8),
EShTargetSpv_1_3 = (1 << 16) | (3 << 8),
EShTargetSpv_1_4 = (1 << 16) | (4 << 8),
EShTargetSpv_1_5 = (1 << 16) | (5 << 8),
EShTargetSpv_1_6 = (1 << 16) | (6 << 8),
LAST_ELEMENT_MARKER(EShTargetLanguageVersionCount = 7),
} EShTargetLanguageVersion;
enum TLayoutPacking {
ElpNone,
ElpShared,
ElpStd140,
ElpStd430,
ElpPacked,
ElpScalar,
ElpCount
};
struct TInputLanguage {
EShSource languageFamily;
EShLanguage stage;
EShClient dialect;
int dialectVersion;
bool vulkanRulesRelaxed;
};
struct TClient {
EShClient client;
EShTargetClientVersion version;
};
struct TTarget {
EShTargetLanguage language;
EShTargetLanguageVersion version;
bool hlslFunctionality1;
};
struct TEnvironment {
TInputLanguage input;
TClient client;
TTarget target;
};
GLSLANG_EXPORT const char* StageName(EShLanguage);
}
typedef enum {
EShExVertexFragment,
EShExFragment
} EShExecutable;
typedef enum {
EShOptNoGeneration,
EShOptNone,
EShOptSimple,
EShOptFull,
LAST_ELEMENT_MARKER(EshOptLevelCount),
} EShOptimizationLevel;
typedef enum {
EShTexSampTransKeep,
EShTexSampTransUpgradeTextureRemoveSampler,
LAST_ELEMENT_MARKER(EShTexSampTransCount),
} EShTextureSamplerTransformMode;
enum EShMessages : unsigned {
EShMsgDefault              = 0,
EShMsgRelaxedErrors        = (1 << 0),
EShMsgSuppressWarnings     = (1 << 1),
EShMsgAST                  = (1 << 2),
EShMsgSpvRules             = (1 << 3),
EShMsgVulkanRules          = (1 << 4),
EShMsgOnlyPreprocessor     = (1 << 5),
EShMsgReadHlsl             = (1 << 6),
EShMsgCascadingErrors      = (1 << 7),
EShMsgKeepUncalled         = (1 << 8),
EShMsgHlslOffsets          = (1 << 9),
EShMsgDebugInfo            = (1 << 10),
EShMsgHlslEnable16BitTypes = (1 << 11),
EShMsgHlslLegalization     = (1 << 12),
EShMsgHlslDX9Compatible    = (1 << 13),
EShMsgBuiltinSymbolTable   = (1 << 14),
EShMsgEnhanced             = (1 << 15),
EShMsgAbsolutePath         = (1 << 16),
EShMsgDisplayErrorColumn   = (1 << 17),
EShMsgLinkTimeOptimization = (1 << 18),
LAST_ELEMENT_MARKER(EShMsgCount),
};
typedef enum {
EShReflectionDefault            = 0,
EShReflectionStrictArraySuffix  = (1 << 0),
EShReflectionBasicArraySuffix   = (1 << 1),
EShReflectionIntermediateIO     = (1 << 2),
EShReflectionSeparateBuffers    = (1 << 3),
EShReflectionAllBlockVariables  = (1 << 4),
EShReflectionUnwrapIOBlocks     = (1 << 5),
EShReflectionAllIOVariables     = (1 << 6),
EShReflectionSharedStd140SSBO   = (1 << 7),
EShReflectionSharedStd140UBO    = (1 << 8),
LAST_ELEMENT_MARKER(EShReflectionCount),
} EShReflectionOptions;
typedef struct {
const char* name;
int binding;
} ShBinding;
typedef struct {
int numBindings;
ShBinding* bindings;
} ShBindingTable;
typedef void* ShHandle;
GLSLANG_EXPORT ShHandle ShConstructCompiler(const EShLanguage, int );
GLSLANG_EXPORT ShHandle ShConstructLinker(const EShExecutable, int );
GLSLANG_EXPORT ShHandle ShConstructUniformMap();
GLSLANG_EXPORT void ShDestruct(ShHandle);
GLSLANG_EXPORT int ShCompile(const ShHandle, const char* const shaderStrings[], const int numStrings,
const int* lengths, const EShOptimizationLevel, const TBuiltInResource* resources,
int,
int defaultVersion = 110,
bool forwardCompatible = false,
EShMessages messages = EShMsgDefault,
const char* fileName = nullptr
);
GLSLANG_EXPORT int ShLinkExt(
const ShHandle,
const ShHandle h[],
const int numHandles);
GLSLANG_EXPORT void ShSetEncryptionMethod(ShHandle);
GLSLANG_EXPORT const char* ShGetInfoLog(const ShHandle);
GLSLANG_EXPORT const void* ShGetExecutable(const ShHandle);
GLSLANG_EXPORT int ShSetVirtualAttributeBindings(const ShHandle, const ShBindingTable*);
GLSLANG_EXPORT int ShSetFixedAttributeBindings(const ShHandle, const ShBindingTable*);
GLSLANG_EXPORT int ShExcludeAttributes(const ShHandle, int *attributes, int count);
GLSLANG_EXPORT int ShGetUniformLocation(const ShHandle uniformMap, const char* name);
#ifdef __cplusplus
}
#endif
#include <list>
#include <string>
#include <utility>
class TCompiler;
class TInfoSink;
namespace glslang {
struct Version {
int major;
int minor;
int patch;
const char* flavor;
};
GLSLANG_EXPORT Version GetVersion();
GLSLANG_EXPORT const char* GetEsslVersionString();
GLSLANG_EXPORT const char* GetGlslVersionString();
GLSLANG_EXPORT int GetKhronosToolId();
class TIntermediate;
class TProgram;
class TPoolAllocator;
class TIoMapResolver;
GLSLANG_EXPORT bool InitializeProcess();
GLSLANG_EXPORT void FinalizeProcess();
enum TResourceType {
EResSampler,
EResTexture,
EResImage,
EResUbo,
EResSsbo,
EResUav,
EResCount
};
enum TBlockStorageClass
{
EbsUniform = 0,
EbsStorageBuffer,
EbsPushConstant,
EbsNone,
EbsCount,
};
class TShader {
public:
GLSLANG_EXPORT explicit TShader(EShLanguage);
GLSLANG_EXPORT virtual ~TShader();
GLSLANG_EXPORT void setStrings(const char* const* s, int n);
GLSLANG_EXPORT void setStringsWithLengths(
const char* const* s, const int* l, int n);
GLSLANG_EXPORT void setStringsWithLengthsAndNames(
const char* const* s, const int* l, const char* const* names, int n);
void setPreamble(const char* s) { preamble = s; }
GLSLANG_EXPORT void setEntryPoint(const char* entryPoint);
GLSLANG_EXPORT void setSourceEntryPoint(const char* sourceEntryPointName);
GLSLANG_EXPORT void addProcesses(const std::vector<std::string>&);
GLSLANG_EXPORT void setUniqueId(unsigned long long id);
GLSLANG_EXPORT void setOverrideVersion(int version);
GLSLANG_EXPORT void setDebugInfo(bool debugInfo);
GLSLANG_EXPORT void setShiftBinding(TResourceType res, unsigned int base);
GLSLANG_EXPORT void setShiftSamplerBinding(unsigned int base);
GLSLANG_EXPORT void setShiftTextureBinding(unsigned int base);
GLSLANG_EXPORT void setShiftImageBinding(unsigned int base);
GLSLANG_EXPORT void setShiftUboBinding(unsigned int base);
GLSLANG_EXPORT void setShiftUavBinding(unsigned int base);
GLSLANG_EXPORT void setShiftCbufferBinding(unsigned int base);
GLSLANG_EXPORT void setShiftSsboBinding(unsigned int base);
GLSLANG_EXPORT void setShiftBindingForSet(TResourceType res, unsigned int base, unsigned int set);
GLSLANG_EXPORT void setResourceSetBinding(const std::vector<std::string>& base);
GLSLANG_EXPORT void setAutoMapBindings(bool map);
GLSLANG_EXPORT void setAutoMapLocations(bool map);
GLSLANG_EXPORT void addUniformLocationOverride(const char* name, int loc);
GLSLANG_EXPORT void setUniformLocationBase(int base);
GLSLANG_EXPORT void setInvertY(bool invert);
GLSLANG_EXPORT void setDxPositionW(bool dxPosW);
GLSLANG_EXPORT void setEnhancedMsgs();
#ifdef ENABLE_HLSL
GLSLANG_EXPORT void setHlslIoMapping(bool hlslIoMap);
GLSLANG_EXPORT void setFlattenUniformArrays(bool flatten);
#endif
GLSLANG_EXPORT void setNoStorageFormat(bool useUnknownFormat);
GLSLANG_EXPORT void setNanMinMaxClamp(bool nanMinMaxClamp);
GLSLANG_EXPORT void setTextureSamplerTransformMode(EShTextureSamplerTransformMode mode);
GLSLANG_EXPORT void addBlockStorageOverride(const char* nameStr, glslang::TBlockStorageClass backing);
GLSLANG_EXPORT void setGlobalUniformBlockName(const char* name);
GLSLANG_EXPORT void setAtomicCounterBlockName(const char* name);
GLSLANG_EXPORT void setGlobalUniformSet(unsigned int set);
GLSLANG_EXPORT void setGlobalUniformBinding(unsigned int binding);
GLSLANG_EXPORT void setAtomicCounterBlockSet(unsigned int set);
GLSLANG_EXPORT void setAtomicCounterBlockBinding(unsigned int binding);
GLSLANG_EXPORT void addSourceText(const char* text, size_t len);
GLSLANG_EXPORT void setSourceFile(const char* file);
void setEnvInput(EShSource lang, EShLanguage envStage, EShClient client, int version)
{
environment.input.languageFamily = lang;
environment.input.stage = envStage;
environment.input.dialect = client;
environment.input.dialectVersion = version;
}
void setEnvClient(EShClient client, EShTargetClientVersion version)
{
environment.client.client = client;
environment.client.version = version;
}
void setEnvTarget(EShTargetLanguage lang, EShTargetLanguageVersion version)
{
environment.target.language = lang;
environment.target.version = version;
}
void getStrings(const char* const* &s, int& n) { s = strings; n = numStrings; }
#ifdef ENABLE_HLSL
void setEnvTargetHlslFunctionality1() { environment.target.hlslFunctionality1 = true; }
bool getEnvTargetHlslFunctionality1() const { return environment.target.hlslFunctionality1; }
#else
bool getEnvTargetHlslFunctionality1() const { return false; }
#endif
void setEnvInputVulkanRulesRelaxed() { environment.input.vulkanRulesRelaxed = true; }
bool getEnvInputVulkanRulesRelaxed() const { return environment.input.vulkanRulesRelaxed; }
void setCompileOnly() { compileOnly = true; }
bool getCompileOnly() const { return compileOnly; }
class Includer {
public:
struct IncludeResult {
IncludeResult(const std::string& headerName, const char* const headerData, const size_t headerLength, void* userData) :
headerName(headerName), headerData(headerData), headerLength(headerLength), userData(userData) { }
const std::string headerName;
const char* const headerData;
const size_t headerLength;
void* userData;
protected:
IncludeResult& operator=(const IncludeResult&);
IncludeResult();
};
virtual IncludeResult* includeSystem(const char* ,
const char* ,
size_t ) { return nullptr; }
virtual IncludeResult* includeLocal(const char* ,
const char* ,
size_t ) { return nullptr; }
virtual void releaseInclude(IncludeResult*) = 0;
virtual ~Includer() {}
};
class ForbidIncluder : public Includer {
public:
virtual void releaseInclude(IncludeResult*) override { }
};
GLSLANG_EXPORT bool parse(
const TBuiltInResource*, int defaultVersion, EProfile defaultProfile,
bool forceDefaultVersionAndProfile, bool forwardCompatible,
EShMessages, Includer&);
bool parse(const TBuiltInResource* res, int defaultVersion, EProfile defaultProfile, bool forceDefaultVersionAndProfile,
bool forwardCompatible, EShMessages messages)
{
TShader::ForbidIncluder includer;
return parse(res, defaultVersion, defaultProfile, forceDefaultVersionAndProfile, forwardCompatible, messages, includer);
}
bool parse(const TBuiltInResource* builtInResources, int defaultVersion, bool forwardCompatible, EShMessages messages)
{
return parse(builtInResources, defaultVersion, ENoProfile, false, forwardCompatible, messages);
}
bool parse(const TBuiltInResource* builtInResources, int defaultVersion, bool forwardCompatible, EShMessages messages,
Includer& includer)
{
return parse(builtInResources, defaultVersion, ENoProfile, false, forwardCompatible, messages, includer);
}
GLSLANG_EXPORT bool preprocess(
const TBuiltInResource* builtInResources, int defaultVersion,
EProfile defaultProfile, bool forceDefaultVersionAndProfile,
bool forwardCompatible, EShMessages message, std::string* outputString,
Includer& includer);
GLSLANG_EXPORT const char* getInfoLog();
GLSLANG_EXPORT const char* getInfoDebugLog();
EShLanguage getStage() const { return stage; }
TIntermediate* getIntermediate() const { return intermediate; }
protected:
TPoolAllocator* pool;
EShLanguage stage;
TCompiler* compiler;
TIntermediate* intermediate;
TInfoSink* infoSink;
const char* const* strings;
const int* lengths;
const char* const* stringNames;
int numStrings;
const char* preamble;
std::string sourceEntryPointName;
int overrideVersion;
TEnvironment environment;
bool compileOnly = false;
friend class TProgram;
private:
TShader& operator=(TShader&);
};
class TObjectReflection {
public:
GLSLANG_EXPORT TObjectReflection(const std::string& pName, const TType& pType, int pOffset, int pGLDefineType, int pSize, int pIndex);
const TType* getType() const { return type; }
GLSLANG_EXPORT int getBinding() const;
GLSLANG_EXPORT void dump() const;
static TObjectReflection badReflection() { return TObjectReflection(); }
GLSLANG_EXPORT unsigned int layoutLocation() const;
std::string name;
int offset;
int glDefineType;
int size;
int index;
int counterIndex;
int numMembers;
int arrayStride;
int topLevelArraySize;
int topLevelArrayStride;
EShLanguageMask stages;
protected:
TObjectReflection()
: offset(-1), glDefineType(-1), size(-1), index(-1), counterIndex(-1), numMembers(-1), arrayStride(0),
topLevelArrayStride(0), stages(EShLanguageMask(0)), type(nullptr)
{
}
const TType* type;
};
class  TReflection;
class  TIoMapper;
struct TVarEntryInfo;
class TIoMapResolver
{
public:
virtual ~TIoMapResolver() {}
virtual bool validateBinding(EShLanguage stage, TVarEntryInfo& ent) = 0;
virtual int resolveBinding(EShLanguage stage, TVarEntryInfo& ent) = 0;
virtual int resolveSet(EShLanguage stage, TVarEntryInfo& ent) = 0;
virtual int resolveUniformLocation(EShLanguage stage, TVarEntryInfo& ent) = 0;
virtual bool validateInOut(EShLanguage stage, TVarEntryInfo& ent) = 0;
virtual int resolveInOutLocation(EShLanguage stage, TVarEntryInfo& ent) = 0;
virtual int resolveInOutComponent(EShLanguage stage, TVarEntryInfo& ent) = 0;
virtual int resolveInOutIndex(EShLanguage stage, TVarEntryInfo& ent) = 0;
virtual void notifyBinding(EShLanguage stage, TVarEntryInfo& ent) = 0;
virtual void notifyInOut(EShLanguage stage, TVarEntryInfo& ent) = 0;
virtual void beginNotifications(EShLanguage stage) = 0;
virtual void endNotifications(EShLanguage stage) = 0;
virtual void beginResolve(EShLanguage stage) = 0;
virtual void endResolve(EShLanguage stage) = 0;
virtual void beginCollect(EShLanguage stage) = 0;
virtual void endCollect(EShLanguage stage) = 0;
virtual void reserverStorageSlot(TVarEntryInfo& ent, TInfoSink& infoSink) = 0;
virtual void reserverResourceSlot(TVarEntryInfo& ent, TInfoSink& infoSink) = 0;
virtual void addStage(EShLanguage stage, TIntermediate& stageIntermediate) = 0;
};
class TIoMapper {
public:
TIoMapper() {}
virtual ~TIoMapper() {}
bool virtual addStage(EShLanguage, TIntermediate&, TInfoSink&, TIoMapResolver*);
bool virtual doMap(TIoMapResolver*, TInfoSink&) { return true; }
bool virtual setAutoPushConstantBlock(const char*, unsigned int, TLayoutPacking) { return false; }
};
GLSLANG_EXPORT TIoMapper* GetGlslIoMapper();
class TProgram {
public:
GLSLANG_EXPORT TProgram();
GLSLANG_EXPORT virtual ~TProgram();
void addShader(TShader* shader) { stages[shader->stage].push_back(shader); }
std::list<TShader*>& getShaders(EShLanguage stage) { return stages[stage]; }
GLSLANG_EXPORT bool link(EShMessages);
GLSLANG_EXPORT const char* getInfoLog();
GLSLANG_EXPORT const char* getInfoDebugLog();
TIntermediate* getIntermediate(EShLanguage stage) const { return intermediate[stage]; }
GLSLANG_EXPORT bool buildReflection(int opts = EShReflectionDefault);
GLSLANG_EXPORT unsigned getLocalSize(int dim) const;
GLSLANG_EXPORT int getReflectionIndex(const char *name) const;
GLSLANG_EXPORT int getReflectionPipeIOIndex(const char* name, const bool inOrOut) const;
GLSLANG_EXPORT int getNumUniformVariables() const;
GLSLANG_EXPORT const TObjectReflection& getUniform(int index) const;
GLSLANG_EXPORT int getNumUniformBlocks() const;
GLSLANG_EXPORT const TObjectReflection& getUniformBlock(int index) const;
GLSLANG_EXPORT int getNumPipeInputs() const;
GLSLANG_EXPORT const TObjectReflection& getPipeInput(int index) const;
GLSLANG_EXPORT int getNumPipeOutputs() const;
GLSLANG_EXPORT const TObjectReflection& getPipeOutput(int index) const;
GLSLANG_EXPORT int getNumBufferVariables() const;
GLSLANG_EXPORT const TObjectReflection& getBufferVariable(int index) const;
GLSLANG_EXPORT int getNumBufferBlocks() const;
GLSLANG_EXPORT const TObjectReflection& getBufferBlock(int index) const;
GLSLANG_EXPORT int getNumAtomicCounters() const;
GLSLANG_EXPORT const TObjectReflection& getAtomicCounter(int index) const;
int getNumLiveUniformVariables() const             { return getNumUniformVariables(); }
int getNumLiveUniformBlocks() const                { return getNumUniformBlocks(); }
int getNumLiveAttributes() const                   { return getNumPipeInputs(); }
int getUniformIndex(const char *name) const        { return getReflectionIndex(name); }
int getPipeIOIndex(const char *name, const bool inOrOut) const
{ return getReflectionPipeIOIndex(name, inOrOut); }
const char *getUniformName(int index) const        { return getUniform(index).name.c_str(); }
int getUniformBinding(int index) const             { return getUniform(index).getBinding(); }
EShLanguageMask getUniformStages(int index) const  { return getUniform(index).stages; }
int getUniformBlockIndex(int index) const          { return getUniform(index).index; }
int getUniformType(int index) const                { return getUniform(index).glDefineType; }
int getUniformBufferOffset(int index) const        { return getUniform(index).offset; }
int getUniformArraySize(int index) const           { return getUniform(index).size; }
const TType *getUniformTType(int index) const      { return getUniform(index).getType(); }
const char *getUniformBlockName(int index) const   { return getUniformBlock(index).name.c_str(); }
int getUniformBlockSize(int index) const           { return getUniformBlock(index).size; }
int getUniformBlockBinding(int index) const        { return getUniformBlock(index).getBinding(); }
int getUniformBlockCounterIndex(int index) const   { return getUniformBlock(index).counterIndex; }
const TType *getUniformBlockTType(int index) const { return getUniformBlock(index).getType(); }
const char *getAttributeName(int index) const      { return getPipeInput(index).name.c_str(); }
int getAttributeType(int index) const              { return getPipeInput(index).glDefineType; }
const TType *getAttributeTType(int index) const    { return getPipeInput(index).getType(); }
GLSLANG_EXPORT void dumpReflection();
GLSLANG_EXPORT TIoMapResolver* getGlslIoResolver(EShLanguage stage);
GLSLANG_EXPORT bool mapIO(TIoMapResolver* pResolver = nullptr, TIoMapper* pIoMapper = nullptr);
protected:
GLSLANG_EXPORT bool linkStage(EShLanguage, EShMessages);
GLSLANG_EXPORT bool crossStageCheck(EShMessages);
TPoolAllocator* pool;
std::list<TShader*> stages[EShLangCount];
TIntermediate* intermediate[EShLangCount];
bool newedIntermediate[EShLangCount];
TInfoSink* infoSink;
TReflection* reflection;
bool linked;
private:
TProgram(TProgram&);
TProgram& operator=(TProgram&);
};
}
#endif