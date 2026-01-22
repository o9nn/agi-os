#pragma once
#include "slang.h"
#ifdef __cplusplus
extern "C"
{
#endif
SLANG_API SlangSession* spCreateSession(const char* deprecated = 0);
SLANG_API void spDestroySession(SlangSession* session);
SLANG_API void spSessionSetSharedLibraryLoader(
SlangSession* session,
ISlangSharedLibraryLoader* loader);
SLANG_API ISlangSharedLibraryLoader* spSessionGetSharedLibraryLoader(SlangSession* session);
SLANG_API SlangResult
spSessionCheckCompileTargetSupport(SlangSession* session, SlangCompileTarget target);
SLANG_API SlangResult
spSessionCheckPassThroughSupport(SlangSession* session, SlangPassThrough passThrough);
SLANG_API void spAddBuiltins(
SlangSession* session,
char const* sourcePath,
char const* sourceString);
SLANG_API SlangCompileRequest* spCreateCompileRequest(SlangSession* session);
SLANG_API void spDestroyCompileRequest(SlangCompileRequest* request);
SLANG_API void spSetFileSystem(SlangCompileRequest* request, ISlangFileSystem* fileSystem);
SLANG_API void spSetCompileFlags(SlangCompileRequest* request, SlangCompileFlags flags);
SLANG_API SlangCompileFlags spGetCompileFlags(SlangCompileRequest* request);
SLANG_API void spSetDumpIntermediates(SlangCompileRequest* request, int enable);
SLANG_API void spSetDumpIntermediatePrefix(SlangCompileRequest* request, const char* prefix);
SLANG_API void spSetLineDirectiveMode(
SlangCompileRequest* request,
SlangLineDirectiveMode mode);
SLANG_API void spSetTargetLineDirectiveMode(
SlangCompileRequest* request,
int targetIndex,
SlangLineDirectiveMode mode);
SLANG_API void spSetTargetForceGLSLScalarBufferLayout(
SlangCompileRequest* request,
int targetIndex,
bool forceScalarLayout);
SLANG_API void spSetTargetUseMinimumSlangOptimization(
slang::ICompileRequest* request,
int targetIndex,
bool val);
SLANG_API void spSetIgnoreCapabilityCheck(slang::ICompileRequest* request, bool val);
SLANG_API void spSetCodeGenTarget(SlangCompileRequest* request, SlangCompileTarget target);
SLANG_API int spAddCodeGenTarget(SlangCompileRequest* request, SlangCompileTarget target);
SLANG_API void spSetTargetProfile(
SlangCompileRequest* request,
int targetIndex,
SlangProfileID profile);
SLANG_API void spSetTargetFlags(
SlangCompileRequest* request,
int targetIndex,
SlangTargetFlags flags);
SLANG_API void spSetTargetFloatingPointMode(
SlangCompileRequest* request,
int targetIndex,
SlangFloatingPointMode mode);
SLANG_API void spAddTargetCapability(
slang::ICompileRequest* request,
int targetIndex,
SlangCapabilityID capability);
SLANG_API void spSetTargetMatrixLayoutMode(
SlangCompileRequest* request,
int targetIndex,
SlangMatrixLayoutMode mode);
SLANG_API void spSetMatrixLayoutMode(SlangCompileRequest* request, SlangMatrixLayoutMode mode);
SLANG_API void spSetDebugInfoLevel(SlangCompileRequest* request, SlangDebugInfoLevel level);
SLANG_API void spSetDebugInfoFormat(SlangCompileRequest* request, SlangDebugInfoFormat format);
SLANG_API void spSetOptimizationLevel(
SlangCompileRequest* request,
SlangOptimizationLevel level);
SLANG_API void spSetOutputContainerFormat(
SlangCompileRequest* request,
SlangContainerFormat format);
SLANG_API void spSetPassThrough(SlangCompileRequest* request, SlangPassThrough passThrough);
SLANG_API void spSetDiagnosticCallback(
SlangCompileRequest* request,
SlangDiagnosticCallback callback,
void const* userData);
SLANG_API void spSetWriter(
SlangCompileRequest* request,
SlangWriterChannel channel,
ISlangWriter* writer);
SLANG_API ISlangWriter* spGetWriter(SlangCompileRequest* request, SlangWriterChannel channel);
SLANG_API void spAddSearchPath(SlangCompileRequest* request, const char* searchDir);
SLANG_API void spAddPreprocessorDefine(
SlangCompileRequest* request,
const char* key,
const char* value);
SLANG_API SlangResult spProcessCommandLineArguments(
SlangCompileRequest* request,
char const* const* args,
int argCount);
SLANG_API int spAddTranslationUnit(
SlangCompileRequest* request,
SlangSourceLanguage language,
char const* name);
SLANG_API void spSetDefaultModuleName(
SlangCompileRequest* request,
const char* defaultModuleName);
SLANG_API void spTranslationUnit_addPreprocessorDefine(
SlangCompileRequest* request,
int translationUnitIndex,
const char* key,
const char* value);
SLANG_API void spAddTranslationUnitSourceFile(
SlangCompileRequest* request,
int translationUnitIndex,
char const* path);
SLANG_API void spAddTranslationUnitSourceString(
SlangCompileRequest* request,
int translationUnitIndex,
char const* path,
char const* source);
SLANG_API SlangResult spAddLibraryReference(
SlangCompileRequest* request,
const char* basePath,
const void* libData,
size_t libDataSize);
SLANG_API void spAddTranslationUnitSourceStringSpan(
SlangCompileRequest* request,
int translationUnitIndex,
char const* path,
char const* sourceBegin,
char const* sourceEnd);
SLANG_API void spAddTranslationUnitSourceBlob(
SlangCompileRequest* request,
int translationUnitIndex,
char const* path,
ISlangBlob* sourceBlob);
SLANG_API SlangProfileID spFindProfile(SlangSession* session, char const* name);
SLANG_API SlangCapabilityID spFindCapability(SlangSession* session, char const* name);
SLANG_API int spAddEntryPoint(
SlangCompileRequest* request,
int translationUnitIndex,
char const* name,
SlangStage stage);
SLANG_API int spAddEntryPointEx(
SlangCompileRequest* request,
int translationUnitIndex,
char const* name,
SlangStage stage,
int genericArgCount,
char const** genericArgs);
SLANG_API SlangResult spSetGlobalGenericArgs(
SlangCompileRequest* request,
int genericArgCount,
char const** genericArgs);
SLANG_API SlangResult spSetTypeNameForGlobalExistentialTypeParam(
SlangCompileRequest* request,
int slotIndex,
char const* typeName);
SLANG_API SlangResult spSetTypeNameForEntryPointExistentialTypeParam(
SlangCompileRequest* request,
int entryPointIndex,
int slotIndex,
char const* typeName);
SLANG_API SlangResult spCompile(SlangCompileRequest* request);
SLANG_API char const* spGetDiagnosticOutput(SlangCompileRequest* request);
SLANG_API SlangResult
spGetDiagnosticOutputBlob(SlangCompileRequest* request, ISlangBlob** outBlob);
SLANG_API int spGetDependencyFileCount(SlangCompileRequest* request);
SLANG_API char const* spGetDependencyFilePath(SlangCompileRequest* request, int index);
SLANG_API int spGetTranslationUnitCount(SlangCompileRequest* request);
SLANG_API char const* spGetEntryPointSource(SlangCompileRequest* request, int entryPointIndex);
SLANG_API void const* spGetEntryPointCode(
SlangCompileRequest* request,
int entryPointIndex,
size_t* outSize);
SLANG_API SlangResult spGetEntryPointCodeBlob(
SlangCompileRequest* request,
int entryPointIndex,
int targetIndex,
ISlangBlob** outBlob);
SLANG_API SlangResult spGetEntryPointHostCallable(
SlangCompileRequest* request,
int entryPointIndex,
int targetIndex,
ISlangSharedLibrary** outSharedLibrary);
SLANG_API SlangResult
spGetTargetCodeBlob(SlangCompileRequest* request, int targetIndex, ISlangBlob** outBlob);
SLANG_API SlangResult spGetTargetHostCallable(
SlangCompileRequest* request,
int targetIndex,
ISlangSharedLibrary** outSharedLibrary);
SLANG_API void const* spGetCompileRequestCode(SlangCompileRequest* request, size_t* outSize);
SLANG_API SlangResult spGetContainerCode(SlangCompileRequest* request, ISlangBlob** outBlob);
SLANG_API SlangResult spLoadRepro(
SlangCompileRequest* request,
ISlangFileSystem* fileSystem,
const void* data,
size_t size);
SLANG_API SlangResult spSaveRepro(SlangCompileRequest* request, ISlangBlob** outBlob);
SLANG_API SlangResult spEnableReproCapture(SlangCompileRequest* request);
SLANG_API SlangResult spGetCompileTimeProfile(
SlangCompileRequest* request,
ISlangProfiler** compileTimeProfile,
bool shouldClear);
SLANG_API SlangResult spExtractRepro(
SlangSession* session,
const void* reproData,
size_t reproDataSize,
ISlangMutableFileSystem* fileSystem);
SLANG_API SlangResult spLoadReproAsFileSystem(
SlangSession* session,
const void* reproData,
size_t reproDataSize,
ISlangFileSystem* replaceFileSystem,
ISlangFileSystemExt** outFileSystem);
SLANG_API void spOverrideDiagnosticSeverity(
SlangCompileRequest* request,
SlangInt messageID,
SlangSeverity overrideSeverity);
SLANG_API SlangDiagnosticFlags spGetDiagnosticFlags(SlangCompileRequest* request);
SLANG_API void spSetDiagnosticFlags(SlangCompileRequest* request, SlangDiagnosticFlags flags);
SLANG_API SlangReflection* spGetReflection(SlangCompileRequest* request);
SLANG_API char const* spReflectionUserAttribute_GetName(SlangReflectionUserAttribute* attrib);
SLANG_API unsigned int spReflectionUserAttribute_GetArgumentCount(
SlangReflectionUserAttribute* attrib);
SLANG_API SlangReflectionType* spReflectionUserAttribute_GetArgumentType(
SlangReflectionUserAttribute* attrib,
unsigned int index);
SLANG_API SlangResult spReflectionUserAttribute_GetArgumentValueInt(
SlangReflectionUserAttribute* attrib,
unsigned int index,
int* rs);
SLANG_API SlangResult spReflectionUserAttribute_GetArgumentValueFloat(
SlangReflectionUserAttribute* attrib,
unsigned int index,
float* rs);
SLANG_API const char* spReflectionUserAttribute_GetArgumentValueString(
SlangReflectionUserAttribute* attrib,
unsigned int index,
size_t* outSize);
SLANG_API SlangTypeKind spReflectionType_GetKind(SlangReflectionType* type);
SLANG_API unsigned int spReflectionType_GetUserAttributeCount(SlangReflectionType* type);
SLANG_API SlangReflectionUserAttribute* spReflectionType_GetUserAttribute(
SlangReflectionType* type,
unsigned int index);
SLANG_API SlangReflectionUserAttribute* spReflectionType_FindUserAttributeByName(
SlangReflectionType* type,
char const* name);
SLANG_API SlangReflectionType* spReflectionType_applySpecializations(
SlangReflectionType* type,
SlangReflectionGeneric* generic);
SLANG_API unsigned int spReflectionType_GetFieldCount(SlangReflectionType* type);
SLANG_API SlangReflectionVariable* spReflectionType_GetFieldByIndex(
SlangReflectionType* type,
unsigned index);
SLANG_API size_t spReflectionType_GetElementCount(SlangReflectionType* type);
SLANG_API SlangReflectionType* spReflectionType_GetElementType(SlangReflectionType* type);
SLANG_API unsigned int spReflectionType_GetRowCount(SlangReflectionType* type);
SLANG_API unsigned int spReflectionType_GetColumnCount(SlangReflectionType* type);
SLANG_API SlangScalarType spReflectionType_GetScalarType(SlangReflectionType* type);
SLANG_API SlangResourceShape spReflectionType_GetResourceShape(SlangReflectionType* type);
SLANG_API SlangResourceAccess spReflectionType_GetResourceAccess(SlangReflectionType* type);
SLANG_API SlangReflectionType* spReflectionType_GetResourceResultType(
SlangReflectionType* type);
SLANG_API char const* spReflectionType_GetName(SlangReflectionType* type);
SLANG_API SlangResult
spReflectionType_GetFullName(SlangReflectionType* type, ISlangBlob** outNameBlob);
SLANG_API SlangReflectionGeneric* spReflectionType_GetGenericContainer(
SlangReflectionType* type);
SLANG_API SlangReflectionType* spReflectionTypeLayout_GetType(SlangReflectionTypeLayout* type);
SLANG_API SlangTypeKind spReflectionTypeLayout_getKind(SlangReflectionTypeLayout* type);
SLANG_API size_t spReflectionTypeLayout_GetSize(
SlangReflectionTypeLayout* type,
SlangParameterCategory category);
SLANG_API size_t spReflectionTypeLayout_GetStride(
SlangReflectionTypeLayout* type,
SlangParameterCategory category);
SLANG_API int32_t spReflectionTypeLayout_getAlignment(
SlangReflectionTypeLayout* type,
SlangParameterCategory category);
SLANG_API uint32_t spReflectionTypeLayout_GetFieldCount(SlangReflectionTypeLayout* type);
SLANG_API SlangReflectionVariableLayout* spReflectionTypeLayout_GetFieldByIndex(
SlangReflectionTypeLayout* type,
unsigned index);
SLANG_API SlangInt spReflectionTypeLayout_findFieldIndexByName(
SlangReflectionTypeLayout* typeLayout,
const char* nameBegin,
const char* nameEnd);
SLANG_API SlangReflectionVariableLayout* spReflectionTypeLayout_GetExplicitCounter(
SlangReflectionTypeLayout* typeLayout);
SLANG_API size_t spReflectionTypeLayout_GetElementStride(
SlangReflectionTypeLayout* type,
SlangParameterCategory category);
SLANG_API SlangReflectionTypeLayout* spReflectionTypeLayout_GetElementTypeLayout(
SlangReflectionTypeLayout* type);
SLANG_API SlangReflectionVariableLayout* spReflectionTypeLayout_GetElementVarLayout(
SlangReflectionTypeLayout* type);
SLANG_API SlangReflectionVariableLayout* spReflectionTypeLayout_getContainerVarLayout(
SlangReflectionTypeLayout* type);
SLANG_API SlangParameterCategory
spReflectionTypeLayout_GetParameterCategory(SlangReflectionTypeLayout* type);
SLANG_API unsigned spReflectionTypeLayout_GetCategoryCount(SlangReflectionTypeLayout* type);
SLANG_API SlangParameterCategory
spReflectionTypeLayout_GetCategoryByIndex(SlangReflectionTypeLayout* type, unsigned index);
SLANG_API SlangMatrixLayoutMode
spReflectionTypeLayout_GetMatrixLayoutMode(SlangReflectionTypeLayout* type);
SLANG_API int spReflectionTypeLayout_getGenericParamIndex(SlangReflectionTypeLayout* type);
SLANG_API SlangReflectionTypeLayout* spReflectionTypeLayout_getPendingDataTypeLayout(
SlangReflectionTypeLayout* type);
SLANG_API SlangReflectionVariableLayout*
spReflectionTypeLayout_getSpecializedTypePendingDataVarLayout(SlangReflectionTypeLayout* type);
SLANG_API SlangInt spReflectionType_getSpecializedTypeArgCount(SlangReflectionType* type);
SLANG_API SlangReflectionType* spReflectionType_getSpecializedTypeArgType(
SlangReflectionType* type,
SlangInt index);
SLANG_API SlangInt
spReflectionTypeLayout_getBindingRangeCount(SlangReflectionTypeLayout* typeLayout);
SLANG_API SlangBindingType spReflectionTypeLayout_getBindingRangeType(
SlangReflectionTypeLayout* typeLayout,
SlangInt index);
SLANG_API SlangInt spReflectionTypeLayout_isBindingRangeSpecializable(
SlangReflectionTypeLayout* typeLayout,
SlangInt index);
SLANG_API SlangInt spReflectionTypeLayout_getBindingRangeBindingCount(
SlangReflectionTypeLayout* typeLayout,
SlangInt index);
SLANG_API SlangReflectionTypeLayout* spReflectionTypeLayout_getBindingRangeLeafTypeLayout(
SlangReflectionTypeLayout* typeLayout,
SlangInt index);
SLANG_API SlangReflectionVariable* spReflectionTypeLayout_getBindingRangeLeafVariable(
SlangReflectionTypeLayout* typeLayout,
SlangInt index);
SLANG_API SlangImageFormat spReflectionTypeLayout_getBindingRangeImageFormat(
SlangReflectionTypeLayout* typeLayout,
SlangInt index);
SLANG_API SlangInt spReflectionTypeLayout_getFieldBindingRangeOffset(
SlangReflectionTypeLayout* typeLayout,
SlangInt fieldIndex);
SLANG_API SlangInt spReflectionTypeLayout_getExplicitCounterBindingRangeOffset(
SlangReflectionTypeLayout* inTypeLayout);
SLANG_API SlangInt spReflectionTypeLayout_getBindingRangeDescriptorSetIndex(
SlangReflectionTypeLayout* typeLayout,
SlangInt index);
SLANG_API SlangInt spReflectionTypeLayout_getBindingRangeFirstDescriptorRangeIndex(
SlangReflectionTypeLayout* typeLayout,
SlangInt index);
SLANG_API SlangInt spReflectionTypeLayout_getBindingRangeDescriptorRangeCount(
SlangReflectionTypeLayout* typeLayout,
SlangInt index);
SLANG_API SlangInt
spReflectionTypeLayout_getDescriptorSetCount(SlangReflectionTypeLayout* typeLayout);
SLANG_API SlangInt spReflectionTypeLayout_getDescriptorSetSpaceOffset(
SlangReflectionTypeLayout* typeLayout,
SlangInt setIndex);
SLANG_API SlangInt spReflectionTypeLayout_getDescriptorSetDescriptorRangeCount(
SlangReflectionTypeLayout* typeLayout,
SlangInt setIndex);
SLANG_API SlangInt spReflectionTypeLayout_getDescriptorSetDescriptorRangeIndexOffset(
SlangReflectionTypeLayout* typeLayout,
SlangInt setIndex,
SlangInt rangeIndex);
SLANG_API SlangInt spReflectionTypeLayout_getDescriptorSetDescriptorRangeDescriptorCount(
SlangReflectionTypeLayout* typeLayout,
SlangInt setIndex,
SlangInt rangeIndex);
SLANG_API SlangBindingType spReflectionTypeLayout_getDescriptorSetDescriptorRangeType(
SlangReflectionTypeLayout* typeLayout,
SlangInt setIndex,
SlangInt rangeIndex);
SLANG_API SlangParameterCategory spReflectionTypeLayout_getDescriptorSetDescriptorRangeCategory(
SlangReflectionTypeLayout* typeLayout,
SlangInt setIndex,
SlangInt rangeIndex);
SLANG_API SlangInt
spReflectionTypeLayout_getSubObjectRangeCount(SlangReflectionTypeLayout* typeLayout);
SLANG_API SlangInt spReflectionTypeLayout_getSubObjectRangeBindingRangeIndex(
SlangReflectionTypeLayout* typeLayout,
SlangInt subObjectRangeIndex);
SLANG_API SlangInt spReflectionTypeLayout_getSubObjectRangeSpaceOffset(
SlangReflectionTypeLayout* typeLayout,
SlangInt subObjectRangeIndex);
SLANG_API SlangReflectionVariableLayout* spReflectionTypeLayout_getSubObjectRangeOffset(
SlangReflectionTypeLayout* typeLayout,
SlangInt subObjectRangeIndex);
#if 0
SLANG_API SlangInt spReflectionTypeLayout_getSubObjectRangeCount(SlangReflectionTypeLayout* typeLayout);
SLANG_API SlangInt spReflectionTypeLayout_getSubObjectRangeObjectCount(SlangReflectionTypeLayout* typeLayout, SlangInt index);
SLANG_API SlangInt spReflectionTypeLayout_getSubObjectRangeBindingRangeIndex(SlangReflectionTypeLayout* typeLayout, SlangInt index);
SLANG_API SlangReflectionTypeLayout* spReflectionTypeLayout_getSubObjectRangeTypeLayout(SlangReflectionTypeLayout* typeLayout, SlangInt index);
SLANG_API SlangInt spReflectionTypeLayout_getSubObjectRangeDescriptorRangeCount(SlangReflectionTypeLayout* typeLayout, SlangInt subObjectRangeIndex);
SLANG_API SlangBindingType spReflectionTypeLayout_getSubObjectRangeDescriptorRangeBindingType(SlangReflectionTypeLayout* typeLayout, SlangInt subObjectRangeIndex, SlangInt bindingRangeIndexInSubObject);
SLANG_API SlangInt spReflectionTypeLayout_getSubObjectRangeDescriptorRangeBindingCount(SlangReflectionTypeLayout* typeLayout, SlangInt subObjectRangeIndex, SlangInt bindingRangeIndexInSubObject);
SLANG_API SlangInt spReflectionTypeLayout_getSubObjectRangeDescriptorRangeIndexOffset(SlangReflectionTypeLayout* typeLayout, SlangInt subObjectRangeIndex, SlangInt bindingRangeIndexInSubObject);
SLANG_API SlangInt spReflectionTypeLayout_getSubObjectRangeDescriptorRangeSpaceOffset(SlangReflectionTypeLayout* typeLayout, SlangInt subObjectRangeIndex, SlangInt bindingRangeIndexInSubObject);
#endif
SLANG_API char const* spReflectionVariable_GetName(SlangReflectionVariable* var);
SLANG_API SlangReflectionType* spReflectionVariable_GetType(SlangReflectionVariable* var);
SLANG_API SlangReflectionModifier* spReflectionVariable_FindModifier(
SlangReflectionVariable* var,
SlangModifierID modifierID);
SLANG_API unsigned int spReflectionVariable_GetUserAttributeCount(SlangReflectionVariable* var);
SLANG_API SlangReflectionUserAttribute* spReflectionVariable_GetUserAttribute(
SlangReflectionVariable* var,
unsigned int index);
SLANG_API SlangReflectionUserAttribute* spReflectionVariable_FindUserAttributeByName(
SlangReflectionVariable* var,
SlangSession* globalSession,
char const* name);
SLANG_API bool spReflectionVariable_HasDefaultValue(SlangReflectionVariable* inVar);
SLANG_API SlangResult
spReflectionVariable_GetDefaultValueInt(SlangReflectionVariable* inVar, int64_t* rs);
SLANG_API SlangReflectionGeneric* spReflectionVariable_GetGenericContainer(
SlangReflectionVariable* var);
SLANG_API SlangReflectionVariable* spReflectionVariable_applySpecializations(
SlangReflectionVariable* var,
SlangReflectionGeneric* generic);
SLANG_API SlangReflectionVariable* spReflectionVariableLayout_GetVariable(
SlangReflectionVariableLayout* var);
SLANG_API SlangReflectionTypeLayout* spReflectionVariableLayout_GetTypeLayout(
SlangReflectionVariableLayout* var);
SLANG_API size_t spReflectionVariableLayout_GetOffset(
SlangReflectionVariableLayout* var,
SlangParameterCategory category);
SLANG_API size_t spReflectionVariableLayout_GetSpace(
SlangReflectionVariableLayout* var,
SlangParameterCategory category);
SLANG_API char const* spReflectionVariableLayout_GetSemanticName(
SlangReflectionVariableLayout* var);
SLANG_API size_t
spReflectionVariableLayout_GetSemanticIndex(SlangReflectionVariableLayout* var);
SLANG_API SlangReflectionDecl* spReflectionFunction_asDecl(SlangReflectionFunction* func);
SLANG_API char const* spReflectionFunction_GetName(SlangReflectionFunction* func);
SLANG_API SlangReflectionModifier* spReflectionFunction_FindModifier(
SlangReflectionFunction* var,
SlangModifierID modifierID);
SLANG_API unsigned int spReflectionFunction_GetUserAttributeCount(
SlangReflectionFunction* func);
SLANG_API SlangReflectionUserAttribute* spReflectionFunction_GetUserAttribute(
SlangReflectionFunction* func,
unsigned int index);
SLANG_API SlangReflectionUserAttribute* spReflectionFunction_FindUserAttributeByName(
SlangReflectionFunction* func,
SlangSession* globalSession,
char const* name);
SLANG_API unsigned int spReflectionFunction_GetParameterCount(SlangReflectionFunction* func);
SLANG_API SlangReflectionVariable* spReflectionFunction_GetParameter(
SlangReflectionFunction* func,
unsigned index);
SLANG_API SlangReflectionType* spReflectionFunction_GetResultType(
SlangReflectionFunction* func);
SLANG_API SlangReflectionGeneric* spReflectionFunction_GetGenericContainer(
SlangReflectionFunction* func);
SLANG_API SlangReflectionFunction* spReflectionFunction_applySpecializations(
SlangReflectionFunction* func,
SlangReflectionGeneric* generic);
SLANG_API SlangReflectionFunction* spReflectionFunction_specializeWithArgTypes(
SlangReflectionFunction* func,
SlangInt argTypeCount,
SlangReflectionType* const* argTypes);
SLANG_API bool spReflectionFunction_isOverloaded(SlangReflectionFunction* func);
SLANG_API unsigned int spReflectionFunction_getOverloadCount(SlangReflectionFunction* func);
SLANG_API SlangReflectionFunction* spReflectionFunction_getOverload(
SlangReflectionFunction* func,
unsigned int index);
SLANG_API unsigned int spReflectionDecl_getChildrenCount(SlangReflectionDecl* parentDecl);
SLANG_API SlangReflectionDecl* spReflectionDecl_getChild(
SlangReflectionDecl* parentDecl,
unsigned int index);
SLANG_API char const* spReflectionDecl_getName(SlangReflectionDecl* decl);
SLANG_API SlangDeclKind spReflectionDecl_getKind(SlangReflectionDecl* decl);
SLANG_API SlangReflectionFunction* spReflectionDecl_castToFunction(SlangReflectionDecl* decl);
SLANG_API SlangReflectionVariable* spReflectionDecl_castToVariable(SlangReflectionDecl* decl);
SLANG_API SlangReflectionGeneric* spReflectionDecl_castToGeneric(SlangReflectionDecl* decl);
SLANG_API SlangReflectionType* spReflection_getTypeFromDecl(SlangReflectionDecl* decl);
SLANG_API SlangReflectionDecl* spReflectionDecl_getParent(SlangReflectionDecl* decl);
SLANG_API SlangReflectionDecl* spReflectionGeneric_asDecl(SlangReflectionGeneric* generic);
SLANG_API char const* spReflectionGeneric_GetName(SlangReflectionGeneric* generic);
SLANG_API unsigned int spReflectionGeneric_GetTypeParameterCount(
SlangReflectionGeneric* generic);
SLANG_API SlangReflectionVariable* spReflectionGeneric_GetTypeParameter(
SlangReflectionGeneric* generic,
unsigned index);
SLANG_API unsigned int spReflectionGeneric_GetValueParameterCount(
SlangReflectionGeneric* generic);
SLANG_API SlangReflectionVariable* spReflectionGeneric_GetValueParameter(
SlangReflectionGeneric* generic,
unsigned index);
SLANG_API unsigned int spReflectionGeneric_GetTypeParameterConstraintCount(
SlangReflectionGeneric* generic,
SlangReflectionVariable* typeParam);
SLANG_API SlangReflectionType* spReflectionGeneric_GetTypeParameterConstraintType(
SlangReflectionGeneric* generic,
SlangReflectionVariable* typeParam,
unsigned index);
SLANG_API SlangDeclKind spReflectionGeneric_GetInnerKind(SlangReflectionGeneric* generic);
SLANG_API SlangReflectionDecl* spReflectionGeneric_GetInnerDecl(
SlangReflectionGeneric* generic);
SLANG_API SlangReflectionGeneric* spReflectionGeneric_GetOuterGenericContainer(
SlangReflectionGeneric* generic);
SLANG_API SlangReflectionType* spReflectionGeneric_GetConcreteType(
SlangReflectionGeneric* generic,
SlangReflectionVariable* typeParam);
SLANG_API int64_t spReflectionGeneric_GetConcreteIntVal(
SlangReflectionGeneric* generic,
SlangReflectionVariable* valueParam);
SLANG_API SlangReflectionGeneric* spReflectionGeneric_applySpecializations(
SlangReflectionGeneric* currGeneric,
SlangReflectionGeneric* generic);
SLANG_API SlangStage spReflectionVariableLayout_getStage(SlangReflectionVariableLayout* var);
SLANG_API SlangReflectionVariableLayout* spReflectionVariableLayout_getPendingDataLayout(
SlangReflectionVariableLayout* var);
SLANG_API unsigned spReflectionParameter_GetBindingIndex(SlangReflectionParameter* parameter);
SLANG_API unsigned spReflectionParameter_GetBindingSpace(SlangReflectionParameter* parameter);
SLANG_API SlangResult spIsParameterLocationUsed(
SlangCompileRequest* request,
SlangInt entryPointIndex,
SlangInt targetIndex,
SlangParameterCategory category,
SlangUInt spaceIndex,
SlangUInt registerIndex,
bool& outUsed);
SLANG_API char const* spReflectionEntryPoint_getName(SlangReflectionEntryPoint* entryPoint);
SLANG_API char const* spReflectionEntryPoint_getNameOverride(
SlangReflectionEntryPoint* entryPoint);
SLANG_API SlangReflectionFunction* spReflectionEntryPoint_getFunction(
SlangReflectionEntryPoint* entryPoint);
SLANG_API unsigned spReflectionEntryPoint_getParameterCount(
SlangReflectionEntryPoint* entryPoint);
SLANG_API SlangReflectionVariableLayout* spReflectionEntryPoint_getParameterByIndex(
SlangReflectionEntryPoint* entryPoint,
unsigned index);
SLANG_API SlangStage spReflectionEntryPoint_getStage(SlangReflectionEntryPoint* entryPoint);
SLANG_API void spReflectionEntryPoint_getComputeThreadGroupSize(
SlangReflectionEntryPoint* entryPoint,
SlangUInt axisCount,
SlangUInt* outSizeAlongAxis);
SLANG_API void spReflectionEntryPoint_getComputeWaveSize(
SlangReflectionEntryPoint* entryPoint,
SlangUInt* outWaveSize);
SLANG_API int spReflectionEntryPoint_usesAnySampleRateInput(
SlangReflectionEntryPoint* entryPoint);
SLANG_API SlangReflectionVariableLayout* spReflectionEntryPoint_getVarLayout(
SlangReflectionEntryPoint* entryPoint);
SLANG_API SlangReflectionVariableLayout* spReflectionEntryPoint_getResultVarLayout(
SlangReflectionEntryPoint* entryPoint);
SLANG_API int spReflectionEntryPoint_hasDefaultConstantBuffer(
SlangReflectionEntryPoint* entryPoint);
SLANG_API char const* spReflectionTypeParameter_GetName(
SlangReflectionTypeParameter* typeParam);
SLANG_API unsigned spReflectionTypeParameter_GetIndex(SlangReflectionTypeParameter* typeParam);
SLANG_API unsigned spReflectionTypeParameter_GetConstraintCount(
SlangReflectionTypeParameter* typeParam);
SLANG_API SlangReflectionType* spReflectionTypeParameter_GetConstraintByIndex(
SlangReflectionTypeParameter* typeParam,
unsigned int index);
SLANG_API SlangResult spReflection_ToJson(
SlangReflection* reflection,
SlangCompileRequest* request,
ISlangBlob** outBlob);
SLANG_API unsigned spReflection_GetParameterCount(SlangReflection* reflection);
SLANG_API SlangReflectionParameter* spReflection_GetParameterByIndex(
SlangReflection* reflection,
unsigned index);
SLANG_API unsigned int spReflection_GetTypeParameterCount(SlangReflection* reflection);
SLANG_API SlangReflectionTypeParameter* spReflection_GetTypeParameterByIndex(
SlangReflection* reflection,
unsigned int index);
SLANG_API SlangReflectionTypeParameter* spReflection_FindTypeParameter(
SlangReflection* reflection,
char const* name);
SLANG_API SlangReflectionType* spReflection_FindTypeByName(
SlangReflection* reflection,
char const* name);
SLANG_API SlangReflectionTypeLayout* spReflection_GetTypeLayout(
SlangReflection* reflection,
SlangReflectionType* reflectionType,
SlangLayoutRules rules);
SLANG_API SlangReflectionFunction* spReflection_FindFunctionByName(
SlangReflection* reflection,
char const* name);
SLANG_API SlangReflectionFunction* spReflection_FindFunctionByNameInType(
SlangReflection* reflection,
SlangReflectionType* reflType,
char const* name);
SLANG_API SlangReflectionVariable* spReflection_FindVarByNameInType(
SlangReflection* reflection,
SlangReflectionType* reflType,
char const* name);
SLANG_API SlangUInt spReflection_getEntryPointCount(SlangReflection* reflection);
SLANG_API SlangReflectionEntryPoint* spReflection_getEntryPointByIndex(
SlangReflection* reflection,
SlangUInt index);
SLANG_API SlangReflectionEntryPoint* spReflection_findEntryPointByName(
SlangReflection* reflection,
char const* name);
SLANG_API SlangUInt spReflection_getGlobalConstantBufferBinding(SlangReflection* reflection);
SLANG_API size_t spReflection_getGlobalConstantBufferSize(SlangReflection* reflection);
SLANG_API SlangReflectionType* spReflection_specializeType(
SlangReflection* reflection,
SlangReflectionType* type,
SlangInt specializationArgCount,
SlangReflectionType* const* specializationArgs,
ISlangBlob** outDiagnostics);
SLANG_API SlangReflectionGeneric* spReflection_specializeGeneric(
SlangReflection* inProgramLayout,
SlangReflectionGeneric* generic,
SlangInt argCount,
SlangReflectionGenericArgType const* argTypes,
SlangReflectionGenericArg const* args,
ISlangBlob** outDiagnostics);
SLANG_API bool spReflection_isSubType(
SlangReflection* reflection,
SlangReflectionType* subType,
SlangReflectionType* superType);
SLANG_API SlangUInt spReflection_getHashedStringCount(SlangReflection* reflection);
SLANG_API const char* spReflection_getHashedString(
SlangReflection* reflection,
SlangUInt index,
size_t* outCount);
SLANG_API SlangUInt32 spComputeStringHash(const char* chars, size_t count);
SLANG_API SlangReflectionTypeLayout* spReflection_getGlobalParamsTypeLayout(
SlangReflection* reflection);
SLANG_API SlangReflectionVariableLayout* spReflection_getGlobalParamsVarLayout(
SlangReflection* reflection);
SLANG_API char const* spGetTranslationUnitSource(
SlangCompileRequest* request,
int translationUnitIndex);
#ifdef __cplusplus
}
#endif
#ifdef __cplusplus
SLANG_API slang::ISession* spReflection_GetSession(SlangReflection* reflection);
namespace slang
{
struct IComponentType;
struct IModule;
}
extern "C"
{
SLANG_API SlangResult
spCompileRequest_getProgram(SlangCompileRequest* request, slang::IComponentType** outProgram);
SLANG_API SlangResult spCompileRequest_getProgramWithEntryPoints(
SlangCompileRequest* request,
slang::IComponentType** outProgram);
SLANG_API SlangResult spCompileRequest_getEntryPoint(
SlangCompileRequest* request,
SlangInt entryPointIndex,
slang::IComponentType** outEntryPoint);
SLANG_API SlangResult spCompileRequest_getModule(
SlangCompileRequest* request,
SlangInt translationUnitIndex,
slang::IModule** outModule);
SLANG_API SlangResult
spCompileRequest_getSession(SlangCompileRequest* request, slang::ISession** outSession);
}
namespace slang
{
struct ICompileRequest : public ISlangUnknown
{
SLANG_COM_INTERFACE(
0x96d33993,
0x317c,
0x4db5,
{0xaf, 0xd8, 0x66, 0x6e, 0xe7, 0x72, 0x48, 0xe2})
virtual SLANG_NO_THROW void SLANG_MCALL setFileSystem(ISlangFileSystem* fileSystem) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setCompileFlags(SlangCompileFlags flags) = 0;
virtual SLANG_NO_THROW SlangCompileFlags SLANG_MCALL getCompileFlags() = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setDumpIntermediates(int enable) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setDumpIntermediatePrefix(const char* prefix) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setLineDirectiveMode(SlangLineDirectiveMode mode) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setCodeGenTarget(SlangCompileTarget target) = 0;
virtual SLANG_NO_THROW int SLANG_MCALL addCodeGenTarget(SlangCompileTarget target) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setTargetProfile(int targetIndex, SlangProfileID profile) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setTargetFlags(int targetIndex, SlangTargetFlags flags) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setTargetFloatingPointMode(int targetIndex, SlangFloatingPointMode mode) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setTargetMatrixLayoutMode(int targetIndex, SlangMatrixLayoutMode mode) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setMatrixLayoutMode(SlangMatrixLayoutMode mode) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setDebugInfoLevel(SlangDebugInfoLevel level) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setOptimizationLevel(SlangOptimizationLevel level) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setOutputContainerFormat(SlangContainerFormat format) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setPassThrough(SlangPassThrough passThrough) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setDiagnosticCallback(SlangDiagnosticCallback callback, void const* userData) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setWriter(SlangWriterChannel channel, ISlangWriter* writer) = 0;
virtual SLANG_NO_THROW ISlangWriter* SLANG_MCALL getWriter(SlangWriterChannel channel) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL addSearchPath(const char* searchDir) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
addPreprocessorDefine(const char* key, const char* value) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
processCommandLineArguments(char const* const* args, int argCount) = 0;
virtual SLANG_NO_THROW int SLANG_MCALL
addTranslationUnit(SlangSourceLanguage language, char const* name) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setDefaultModuleName(const char* defaultModuleName) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL addTranslationUnitPreprocessorDefine(
int translationUnitIndex,
const char* key,
const char* value) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
addTranslationUnitSourceFile(int translationUnitIndex, char const* path) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL addTranslationUnitSourceString(
int translationUnitIndex,
char const* path,
char const* source) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
addLibraryReference(const char* basePath, const void* libData, size_t libDataSize) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL addTranslationUnitSourceStringSpan(
int translationUnitIndex,
char const* path,
char const* sourceBegin,
char const* sourceEnd) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL addTranslationUnitSourceBlob(
int translationUnitIndex,
char const* path,
ISlangBlob* sourceBlob) = 0;
virtual SLANG_NO_THROW int SLANG_MCALL
addEntryPoint(int translationUnitIndex, char const* name, SlangStage stage) = 0;
virtual SLANG_NO_THROW int SLANG_MCALL addEntryPointEx(
int translationUnitIndex,
char const* name,
SlangStage stage,
int genericArgCount,
char const** genericArgs) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
setGlobalGenericArgs(int genericArgCount, char const** genericArgs) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
setTypeNameForGlobalExistentialTypeParam(int slotIndex, char const* typeName) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL setTypeNameForEntryPointExistentialTypeParam(
int entryPointIndex,
int slotIndex,
char const* typeName) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setAllowGLSLInput(bool value) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL compile() = 0;
virtual SLANG_NO_THROW char const* SLANG_MCALL getDiagnosticOutput() = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getDiagnosticOutputBlob(ISlangBlob** outBlob) = 0;
virtual SLANG_NO_THROW int SLANG_MCALL getDependencyFileCount() = 0;
virtual SLANG_NO_THROW char const* SLANG_MCALL getDependencyFilePath(int index) = 0;
virtual SLANG_NO_THROW int SLANG_MCALL getTranslationUnitCount() = 0;
virtual SLANG_NO_THROW char const* SLANG_MCALL getEntryPointSource(int entryPointIndex) = 0;
virtual SLANG_NO_THROW void const* SLANG_MCALL
getEntryPointCode(int entryPointIndex, size_t* outSize) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getEntryPointCodeBlob(int entryPointIndex, int targetIndex, ISlangBlob** outBlob) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL getEntryPointHostCallable(
int entryPointIndex,
int targetIndex,
ISlangSharedLibrary** outSharedLibrary) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getTargetCodeBlob(int targetIndex, ISlangBlob** outBlob) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getTargetHostCallable(int targetIndex, ISlangSharedLibrary** outSharedLibrary) = 0;
virtual SLANG_NO_THROW void const* SLANG_MCALL getCompileRequestCode(size_t* outSize) = 0;
virtual SLANG_NO_THROW ISlangMutableFileSystem* SLANG_MCALL
getCompileRequestResultAsFileSystem() = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL getContainerCode(ISlangBlob** outBlob) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
loadRepro(ISlangFileSystem* fileSystem, const void* data, size_t size) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL saveRepro(ISlangBlob** outBlob) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL enableReproCapture() = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getProgram(slang::IComponentType** outProgram) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getEntryPoint(SlangInt entryPointIndex, slang::IComponentType** outEntryPoint) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getModule(SlangInt translationUnitIndex, slang::IModule** outModule) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL getSession(slang::ISession** outSession) = 0;
virtual SLANG_NO_THROW SlangReflection* SLANG_MCALL getReflection() = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setCommandLineCompilerMode() = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
addTargetCapability(SlangInt targetIndex, SlangCapabilityID capability) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getProgramWithEntryPoints(slang::IComponentType** outProgram) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL isParameterLocationUsed(
SlangInt entryPointIndex,
SlangInt targetIndex,
SlangParameterCategory category,
SlangUInt spaceIndex,
SlangUInt registerIndex,
bool& outUsed) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setTargetLineDirectiveMode(SlangInt targetIndex, SlangLineDirectiveMode mode) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setTargetForceGLSLScalarBufferLayout(int targetIndex, bool forceScalarLayout) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
overrideDiagnosticSeverity(SlangInt messageID, SlangSeverity overrideSeverity) = 0;
virtual SLANG_NO_THROW SlangDiagnosticFlags SLANG_MCALL getDiagnosticFlags() = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setDiagnosticFlags(SlangDiagnosticFlags flags) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setDebugInfoFormat(SlangDebugInfoFormat debugFormat) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setEnableEffectAnnotations(bool value) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setReportDownstreamTime(bool value) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setReportPerfBenchmark(bool value) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setSkipSPIRVValidation(bool value) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setTargetUseMinimumSlangOptimization(int targetIndex, bool value) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setIgnoreCapabilityCheck(bool value) = 0;
virtual SLANG_NO_THROW SlangResult SLANG_MCALL
getCompileTimeProfile(ISlangProfiler** compileTimeProfile, bool shouldClear) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setTargetGenerateWholeProgram(int targetIndex, bool value) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL setTargetForceDXLayout(int targetIndex, bool value) = 0;
virtual SLANG_NO_THROW void SLANG_MCALL
setTargetEmbedDownstreamIR(int targetIndex, bool value) = 0;
};
#define SLANG_UUID_ICompileRequest ICompileRequest::getTypeGuid()
}
#endif