#pragma once
#include "GlobalShader.h"
#include "ShaderParameterStruct.h"
#include "ShaderParameterUtils.h"
#include "TextureResource.h"
#include "DataDrivenShaderPlatformInfo.h"
#include "Runtime/Launch/Resources/Version.h"
struct FCubismMeshMaskVertex
{
FVector2f Position;
FVector2f UV;
};
struct FMaskDrawInfo
{
TArray<uint16> Indices;
TArray<FCubismMeshMaskVertex> Vertices;
FVector4 Offset;
FVector4 Channel;
FTexture* MainTexture;
};
void DrawCubismMeshMask_RenderThread(FRHICommandList& RHICmdList, FTextureRenderTargetResource* RenderTargetResource, const TArray<FMaskDrawInfo>& MaskDrawInfos);
class FCubismMeshMaskVS : public FGlobalShader
{
DECLARE_GLOBAL_SHADER(FCubismMeshMaskVS);
FCubismMeshMaskVS() {}
FCubismMeshMaskVS(const ShaderMetaType::CompiledShaderInitializerType& Initializer)
: FGlobalShader(Initializer)
{
Offset.Bind(Initializer.ParameterMap, TEXT("Offset"));
}
static bool ShouldCompilePermutation(const FGlobalShaderPermutationParameters& Parameters)
{
return IsFeatureLevelSupported(Parameters.Platform, ERHIFeatureLevel::SM5);
}
#if ENGINE_MAJOR_VERSION == 5 && ENGINE_MINOR_VERSION >= 3
void SetParameters(FRHIBatchedShaderParameters& BatchedParameters, const FVector4& InOffset)
{
SetShaderValue(BatchedParameters, Offset, (FVector4f)InOffset);
}
#else
template<typename TShaderRHIParamRef>
void SetParameters(FRHICommandList& RHICmdList, const TShaderRHIParamRef ShaderRHI, const FVector4& InOffset)
{
SetShaderValue(RHICmdList, ShaderRHI, Offset, (FVector4f)InOffset);
}
#endif
private:
LAYOUT_FIELD(FShaderParameter, Offset);
};
class FCubismMeshMaskPS : public FGlobalShader
{
DECLARE_GLOBAL_SHADER(FCubismMeshMaskPS);
SHADER_USE_PARAMETER_STRUCT(FCubismMeshMaskPS, FGlobalShader);
BEGIN_SHADER_PARAMETER_STRUCT(FParameters, )
SHADER_PARAMETER(FVector4f, Channel)
SHADER_PARAMETER_TEXTURE(Texture2D, MainTexture)
SHADER_PARAMETER_SAMPLER(SamplerState, MainSampler)
END_SHADER_PARAMETER_STRUCT()
static bool ShouldCompilePermutation(const FGlobalShaderPermutationParameters& Parameters)
{
return IsFeatureLevelSupported(Parameters.Platform, ERHIFeatureLevel::SM5);
}
};