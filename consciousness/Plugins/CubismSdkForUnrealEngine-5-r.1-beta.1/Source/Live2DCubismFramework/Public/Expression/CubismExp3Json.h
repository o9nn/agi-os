#pragma once
#include "Model/CubismModelComponent.h"
#include "EditorFramework/AssetImportData.h"
#include "CubismExp3Json.generated.h"
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FCubismExpressionParameter
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Parameter")
FString Id;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Parameter")
float Value;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Parameter")
ECubismParameterBlendMode Blend;
FCubismExpressionParameter()
: Id()
, Value(0.0f)
, Blend(ECubismParameterBlendMode::Additive)
{ }
};
UCLASS(BlueprintType)
class LIVE2DCUBISMFRAMEWORK_API UCubismExp3Json : public UObject
{
GENERATED_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Expression Data")
FString Type;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Expression Data")
float FadeInTime;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Expression Data")
float FadeOutTime;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Expression Data")
TArray<FCubismExpressionParameter> Parameters;
virtual void PostInitProperties() override;
#if WITH_EDITORONLY_DATA
UPROPERTY(VisibleAnywhere, Instanced, Category=ImportSettings)
TObjectPtr<class UAssetImportData> AssetImportData;
virtual void GetAssetRegistryTags(TArray<FAssetRegistryTag>& OutTags) const override;
virtual void Serialize(FArchive& Ar) override;
#endif
};