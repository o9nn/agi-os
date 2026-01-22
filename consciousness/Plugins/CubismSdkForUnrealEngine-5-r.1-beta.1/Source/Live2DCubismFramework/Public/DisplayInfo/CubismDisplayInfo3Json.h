#pragma once
#include "EditorFramework/AssetImportData.h"
#include "CubismDisplayInfo3Json.generated.h"
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FCubismDisplayInfoParameter
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Parameter")
FString Id;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Parameter")
FString GroupId;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Parameter")
FText Name;
};
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FCubismDisplayInfoParameterGroup
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "ParameterGroup")
FString Id;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "ParameterGroup")
FString GroupId;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "ParameterGroup")
FText Name;
};
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FCubismDisplayInfoPart
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Part")
FString Id;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Part")
FText Name;
};
UCLASS(BlueprintType)
class LIVE2DCUBISMFRAMEWORK_API UCubismDisplayInfo3Json : public UObject
{
GENERATED_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Display Info Data")
TArray<FCubismDisplayInfoParameter> Parameters;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Display Info Data")
TArray<FCubismDisplayInfoParameterGroup> ParameterGroups;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Display Info Data")
TArray<FCubismDisplayInfoPart> Parts;
virtual void PostInitProperties() override;
#if WITH_EDITORONLY_DATA
UPROPERTY(VisibleAnywhere, Instanced, Category=ImportSettings)
TObjectPtr<class UAssetImportData> AssetImportData;
virtual void GetAssetRegistryTags(TArray<FAssetRegistryTag>& OutTags) const override;
virtual void Serialize(FArchive& Ar) override;
#endif
};