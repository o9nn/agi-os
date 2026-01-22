#pragma once
#include "EditorFramework/AssetImportData.h"
#include "CubismPose3Json.generated.h"
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FCubismPosePart
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Part")
FString Id;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Part")
TArray<FString> Links;
};
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FCubismPosePartGroup
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Part Group")
TArray<FCubismPosePart> Parts;
};
UCLASS(BlueprintType)
class LIVE2DCUBISMFRAMEWORK_API UCubismPose3Json : public UObject
{
GENERATED_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Pose Data")
int32 Version;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Pose Data")
float FadeInTime;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Pose Data")
TArray<FCubismPosePartGroup> PartGroups;
virtual void PostInitProperties() override;
#if WITH_EDITORONLY_DATA
UPROPERTY(VisibleAnywhere, Instanced, Category=ImportSettings)
TObjectPtr<class UAssetImportData> AssetImportData;
virtual void GetAssetRegistryTags(TArray<FAssetRegistryTag>& OutTags) const override;
virtual void Serialize(FArchive& Ar) override;
#endif
};