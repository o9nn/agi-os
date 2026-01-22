#pragma once
#include "EditorFramework/AssetImportData.h"
#include "CubismModel3Json.generated.h"
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FExpressionEntry
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
FString Name;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
FString Path;
};
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FMotionGroupEntry
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
FString Name;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
TArray<FString> Paths;
};
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FHitAreaEntry
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
FString Id;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
FString Name;
};
UCLASS(BlueprintType)
class LIVE2DCUBISMFRAMEWORK_API UCubismModel3Json : public UObject
{
GENERATED_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
int32 Version;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
FString MocPath;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
TArray<FString> TexturePaths;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
FString PhysicsPath;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
FString PosePath;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
FString DisplayInfoPath;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
TArray<FExpressionEntry> Expressions;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
TArray<FMotionGroupEntry> Motions;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
FString UserDataPath;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
TArray<FString> EyeBlinks;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
TArray<FString> LipSyncs;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Model Data")
TArray<FHitAreaEntry> HitAreas;
virtual void PostInitProperties() override;
#if WITH_EDITORONLY_DATA
UPROPERTY(VisibleAnywhere, Instanced, Category=ImportSettings)
TObjectPtr<class UAssetImportData> AssetImportData;
virtual void GetAssetRegistryTags(TArray<FAssetRegistryTag>& OutTags) const override;
virtual void Serialize(FArchive& Ar) override;
#endif
};