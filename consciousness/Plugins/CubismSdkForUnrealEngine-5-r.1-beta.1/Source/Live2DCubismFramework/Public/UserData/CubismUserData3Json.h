#pragma once
#include "EditorFramework/AssetImportData.h"
#include "CubismUserData3Json.generated.h"
UENUM(BlueprintType)
enum class ECubismUserDataTargetType : uint8
{
ArtMesh,
};
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FCubismUserDataEntry
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "User Data")
TMap<FString, FString> Tags;
};
UCLASS(BlueprintType)
class LIVE2DCUBISMFRAMEWORK_API UCubismUserData3Json : public UObject
{
GENERATED_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "User Data")
int32 Size;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "User Data")
TMap<ECubismUserDataTargetType, FCubismUserDataEntry> Data;
virtual void PostInitProperties() override;
#if WITH_EDITORONLY_DATA
UPROPERTY(VisibleAnywhere, Instanced, Category=ImportSettings)
TObjectPtr<class UAssetImportData> AssetImportData;
virtual void GetAssetRegistryTags(TArray<FAssetRegistryTag>& OutTags) const override;
virtual void Serialize(FArchive& Ar) override;
#endif
};