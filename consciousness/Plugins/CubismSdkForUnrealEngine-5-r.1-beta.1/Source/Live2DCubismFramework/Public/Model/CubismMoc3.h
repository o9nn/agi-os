#pragma once
#include "Live2DCubismCore.h"
#include "EditorFramework/AssetImportData.h"
#include "CubismMoc3.generated.h"
typedef void (*CubismLogFunction)(const char* message);
UCLASS(Blueprintable)
class LIVE2DCUBISMFRAMEWORK_API UCubismMoc3 : public UObject
{
GENERATED_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Moc Data")
int32 Version;
public:
UFUNCTION(BlueprintCallable, Category = "Moc Data")
void SetupModel(UCubismModelComponent* InModel);
UFUNCTION(BlueprintCallable, Category = "Moc Data")
void DeleteModel(UCubismModelComponent* InModel);
UFUNCTION(BlueprintCallable, Category = "Moc Data")
void Setup();
UFUNCTION(BlueprintCallable, Category = "Moc Data")
static int32 GetVersion();
UFUNCTION(BlueprintCallable, Category = "Moc Data")
static int32 GetLatestMocVersion();
static int32 GetMocVersion(const void* Address, const int32 Size);
static bool HasMocConsistency(void* Address, const int32 Size);
CubismLogFunction GetLogFunction() const;
void SetLogFunction(CubismLogFunction LogFunction);
UFUNCTION(BlueprintCallable, Category = "Moc Data")
int32 GetSizeOfModel() const;
private:
friend class UCubismMoc3Factory;
UPROPERTY(SaveGame)
TArray<uint8> Bytes;
private:
csmMoc* RawMoc;
public:
virtual void PostLoad() override;
virtual void PostInitProperties() override;
#if WITH_EDITORONLY_DATA
UPROPERTY(VisibleAnywhere, Instanced, Category=ImportSettings)
TObjectPtr<class UAssetImportData> AssetImportData;
virtual void GetAssetRegistryTags(TArray<FAssetRegistryTag>& OutTags) const override;
virtual void Serialize(FArchive& Ar) override;
#endif
};