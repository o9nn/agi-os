#pragma once
#include "UserData/CubismUserData3Json.h"
#include "AssetDefinitionDefault.h"
#include "AssetDefinition_CubismUserData3Json.generated.h"
UCLASS()
class UAssetDefinition_CubismUserData3Json : public UAssetDefinitionDefault
{
GENERATED_BODY()
public:
virtual FText GetAssetDisplayName() const override { return NSLOCTEXT("AssetTypeActions", "AssetTypeActions_CubismUserData3Json", "CubismUserData3Json"); }
virtual FLinearColor GetAssetColor() const override { return FLinearColor(FColor::Orange); }
virtual TSoftClassPtr<UObject> GetAssetClass() const override { return UCubismUserData3Json::StaticClass(); }
virtual TConstArrayView<FAssetCategoryPath> GetAssetCategories() const override { return TArray<FAssetCategoryPath>(); }
virtual bool CanImport() const override { return true; }
};