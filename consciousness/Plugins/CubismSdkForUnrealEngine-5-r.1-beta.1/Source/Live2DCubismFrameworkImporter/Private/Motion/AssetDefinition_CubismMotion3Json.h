#pragma once
#include "Motion/CubismMotion3Json.h"
#include "AssetDefinitionDefault.h"
#include "AssetDefinition_CubismMotion3Json.generated.h"
UCLASS()
class UAssetDefinition_CubismMotion3Json : public UAssetDefinitionDefault
{
GENERATED_BODY()
public:
virtual FText GetAssetDisplayName() const override { return NSLOCTEXT("AssetTypeActions", "AssetTypeActions_CubismMotion3Json", "CubismMotion3Json"); }
virtual FLinearColor GetAssetColor() const override { return FLinearColor(FColor::Orange); }
virtual TSoftClassPtr<UObject> GetAssetClass() const override { return UCubismMotion3Json::StaticClass(); }
virtual TConstArrayView<FAssetCategoryPath> GetAssetCategories() const override { return TArray<FAssetCategoryPath>(); }
virtual bool CanImport() const override { return true; }
};