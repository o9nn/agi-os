#pragma once
#include "DisplayInfo/CubismDisplayInfo3Json.h"
#include "AssetDefinitionDefault.h"
#include "AssetDefinition_CubismDisplayInfo3Json.generated.h"
UCLASS()
class UAssetDefinition_CubismDisplayInfo3Json : public UAssetDefinitionDefault
{
GENERATED_BODY()
public:
virtual FText GetAssetDisplayName() const override { return NSLOCTEXT("AssetTypeActions", "AssetTypeActions_CubismDisplayInfo3Json", "CubismDisplayInfo3Json"); }
virtual FLinearColor GetAssetColor() const override { return FLinearColor(FColor::Orange); }
virtual TSoftClassPtr<UObject> GetAssetClass() const override { return UCubismDisplayInfo3Json::StaticClass(); }
virtual TConstArrayView<FAssetCategoryPath> GetAssetCategories() const override { return TArray<FAssetCategoryPath>(); }
virtual bool CanImport() const override { return true; }
};