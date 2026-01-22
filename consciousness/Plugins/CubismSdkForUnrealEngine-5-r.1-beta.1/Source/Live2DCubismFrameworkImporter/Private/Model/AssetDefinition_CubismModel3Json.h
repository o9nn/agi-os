#pragma once
#include "Model/CubismModel3Json.h"
#include "AssetDefinitionDefault.h"
#include "AssetDefinition_CubismModel3Json.generated.h"
UCLASS()
class UAssetDefinition_CubismModel3Json : public UAssetDefinitionDefault
{
GENERATED_BODY()
public:
virtual FText GetAssetDisplayName() const override { return NSLOCTEXT("AssetTypeActions", "AssetTypeActions_CubismModel3Json", "CubismModel3Json"); }
virtual FLinearColor GetAssetColor() const override { return FLinearColor(FColor::Orange); }
virtual TSoftClassPtr<UObject> GetAssetClass() const override { return UCubismModel3Json::StaticClass(); }
virtual TConstArrayView<FAssetCategoryPath> GetAssetCategories() const override { return TArray<FAssetCategoryPath>(); }
virtual bool CanImport() const override { return true; }
};