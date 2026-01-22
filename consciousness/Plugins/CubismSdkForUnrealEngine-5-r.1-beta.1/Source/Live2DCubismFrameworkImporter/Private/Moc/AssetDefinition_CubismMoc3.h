#pragma once
#include "Model/CubismMoc3.h"
#include "AssetDefinitionDefault.h"
#include "AssetDefinition_CubismMoc3.generated.h"
UCLASS()
class UAssetDefinition_CubismMoc3 : public UAssetDefinitionDefault
{
GENERATED_BODY()
public:
virtual FText GetAssetDisplayName() const override { return NSLOCTEXT("AssetTypeActions", "AssetTypeActions_CubismMoc3", "CubismMoc3"); }
virtual FLinearColor GetAssetColor() const override { return FLinearColor(FColor::Orange); }
virtual TSoftClassPtr<UObject> GetAssetClass() const override { return UCubismMoc3::StaticClass(); }
virtual TConstArrayView<FAssetCategoryPath> GetAssetCategories() const override { return TArray<FAssetCategoryPath>(); }
virtual bool CanImport() const override { return true; }
};