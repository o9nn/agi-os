#pragma once
#include "Physics/CubismPhysics3Json.h"
#include "AssetDefinitionDefault.h"
#include "AssetDefinition_CubismPhysics3Json.generated.h"
UCLASS()
class UAssetDefinition_CubismPhysics3Json : public UAssetDefinitionDefault
{
GENERATED_BODY()
public:
virtual FText GetAssetDisplayName() const override { return NSLOCTEXT("AssetTypeActions", "AssetTypeActions_CubismPhysics3Json", "CubismPhysics3Json"); }
virtual FLinearColor GetAssetColor() const override { return FLinearColor(FColor::Orange); }
virtual TSoftClassPtr<UObject> GetAssetClass() const override { return UCubismPhysics3Json::StaticClass(); }
virtual TConstArrayView<FAssetCategoryPath> GetAssetCategories() const override { return TArray<FAssetCategoryPath>(); }
virtual bool CanImport() const override { return true; }
};