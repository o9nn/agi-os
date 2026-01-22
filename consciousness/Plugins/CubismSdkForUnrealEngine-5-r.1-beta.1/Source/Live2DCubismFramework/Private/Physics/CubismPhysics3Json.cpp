#include "Physics/CubismPhysics3Json.h"
void UCubismPhysics3Json::PostInitProperties()
{
#if WITH_EDITORONLY_DATA
if (!HasAnyFlags(RF_ClassDefaultObject))
{
AssetImportData = NewObject<UAssetImportData>(this, TEXT("AssetImportData"));
}
#endif
Super::PostInitProperties();
}
#if WITH_EDITORONLY_DATA
void UCubismPhysics3Json::GetAssetRegistryTags(TArray<FAssetRegistryTag>& OutTags) const
{
if (AssetImportData)
{
OutTags.Add( FAssetRegistryTag(SourceFileTagName(), AssetImportData->GetSourceData().ToJson(), FAssetRegistryTag::TT_Hidden) );
}
Super::GetAssetRegistryTags(OutTags);
}
void UCubismPhysics3Json::Serialize(FArchive& Ar)
{
Super::Serialize(Ar);
if (Ar.IsLoading() && Ar.UEVer() < VER_UE4_ASSET_IMPORT_DATA_AS_JSON && !AssetImportData)
{
AssetImportData = NewObject<UAssetImportData>(this, TEXT("AssetImportData"));
}
}
#endif