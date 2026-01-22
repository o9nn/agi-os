#include "CubismModelActorFactory.h"
#include "Model/CubismModelActor.h"
UCubismModelActorFactory::UCubismModelActorFactory()
{
DisplayName = NSLOCTEXT("Live2D Cubism Framework", "CubismModelFactoryDisplayName", "Add Cubism Model");
NewActorClass = ACubismModel::StaticClass();
}
void UCubismModelActorFactory::PostSpawnActor(UObject* Asset, AActor* NewActor)
{
Super::PostSpawnActor(Asset, NewActor);
if (const TObjectPtr<UCubismModel3Json>& Model3Json = Cast<UCubismModel3Json>(Asset))
{
ACubismModel* ModelActor = CastChecked<ACubismModel>(NewActor);
ModelActor->Initialize(Model3Json);
}
NewActor->SetActorLocation(GEditor->ClickLocation);
}
bool UCubismModelActorFactory::CanCreateActorFrom(const FAssetData& AssetData, FText& OutErrorMsg)
{
return AssetData.IsValid() && AssetData.IsInstanceOf(UCubismModel3Json::StaticClass());
}