#include "Live2DCubismFrameworkBPLibrary.h"
#include "Engine/World.h"
#include "Engine/Engine.h"
ACubismModel* ULive2DCubismFrameworkBPLibrary::SpawnCubismModel(
UObject* WorldContextObject,
UCubismModel3Json* Model3Json,
const FTransform& Transform,
const bool bRenderInWorldSpace,
UTextureRenderTarget2D* RenderTarget
)
{
if (!WorldContextObject || !Model3Json)
{
UE_LOG(LogTemp, Warning, TEXT("SpawnCubismModelFromJson: Invalid context or model asset."));
return nullptr;
}
UWorld* World = GEngine->GetWorldFromContextObjectChecked(WorldContextObject);
if (!World)
{
return nullptr;
}
ACubismModel* ModelActor = World->SpawnActor<ACubismModel>(ACubismModel::StaticClass());
if (!ModelActor)
{
return nullptr;
}
ModelActor->Initialize(Model3Json);
ModelActor->SetActorTransform(Transform);
ModelActor->Model->bRenderInWorldSpace = bRenderInWorldSpace;
ModelActor->Model->SetVisibility(bRenderInWorldSpace, true);
ModelActor->Model->RenderTarget = RenderTarget;
return ModelActor;
}