#pragma once
#include "ActorFactories/ActorFactory.h"
#include "CubismModelActorFactory.generated.h"
class UTexture2D;
UCLASS()
class UCubismModelActorFactory : public UActorFactory
{
GENERATED_BODY()
public:
UCubismModelActorFactory();
public:
virtual void PostSpawnActor(UObject* Asset, AActor* NewActor) override;
virtual bool CanCreateActorFrom(const FAssetData& AssetData, FText& OutErrorMsg) override;
};