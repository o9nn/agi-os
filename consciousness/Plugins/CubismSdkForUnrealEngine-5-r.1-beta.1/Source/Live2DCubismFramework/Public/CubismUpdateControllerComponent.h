#pragma once
#include "Components/ActorComponent.h"
#include "CubismUpdateControllerComponent.generated.h"
class ICubismUpdatableInterface;
UCLASS(ClassGroup = (Custom), meta = (BlueprintSpawnableComponent))
class LIVE2DCUBISMFRAMEWORK_API UCubismUpdateControllerComponent : public UActorComponent
{
GENERATED_BODY()
public:
UCubismUpdateControllerComponent();
protected:
virtual void BeginPlay() override;
virtual void OnComponentCreated() override;
public:
virtual void TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction) override;
void RefreshUpdatables();
private:
UPROPERTY()
TArray<TScriptInterface<ICubismUpdatableInterface>> Updatables;
};