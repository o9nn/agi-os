#pragma once
#include "Components/ActorComponent.h"
#include "CubismParameterStoreComponent.generated.h"
class UCubismModelComponent;
UCLASS(Blueprintable)
class LIVE2DCUBISMFRAMEWORK_API UCubismParameterStoreComponent : public UActorComponent
{
GENERATED_BODY()
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup(UCubismModelComponent* InModel);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void SaveParameterValue(const int32 ParameterIndex);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void SavePartOpacity(const int32 PartIndex);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void SaveParameters();
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void LoadParameters();
private:
friend class UCubismModelComponent;
UPROPERTY()
TObjectPtr<UCubismModelComponent> Model;
private:
UCubismParameterStoreComponent();
TMap<int32, float> ParameterValues;
TMap<int32, float> PartOpacities;
public:
virtual void PostLoad() override;
virtual void OnComponentCreated() override;
virtual void OnComponentDestroyed(bool bDestroyingHierarchy) override;
#if WITH_EDITOR
virtual void PostEditUndo() override;
#endif
virtual void TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction) override;
};