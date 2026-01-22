#pragma once
#include "Components/ActorComponent.h"
#include "CubismHarmonicMotionComponent.generated.h"
class UCubismModelComponent;
struct FCubismHarmonicMotionParameter;
UCLASS( ClassGroup=(Custom), meta=(BlueprintSpawnableComponent) )
class LIVE2DCUBISMFRAMEWORK_API UCubismHarmonicMotionComponent : public UActorComponent
{
GENERATED_BODY()
public:
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TArray<FCubismHarmonicMotionParameter> Parameters;
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup(UCubismModelComponent* InModel);
private:
friend class UCubismModelComponent;
UPROPERTY()
TObjectPtr<UCubismModelComponent> Model;
private:
UCubismHarmonicMotionComponent();
float Time;
public:
virtual void PostLoad() override;
virtual void OnComponentCreated() override;
virtual void OnComponentDestroyed(bool bDestroyingHierarchy) override;
#if WITH_EDITOR
virtual void PostEditUndo() override;
#endif
virtual void TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction) override;
};