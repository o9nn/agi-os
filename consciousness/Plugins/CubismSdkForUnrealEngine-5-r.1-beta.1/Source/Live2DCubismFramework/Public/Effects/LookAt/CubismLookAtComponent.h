#pragma once
#include "Components/ActorComponent.h"
#include "CubismLookAtComponent.generated.h"
class UCubismModelComponent;
struct FCubismLookAtParameter;
UCLASS(ClassGroup = (Custom), meta = (BlueprintSpawnableComponent))
class LIVE2DCUBISMFRAMEWORK_API UCubismLookAtComponent : public UActorComponent
{
GENERATED_BODY()
public:
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TObjectPtr<AActor> Target;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TArray<FCubismLookAtParameter> Parameters;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta = (ClampMin = "0.0001", ClampMax = "1.0", SliderMin = "0.0001", SliderMax = "1.0"))
float Smoothing = 0.15f;
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup(UCubismModelComponent* InModel);
private:
friend class UCubismModelComponent;
UPROPERTY()
TObjectPtr<UCubismModelComponent> Model;
private:
UCubismLookAtComponent();
FVector LastPosition;
FVector CurrentVelocity;
FVector SmoothDamp(const FVector CurrentValue, const float DeltaTime);
public:
virtual void PostLoad() override;
virtual void OnComponentCreated() override;
virtual void OnComponentDestroyed(bool bDestroyingHierarchy) override;
#if WITH_EDITOR
virtual void PostEditUndo() override;
#endif
virtual void TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction) override;
};