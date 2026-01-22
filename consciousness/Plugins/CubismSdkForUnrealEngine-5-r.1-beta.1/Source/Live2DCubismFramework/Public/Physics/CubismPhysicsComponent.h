#pragma once
#include "CubismUpdatableInterface.h"
#include "Physics/CubismPhysicsRig.h"
#include "Components/ActorComponent.h"
#include "CubismPhysicsComponent.generated.h"
class UCubismModelComponent;
UCLASS( ClassGroup=(Custom), meta=(BlueprintSpawnableComponent) )
class LIVE2DCUBISMFRAMEWORK_API UCubismPhysicsComponent : public UActorComponent, public ICubismUpdatableInterface
{
GENERATED_BODY()
public:
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TObjectPtr<UCubismPhysics3Json> Json;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
FVector2D Gravity = FVector2D(0.0f, -1.0f);
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
FVector2D Wind = FVector2D::ZeroVector;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta = (ClampMin = "0.0", UIMin = "0.0", UIMax = "120.0"))
float Fps = 0.0f;
virtual bool IsControlledByUpdateController() const override { return true; }
virtual int32 GetExecutionOrder() const override;
virtual void OnCubismUpdate(float DeltaTime) override;
#if WITH_EDITORONLY_DATA
UPROPERTY(EditAnywhere, Category = "Live2D Cubism")
bool bEnablePhysicsInEditor = false;
#endif
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup(UCubismModelComponent* InModel);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Stabilization();
private:
friend class UCubismModelComponent;
UPROPERTY()
TObjectPtr<UCubismModelComponent> Model;
private:
UCubismPhysicsComponent();
TArray<FCubismPhysicsRig> Rigs;
float CurrentRemainTime;
TArray<float> ParameterCaches;
TArray<float> ParameterInputCaches;
void Initialize();
void UpdateParticles(
TArray<FCubismPhysicsRigParticle>& Particles,
const FVector2D TotalTranslation,
const float TotalAngle,
const float ThresholdValue,
const float DeltaTime,
const float Resistance
);
void UpdateParticlesForStabilization(
TArray<FCubismPhysicsRigParticle>& Particles,
const FVector2D TotalTranslation,
const float TotalAngle,
const float ThresholdValue
);
public:
virtual void PostLoad() override;
#if WITH_EDITORONLY_DATA
virtual void PostEditChangeProperty(struct FPropertyChangedEvent& PropertyChangedEvent) override;
#endif
virtual void OnComponentCreated() override;
virtual void OnComponentDestroyed(bool bDestroyingHierarchy) override;
#if WITH_EDITOR
virtual void PostEditUndo() override;
#endif
virtual void TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction) override;
};