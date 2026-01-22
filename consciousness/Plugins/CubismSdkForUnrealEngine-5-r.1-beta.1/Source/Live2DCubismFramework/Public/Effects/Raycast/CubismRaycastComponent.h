#pragma once
#include "CubismUpdatableInterface.h"
#include "Effects/Raycast/CubismRaycastParameter.h"
#include "Components/ActorComponent.h"
#include "CubismRaycastComponent.generated.h"
class UCubismModelComponent;
class UCubismDrawableComponent;
class UCubismModel3Json;
USTRUCT(BlueprintType)
struct FCubismRaycastHit
{
GENERATED_USTRUCT_BODY()
public:
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TObjectPtr<UCubismDrawableComponent> Drawable;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
float Distance = INFINITY;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
FVector GlobalPosition = FVector::ZeroVector;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
FVector LocalPosition = FVector::ZeroVector;
};
UCLASS( ClassGroup=(Custom), meta=(BlueprintSpawnableComponent) )
class LIVE2DCUBISMFRAMEWORK_API UCubismRaycastComponent : public UActorComponent, public ICubismUpdatableInterface
{
GENERATED_BODY()
public:
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TObjectPtr<UCubismModel3Json> Json;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TArray<FCubismRaycastParameter> Parameters;
virtual bool IsControlledByUpdateController() const override { return true; }
virtual int32 GetExecutionOrder() const override;
virtual void OnCubismUpdate(float DeltaTime) override;
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup(UCubismModelComponent* InModel);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Raycast(const FVector Origin, const FVector Direction, TArray<FCubismRaycastHit>& Result, const float Length = 10000.0f) const;
private:
friend class UCubismModelComponent;
UPROPERTY()
TObjectPtr<UCubismModelComponent> Model;
private:
UCubismRaycastComponent();
bool RaycastDrawable(
const FVector Origin, const FVector Dir, const float Length,
const FTransform& Transform,
const ECubismRaycastPrecision& Precision,
const UCubismDrawableComponent* Drawable,
FVector& HitPosition, FVector& HitNormal, float& HitTime
) const;
static bool RayIntersectMesh(
const FVector Origin, const FVector Dir, const float Length,
const TArray<FVector> Positions, const TArray<int32> Indices,
FVector& HitPosition, float& HitTime
);
static bool RayIntersectTriangle
(
const FVector Origin, const FVector Dir, const float Length,
const FVector T0, const FVector T1, const FVector T2,
FVector& HitPosition, float& HitTime
);
public:
virtual void PostLoad() override;
virtual void OnComponentCreated() override;
virtual void OnComponentDestroyed(bool bDestroyingHierarchy) override;
#if WITH_EDITOR
virtual void PostEditUndo() override;
#endif
};