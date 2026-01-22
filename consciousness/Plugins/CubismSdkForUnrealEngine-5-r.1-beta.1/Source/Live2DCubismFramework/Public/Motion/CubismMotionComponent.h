#pragma once
#include "Motion/CubismMotion3Json.h"
#include "CubismUpdatableInterface.h"
#include "Components/ActorComponent.h"
#include "CubismMotionComponent.generated.h"
class UCubismModelComponent;
class FCubismMotion;
DECLARE_DYNAMIC_MULTICAST_DELEGATE(FCubismMotionPlaybackFinishedHandler);
UENUM()
enum class ECubismMotionPriority : uint8
{
None,
Idle,
Normal,
Force,
};
UCLASS( ClassGroup=(Custom), meta=(BlueprintSpawnableComponent) )
class LIVE2DCUBISMFRAMEWORK_API UCubismMotionComponent : public UActorComponent, public ICubismUpdatableInterface
{
GENERATED_BODY()
public:
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
int32 Index = -1;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TArray<TObjectPtr<UCubismMotion3Json>> Jsons;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta = (ClampMin = "0.0", SliderMin = "0.0", SliderMax = "10.0"))
float Speed = 1.0f;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
ECubismMotionPriority CurrentPriority = ECubismMotionPriority::None;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
ECubismMotionPriority ReservedPriority = ECubismMotionPriority::None;
UPROPERTY(BlueprintAssignable, Category = "Live2D Cubism")
FCubismMotionPlaybackFinishedHandler OnMotionPlaybackFinished;
virtual bool IsControlledByUpdateController() const override { return true; }
virtual int32 GetExecutionOrder() const override;
virtual void OnCubismUpdate(float DeltaTime) override;
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup(UCubismModelComponent* InModel);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
bool IsFinished() const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
bool ReserveMotion(const ECubismMotionPriority Priority);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void PlayMotion(const int32 InIndex, const float OffsetTime=0.0f, const ECubismMotionPriority Priority=ECubismMotionPriority::Normal);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void StopAllMotions(const bool bForce = false);
private:
friend class UCubismModelComponent;
UPROPERTY()
TObjectPtr<UCubismModelComponent> Model;
private:
UCubismMotionComponent();
float Time;
TArray<TSharedPtr<FCubismMotion>> MotionQueue;
void UpdateMotion(float UserTimeSeconds, float FadeWeight, const TSharedPtr<FCubismMotion>& CubismMotion);
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