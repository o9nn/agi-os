#pragma once
#include "Components/ActorComponent.h"
#include "CubismUpdatableInterface.h"
#include "CubismPoseComponent.generated.h"
class UCubismModelComponent;
class UCubismParameterComponent;
class UCubismPartComponent;
class UCubismPose3Json;
struct FCubismPosePartParameter
{
TObjectPtr<UCubismPartComponent> Part;
TObjectPtr<UCubismParameterComponent> Parameter;
TArray<TObjectPtr<UCubismPartComponent>> LinkParts;
};
struct FCubismPosePartGroupParameter
{
TArray<FCubismPosePartParameter> Parts;
};
UCLASS(ClassGroup = (Custom), meta = (BlueprintSpawnableComponent, ImplementsInterface = "CubismUpdatableInterface"))
class LIVE2DCUBISMFRAMEWORK_API UCubismPoseComponent : public UActorComponent, public ICubismUpdatableInterface
{
GENERATED_BODY()
public:
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta = (ClampMin = "0.01", ClampMax = "10.0", SliderMin = "0.0", SliderMax = "10.0"))
float FadeInTime;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TObjectPtr<UCubismPose3Json> Json;
virtual bool IsControlledByUpdateController() const override { return true; }
virtual int32 GetExecutionOrder() const override;
virtual void OnCubismUpdate(float DeltaTime) override;
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup(UCubismModelComponent* InModel);
private:
friend class UCubismModelComponent;
UPROPERTY()
TObjectPtr<UCubismModelComponent> Model;
private:
UCubismPoseComponent();
void DoFade(float DeltaTime);
void CopyPartOpacities();
TArray<FCubismPosePartGroupParameter> PartGroups;
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
#if WITH_EDITORONLY_DATA
UPROPERTY(EditAnywhere, Category = "Live2D Cubism")
bool bEnablePoseInEditor = false;
#endif
};