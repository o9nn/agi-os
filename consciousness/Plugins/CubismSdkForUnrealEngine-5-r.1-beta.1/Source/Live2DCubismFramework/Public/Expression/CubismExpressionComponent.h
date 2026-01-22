#pragma once
#include "Expression/CubismExp3Json.h"
#include "CubismUpdatableInterface.h"
#include "CubismExpressionComponent.generated.h"
class FCubismExpression;
class UCubismModelComponent;
DECLARE_DYNAMIC_MULTICAST_DELEGATE(FCubismExpressionPlaybackFinishedHandler);
USTRUCT(Blueprintable)
struct LIVE2DCUBISMFRAMEWORK_API FCubismExpressionParameterValue
{
GENERATED_USTRUCT_BODY()
int32 Index;
FString Id;
float AdditiveValue;
float MultiplyValue;
float OverwriteValue;
};
UCLASS( ClassGroup=(Custom), meta=(BlueprintSpawnableComponent) )
class LIVE2DCUBISMFRAMEWORK_API UCubismExpressionComponent : public UActorComponent, public ICubismUpdatableInterface
{
GENERATED_BODY()
public:
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta = (ClampMin = "-1", SliderMin = "-1"))
int32 Index = -1;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TArray<TObjectPtr<UCubismExp3Json>> Jsons;
UPROPERTY(BlueprintAssignable, Category = "Live2D Cubism")
FCubismExpressionPlaybackFinishedHandler OnExpressionPlaybackFinished;
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup(UCubismModelComponent* InModel);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void PlayExpression(const int32 InIndex);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void StopAllExpressions(const bool bForce = false);
virtual bool IsControlledByUpdateController() const override { return true; }
virtual int32 GetExecutionOrder() const override;
virtual void OnCubismUpdate(float DeltaTime) override;
private:
friend class UCubismModelComponent;
UPROPERTY()
TObjectPtr<UCubismModelComponent> Model;
private:
UCubismExpressionComponent();
float Time;
TArray<TSharedPtr<FCubismExpression>> ExpressionQueue;
TArray<FCubismExpressionParameterValue> ParameterValues;
void UpdateExpression(const int32 ExpressionIndex, const TSharedPtr<FCubismExpression>& Expression);
static float CalculateValue(float Source, float Destination, float FadeWeight);
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