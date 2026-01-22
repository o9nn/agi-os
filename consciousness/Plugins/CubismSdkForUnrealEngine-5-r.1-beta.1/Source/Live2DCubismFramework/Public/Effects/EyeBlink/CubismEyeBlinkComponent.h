#pragma once
#include "Model/CubismModelComponent.h"
#include "CubismUpdatableInterface.h"
#include "CubismEyeBlinkComponent.generated.h"
class UCubismModel3Json;
enum class ECubismEyeBlinkPhase : uint8
{
Idle,
Closing,
Closed,
Opening,
};
UCLASS( ClassGroup=(Custom), meta=(BlueprintSpawnableComponent) )
class LIVE2DCUBISMFRAMEWORK_API UCubismEyeBlinkComponent : public UActorComponent, public ICubismUpdatableInterface
{
GENERATED_BODY()
public:
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TObjectPtr<UCubismModel3Json> Json;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta=(EditCondition="!bAutoEnabled"))
float Value = 1.0f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
ECubismParameterBlendMode BlendMode = ECubismParameterBlendMode::Multiplicative;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bAutoEnabled = true;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta=(EditCondition="bAutoEnabled"), meta=(ClampMin="1.0", ClampMax="10.0", SliderMin="1.0", SliderMax="10.0"))
float Mean = 2.5f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta=(EditCondition="bAutoEnabled"), meta=(ClampMin="0.5", ClampMax="5.0", SliderMin="0.5", SliderMax="5.0"))
float MaximumDeviation = 2.0f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta=(EditCondition="bAutoEnabled"), meta=(ClampMin="0.0", ClampMax="20.0", SliderMin="0.0", SliderMax="20.0"))
float TimeScale = 10.0f;
UPROPERTY(VisibleAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta=(EditCondition="bAutoEnabled"), meta=(ClampMin="0.0", ClampMax="5.0", SliderMin="0.0", SliderMax="5.0"))
float ClosingPeriod = 1.0f;
UPROPERTY(VisibleAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta=(EditCondition="bAutoEnabled"), meta=(ClampMin="0.0", ClampMax="5.0", SliderMin="0.0", SliderMax="5.0"))
float ClosedPeriod = 0.5f;
UPROPERTY(VisibleAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta=(EditCondition="bAutoEnabled"), meta=(ClampMin="0.0", ClampMax="5.0", SliderMin="0.0", SliderMax="5.0"))
float OpeningPeriod = 1.5f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TArray<FString> Ids;
virtual bool IsControlledByUpdateController() const override { return true; }
virtual int32 GetExecutionOrder() const override;
virtual void OnCubismUpdate(float DeltaTime) override;
#if WITH_EDITORONLY_DATA
UPROPERTY(EditAnywhere, Category = "Live2D Cubism")
bool bEnableEyeBlinkInEditor = false;
#endif
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup(UCubismModelComponent* InModel);
private:
friend class UCubismModelComponent;
UPROPERTY()
TObjectPtr<UCubismModelComponent> Model;
private:
UCubismEyeBlinkComponent();
ECubismEyeBlinkPhase Phase;
float Time;
float StartTime;
void Update(const float DeltaTime);
public:
virtual void PostLoad() override;
#if WITH_EDITORONLY_DATA
void PostEditChangeProperty(FPropertyChangedEvent& PropertyChangedEvent) override;
#endif
virtual void OnComponentCreated() override;
virtual void OnComponentDestroyed(bool bDestroyingHierarchy) override;
#if WITH_EDITOR
virtual void PostEditUndo() override;
#endif
virtual void TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction) override;
};