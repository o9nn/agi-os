#pragma once
#include "Model/CubismModelComponent.h"
#include "CubismUpdatableInterface.h"
#include "CubismLipSyncComponent.generated.h"
class UCubismModel3Json;
UCLASS(ClassGroup=(Custom), meta=(BlueprintSpawnableComponent), hidecategories = (Object, ActorComponent, Physics, Rendering, Mobility, LOD))
class LIVE2DCUBISMFRAMEWORK_API UCubismLipSyncComponent : public UActorComponent, public ICubismUpdatableInterface
{
GENERATED_BODY()
public:
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TObjectPtr<UCubismModel3Json> Json;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism", meta=(ClampMin = "0.0", ClampMax = "1.0", SliderMin = "0.0", SliderMax = "1.0"))
float Value = 0.0f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
ECubismParameterBlendMode BlendMode = ECubismParameterBlendMode::Additive;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bAutoEnabled = false;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta=(EditCondition = "bAutoEnabled"), meta=(ClampMin = "0.0", ClampMax = "20.0", SliderMin = "0.0", SliderMax = "20.0"))
float TimeScale = 10.0f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta=(EditCondition = "!bAutoEnabled"), meta=(ClampMin = "1.0", ClampMax = "10.0", SliderMin = "1.0", SliderMax = "10.0"))
float Gain = 1.0f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TArray<FString> Ids;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TObjectPtr<USoundWave> Source;
virtual bool IsControlledByUpdateController() const override { return true; }
virtual int32 GetExecutionOrder() const override;
virtual void OnCubismUpdate(float DeltaTime) override;
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup(UCubismModelComponent* InModel);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
UAudioComponent* GetAudioComponent();
protected:
void Update(const float DeltaTime);
TObjectPtr<UAudioComponent> CreateAudioComponent();
private:
friend class UCubismModelComponent;
UPROPERTY()
TObjectPtr<UCubismModelComponent> Model;
private:
UCubismLipSyncComponent();
float Time;
float TargetValue;
float CurrentVelocity;
UPROPERTY(Transient)
TObjectPtr<UAudioComponent> Audio;
float SmoothDamp(const float CurrentValue, const float DeltaTime);
UFUNCTION()
void OnEnvelopeValue(const USoundWave* InSoundWave, const float InEnvelopeValue);
float LipSyncTargetValue;
float LipSyncValue;
float LipSyncVValue;
float LastTimeSeconds;
float UserTimeSeconds;
public:
void PostLoad() override;
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