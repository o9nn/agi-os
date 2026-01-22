#pragma once
#include "Model/CubismModelComponent.h"
#include "CubismParameterComponent.generated.h"
UCLASS(Blueprintable)
class LIVE2DCUBISMFRAMEWORK_API UCubismParameterComponent : public UActorComponent
{
GENERATED_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
int32 Index;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
FString Id;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
ECubismParameterType Type;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
float MinimumValue;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
float MaximumValue;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
float DefaultValue;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
float Value;
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup(UCubismModelComponent* InModel);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void SetParameterValue(float TargetValue, const float Weight = 1.0f);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void AddParameterValue(float TargetValue, const float Weight = 1.0f);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void MultiplyParameterValue(float TargetValue, const float Weight = 1.0f);
private:
friend class UCubismModelComponent;
UPROPERTY()
TObjectPtr<UCubismModelComponent> Model;
private:
UCubismParameterComponent();
public:
virtual void PostLoad() override;
#if WITH_EDITORONLY_DATA
void PostEditChangeProperty(FPropertyChangedEvent& PropertyChangedEvent) override;
#endif
virtual void OnComponentCreated() override;
#if WITH_EDITOR
virtual void PostEditUndo() override;
#endif
};