#pragma once
#include "Model/CubismModelComponent.h"
#include "CubismPartComponent.generated.h"
class UCubismModelComponent;
UCLASS(Blueprintable)
class LIVE2DCUBISMFRAMEWORK_API UCubismPartComponent : public UActorComponent
{
GENERATED_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
int32 Index;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
FString Id;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta = (ClampMin = "0.0", ClampMax = "1.0", SliderMin = "0.0", SliderMax = "1.0"))
float Opacity = 1.0f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bOverwriteFlagForPartMultiplyColors;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
FLinearColor MultiplyColor = FLinearColor::White;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bOverwriteFlagForPartScreenColors;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
FLinearColor ScreenColor = FLinearColor::Black;
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup(UCubismModelComponent* InModel);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void SetPartOpacity(float TargetOpacity);
private:
friend class UCubismModelComponent;
UPROPERTY()
TObjectPtr<UCubismModelComponent> Model;
private:
UCubismPartComponent();
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