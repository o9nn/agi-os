#pragma once
#include "CoreMinimal.h"
#include "Components/ActorComponent.h"
#include "Live2DCubismAvatarComponent.generated.h"
class UTexture2D;
class UMaterialInstanceDynamic;
UCLASS(ClassGroup=(Custom), meta=(BlueprintSpawnableComponent))
class DEEPTREECHO_API ULive2DCubismAvatarComponent : public UActorComponent
{
GENERATED_BODY()
public:
ULive2DCubismAvatarComponent();
protected:
virtual void BeginPlay() override;
public:
virtual void TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction) override;
UFUNCTION(BlueprintCallable, Category = "Live2D")
void LoadLive2DModel(const FString& ModelPath);
UFUNCTION(BlueprintCallable, Category = "Live2D")
void SetParameterValue(const FName& ParameterName, float Value);
UFUNCTION(BlueprintCallable, Category = "Live2D")
float GetParameterValue(const FName& ParameterName) const;
private:
UPROPERTY(Transient)
UObject* Live2DModel;
UPROPERTY(Transient)
UTexture2D* RenderTarget;
UPROPERTY(Transient)
UMaterialInstanceDynamic* DynamicMaterial;
};