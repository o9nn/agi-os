#pragma once
#include "Model/CubismModelActor.h"
#include "Engine/TextureRenderTarget2D.h"
#include "CubismMaskTextureComponent.generated.h"
class FCubismMaskJunction;
class UCubismRendererComponent;
UCLASS(BlueprintType)
class LIVE2DCUBISMFRAMEWORK_API UCubismMaskTextureComponent : public UActorComponent
{
GENERATED_BODY()
public:
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
int32 Size = 4096;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bUseMultiRenderTargets = false;
UPROPERTY(EditAnywhere, BlueprintReadWrite, meta = (ClampMin = "1", SliderMin = "1", EditCondition = "bUseMultiRenderTargets"), Category = "Live2D Cubism")
int32 RenderTargetCount = 1;
UPROPERTY(EditAnywhere, BlueprintReadWrite, meta = (ClampMin = "0", SliderMin = "0", EditCondition = "bUseMultiRenderTargets"), Category = "Live2D Cubism")
int32 LOD = 0;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
int32 NumMasks;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
TArray<TObjectPtr<ACubismModel>> Models;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
TArray<TObjectPtr<UTextureRenderTarget2D>> RenderTargets;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void AddModel(ACubismModel* Model);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void RemoveModel(ACubismModel* Model);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void ResolveMaskLayout();
private:
UCubismMaskTextureComponent();
inline int32 CalcOptimalLOD() const;
inline void AllocateRenderTargets(const int32 RequiredRTs);
bool bDirty = true;
public:
#if WITH_EDITOR
void PostEditChangeProperty(FPropertyChangedEvent& PropertyChangedEvent);
#endif
virtual void OnComponentCreated() override;
virtual void OnComponentDestroyed(bool bDestroyingHierarchy) override;
#if WITH_EDITOR
virtual void PostEditUndo() override;
#endif
virtual void TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction) override;
};