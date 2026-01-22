#pragma once
#include "Kismet/BlueprintFunctionLibrary.h"
#include "Model/CubismModel3Json.h"
#include "Model/CubismModelActor.h"
#include "Engine/TextureRenderTarget2D.h"
#include "Live2DCubismFrameworkBPLibrary.generated.h"
UCLASS(Blueprintable)
class LIVE2DCUBISMFRAMEWORK_API ULive2DCubismFrameworkBPLibrary : public UBlueprintFunctionLibrary
{
GENERATED_BODY()
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism", meta = (WorldContext = "WorldContextObject"))
static ACubismModel* SpawnCubismModel(
UObject* WorldContextObject,
UCubismModel3Json* Model3Json,
const FTransform& Transform,
const bool bRenderInWorldSpace = true,
UTextureRenderTarget2D* RenderTarget = nullptr
);
};