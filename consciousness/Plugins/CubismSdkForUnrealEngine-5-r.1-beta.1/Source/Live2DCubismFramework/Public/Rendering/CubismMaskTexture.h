#pragma once
#include "Rendering/CubismMaskTextureComponent.h"
#include "CubismMaskTexture.generated.h"
class UCubismMaskTextureComponent;
UCLASS(Blueprintable)
class LIVE2DCUBISMFRAMEWORK_API ACubismMaskTexture : public AActor
{
GENERATED_BODY()
private:
ACubismMaskTexture()
{
MaskTextureComponent = CreateDefaultSubobject<UCubismMaskTextureComponent>(TEXT("CubismTextureComponent"));
#if WITH_EDITORONLY_DATA
bIsSpatiallyLoaded = false;
#endif
}
private:
friend class UCubismRendererComponent;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, BlueprintReadOnly, meta=(ExposeFunctionCategories="Sprite,Rendering,Physics,Components", AllowPrivateAccess="true"), Category = "Live2D Cubism")
TObjectPtr<UCubismMaskTextureComponent> MaskTextureComponent;
};