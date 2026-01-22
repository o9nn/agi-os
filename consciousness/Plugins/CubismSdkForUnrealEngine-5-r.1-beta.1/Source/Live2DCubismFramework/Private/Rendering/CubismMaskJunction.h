#pragma once
#include "Rendering/CubismMaskTextureComponent.h"
class UCubismDrawableComponent;
class UTextureRenderTarget2D;
class FCubismMaskJunction
{
public:
TArray<TObjectPtr<UCubismDrawableComponent>> Drawables;
TArray<TObjectPtr<UCubismDrawableComponent>> MaskDrawables;
UTextureRenderTarget2D* RenderTarget;
FVector4 Offset;
FVector4 Channel;
};