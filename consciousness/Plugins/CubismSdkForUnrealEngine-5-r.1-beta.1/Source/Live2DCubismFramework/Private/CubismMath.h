#pragma once
#include "CoreMinimal.h"
class FCubismMath
{
public:
static float DirectionToRadian(const FVector2D From, const FVector2D To)
{
float Radian = FMath::Atan2(To.Y, To.X) - FMath::Atan2(From.Y, From.X);
while (Radian < -PI)
{
Radian += PI * 2.0f;
}
while (Radian > PI)
{
Radian -= PI * 2.0f;
}
return Radian;
}
};