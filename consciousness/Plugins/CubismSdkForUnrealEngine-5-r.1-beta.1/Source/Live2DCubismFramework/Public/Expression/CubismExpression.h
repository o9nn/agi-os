#pragma once
#include "Expression/CubismExp3Json.h"
enum class ECubismExpressionState : uint8
{
None,
Play,
End,
};
class FCubismExpression
{
public:
FCubismExpression(const UCubismExp3Json* Json);
float FadeInTime;
float FadeOutTime;
float Weight;
TArray<FCubismExpressionParameter> Parameters;
float StartTime;
float EndTime;
public:
void Init(const float Time);
float CalcExpressionWeight(const float Time) const;
float UpdateWeight(const float ElapsedTime);
void StartFadeout(const float Time);
private:
friend class UCubismExpressionComponent;
ECubismExpressionState State = ECubismExpressionState::None;
float FadeWeight = 0.0f;
static inline float EasingSin(const float Value)
{
return Value < 0.0f? 0.0f : Value < 1.0f? 0.5f - 0.5f * FMath::Cos(PI * Value) : 1.0f;
}
};