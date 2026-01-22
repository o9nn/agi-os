#pragma once
#include "Motion/CubismMotion3Json.h"
#include "Engine/CurveTable.h"
enum class ECubismMotionState : uint8
{
None,
Play,
PlayInLoop,
End,
};
class FCubismMotion
{
public:
FCubismMotion(const UCubismMotion3Json* Json, const float InOffsetTime);
float Duration;
bool bLoop;
float Fps;
float FadeInTime;
float FadeOutTime;
TArray<FCubismMotionCurve> Curves;
TObjectPtr<UCurveTable> CurveTable;
TArray<FCubismMotionEvent> Events;
float StartTime;
float OffsetTime;
float EndTime;
public:
void Init(const float Time);
float UpdateFadeWeight(const TSharedPtr<FCubismMotion>& CubismMotion, float UserTimeSeconds);
void FadeOut(const float Time);
void SetWeight(float weight);
float GetWeight() const;
void SetFadeout(float NewFadeOutSeconds);
void StartFadeout(float NewFadeOutSeconds, float UserTimeSeconds);
bool IsTriggeredFadeOut();
float GetFadeOutSeconds();
float GetEndTime();
void IsFinished(bool F);
bool IsFinished() const;
private:
friend class UCubismMotionComponent;
ECubismMotionState State = ECubismMotionState::None;
static inline float EasingSin(const float Value)
{
if (Value < 0.0f)
{
return 0.0f;
}
if (Value >= 1.0f)
{
return 1.0f;
}
return 0.5f - 0.5f * FMath::Cos(PI * Value);
}
float GetValue(const FString Id, const float Time) const
{
return CurveTable->FindRichCurve(*Id, Id)->Eval(Time, 0.0f);
}
float Weight;
float FadeOutSeconds;
float EndTimeSeconds;
bool bIsTriggeredFadeOut;
bool bFinished;
};