#include "ExpressionSynthesizer.h"
void UExpressionSynthesizer::SynthesizeExpression(const FEmotionalState& EmotionalState, TMap<FName, float>& OutParameters, float DeltaTime)
{
TMap<FName, float> TargetParameters;
TargetParameters.Add(FName("ParamMouthSmile"), EmotionalState.Happiness);
TargetParameters.Add(FName("ParamEyeOpen"), 0.5f + (EmotionalState.Happiness * 0.3f) + (EmotionalState.Surprise * 0.5f));
TargetParameters.Add(FName("ParamBrowY"), EmotionalState.Surprise * 0.8f - EmotionalState.Sadness * 0.5f);
TargetParameters.Add(FName("ParamMouthForm"), -EmotionalState.Sadness * 0.6f);
TargetParameters.Add(FName("ParamBrowAngle"), -EmotionalState.Anger * 0.7f);
ApplyParametersSmoothed(TargetParameters, DeltaTime);
OutParameters = CurrentParameters;
}
void UExpressionSynthesizer::ApplyParametersSmoothed(TMap<FName, float>& TargetParameters, float DeltaTime)
{
for (auto const& [ParamName, TargetValue] : TargetParameters)
{
float* CurrentValue = CurrentParameters.Find(ParamName);
if (CurrentValue)
{
*CurrentValue = FMath::FInterpTo(*CurrentValue, TargetValue, DeltaTime, 5.0f);
}
else
{
CurrentParameters.Add(ParamName, TargetValue);
}
}
}