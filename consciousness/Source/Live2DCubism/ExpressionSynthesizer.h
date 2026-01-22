#pragma once
#include "CoreMinimal.h"
#include "UObject/NoExportTypes.h"
#include "ExpressionSynthesizer.generated.h"
USTRUCT(BlueprintType)
struct FEmotionalState
{
GENERATED_BODY()
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Emotions")
float Happiness = 0.0f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Emotions")
float Surprise = 0.0f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Emotions")
float Sadness = 0.0f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Emotions")
float Anger = 0.0f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Emotions")
float Fear = 0.0f;
};
UCLASS(Blueprintable)
class DEEPTREECHO_API UExpressionSynthesizer : public UObject
{
GENERATED_BODY()
public:
UFUNCTION(BlueprintCallable, Category = "Live2D")
void SynthesizeExpression(const FEmotionalState& EmotionalState, TMap<FName, float>& OutParameters, float DeltaTime);
private:
void ApplyParametersSmoothed(TMap<FName, float>& TargetParameters, float DeltaTime);
UPROPERTY()
TMap<FName, float> CurrentParameters;
};