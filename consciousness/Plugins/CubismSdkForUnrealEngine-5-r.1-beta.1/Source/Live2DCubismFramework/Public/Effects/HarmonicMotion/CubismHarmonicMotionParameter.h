#pragma once
#include "Model/CubismModelComponent.h"
#include "CubismHarmonicMotionParameter.generated.h"
UENUM(BlueprintType)
enum class ECubismHarmonicMotionDirection : uint8
{
Centric,
Left,
Right,
};
USTRUCT(BlueprintType)
struct FCubismHarmonicMotionParameter
{
GENERATED_USTRUCT_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
float Value = 0.0f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
ECubismParameterBlendMode BlendMode = ECubismParameterBlendMode::Additive;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bEnabled = false;
UPROPERTY(EditAnywhere, meta=(ClampMin="0.0", SliderMin="0.0", SliderMax="100.0"), Category = "Live2D Cubism")
float TimeScale = 1.0f;
UPROPERTY(EditAnywhere, Category = "Live2D Cubism")
ECubismHarmonicMotionDirection Direction = ECubismHarmonicMotionDirection::Centric;
UPROPERTY(EditAnywhere, meta=(ClampMin="0.0", ClampMax="1.0", SliderMin="0.0", SliderMax="1.0"), Category = "Live2D Cubism")
float NormalizedOrigin = 0.5f;
UPROPERTY(EditAnywhere, meta=(ClampMin="0.0", ClampMax="1.0", SliderMin="0.0", SliderMax="1.0"), Category = "Live2D Cubism")
float NormalizedRange = 0.5f;
UPROPERTY(EditAnywhere, meta=(ClampMin="0.01", ClampMax="10.0", SliderMin="0.01", SliderMax="10.0"), Category = "Live2D Cubism")
float Duration = 3.0f;
UPROPERTY(EditAnywhere, Category = "Live2D Cubism")
FString Id = TEXT("");
public:
float CalcValue(const float Time, const float Min, const float Max);
};