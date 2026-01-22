#pragma once
#include "Model/CubismModelComponent.h"
#include "CubismLookAtParameter.generated.h"
UENUM(BlueprintType)
enum class ECubismLookAtAxis : uint8
{
X,
Y,
Z,
};
USTRUCT(BlueprintType)
struct FCubismLookAtParameter
{
GENERATED_USTRUCT_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
float Value = 0.0f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
ECubismParameterBlendMode BlendMode = ECubismParameterBlendMode::Additive;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bEnabled = false;
UPROPERTY(EditAnywhere, BlueprintReadWrite, meta=(EditCondition="bEnabled"), Category = "Live2D Cubism")
ECubismLookAtAxis Axis = ECubismLookAtAxis::X;
UPROPERTY(EditAnywhere, BlueprintReadWrite, meta = (EditCondition = "bEnabled"), meta = (ClampMin = "-100.0", SliderMin = "0.0", SliderMax = "100.0"), Category = "Live2D Cubism")
float Factor = 1.0f;
UPROPERTY(EditAnywhere, Category = "Live2D Cubism")
FString Id = TEXT("");
};