#pragma once
#include "CubismRaycastParameter.generated.h"
UENUM(BlueprintType)
enum class ECubismRaycastPrecision : uint8
{
BoundingBox,
Mesh
};
USTRUCT(BlueprintType)
struct FCubismRaycastParameter
{
GENERATED_USTRUCT_BODY()
public:
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bEnabled = false;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
ECubismRaycastPrecision Precision = ECubismRaycastPrecision::BoundingBox;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
FString Id = TEXT("");
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
FString Name = TEXT("");
};