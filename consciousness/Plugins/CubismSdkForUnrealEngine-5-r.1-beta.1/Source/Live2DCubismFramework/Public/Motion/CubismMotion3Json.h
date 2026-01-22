#pragma once
#include "EditorFramework/AssetImportData.h"
#include "Engine/CurveTable.h"
#include "CubismMotion3Json.generated.h"
UENUM(BlueprintType)
enum class ECubismMotionCurveTarget : uint8
{
None,
Model,
Parameter,
PartOpacity,
};
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FCubismMotionCurve
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Curve")
FString Id;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Curve")
ECubismMotionCurveTarget Target;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Curve")
float FadeInTime;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Curve")
float FadeOutTime;
FCubismMotionCurve()
: Id()
, Target(ECubismMotionCurveTarget::None)
, FadeInTime(-1.0f)
, FadeOutTime(-1.0f)
{ }
};
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FCubismMotionEvent
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Event")
float Time;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Event")
FString Value;
FCubismMotionEvent()
: Time(0.0f)
, Value()
{ }
};
UCLASS(BlueprintType)
class LIVE2DCUBISMFRAMEWORK_API UCubismMotion3Json : public UObject
{
GENERATED_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Motion Data")
float Duration;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Motion Data")
bool bLoop;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Motion Data")
float Fps;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Motion Data")
float FadeInTime;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Motion Data")
float FadeOutTime;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Motion Data")
TArray<FCubismMotionCurve> Curves;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Motion Data")
TObjectPtr<UCurveTable> CurveTable;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Motion Data")
TArray<FCubismMotionEvent> Events;
virtual void PostInitProperties() override;
#if WITH_EDITORONLY_DATA
UPROPERTY(VisibleAnywhere, Instanced, Category=ImportSettings)
TObjectPtr<class UAssetImportData> AssetImportData;
virtual void GetAssetRegistryTags(TArray<FAssetRegistryTag>& OutTags) const override;
virtual void Serialize(FArchive& Ar) override;
#endif
};
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FMotion3JsonGroup
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Motion Group")
FString Name;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Motion Group")
TArray<TObjectPtr<UCubismMotion3Json>> Motion3Jsons;
FMotion3JsonGroup()
: Name()
, Motion3Jsons()
{ }
};