#pragma once
#include "EditorFramework/AssetImportData.h"
#include "CubismPhysics3Json.generated.h"
USTRUCT(BlueprintType)
struct FCubismPhysicsNormalization
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Normalization")
float Minimum;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Normalization")
float Maximum;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Normalization")
float Default;
FCubismPhysicsNormalization()
: Minimum(0.0f)
, Maximum(0.0f)
, Default(0.0f)
{ }
};
UENUM(BlueprintType)
enum class ECubismPhysicsSource : uint8
{
X,
Y,
Angle,
};
UENUM(BlueprintType)
enum class ECubismPhysicsTargetType : uint8
{
Parameter,
};
USTRUCT(BlueprintType)
struct FCubismPhysicsParameter
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Parameter")
FString Id;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Parameter")
ECubismPhysicsTargetType Target;
FCubismPhysicsParameter()
: Id()
, Target(ECubismPhysicsTargetType::Parameter)
{ }
};
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FCubismPhysicsInput
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Input")
float Weight;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Input")
bool bReflect;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Input")
ECubismPhysicsSource Type;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Input")
FCubismPhysicsParameter Source;
FCubismPhysicsInput()
: Weight(0.0f)
, bReflect(false)
, Type(ECubismPhysicsSource::X)
, Source()
{ }
};
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FCubismPhysicsOutput
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Output")
int32 VertexIndex;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Output")
float AngleScale;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Output")
float Weight;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Output")
bool bReflect;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Output")
ECubismPhysicsSource Type;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Output")
FCubismPhysicsParameter Destination;
FCubismPhysicsOutput()
: VertexIndex(0)
, AngleScale(0.0f)
, Weight(0.0f)
, bReflect(false)
, Type(ECubismPhysicsSource::X)
, Destination()
{ }
};
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FCubismPhysicsParticle
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Particle")
float Mobility;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Particle")
float Delay;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Particle")
float Acceleration;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Particle")
float Radius;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Particle")
FVector2D Position;
FCubismPhysicsParticle()
: Mobility(0.0f)
, Delay(0.0f)
, Acceleration(0.0f)
, Radius(0.0f)
, Position(FVector2D::ZeroVector)
{ }
};
USTRUCT(BlueprintType)
struct LIVE2DCUBISMFRAMEWORK_API FCubismPhysicsSetting
{
GENERATED_USTRUCT_BODY()
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Setting")
FString Id;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Setting")
FText Name;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Setting")
TArray<FCubismPhysicsInput> Inputs;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Setting")
TArray<FCubismPhysicsOutput> Outputs;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Setting")
TArray<FCubismPhysicsParticle> Particles;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Setting")
FCubismPhysicsNormalization NormalizationPosition;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Setting")
FCubismPhysicsNormalization NormalizationAngle;
FCubismPhysicsSetting()
: Id()
, Name()
, Inputs()
, Outputs()
, Particles()
, NormalizationPosition()
, NormalizationAngle()
{ }
};
UCLASS(BlueprintType)
class LIVE2DCUBISMFRAMEWORK_API UCubismPhysics3Json : public UObject
{
GENERATED_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Physics Data")
int32 Version;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Physics Data")
FVector2D Gravity;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Physics Data")
FVector2D Wind;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Physics Data")
float Fps;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Physics Data")
int32 TotalInputCount;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Physics Data")
int32 TotalOutputCount;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Physics Data")
int32 TotalParticleCount;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Physics Data")
TArray<FCubismPhysicsSetting> PhysicsSettings;
virtual void PostInitProperties() override;
#if WITH_EDITORONLY_DATA
UPROPERTY(VisibleAnywhere, Instanced, Category=ImportSettings)
TObjectPtr<class UAssetImportData> AssetImportData;
virtual void GetAssetRegistryTags(TArray<FAssetRegistryTag>& OutTags) const override;
virtual void Serialize(FArchive& Ar) override;
#endif
};