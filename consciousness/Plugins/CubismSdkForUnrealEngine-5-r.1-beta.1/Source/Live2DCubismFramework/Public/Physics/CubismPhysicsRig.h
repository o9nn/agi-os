#pragma once
#include "Physics/CubismPhysics3Json.h"
class UCubismParameterComponent;
struct FCubismPhysicsRigInput
{
float Weight;
bool bReflect;
ECubismPhysicsSource Type;
FCubismPhysicsParameter Source;
int32 ParameterIndex;
TObjectPtr<UCubismParameterComponent> Parameter;
void GetNormalizedParameterValue(
FVector2D& TargetTranslation,
float& TargetAngle,
const float Value,
const FCubismPhysicsNormalization& NormalizationPosition,
const FCubismPhysicsNormalization& NormalizationAngle
);
};
struct FCubismPhysicsRigOutput
{
int32 ParticleIndex;
float AngleScale;
float Weight;
bool bReflect;
ECubismPhysicsSource Type;
FCubismPhysicsParameter Destination;
int32 ParameterIndex;
TObjectPtr<UCubismParameterComponent> Parameter;
FVector2D TranslationScale;
float ValueBelowMinimum;
float ValueExceededMaximum;
float PreviousValue;
float CurrentValue;
void UpdateOutputParameterValue(float& TargetValue, float Value);
float GetValue(const TArray<struct FCubismPhysicsRigParticle> Particles, const FVector2D ParentGravity) const;
};
struct FCubismPhysicsRigParticle
{
FVector2D InitialPosition;
float Mobility;
float Delay;
float Acceleration;
float Radius;
FVector2D Position;
FVector2D LastPosition;
FVector2D LastGravity;
FVector2D Force;
FVector2D Velocity;
};
struct FCubismPhysicsRig
{
FCubismPhysicsNormalization NormalizationPosition;
FCubismPhysicsNormalization NormalizationAngle;
TArray<FCubismPhysicsRigInput> Inputs;
TArray<FCubismPhysicsRigOutput> Outputs;
TArray<FCubismPhysicsRigParticle> Particles;
};