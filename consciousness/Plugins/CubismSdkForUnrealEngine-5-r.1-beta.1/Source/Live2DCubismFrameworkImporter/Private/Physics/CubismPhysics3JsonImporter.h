#pragma once
#include "Physics/CubismPhysics3Json.h"
class FCubismPhysics3JsonImporter
{
public:
bool ImportFromString(const FString& FileContent);
void ApplyParams(EObjectFlags Flags, const TObjectPtr<UCubismPhysics3Json>& Json);
private:
static TSharedPtr<FJsonObject> ParseJSON(const FString& FileContent, bool bSilent);
protected:
int32 Version;
FVector2D Gravity = FVector2D(0.0f, -1.0f);
FVector2D Wind = FVector2D(0.0f, 0.0f);
float Fps = 0.0f;
int32 TotalInputCount = 0;
int32 TotalOutputCount = 0;
int32 TotalParticleCount = 0;
TArray<FCubismPhysicsSetting> PhysicsSettings;
private:
TMap<FString, FText> PhysicsSettingNameMap;
};