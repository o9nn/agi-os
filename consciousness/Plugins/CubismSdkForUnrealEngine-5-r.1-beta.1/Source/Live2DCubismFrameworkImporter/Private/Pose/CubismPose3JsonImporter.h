#pragma once
#include "Pose/CubismPose3Json.h"
class FCubismPose3JsonImporter
{
public:
bool ImportFromString(const FString& FileContent);
void ApplyParams(EObjectFlags Flags, const TObjectPtr<UCubismPose3Json>& Json);
private:
static TSharedPtr<FJsonObject> ParseJSON(const FString& FileContent, bool bSilent);
protected:
int32 Version;
float FadeInTime = 0.5f;
TArray<FCubismPosePartGroup> PartGroups;
};