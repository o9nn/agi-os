#pragma once
#include "DisplayInfo/CubismDisplayInfo3Json.h"
class FCubismDisplayInfo3JsonImporter
{
public:
bool ImportFromString(const FString& FileContent);
void ApplyParams(EObjectFlags Flags, const TObjectPtr<UCubismDisplayInfo3Json>& Json);
private:
static TSharedPtr<FJsonObject> ParseJSON(const FString& FileContent, bool bSilent);
protected:
int32 Version;
TArray<FCubismDisplayInfoParameter> Parameters;
TArray<FCubismDisplayInfoParameterGroup> ParameterGroups;
TArray<FCubismDisplayInfoPart> Parts;
};