#pragma once
#include "UserData/CubismUserData3Json.h"
class FCubismUserData3JsonImporter
{
public:
bool ImportFromString(const FString& FileContent);
void ApplyParams(EObjectFlags Flags, const TObjectPtr<UCubismUserData3Json>& Json);
private:
static TSharedPtr<FJsonObject> ParseJSON(const FString& FileContent, bool bSilent);
protected:
int32 Version;
int32 Size;
TMap<ECubismUserDataTargetType, FCubismUserDataEntry> Data;
};