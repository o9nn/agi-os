#pragma once
#include "Expression/CubismExp3Json.h"
class FCubismExp3JsonImporter
{
public:
bool ImportFromString(const FString& FileContent);
void ApplyParams(EObjectFlags Flags, const TObjectPtr<UCubismExp3Json>& Json);
private:
static TSharedPtr<FJsonObject> ParseJSON(const FString& FileContent, bool bSilent);
protected:
FString Type;
float FadeInTime = 1.0f;
float FadeOutTime = 1.0f;
TArray<FCubismExpressionParameter> Parameters;
};