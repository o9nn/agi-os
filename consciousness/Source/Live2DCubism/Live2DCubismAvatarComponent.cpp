#include "Live2DCubismAvatarComponent.h"
#include "Materials/MaterialInstanceDynamic.h"
#include "Engine/Texture2D.h"
ULive2DCubismAvatarComponent::ULive2DCubismAvatarComponent()
{
PrimaryComponentTick.bCanEverTick = true;
}
void ULive2DCubismAvatarComponent::BeginPlay()
{
Super::BeginPlay();
UE_LOG(LogTemp, Log, TEXT("Live2D Cubism SDK Initialized (Placeholder)"));
}
void ULive2DCubismAvatarComponent::TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction)
{
Super::TickComponent(DeltaTime, TickType, ThisTickFunction);
}
void ULive2DCubismAvatarComponent::LoadLive2DModel(const FString& ModelPath)
{
if (!FPaths::FileExists(ModelPath))
{
UE_LOG(LogTemp, Error, TEXT("Model file not found: %s"), *ModelPath);
return;
}
TArray<uint8> ModelData;
if (!FFileHelper::LoadFileToArray(ModelData, *ModelPath))
{
UE_LOG(LogTemp, Error, TEXT("Failed to load model file: %s"), *ModelPath);
return;
}
UE_LOG(LogTemp, Log, TEXT("Live2D model created from data (Placeholder)"));
Live2DModel = NewObject<UObject>();
}
void ULive2DCubismAvatarComponent::SetParameterValue(const FName& ParameterName, float Value)
{
if (Live2DModel)
{
}
}
float ULive2DCubismAvatarComponent::GetParameterValue(const FName& ParameterName) const
{
if (Live2DModel)
{
return 0.5f;
}
return 0.0f;
return 0.0f;
}