#pragma once
#include "GameFramework/Actor.h"
#include "Model/CubismMoc3.h"
#include "Physics/CubismPhysics3Json.h"
#include "Pose/CubismPose3Json.h"
#include "DisplayInfo/CubismDisplayInfo3Json.h"
#include "Expression/CubismExp3Json.h"
#include "Motion/CubismMotion3Json.h"
#include "UserData/CubismUserData3Json.h"
#include "Model/CubismModel3Json.h"
#include "CubismModelActor.generated.h"
UCLASS(Blueprintable)
class LIVE2DCUBISMFRAMEWORK_API ACubismModel : public AActor
{
GENERATED_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
TObjectPtr<class UCubismModelComponent> Model;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism", meta = (WorldContext = "WorldContextObject"))
void Initialize(UCubismModel3Json* Model3Json);
private:
ACubismModel();
static TObjectPtr<UCubismMoc3> LoadMoc(const TObjectPtr<UCubismModel3Json>& Model3Json);
static TArray<TObjectPtr<UTexture2D>> LoadTextures(const TObjectPtr<UCubismModel3Json>& Model3Json);
static TObjectPtr<UCubismPhysics3Json> LoadPhysics3Json(const TObjectPtr<UCubismModel3Json>& Model3Json);
static TObjectPtr<UCubismPose3Json> LoadPose3Json(const TObjectPtr<UCubismModel3Json>& Model3Json);
static TArray<TObjectPtr<UCubismExp3Json>> LoadExp3Jsons(const TObjectPtr<UCubismModel3Json>& Model3Json);
static TArray<FMotion3JsonGroup> LoadMotion3Jsons(const TObjectPtr<UCubismModel3Json>& Model3Json);
static TObjectPtr<UCubismDisplayInfo3Json> LoadDisplayInfo3Json(const TObjectPtr<UCubismModel3Json>& Model3Json);
static TObjectPtr<UCubismUserData3Json> LoadUserData3Json(const TObjectPtr<UCubismModel3Json>& Model3Json);
};