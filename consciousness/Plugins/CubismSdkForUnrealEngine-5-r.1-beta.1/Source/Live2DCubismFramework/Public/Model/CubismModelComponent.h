#pragma once
#include "Live2DCubismCore.h"
#include "CubismUpdatableInterface.h"
#include "Components/SceneComponent.h"
#include "Engine/TextureRenderTarget2D.h"
#include "CubismModelComponent.generated.h"
class UCubismMoc3;
class UCubismDisplayInfo3Json;
class UCubismUserData3Json;
class UCubismDrawableComponent;
class UCubismParameterComponent;
class UCubismPartComponent;
class UCubismRendererComponent;
class UCubismParameterStoreComponent;
class UCubismMotionComponent;
class UCubismExpressionComponent;
class UCubismPhysicsComponent;
class UCubismPoseComponent;
class UCubismEyeBlinkComponent;
class UCubismHarmonicMotionComponent;
class UCubismLipSyncComponent;
class UCubismLookAtComponent;
class UCubismRaycastComponent;
UENUM()
enum class ECubismDrawableBlendMode : uint8
{
Normal,
Additive,
Multiplicative,
};
UENUM(BlueprintType)
enum class ECubismParameterBlendMode : uint8
{
Overwrite,
Additive,
Multiplicative,
};
UENUM()
enum class ECubismParameterType : uint8
{
Normal,
BlendShape,
};
UCLASS(Blueprintable)
class LIVE2DCUBISMFRAMEWORK_API UCubismModelComponent : public USceneComponent, public ICubismUpdatableInterface
{
GENERATED_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
TObjectPtr<UCubismMoc3> Moc;
UPROPERTY(BlueprintReadOnly, Category = "Live2D Cubism")
TArray<TObjectPtr<UCubismDrawableComponent>> Drawables;
UPROPERTY(BlueprintReadOnly, Category = "Live2D Cubism")
TArray<TObjectPtr<UCubismParameterComponent>> Parameters;
UPROPERTY(BlueprintReadOnly, Category = "Live2D Cubism")
TArray<TObjectPtr<UCubismPartComponent>> Parts;
UPROPERTY(BlueprintReadOnly, Category = "Live2D Cubism")
TObjectPtr<UCubismRendererComponent> Renderer;
UPROPERTY(BlueprintReadOnly, Category = "Live2D Cubism")
TObjectPtr<UCubismParameterStoreComponent> ParameterStore;
UPROPERTY(BlueprintReadOnly, Category = "Live2D Cubism")
TObjectPtr<UCubismMotionComponent> Motion;
UPROPERTY(BlueprintReadOnly, Category = "Live2D Cubism")
TObjectPtr<UCubismExpressionComponent> Expression;
UPROPERTY(BlueprintReadOnly, Category = "Live2D Cubism")
TObjectPtr<UCubismPhysicsComponent> Physics;
UPROPERTY(BlueprintReadOnly, Category = "Live2D Cubism")
TObjectPtr<UCubismPoseComponent> Pose;
UPROPERTY(BlueprintReadOnly, Category = "Live2D Cubism")
TObjectPtr<UCubismEyeBlinkComponent> EyeBlink;
UPROPERTY(BlueprintReadOnly, Category = "Live2D Cubism")
TObjectPtr<UCubismHarmonicMotionComponent> HarmonicMotion;
UPROPERTY(BlueprintReadOnly, Category = "Live2D Cubism")
TObjectPtr<UCubismLipSyncComponent> LipSync;
UPROPERTY(BlueprintReadOnly, Category = "Live2D Cubism")
TObjectPtr<UCubismLookAtComponent> LookAt;
UPROPERTY(BlueprintReadOnly, Category = "Live2D Cubism")
TObjectPtr<UCubismRaycastComponent> Raycast;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bRenderInWorldSpace = true;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Texture")
TObjectPtr<UTextureRenderTarget2D> RenderTarget;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Texture")
TArray<TObjectPtr<UTexture2D>> Textures;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TObjectPtr<UCubismDisplayInfo3Json> DisplayInfoJson;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TObjectPtr<UCubismUserData3Json> UserDataJson;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta = (ClampMin = "0.0", ClampMax = "1.0", SliderMin = "0.0", SliderMax = "1.0"))
float Opacity = 1.0f;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bOverwriteFlagForModelMultiplyColors;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
FLinearColor MultiplyColor = FLinearColor::White;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bOverwriteFlagForModelScreenColors;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
FLinearColor ScreenColor = FLinearColor::Black;
virtual bool IsControlledByUpdateController() const override { return true; }
virtual int32 GetExecutionOrder() const override;
virtual void OnCubismUpdate(float DeltaTime) override;
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup();
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
FVector2D GetCanvasSize() const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
FVector2D GetCanvasOrigin() const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
float GetPixelsPerUnit() const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
int32 GetDrawableCount() const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
FString GetDrawableId(const int32 DrawableIndex) const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
int32 GetDrawableIndex(const FString DrawableId) const;
UCubismDrawableComponent* GetDrawable(const int32 DrawableIndex) const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
UCubismDrawableComponent* GetDrawable(const FString DrawableId) const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
int32 GetParameterCount() const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
FString GetParameterId(const int32 ParameterIndex) const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
int32 GetParameterIndex(const FString ParameterId);
UCubismParameterComponent* GetParameter(const int32 ParameterIndex) const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
UCubismParameterComponent* GetParameter(const FString ParameterId);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
int32 GetPartCount() const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
FString GetPartId(const int32 PartIndex) const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
int32 GetPartIndex(const FString PartId);
UCubismPartComponent* GetPart(const int32 PartIndex) const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
UCubismPartComponent* GetPart(const FString PartId);
private:
UCubismModelComponent();
~UCubismModelComponent();
private:
friend class UCubismDrawableComponent;
ECubismDrawableBlendMode GetDrawableBlendMode(const int32 DrawableIndex) const;
bool GetDrawableInvertedMask(const int32 DrawableIndex) const;
bool GetDrawableIsTwoSided(const int32 DrawableIndex) const;
bool GetDrawableDynamicFlagIsVisible(const int32 DrawableIndex) const;
bool GetDrawableDynamicFlagVisibilityDidChange(const int32 DrawableIndex) const;
bool GetDrawableDynamicFlagOpacityDidChange(const int32 DrawableIndex) const;
bool GetDrawableDynamicFlagDrawOrderDidChange(const int32 DrawableIndex) const;
bool GetDrawableDynamicFlagRenderOrderDidChange(const int32 DrawableIndex) const;
bool GetDrawableDynamicFlagVertexPositionsDidChange(const int32 DrawableIndex) const;
bool GetDrawableDynamicFlagBlendColorDidChange(const int32 DrawableIndex) const;
int32 GetDrawableTextureIndex(const int32 DrawableIndex) const;
int32 GetDrawableDrawOrder(const int32 DrawableIndex) const;
int32 GetDrawableRenderOrder(const int32 DrawableIndex) const;
float GetDrawableOpacity(const int32 DrawableIndex) const;
int32 GetDrawableMaskCount(const int32 DrawableIndex) const;
const int32* GetDrawableMask(const int32 DrawableIndex) const;
int32 GetDrawableVertexCount(const int32 DrawableIndex) const;
const csmVector2* GetDrawableVertexPosition(const int32 DrawableIndex) const;
const csmVector2* GetDrawableVertexUv(const int32 DrawableIndex) const;
int32 GetDrawableVertexIndexCount(const int32 DrawableIndex) const;
const uint16* GetDrawableVertexIndex(const int32 DrawableIndex) const;
FLinearColor GetDrawableMultiplyColor(const int32 DrawableIndex) const;
FLinearColor GetDrawableScreenColor(const int32 DrawableIndex) const;
int32 GetDrawableParentPartIndex(const int32 DrawableIndex) const;
UPROPERTY()
TMap<FString, int32> DrawableIndices;
private:
friend class UCubismParameterComponent;
ECubismParameterType GetParameterType(const int32 ParameterIndex) const;
float GetParameterMinimumValue(const int32 ParameterIndex) const;
float GetParameterMaximumValue(const int32 ParameterIndex) const;
float GetParameterDefaultValue(const int32 ParameterIndex) const;
float GetParameterValue(const int32 ParameterIndex) const;
void SetParameterValue(const int32 ParameterIndex, const float Value);
int32 GetParameterKeyCount(const int32 ParameterIndex) const;
const float* GetParameterKeyValue(const int32 ParameterIndex) const;
void AddParameter(const FString ParameterId);
UPROPERTY()
TMap<FString, int32> ParameterIndices;
UPROPERTY()
TMap<int32, FString> NonNativeParameterIds;
UPROPERTY()
TMap<int32, float> NonNativeParameterValues;
private:
friend class UCubismPartComponent;
float GetPartOpacity(const int32 PartIndex) const;
void SetPartOpacity(const int32 PartIndex, const float InOpacity);
int GetPartParentPartIndex(const int32 PartIndex) const;
void AddPart(const FString PartId);
UPROPERTY()
TMap<FString, int32> PartIndices;
UPROPERTY()
TMap<int32, FString> NonNativePartIds;
UPROPERTY()
TMap<int32, float> NonNativePartOpacities;
private:
friend class UCubismMoc3;
csmModel* RawModel;
public:
virtual void PostLoad() override;
#if WITH_EDITORONLY_DATA
void PostEditChangeProperty(FPropertyChangedEvent& PropertyChangedEvent) override;
#endif
virtual void OnComponentCreated() override;
virtual void OnComponentDestroyed(bool bDestroyingHierarchy) override;
#if WITH_EDITOR
virtual void PostEditUndo() override;
#endif
virtual void TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction) override;
};