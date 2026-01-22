#pragma once
#include "Model/CubismModelComponent.h"
#include "Components/MeshComponent.h"
#include "CubismDrawableComponent.generated.h"
class UTexture2D;
class UTextureRenderTarget2D;
UCLASS(Blueprintable)
class LIVE2DCUBISMFRAMEWORK_API UCubismDrawableComponent : public UMeshComponent
{
GENERATED_BODY()
public:
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
int32 Index;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
FString Id;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
int32 RenderOrder;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
int32 RenderOrderOffset = 0;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
int32 TextureIndex;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta = (ClampMin = "0.0", ClampMax = "1.0", SliderMin = "0.0", SliderMax = "1.0"))
float Opacity;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
FLinearColor BaseColor;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bOverwriteFlagForDrawableMultiplyColors;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta = (EditCondition = "bOverwriteFlagForDrawableMultiplyColors"))
FLinearColor MultiplyColor;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bOverwriteFlagForDrawableScreenColors;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta = (EditCondition = "bOverwriteFlagForDrawableScreenColors"))
FLinearColor ScreenColor;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bOverwriteFlagForDrawableIsTwoSided;
UPROPERTY(EditAnywhere, BlueprintReadOnly, Category = "Live2D Cubism", meta = (EditCondition = "bOverwriteFlagForDrawableIsTwoSided"))
bool bTwoSided;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
int32 ParentPartIndex;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
ECubismDrawableBlendMode BlendMode;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
bool InvertedMask;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
TArray<int32> Masks;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
FString UserDataTag;
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup(UCubismModelComponent* InModel);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
inline bool IsMasked() const
{
return Masks.Num() > 0;
}
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
FVector ToGlobalPosition(const FVector2D VertexPosition) const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
TArray<int32> GetVertexIndices() const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
TArray<FVector2D> GetVertexPositions() const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
TArray<FVector2D> GetVertexUvs() const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
const TArray<int32> GetDrawableMask() const;
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
int32 GetDrawableMaskCount() const;
private:
friend class UCubismModelComponent;
UPROPERTY()
TObjectPtr<UCubismModelComponent> Model;
private:
UCubismDrawableComponent();
mutable bool bBoundsDirty;
mutable FBoxSphereBounds LocalBounds;
UPROPERTY()
TArray<int32> VertexIndices;
UPROPERTY()
TArray<FVector2D> VertexPositions;
UPROPERTY()
TArray<FVector2D> VertexUvs;
FLinearColor UserMultiplyColor;
FLinearColor UserScreenColor;
bool bUserTwoSided;
public:
virtual FBoxSphereBounds CalcBounds(const FTransform& LocalToWorld) const override;
private:
virtual void PostLoad() override;
#if WITH_EDITORONLY_DATA
void PostEditChangeProperty(FPropertyChangedEvent& PropertyChangedEvent) override;
#endif
virtual void OnComponentCreated() override;
#if WITH_EDITOR
virtual void PostEditUndo() override;
#endif
virtual void SendRenderDynamicData_Concurrent() override;
virtual void TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction) override;
virtual int32 GetNumMaterials() const override { return 1; }
virtual FPrimitiveSceneProxy* CreateSceneProxy() override;
};