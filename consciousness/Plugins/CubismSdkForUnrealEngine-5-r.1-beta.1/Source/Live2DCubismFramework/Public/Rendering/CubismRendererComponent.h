#pragma once
#include "Components/ActorComponent.h"
#include "CubismUpdatableInterface.h"
#include "CubismRendererComponent.generated.h"
class ACubismModel;
class ACubismMaskTexture;
class FCubismMaskJunction;
class UCubismModelComponent;
UENUM(BlueprintType)
enum class ECubismRendererSortingOrder : uint8
{
FrontToBack,
BackToFront,
};
UCLASS( ClassGroup=(Custom), meta=(BlueprintSpawnableComponent) )
class LIVE2DCUBISMFRAMEWORK_API UCubismRendererComponent : public UActorComponent, public ICubismUpdatableInterface
{
GENERATED_BODY()
public:
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
TObjectPtr<ACubismMaskTexture> MaskTexture;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
ECubismRendererSortingOrder SortingOrder = ECubismRendererSortingOrder::FrontToBack;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
bool bZSort = false;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism")
int32 RenderOrder = 0;
UPROPERTY(EditAnywhere, BlueprintReadWrite, Category = "Live2D Cubism", meta = (EditCondition = "bZSort"))
float Epsilon = 0.1f;
UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Live2D Cubism")
int32 NumMasks;
TArray<TSharedPtr<FCubismMaskJunction>> Junctions;
virtual bool IsControlledByUpdateController() const override { return true; }
virtual int32 GetExecutionOrder() const override;
virtual void OnCubismUpdate(float DeltaTime) override;
public:
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
void Setup(UCubismModelComponent* InModel);
UFUNCTION(BlueprintCallable, Category = "Live2D Cubism")
int32 CalcRenderOrder(const UCubismDrawableComponent* Drawable) const;
private:
friend class UCubismModelComponent;
UPROPERTY()
TObjectPtr<UCubismModelComponent> Model;
private:
UCubismRendererComponent();
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