#include "Model/CubismPartComponent.h"
#include "Model/CubismParameterStoreComponent.h"
#include "Model/CubismModelActor.h"
UCubismPartComponent::UCubismPartComponent()
{
PrimaryComponentTick.bCanEverTick = true;
PrimaryComponentTick.TickGroup = TG_DuringPhysics;
bTickInEditor = true;
}
void UCubismPartComponent::Setup(UCubismModelComponent* InModel)
{
if (!InModel)
{
return;
}
check(Index >= 0 && Index < InModel->GetPartCount() || InModel->NonNativePartIds.Contains(Index));
if (Model == InModel)
{
return;
}
Model = InModel;
if (Index >= 0 && Index < InModel->GetPartCount())
{
Id = Model->GetPartId(Index);
Opacity = Model->GetPartOpacity(Index);
}
else
{
Id = Model->NonNativePartIds[Index];
Opacity = 1.0f;
}
check(!FGenericPlatformMath::IsNaN(Opacity));
}
void UCubismPartComponent::SetPartOpacity(float TargetOpacity)
{
Opacity = TargetOpacity;
Model->SetPartOpacity(Index, Opacity);
}
void UCubismPartComponent::PostLoad()
{
Super::PostLoad();
const ACubismModel* Owner = Cast<ACubismModel>(GetOwner());
Setup(Owner->Model);
}
#if WITH_EDITOR
void UCubismPartComponent::PostEditChangeProperty(FPropertyChangedEvent& PropertyChangedEvent)
{
Super::PostEditChangeProperty(PropertyChangedEvent);
const FName PropertyName = PropertyChangedEvent.GetPropertyName();
if (PropertyName == GET_MEMBER_NAME_CHECKED(UCubismPartComponent, Opacity))
{
Model->SetPartOpacity(Index, Opacity);
if(Model->ParameterStore)
{
Model->ParameterStore->SavePartOpacity(Index);
}
}
}
#endif
void UCubismPartComponent::OnComponentCreated()
{
Super::OnComponentCreated();
const ACubismModel* Owner = Cast<ACubismModel>(GetOwner());
Setup(Owner->Model);
}
#if WITH_EDITOR
void UCubismPartComponent::PostEditUndo()
{
Super::PostEditUndo();
const ACubismModel* Owner = Cast<ACubismModel>(GetOwner());
Setup(Owner->Model);
}
#endif