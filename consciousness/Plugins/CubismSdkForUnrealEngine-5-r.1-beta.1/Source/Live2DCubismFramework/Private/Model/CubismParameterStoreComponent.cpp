#include "Model/CubismParameterStoreComponent.h"
#include "Components/ActorComponent.h"
#include "Model/CubismParameterComponent.h"
#include "Model/CubismPartComponent.h"
#include "Model/CubismModelActor.h"
UCubismParameterStoreComponent::UCubismParameterStoreComponent()
{
PrimaryComponentTick.bCanEverTick = true;
PrimaryComponentTick.TickGroup = TG_PrePhysics;
bTickInEditor = true;
}
void UCubismParameterStoreComponent::Setup(UCubismModelComponent* InModel)
{
if (!InModel)
{
return;
}
if (Model != InModel)
{
Model = InModel;
}
ParameterValues.Empty();
PartOpacities.Empty();
SaveParameters();
if (Model->ParameterStore != this)
{
if (Model->ParameterStore)
{
Model->ParameterStore->DestroyComponent();
}
Model->ParameterStore = this;
}
}
void UCubismParameterStoreComponent::SaveParameterValue(const int32 ParameterIndex)
{
if (const UCubismParameterComponent* Parameter = Model->GetParameter(ParameterIndex))
{
ParameterValues.Add(ParameterIndex, Parameter->Value);
}
}
void UCubismParameterStoreComponent::SavePartOpacity(const int32 PartIndex)
{
if (const UCubismPartComponent* Part = Model->GetPart(PartIndex))
{
PartOpacities.Add(PartIndex, Part->Opacity);
}
}
void UCubismParameterStoreComponent::SaveParameters()
{
for (const TObjectPtr<UCubismParameterComponent>& Parameter : Model->Parameters)
{
if (!IsValid(Parameter))
{
continue;
}
if (ParameterValues.Contains(Parameter->Index))
{
ParameterValues[Parameter->Index] = Parameter->Value;
}
else
{
ParameterValues.Add(Parameter->Index, Parameter->Value);
}
}
for (const TObjectPtr<UCubismPartComponent>& Part : Model->Parts)
{
if (!IsValid(Part))
{
continue;
}
if (PartOpacities.Contains(Part->Index))
{
PartOpacities[Part->Index] = Part->Opacity;
}
else
{
PartOpacities.Add(Part->Index, Part->Opacity);
}
}
}
void UCubismParameterStoreComponent::LoadParameters()
{
for (const TObjectPtr<UCubismParameterComponent>& Parameter : Model->Parameters)
{
if (!IsValid(Parameter))
{
continue;
}
if (ParameterValues.Contains(Parameter->Index))
{
Parameter->SetParameterValue(ParameterValues[Parameter->Index]);
}
else
{
ParameterValues.Add(Parameter->Index, Parameter->Value);
}
}
for (const TObjectPtr<UCubismPartComponent>& Part : Model->Parts)
{
if (!IsValid(Part))
{
continue;
}
if (PartOpacities.Contains(Part->Index))
{
Part->SetPartOpacity(PartOpacities[Part->Index]);
}
else
{
PartOpacities.Add(Part->Index, Part->Opacity);
}
}
}
void UCubismParameterStoreComponent::PostLoad()
{
Super::PostLoad();
const ACubismModel* Owner = Cast<ACubismModel>(GetOwner());
Setup(Owner->Model);
}
void UCubismParameterStoreComponent::OnComponentCreated()
{
Super::OnComponentCreated();
const ACubismModel* Owner = Cast<ACubismModel>(GetOwner());
Setup(Owner->Model);
}
void UCubismParameterStoreComponent::OnComponentDestroyed(bool bDestroyingHierarchy)
{
if (Model && Model->ParameterStore == this)
{
Model->ParameterStore = nullptr;
}
Super::OnComponentDestroyed(bDestroyingHierarchy);
}
#if WITH_EDITOR
void UCubismParameterStoreComponent::PostEditUndo()
{
Super::PostEditUndo();
const ACubismModel* Owner = Cast<ACubismModel>(GetOwner());
Setup(Owner->Model);
}
#endif
void UCubismParameterStoreComponent::TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction)
{
Super::TickComponent(DeltaTime, TickType, ThisTickFunction);
if (!Model)
{
return;
}
LoadParameters();
}