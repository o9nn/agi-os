#include "Effects/HarmonicMotion/CubismHarmonicMotionComponent.h"
#include "Effects/HarmonicMotion/CubismHarmonicMotionParameter.h"
#include "Model/CubismModelActor.h"
#include "Model/CubismModelComponent.h"
#include "Model/CubismParameterComponent.h"
UCubismHarmonicMotionComponent::UCubismHarmonicMotionComponent()
{
PrimaryComponentTick.bCanEverTick = true;
PrimaryComponentTick.TickGroup = TG_DuringPhysics;
bTickInEditor = true;
}
void UCubismHarmonicMotionComponent::Setup(UCubismModelComponent* InModel)
{
if (!InModel)
{
return;
}
if (Model != InModel)
{
Model = InModel;
}
if (Model->HarmonicMotion != this)
{
if (Model->HarmonicMotion)
{
Model->HarmonicMotion->DestroyComponent();
}
Model->HarmonicMotion = this;
}
Model->AddTickPrerequisiteComponent(this);
}
void UCubismHarmonicMotionComponent::PostLoad()
{
Super::PostLoad();
const ACubismModel* Owner = Cast<ACubismModel>(GetOwner());
Setup(Owner->Model);
}
void UCubismHarmonicMotionComponent::OnComponentCreated()
{
Super::OnComponentCreated();
const ACubismModel* Owner = Cast<ACubismModel>(GetOwner());
Setup(Owner->Model);
}
void UCubismHarmonicMotionComponent::OnComponentDestroyed(bool bDestroyingHierarchy)
{
if (Model && Model->HarmonicMotion == this)
{
Model->HarmonicMotion = nullptr;
}
Super::OnComponentDestroyed(bDestroyingHierarchy);
}
#if WITH_EDITOR
void UCubismHarmonicMotionComponent::PostEditUndo()
{
Super::PostEditUndo();
const ACubismModel* Owner = Cast<ACubismModel>(GetOwner());
Setup(Owner->Model);
}
#endif
void UCubismHarmonicMotionComponent::TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction)
{
Super::TickComponent(DeltaTime, TickType, ThisTickFunction);
if (!Model)
{
return;
}
for (FCubismHarmonicMotionParameter& Parameter : Parameters)
{
if (!Parameter.bEnabled)
{
continue;
}
UCubismParameterComponent* Destination = Model->GetParameter(Parameter.Id);
if (!Destination)
{
continue;
}
Time += DeltaTime * Parameter.TimeScale;
Parameter.Value = Parameter.CalcValue(Time, Destination->MinimumValue, Destination->MaximumValue);
switch (Parameter.BlendMode)
{
case ECubismParameterBlendMode::Overwrite:
{
Destination->SetParameterValue(Parameter.Value);
break;
}
case ECubismParameterBlendMode::Additive:
{
Destination->AddParameterValue(Parameter.Value);
break;
}
case ECubismParameterBlendMode::Multiplicative:
{
Destination->MultiplyParameterValue(Parameter.Value);
break;
}
default:
{
ensure(false);
break;
}
}
}
}