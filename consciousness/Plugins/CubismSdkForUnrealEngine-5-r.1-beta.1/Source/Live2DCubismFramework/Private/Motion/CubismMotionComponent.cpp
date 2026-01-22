#include "Motion/CubismMotionComponent.h"
#include "CubismUpdateExecutionOrder.h"
#include "Motion/CubismMotion3Json.h"
#include "Motion/CubismMotion.h"
#include "Model/CubismParameterComponent.h"
#include "Model/CubismParameterStoreComponent.h"
#include "Model/CubismPartComponent.h"
#include "Model/CubismModelActor.h"
#include "CubismLog.h"
UCubismMotionComponent::UCubismMotionComponent()
{
PrimaryComponentTick.bCanEverTick = true;
PrimaryComponentTick.TickGroup = TG_PrePhysics;
bTickInEditor = true;
}
void UCubismMotionComponent::Setup(UCubismModelComponent* InModel)
{
if (!InModel)
{
UE_LOG(LogTemp, Warning, TEXT("CubismMotionComponent::Setup - InModel is null. Skipping setup."));
return;
}
check(InModel);
if (Model != InModel)
{
Model = InModel;
}
Time = 0.0f;
MotionQueue.Empty();
if (Model->Motion != this)
{
if (Model->Motion)
{
Model->Motion->DestroyComponent();
}
Model->Motion = this;
}
AddTickPrerequisiteComponent(Model->ParameterStore);
}
bool UCubismMotionComponent::IsFinished() const
{
for (const TSharedPtr<FCubismMotion>& Motion : MotionQueue)
{
if (Motion->State != ECubismMotionState::End)
{
return false;
}
}
return true;
}
bool UCubismMotionComponent::ReserveMotion(const ECubismMotionPriority Priority)
{
if (Priority <= ReservedPriority || Priority <= CurrentPriority)
{
return false;
}
ReservedPriority = Priority;
return true;
}
void UCubismMotionComponent::PlayMotion(const int32 InIndex, const float OffsetTime, const ECubismMotionPriority Priority)
{
if (!Jsons.IsValidIndex(InIndex))
{
UE_LOG(LogCubism, Warning, TEXT("Motion cannot be played. Index is out of range."));
return;
}
const TObjectPtr<UCubismMotion3Json>& Json = Jsons[InIndex];
if (Priority == ReservedPriority || Priority == ECubismMotionPriority::Force)
{
ReservedPriority = ECubismMotionPriority::None;
}
CurrentPriority = Priority;
for (const TSharedPtr<FCubismMotion>& Motion : MotionQueue)
{
Motion->SetFadeout(Motion->FadeOutTime);
}
TSharedPtr<FCubismMotion> NextMotion = MakeShared<FCubismMotion>(Json, OffsetTime);
MotionQueue.Add(NextMotion);
}
void UCubismMotionComponent::StopAllMotions(const bool bForce)
{
if (bForce)
{
MotionQueue.Empty();
}
else
{
for (const TSharedPtr<FCubismMotion>& Motion : MotionQueue)
{
Motion->FadeOut(Time);
}
}
}
void UCubismMotionComponent::PostLoad()
{
Super::PostLoad();
const ACubismModel* Owner = Cast<ACubismModel>(GetOwner());
if (!Owner || !Owner->Model)
{
UE_LOG(LogTemp, Warning, TEXT("No Owner or Model."));
return;
}
Setup(Owner->Model);
if (Index >= 0 && Jsons.IsValidIndex(Index))
{
PlayMotion(Index, 0.0f, ECubismMotionPriority::Normal);
}
else
{
UE_LOG(LogTemp, Warning, TEXT("CubismMotionComponent: Animation not started (index %d)"), Index);
}
}
#if WITH_EDITOR
void UCubismMotionComponent::PostEditChangeProperty(struct FPropertyChangedEvent& PropertyChangedEvent)
{
Super::PostEditChangeProperty(PropertyChangedEvent);
const FName PropertyName = PropertyChangedEvent.Property ? PropertyChangedEvent.Property->GetFName() : NAME_None;
if (PropertyName == GET_MEMBER_NAME_CHECKED(UCubismMotionComponent, Index))
{
if (Jsons.IsValidIndex(Index))
{
PlayMotion(Index, 0.0f, ECubismMotionPriority::Force);
}
else
{
StopAllMotions();
}
}
}
#endif
void UCubismMotionComponent::OnComponentCreated()
{
Super::OnComponentCreated();
const ACubismModel* Owner = Cast<ACubismModel>(GetOwner());
Setup(Owner->Model);
}
void UCubismMotionComponent::OnComponentDestroyed(bool bDestroyingHierarchy)
{
if (!Model) return;
if (Model->Motion == this)
{
Model->Motion = nullptr;
}
Super::OnComponentDestroyed(bDestroyingHierarchy);
}
#if WITH_EDITOR
void UCubismMotionComponent::PostEditUndo()
{
Super::PostEditUndo();
const ACubismModel* Owner = Cast<ACubismModel>(GetOwner());
Setup(Owner->Model);
}
#endif
void UCubismMotionComponent::TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction)
{
Super::TickComponent(DeltaTime, TickType, ThisTickFunction);
if (IsControlledByUpdateController())
{
return;
}
OnCubismUpdate(DeltaTime);
}
void UCubismMotionComponent::OnCubismUpdate(float DeltaTime)
{
if (!Model)
{
return;
}
Time += Speed * DeltaTime;
for (int32 i = 0; i < MotionQueue.Num();)
{
TSharedPtr<FCubismMotion>& Motion = MotionQueue[i];
if (Motion->State == ECubismMotionState::None)
{
Motion->Init(Time);
}
float FadeWeight = Motion->UpdateFadeWeight(Motion, Time);
UpdateMotion(Time, FadeWeight, Motion);
if (Motion->IsFinished())
{
MotionQueue.RemoveAt(i);
}
else
{
if (Motion->IsTriggeredFadeOut())
{
Motion->StartFadeout(Motion->GetFadeOutSeconds(), Time);
}
i++;
}
}
if (IsFinished())
{
CurrentPriority = ECubismMotionPriority::None;
OnMotionPlaybackFinished.Broadcast();
}
}
int32 UCubismMotionComponent::GetExecutionOrder() const
{
return CUBISM_EXECUTION_ORDER_MOTION;
}
void UCubismMotionComponent::UpdateMotion(float UserTimeSeconds, float FadeWeight, const TSharedPtr<FCubismMotion>& CubismMotion)
{
float TimeOffsetSeconds = UserTimeSeconds - CubismMotion->StartTime;
if (TimeOffsetSeconds < 0.0f)
{
TimeOffsetSeconds = 0.0f;
}
const float TmpFadeIn = (CubismMotion->FadeInTime <= 0.0f)
? 1.0f
: FCubismMotion::EasingSin((UserTimeSeconds - CubismMotion->EndTime) / CubismMotion->FadeInTime);
const float TmpFadeOut = (CubismMotion->FadeOutTime <= 0.0f || CubismMotion->EndTime < 0.0f)
? 1.0f
: FCubismMotion::EasingSin((CubismMotion->EndTime - UserTimeSeconds) / CubismMotion->FadeOutTime);
float MotionTime = TimeOffsetSeconds;
if (CubismMotion->State == ECubismMotionState::PlayInLoop)
{
while (MotionTime > CubismMotion->Duration)
{
MotionTime -= CubismMotion->Duration;
}
}
TArray<FCubismMotionCurve> Curves = CubismMotion->Curves;
for (const FCubismMotionCurve& Curve : Curves)
{
if (Curve.Target != ECubismMotionCurveTarget::Model)
{
continue;
}
float Value = CubismMotion->GetValue(Curve.Id, MotionTime);
if (Curve.Id == "PartOpacity")
{
Model->Opacity = Value;
}
}
for (const FCubismMotionCurve& Curve : Curves)
{
if (Curve.Target != ECubismMotionCurveTarget::Parameter)
{
continue;
}
UCubismParameterComponent* Parameter = Model->GetParameter(Curve.Id);
if (!Parameter)
{
continue;
}
const float SourceValue = Parameter->Value;
float Value = CubismMotion->GetValue(Curve.Id, MotionTime);
float NewValue;
if (Curve.FadeInTime < 0.0f && Curve.FadeOutTime < 0.0f)
{
NewValue = SourceValue + (Value - SourceValue) * FadeWeight;
}
else
{
float FadeInWeight;
float FadeOutWeight;
if (Curve.FadeInTime < 0.0f)
{
FadeInWeight = TmpFadeIn;
}
else
{
FadeInWeight = Curve.FadeInTime == 0.0f
? 1.0f
: FCubismMotion::EasingSin((UserTimeSeconds - CubismMotion->StartTime) / Curve.FadeInTime);
}
if (Curve.FadeOutTime < 0.0f)
{
FadeOutWeight = TmpFadeOut;
}
else
{
FadeOutWeight = (Curve.FadeOutTime == 0.0f || CubismMotion->EndTime < 0.0f)
? 1.0f
: FCubismMotion::EasingSin((UserTimeSeconds - CubismMotion->EndTime) / Curve.FadeOutTime);
}
const float ParamWeight = CubismMotion->GetWeight() * FadeInWeight * FadeOutWeight;
NewValue = SourceValue + (Value - SourceValue) * ParamWeight;
}
Parameter->SetParameterValue(NewValue);
}
for (const FCubismMotionCurve& Curve : Curves)
{
if (Curve.Target != ECubismMotionCurveTarget::PartOpacity)
{
continue;
}
UCubismParameterComponent* Parameter = Model->GetParameter(Curve.Id);
if (!Parameter)
{
continue;
}
float Value = CubismMotion->GetValue(Curve.Id, MotionTime);
Parameter->SetParameterValue(Value);
}
if ((CubismMotion->GetEndTime() > 0.0f) && (CubismMotion->GetEndTime() < UserTimeSeconds))
{
CubismMotion->IsFinished(true);
}
if (Time - CubismMotion->StartTime > CubismMotion->Duration)
{
if (CubismMotion->State == ECubismMotionState::PlayInLoop)
{
CubismMotion->StartTime = UserTimeSeconds;
}
else
{
CubismMotion->IsFinished(true);
}
}
}