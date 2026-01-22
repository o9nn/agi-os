#pragma once
#include "Model/CubismModelActor.h"
#include "Model/CubismModelComponent.h"
#include "Model/CubismParameterComponent.h"
#include "Model/CubismPartComponent.h"
class SCubismModelParameterControllerWindow : public SCompoundWidget
{
public:
SLATE_BEGIN_ARGS(SCubismModelParameterControllerWindow)
: _ModelActor(nullptr)
{}
SLATE_ARGUMENT(TWeakObjectPtr<ACubismModel>, ModelActor)
SLATE_END_ARGS()
~SCubismModelParameterControllerWindow();
void Construct(const FArguments& InArgs);
virtual void Tick(const FGeometry& AllottedGeometry, const double InCurrentTime, const float InDeltaTime) override;
private:
void HandleEditorSelectionChanged(const TArray<UObject*>& NewSelection, bool bForceRefresh);
void RefreshTabs();
TSharedPtr<SWidget> BuildParameterList();
TSharedPtr<SWidget> BuildPartList();
void OnParameterNumericValueCommitted(float NewValue, const FString& ParameterId);
void OnParameterSliderValueChanged(float NewValue, const FString& ParameterId);
void OnPartNumericValueCommitted(float NewValue, const FString& PartId);
void OnPartSliderValueChanged(float NewValue, const FString& PartId);
void SetCurrentTab(int32 NewTabIndex);
private:
TWeakObjectPtr<ACubismModel> ModelActor;
TSharedPtr<SWidgetSwitcher> TabContentSwitcher;
TSharedPtr<SBox> ParametersContainer;
TSharedPtr<SBox> PartsContainer;
int32 CurrentTab = 0;
};