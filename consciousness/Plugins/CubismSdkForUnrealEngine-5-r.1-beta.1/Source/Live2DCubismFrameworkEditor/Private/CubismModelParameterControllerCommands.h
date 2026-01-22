#pragma once
#include "Framework/Commands/Commands.h"
class FCubismModelParameterControllerCommands : public TCommands<FCubismModelParameterControllerCommands>
{
public:
FCubismModelParameterControllerCommands()
: TCommands<FCubismModelParameterControllerCommands>(
TEXT("CubismModelParameterController"),
NSLOCTEXT("Contexts", "CubismModelParameterController", "Cubism Model Parameter Controller"),
NAME_None,
FName(TEXT("CubismModelParameterControllerStyle"))
)
{
}
virtual void RegisterCommands() override;
public:
TSharedPtr<FUICommandInfo> OpenPluginWindow;
};