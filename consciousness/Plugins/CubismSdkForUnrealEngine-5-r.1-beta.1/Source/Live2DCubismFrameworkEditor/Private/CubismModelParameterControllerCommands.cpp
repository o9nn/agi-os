#pragma once
#include "CubismModelParameterControllerCommands.h"
#define LOCTEXT_NAMESPACE "CubismModelParameterController"
void FCubismModelParameterControllerCommands::RegisterCommands()
{
UI_COMMAND(OpenPluginWindow, "Cubism Model Parameter Controller", "Open the Cubism model parameter controller tab window.", EUserInterfaceActionType::Button, FInputChord());
}
#undef LOCTEXT_NAMESPACE