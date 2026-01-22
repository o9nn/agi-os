#pragma once
#include "Live2DCubismCore.h"
#include "Modules/ModuleManager.h"
class LIVE2DCUBISMFRAMEWORK_API FLive2DCubismFrameworkModule : public IModuleInterface
{
public:
virtual void StartupModule() override;
virtual void ShutdownModule() override;
private:
bool bIsModuleStarted;
};