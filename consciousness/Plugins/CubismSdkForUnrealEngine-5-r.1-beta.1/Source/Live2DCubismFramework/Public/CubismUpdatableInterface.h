#pragma once
#include "UObject/Interface.h"
#include "CubismUpdatableInterface.generated.h"
UINTERFACE(MinimalAPI)
class UCubismUpdatableInterface : public UInterface
{
GENERATED_BODY()
};
class ICubismUpdatableInterface
{
GENERATED_BODY()
public:
virtual int32 GetExecutionOrder() const = 0;
virtual void OnCubismUpdate(float DeltaTime) = 0;
virtual bool IsControlledByUpdateController() const = 0;
};