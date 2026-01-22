#pragma once
#include "EditorReimportHandler.h"
#include "CubismPhysics3JsonFactory.generated.h"
UCLASS()
class UCubismPhysics3JsonFactory : public UFactory, public FReimportHandler
{
GENERATED_BODY()
UCubismPhysics3JsonFactory();
virtual FText GetToolTip() const override;
virtual bool FactoryCanImport(const FString& Filename) override;
virtual UObject* FactoryCreateText(
UClass* InClass, UObject* InParent, FName InName, EObjectFlags Flags,
UObject* Context, const TCHAR* Type, const TCHAR*& Buffer, const TCHAR* BufferEnd,
FFeedbackContext * Warn
) override;
virtual bool CanReimport(UObject* Obj, TArray<FString>& OutFilenames) override;
virtual void SetReimportPaths(UObject* Obj, const TArray<FString>& NewReimportPaths) override;
virtual EReimportResult::Type Reimport(UObject* Obj) override;
};