#pragma once
#include "EditorReimportHandler.h"
#include "CubismMoc3Factory.generated.h"
UCLASS()
class UCubismMoc3Factory : public UFactory, public FReimportHandler
{
GENERATED_BODY()
UCubismMoc3Factory();
virtual FText GetToolTip() const override;
virtual bool FactoryCanImport(const FString& Filename) override;
virtual UObject* FactoryCreateBinary(
UClass* InClass, UObject* InParent, FName InName, EObjectFlags Flags,
UObject* Context, const TCHAR* Type, const uint8*& Buffer, const uint8* BufferEnd,
FFeedbackContext * Warn
) override;
virtual bool CanReimport(UObject* Obj, TArray<FString>& OutFilenames) override;
virtual void SetReimportPaths(UObject* Obj, const TArray<FString>& NewReimportPaths) override;
virtual EReimportResult::Type Reimport(UObject* Obj) override;
};