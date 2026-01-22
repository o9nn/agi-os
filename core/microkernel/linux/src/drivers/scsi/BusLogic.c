#define BusLogic_DriverVersion		"2.0.15"
#define BusLogic_DriverDate		"17 August 1998"
#include <linux/version.h>
#include <linux/module.h>
#include <linux/config.h>
#include <linux/types.h>
#include <linux/blkdev.h>
#include <linux/delay.h>
#include <linux/ioport.h>
#include <linux/mm.h>
#include <linux/sched.h>
#include <linux/stat.h>
#include <linux/pci.h>
#include <linux/bios32.h>
#include <asm/dma.h>
#include <asm/io.h>
#include <asm/irq.h>
#include <asm/system.h>
#include "scsi.h"
#include "hosts.h"
#include "sd.h"
#include "BusLogic.h"
#include "FlashPoint.c"
static int
BusLogic_DriverOptionsCount =			0;
static BusLogic_DriverOptions_T
BusLogic_DriverOptions[BusLogic_MaxHostAdapters];
static char
*BusLogic_Options =				NULL;
static BusLogic_ProbeOptions_T
BusLogic_ProbeOptions =			{ 0 };
static BusLogic_GlobalOptions_T
BusLogic_GlobalOptions =			{ 0 };
static BusLogic_HostAdapter_T
*BusLogic_FirstRegisteredHostAdapter =	NULL,
*BusLogic_LastRegisteredHostAdapter =		NULL;
static int
BusLogic_ProbeInfoCount =			0;
static BusLogic_ProbeInfo_T
*BusLogic_ProbeInfoList =			NULL;
static char
*BusLogic_CommandFailureReason;
PROC_DirectoryEntry_T
BusLogic_ProcDirectoryEntry =
{ PROC_SCSI_BUSLOGIC, 8, "BusLogic", S_IFDIR | S_IRUGO | S_IXUGO, 2 };
static void BusLogic_AnnounceDriver(BusLogic_HostAdapter_T *HostAdapter)
{
BusLogic_Announce("***** BusLogic SCSI Driver Version "
BusLogic_DriverVersion " of "
BusLogic_DriverDate " *****\n", HostAdapter);
BusLogic_Announce("Copyright 1995-1998 by Leonard N. Zubkoff "
"<lnz@dandelion.com>\n", HostAdapter);
}
const char *BusLogic_DriverInfo(SCSI_Host_T *Host)
{
BusLogic_HostAdapter_T *HostAdapter =
(BusLogic_HostAdapter_T *) Host->hostdata;
return HostAdapter->FullModelName;
}
static void BusLogic_RegisterHostAdapter(BusLogic_HostAdapter_T *HostAdapter)
{
HostAdapter->Next = NULL;
if (BusLogic_FirstRegisteredHostAdapter == NULL)
{
BusLogic_FirstRegisteredHostAdapter = HostAdapter;
BusLogic_LastRegisteredHostAdapter = HostAdapter;
}
else
{
BusLogic_LastRegisteredHostAdapter->Next = HostAdapter;
BusLogic_LastRegisteredHostAdapter = HostAdapter;
}
}
static void BusLogic_UnregisterHostAdapter(BusLogic_HostAdapter_T *HostAdapter)
{
if (HostAdapter == BusLogic_FirstRegisteredHostAdapter)
{
BusLogic_FirstRegisteredHostAdapter =
BusLogic_FirstRegisteredHostAdapter->Next;
if (HostAdapter == BusLogic_LastRegisteredHostAdapter)
BusLogic_LastRegisteredHostAdapter = NULL;
}
else
{
BusLogic_HostAdapter_T *PreviousHostAdapter =
BusLogic_FirstRegisteredHostAdapter;
while (PreviousHostAdapter != NULL &&
PreviousHostAdapter->Next != HostAdapter)
PreviousHostAdapter = PreviousHostAdapter->Next;
if (PreviousHostAdapter != NULL)
PreviousHostAdapter->Next = HostAdapter->Next;
}
HostAdapter->Next = NULL;
}
static void BusLogic_InitializeCCBs(BusLogic_HostAdapter_T *HostAdapter,
void *BlockPointer, int BlockSize)
{
BusLogic_CCB_T *CCB = (BusLogic_CCB_T *) BlockPointer;
memset(BlockPointer, 0, BlockSize);
CCB->AllocationGroupHead = true;
while ((BlockSize -= sizeof(BusLogic_CCB_T)) >= 0)
{
CCB->Status = BusLogic_CCB_Free;
CCB->HostAdapter = HostAdapter;
if (BusLogic_FlashPointHostAdapterP(HostAdapter))
{
CCB->CallbackFunction = BusLogic_QueueCompletedCCB;
CCB->BaseAddress = HostAdapter->FlashPointInfo.BaseAddress;
}
CCB->Next = HostAdapter->Free_CCBs;
CCB->NextAll = HostAdapter->All_CCBs;
HostAdapter->Free_CCBs = CCB;
HostAdapter->All_CCBs = CCB;
HostAdapter->AllocatedCCBs++;
CCB++;
}
}
static boolean BusLogic_CreateInitialCCBs(BusLogic_HostAdapter_T *HostAdapter)
{
int BlockSize = BusLogic_CCB_AllocationGroupSize * sizeof(BusLogic_CCB_T);
while (HostAdapter->AllocatedCCBs < HostAdapter->InitialCCBs)
{
void *BlockPointer = kmalloc(BlockSize,
(HostAdapter->BounceBuffersRequired
? GFP_ATOMIC | GFP_DMA
: GFP_ATOMIC));
if (BlockPointer == NULL)
{
BusLogic_Error("UNABLE TO ALLOCATE CCB GROUP - DETACHING\n",
HostAdapter);
return false;
}
BusLogic_InitializeCCBs(HostAdapter, BlockPointer, BlockSize);
}
return true;
}
static void BusLogic_DestroyCCBs(BusLogic_HostAdapter_T *HostAdapter)
{
BusLogic_CCB_T *NextCCB = HostAdapter->All_CCBs, *CCB;
HostAdapter->All_CCBs = NULL;
HostAdapter->Free_CCBs = NULL;
while ((CCB = NextCCB) != NULL)
{
NextCCB = CCB->NextAll;
if (CCB->AllocationGroupHead)
kfree(CCB);
}
}
static void BusLogic_CreateAdditionalCCBs(BusLogic_HostAdapter_T *HostAdapter,
int AdditionalCCBs,
boolean SuccessMessageP)
{
int BlockSize = BusLogic_CCB_AllocationGroupSize * sizeof(BusLogic_CCB_T);
int PreviouslyAllocated = HostAdapter->AllocatedCCBs;
if (AdditionalCCBs <= 0) return;
while (HostAdapter->AllocatedCCBs - PreviouslyAllocated < AdditionalCCBs)
{
void *BlockPointer = kmalloc(BlockSize,
(HostAdapter->BounceBuffersRequired
? GFP_ATOMIC | GFP_DMA
: GFP_ATOMIC));
if (BlockPointer == NULL) break;
BusLogic_InitializeCCBs(HostAdapter, BlockPointer, BlockSize);
}
if (HostAdapter->AllocatedCCBs > PreviouslyAllocated)
{
if (SuccessMessageP)
BusLogic_Notice("Allocated %d additional CCBs (total now %d)\n",
HostAdapter,
HostAdapter->AllocatedCCBs - PreviouslyAllocated,
HostAdapter->AllocatedCCBs);
return;
}
BusLogic_Notice("Failed to allocate additional CCBs\n", HostAdapter);
if (HostAdapter->DriverQueueDepth >
HostAdapter->AllocatedCCBs - HostAdapter->TargetDeviceCount)
{
HostAdapter->DriverQueueDepth =
HostAdapter->AllocatedCCBs - HostAdapter->TargetDeviceCount;
HostAdapter->SCSI_Host->can_queue = HostAdapter->DriverQueueDepth;
}
}
static BusLogic_CCB_T *BusLogic_AllocateCCB(BusLogic_HostAdapter_T
*HostAdapter)
{
static unsigned long SerialNumber = 0;
BusLogic_CCB_T *CCB;
CCB = HostAdapter->Free_CCBs;
if (CCB != NULL)
{
CCB->SerialNumber = ++SerialNumber;
HostAdapter->Free_CCBs = CCB->Next;
CCB->Next = NULL;
if (HostAdapter->Free_CCBs == NULL)
BusLogic_CreateAdditionalCCBs(HostAdapter,
HostAdapter->IncrementalCCBs,
true);
return CCB;
}
BusLogic_CreateAdditionalCCBs(HostAdapter,
HostAdapter->IncrementalCCBs,
true);
CCB = HostAdapter->Free_CCBs;
if (CCB == NULL) return NULL;
CCB->SerialNumber = ++SerialNumber;
HostAdapter->Free_CCBs = CCB->Next;
CCB->Next = NULL;
return CCB;
}
static void BusLogic_DeallocateCCB(BusLogic_CCB_T *CCB)
{
BusLogic_HostAdapter_T *HostAdapter = CCB->HostAdapter;
CCB->Command = NULL;
CCB->Status = BusLogic_CCB_Free;
CCB->Next = HostAdapter->Free_CCBs;
HostAdapter->Free_CCBs = CCB;
}
static int BusLogic_Command(BusLogic_HostAdapter_T *HostAdapter,
BusLogic_OperationCode_T OperationCode,
void *ParameterData,
int ParameterLength,
void *ReplyData,
int ReplyLength)
{
unsigned char *ParameterPointer = (unsigned char *) ParameterData;
unsigned char *ReplyPointer = (unsigned char *) ReplyData;
BusLogic_StatusRegister_T StatusRegister;
BusLogic_InterruptRegister_T InterruptRegister;
ProcessorFlags_T ProcessorFlags = 0;
int ReplyBytes = 0, Result;
long TimeoutCounter;
if (ReplyLength > 0)
memset(ReplyData, 0, ReplyLength);
if (!HostAdapter->IRQ_ChannelAcquired)
{
save_flags(ProcessorFlags);
cli();
}
TimeoutCounter = 10000;
while (--TimeoutCounter >= 0)
{
StatusRegister.All = BusLogic_ReadStatusRegister(HostAdapter);
if (StatusRegister.Bits.HostAdapterReady &&
!StatusRegister.Bits.CommandParameterRegisterBusy)
break;
udelay(100);
}
if (TimeoutCounter < 0)
{
BusLogic_CommandFailureReason = "Timeout waiting for Host Adapter Ready";
Result = -2;
goto Done;
}
HostAdapter->HostAdapterCommandCompleted = false;
BusLogic_WriteCommandParameterRegister(HostAdapter, OperationCode);
TimeoutCounter = 10000;
while (ParameterLength > 0 && --TimeoutCounter >= 0)
{
udelay(100);
InterruptRegister.All = BusLogic_ReadInterruptRegister(HostAdapter);
StatusRegister.All = BusLogic_ReadStatusRegister(HostAdapter);
if (InterruptRegister.Bits.CommandComplete) break;
if (HostAdapter->HostAdapterCommandCompleted) break;
if (StatusRegister.Bits.DataInRegisterReady) break;
if (StatusRegister.Bits.CommandParameterRegisterBusy) continue;
BusLogic_WriteCommandParameterRegister(HostAdapter, *ParameterPointer++);
ParameterLength--;
}
if (TimeoutCounter < 0)
{
BusLogic_CommandFailureReason =
"Timeout waiting for Parameter Acceptance";
Result = -2;
goto Done;
}
if (OperationCode == BusLogic_ModifyIOAddress)
{
StatusRegister.All = BusLogic_ReadStatusRegister(HostAdapter);
if (StatusRegister.Bits.CommandInvalid)
{
BusLogic_CommandFailureReason = "Modify I/O Address Invalid";
Result = -1;
goto Done;
}
if (BusLogic_GlobalOptions.TraceConfiguration)
BusLogic_Notice("BusLogic_Command(%02X) Status = %02X: "
"(Modify I/O Address)\n", HostAdapter,
OperationCode, StatusRegister.All);
Result = 0;
goto Done;
}
switch (OperationCode)
{
case BusLogic_InquireInstalledDevicesID0to7:
case BusLogic_InquireInstalledDevicesID8to15:
case BusLogic_InquireTargetDevices:
TimeoutCounter = 60*10000;
break;
default:
TimeoutCounter = 10000;
break;
}
while (--TimeoutCounter >= 0)
{
InterruptRegister.All = BusLogic_ReadInterruptRegister(HostAdapter);
StatusRegister.All = BusLogic_ReadStatusRegister(HostAdapter);
if (InterruptRegister.Bits.CommandComplete) break;
if (HostAdapter->HostAdapterCommandCompleted) break;
if (StatusRegister.Bits.DataInRegisterReady)
{
if (++ReplyBytes <= ReplyLength)
*ReplyPointer++ = BusLogic_ReadDataInRegister(HostAdapter);
else BusLogic_ReadDataInRegister(HostAdapter);
}
if (OperationCode == BusLogic_FetchHostAdapterLocalRAM &&
StatusRegister.Bits.HostAdapterReady) break;
udelay(100);
}
if (TimeoutCounter < 0)
{
BusLogic_CommandFailureReason = "Timeout waiting for Command Complete";
Result = -2;
goto Done;
}
BusLogic_InterruptReset(HostAdapter);
if (BusLogic_GlobalOptions.TraceConfiguration)
{
int i;
BusLogic_Notice("BusLogic_Command(%02X) Status = %02X: %2d ==> %2d:",
HostAdapter, OperationCode,
StatusRegister.All, ReplyLength, ReplyBytes);
if (ReplyLength > ReplyBytes) ReplyLength = ReplyBytes;
for (i = 0; i < ReplyLength; i++)
BusLogic_Notice(" %02X", HostAdapter,
((unsigned char *) ReplyData)[i]);
BusLogic_Notice("\n", HostAdapter);
}
if (StatusRegister.Bits.CommandInvalid)
{
udelay(1000);
StatusRegister.All = BusLogic_ReadStatusRegister(HostAdapter);
if (StatusRegister.Bits.CommandInvalid ||
StatusRegister.Bits.Reserved ||
StatusRegister.Bits.DataInRegisterReady ||
StatusRegister.Bits.CommandParameterRegisterBusy ||
!StatusRegister.Bits.HostAdapterReady ||
!StatusRegister.Bits.InitializationRequired ||
StatusRegister.Bits.DiagnosticActive ||
StatusRegister.Bits.DiagnosticFailure)
{
BusLogic_SoftReset(HostAdapter);
udelay(1000);
}
BusLogic_CommandFailureReason = "Command Invalid";
Result = -1;
goto Done;
}
if (ParameterLength > 0)
{
BusLogic_CommandFailureReason = "Excess Parameters Supplied";
Result = -1;
goto Done;
}
BusLogic_CommandFailureReason = NULL;
Result = ReplyBytes;
Done:
if (!HostAdapter->IRQ_ChannelAcquired)
restore_flags(ProcessorFlags);
return Result;
}
static void BusLogic_AppendProbeAddressISA(BusLogic_IO_Address_T IO_Address)
{
BusLogic_ProbeInfo_T *ProbeInfo;
if (BusLogic_ProbeInfoCount >= BusLogic_MaxHostAdapters) return;
ProbeInfo = &BusLogic_ProbeInfoList[BusLogic_ProbeInfoCount++];
ProbeInfo->HostAdapterType = BusLogic_MultiMaster;
ProbeInfo->HostAdapterBusType = BusLogic_ISA_Bus;
ProbeInfo->IO_Address = IO_Address;
}
static void BusLogic_InitializeProbeInfoListISA(BusLogic_HostAdapter_T
*PrototypeHostAdapter)
{
if (BusLogic_ProbeOptions.NoProbeISA) return;
if (BusLogic_ProbeOptions.LimitedProbeISA
? BusLogic_ProbeOptions.Probe330
: check_region(0x330, BusLogic_MultiMasterAddressCount) == 0)
BusLogic_AppendProbeAddressISA(0x330);
if (BusLogic_ProbeOptions.LimitedProbeISA
? BusLogic_ProbeOptions.Probe334
: check_region(0x334, BusLogic_MultiMasterAddressCount) == 0)
BusLogic_AppendProbeAddressISA(0x334);
if (BusLogic_ProbeOptions.LimitedProbeISA
? BusLogic_ProbeOptions.Probe230
: check_region(0x230, BusLogic_MultiMasterAddressCount) == 0)
BusLogic_AppendProbeAddressISA(0x230);
if (BusLogic_ProbeOptions.LimitedProbeISA
? BusLogic_ProbeOptions.Probe234
: check_region(0x234, BusLogic_MultiMasterAddressCount) == 0)
BusLogic_AppendProbeAddressISA(0x234);
if (BusLogic_ProbeOptions.LimitedProbeISA
? BusLogic_ProbeOptions.Probe130
: check_region(0x130, BusLogic_MultiMasterAddressCount) == 0)
BusLogic_AppendProbeAddressISA(0x130);
if (BusLogic_ProbeOptions.LimitedProbeISA
? BusLogic_ProbeOptions.Probe134
: check_region(0x134, BusLogic_MultiMasterAddressCount) == 0)
BusLogic_AppendProbeAddressISA(0x134);
}
#ifdef CONFIG_PCI
static void BusLogic_SortProbeInfo(BusLogic_ProbeInfo_T *ProbeInfoList,
int ProbeInfoCount)
{
int LastInterchange = ProbeInfoCount-1, Bound, j;
while (LastInterchange > 0)
{
Bound = LastInterchange;
LastInterchange = 0;
for (j = 0; j < Bound; j++)
{
BusLogic_ProbeInfo_T *ProbeInfo1 = &ProbeInfoList[j];
BusLogic_ProbeInfo_T *ProbeInfo2 = &ProbeInfoList[j+1];
if (ProbeInfo1->Bus > ProbeInfo2->Bus ||
(ProbeInfo1->Bus == ProbeInfo2->Bus &&
(ProbeInfo1->Device > ProbeInfo2->Device)))
{
BusLogic_ProbeInfo_T TempProbeInfo;
memcpy(&TempProbeInfo, ProbeInfo1, sizeof(BusLogic_ProbeInfo_T));
memcpy(ProbeInfo1, ProbeInfo2, sizeof(BusLogic_ProbeInfo_T));
memcpy(ProbeInfo2, &TempProbeInfo, sizeof(BusLogic_ProbeInfo_T));
LastInterchange = j;
}
}
}
}
static int BusLogic_InitializeMultiMasterProbeInfo(BusLogic_HostAdapter_T
*PrototypeHostAdapter)
{
BusLogic_ProbeInfo_T *PrimaryProbeInfo =
&BusLogic_ProbeInfoList[BusLogic_ProbeInfoCount];
int NonPrimaryPCIMultiMasterIndex = BusLogic_ProbeInfoCount + 1;
int NonPrimaryPCIMultiMasterCount = 0, PCIMultiMasterCount = 0;
boolean ForceBusDeviceScanningOrder = false;
boolean ForceBusDeviceScanningOrderChecked = false;
boolean StandardAddressSeen[6];
unsigned char Bus, DeviceFunction;
unsigned int BaseAddress0, BaseAddress1;
unsigned char IRQ_Channel;
BusLogic_IO_Address_T IO_Address;
BusLogic_PCI_Address_T PCI_Address;
unsigned short Index = 0;
int i;
if (BusLogic_ProbeInfoCount >= BusLogic_MaxHostAdapters) return 0;
BusLogic_ProbeInfoCount++;
for (i = 0; i < 6; i++)
StandardAddressSeen[i] = false;
PrimaryProbeInfo->IO_Address = 0;
while (pcibios_find_device(PCI_VENDOR_ID_BUSLOGIC,
PCI_DEVICE_ID_BUSLOGIC_MULTIMASTER,
Index++, &Bus, &DeviceFunction) == 0)
if (pcibios_read_config_dword(Bus, DeviceFunction,
PCI_BASE_ADDRESS_0, &BaseAddress0) == 0 &&
pcibios_read_config_dword(Bus, DeviceFunction,
PCI_BASE_ADDRESS_1, &BaseAddress1) == 0 &&
pcibios_read_config_byte(Bus, DeviceFunction,
PCI_INTERRUPT_LINE, &IRQ_Channel) == 0)
{
BusLogic_HostAdapter_T *HostAdapter = PrototypeHostAdapter;
BusLogic_PCIHostAdapterInformation_T PCIHostAdapterInformation;
BusLogic_ModifyIOAddressRequest_T ModifyIOAddressRequest;
unsigned char Device = DeviceFunction >> 3;
IO_Address = BaseAddress0 & PCI_BASE_ADDRESS_IO_MASK;
PCI_Address = BaseAddress1 & PCI_BASE_ADDRESS_MEM_MASK;
if ((BaseAddress0 & PCI_BASE_ADDRESS_SPACE)
!= PCI_BASE_ADDRESS_SPACE_IO)
{
BusLogic_Error("BusLogic: Base Address0 0x%X not I/O for "
"MultiMaster Host Adapter\n", NULL, BaseAddress0);
BusLogic_Error("at PCI Bus %d Device %d I/O Address 0x%X\n",
NULL, Bus, Device, IO_Address);
continue;
}
if ((BaseAddress1 & PCI_BASE_ADDRESS_SPACE)
!= PCI_BASE_ADDRESS_SPACE_MEMORY)
{
BusLogic_Error("BusLogic: Base Address1 0x%X not Memory for "
"MultiMaster Host Adapter\n", NULL, BaseAddress1);
BusLogic_Error("at PCI Bus %d Device %d PCI Address 0x%X\n",
NULL, Bus, Device, PCI_Address);
continue;
}
if (IRQ_Channel == 0 || IRQ_Channel >= NR_IRQS)
{
BusLogic_Error("BusLogic: IRQ Channel %d illegal for "
"MultiMaster Host Adapter\n", NULL, IRQ_Channel);
BusLogic_Error("at PCI Bus %d Device %d I/O Address 0x%X\n",
NULL, Bus, Device, IO_Address);
continue;
}
if (BusLogic_GlobalOptions.TraceProbe)
{
BusLogic_Notice("BusLogic: PCI MultiMaster Host Adapter "
"detected at\n", NULL);
BusLogic_Notice("BusLogic: PCI Bus %d Device %d I/O Address "
"0x%X PCI Address 0x%X\n", NULL,
Bus, Device, IO_Address, PCI_Address);
}
HostAdapter->IO_Address = IO_Address;
BusLogic_InterruptReset(HostAdapter);
if (BusLogic_Command(HostAdapter,
BusLogic_InquirePCIHostAdapterInformation,
NULL, 0, &PCIHostAdapterInformation,
sizeof(PCIHostAdapterInformation))
== sizeof(PCIHostAdapterInformation))
{
if (PCIHostAdapterInformation.ISACompatibleIOPort < 6)
StandardAddressSeen[PCIHostAdapterInformation
.ISACompatibleIOPort] = true;
}
else PCIHostAdapterInformation.ISACompatibleIOPort =
BusLogic_IO_Disable;
ModifyIOAddressRequest = BusLogic_IO_Disable;
BusLogic_Command(HostAdapter, BusLogic_ModifyIOAddress,
&ModifyIOAddressRequest,
sizeof(ModifyIOAddressRequest), NULL, 0);
if (!ForceBusDeviceScanningOrderChecked)
{
BusLogic_FetchHostAdapterLocalRAMRequest_T
FetchHostAdapterLocalRAMRequest;
BusLogic_AutoSCSIByte45_T AutoSCSIByte45;
BusLogic_BoardID_T BoardID;
FetchHostAdapterLocalRAMRequest.ByteOffset =
BusLogic_AutoSCSI_BaseOffset + 45;
FetchHostAdapterLocalRAMRequest.ByteCount =
sizeof(AutoSCSIByte45);
BusLogic_Command(HostAdapter,
BusLogic_FetchHostAdapterLocalRAM,
&FetchHostAdapterLocalRAMRequest,
sizeof(FetchHostAdapterLocalRAMRequest),
&AutoSCSIByte45, sizeof(AutoSCSIByte45));
BusLogic_Command(HostAdapter, BusLogic_InquireBoardID,
NULL, 0, &BoardID, sizeof(BoardID));
if (BoardID.FirmwareVersion1stDigit == '5')
ForceBusDeviceScanningOrder =
AutoSCSIByte45.ForceBusDeviceScanningOrder;
ForceBusDeviceScanningOrderChecked = true;
}
if (PCIHostAdapterInformation.ISACompatibleIOPort == BusLogic_IO_330)
{
PrimaryProbeInfo->HostAdapterType = BusLogic_MultiMaster;
PrimaryProbeInfo->HostAdapterBusType = BusLogic_PCI_Bus;
PrimaryProbeInfo->IO_Address = IO_Address;
PrimaryProbeInfo->PCI_Address = PCI_Address;
PrimaryProbeInfo->Bus = Bus;
PrimaryProbeInfo->Device = Device;
PrimaryProbeInfo->IRQ_Channel = IRQ_Channel;
PCIMultiMasterCount++;
}
else if (BusLogic_ProbeInfoCount < BusLogic_MaxHostAdapters)
{
BusLogic_ProbeInfo_T *ProbeInfo =
&BusLogic_ProbeInfoList[BusLogic_ProbeInfoCount++];
ProbeInfo->HostAdapterType = BusLogic_MultiMaster;
ProbeInfo->HostAdapterBusType = BusLogic_PCI_Bus;
ProbeInfo->IO_Address = IO_Address;
ProbeInfo->PCI_Address = PCI_Address;
ProbeInfo->Bus = Bus;
ProbeInfo->Device = Device;
ProbeInfo->IRQ_Channel = IRQ_Channel;
NonPrimaryPCIMultiMasterCount++;
PCIMultiMasterCount++;
}
else BusLogic_Warning("BusLogic: Too many Host Adapters "
"detected\n", NULL);
}
if (ForceBusDeviceScanningOrder)
BusLogic_SortProbeInfo(&BusLogic_ProbeInfoList[
NonPrimaryPCIMultiMasterIndex],
NonPrimaryPCIMultiMasterCount);
if (!BusLogic_ProbeOptions.NoProbeISA)
if (PrimaryProbeInfo->IO_Address == 0 &&
(BusLogic_ProbeOptions.LimitedProbeISA
? BusLogic_ProbeOptions.Probe330
: check_region(0x330, BusLogic_MultiMasterAddressCount) == 0))
{
PrimaryProbeInfo->HostAdapterType = BusLogic_MultiMaster;
PrimaryProbeInfo->HostAdapterBusType = BusLogic_ISA_Bus;
PrimaryProbeInfo->IO_Address = 0x330;
}
if (!BusLogic_ProbeOptions.NoProbeISA)
{
if (!StandardAddressSeen[1] &&
(BusLogic_ProbeOptions.LimitedProbeISA
? BusLogic_ProbeOptions.Probe334
: check_region(0x334, BusLogic_MultiMasterAddressCount) == 0))
BusLogic_AppendProbeAddressISA(0x334);
if (!StandardAddressSeen[2] &&
(BusLogic_ProbeOptions.LimitedProbeISA
? BusLogic_ProbeOptions.Probe230
: check_region(0x230, BusLogic_MultiMasterAddressCount) == 0))
BusLogic_AppendProbeAddressISA(0x230);
if (!StandardAddressSeen[3] &&
(BusLogic_ProbeOptions.LimitedProbeISA
? BusLogic_ProbeOptions.Probe234
: check_region(0x234, BusLogic_MultiMasterAddressCount) == 0))
BusLogic_AppendProbeAddressISA(0x234);
if (!StandardAddressSeen[4] &&
(BusLogic_ProbeOptions.LimitedProbeISA
? BusLogic_ProbeOptions.Probe130
: check_region(0x130, BusLogic_MultiMasterAddressCount) == 0))
BusLogic_AppendProbeAddressISA(0x130);
if (!StandardAddressSeen[5] &&
(BusLogic_ProbeOptions.LimitedProbeISA
? BusLogic_ProbeOptions.Probe134
: check_region(0x134, BusLogic_MultiMasterAddressCount) == 0))
BusLogic_AppendProbeAddressISA(0x134);
}
Index = 0;
while (pcibios_find_device(PCI_VENDOR_ID_BUSLOGIC,
PCI_DEVICE_ID_BUSLOGIC_MULTIMASTER_NC,
Index++, &Bus, &DeviceFunction) == 0)
if (pcibios_read_config_dword(Bus, DeviceFunction,
PCI_BASE_ADDRESS_0, &BaseAddress0) == 0 &&
pcibios_read_config_byte(Bus, DeviceFunction,
PCI_INTERRUPT_LINE, &IRQ_Channel) == 0)
{
unsigned char Device = DeviceFunction >> 3;
IO_Address = BaseAddress0 & PCI_BASE_ADDRESS_IO_MASK;
if (IO_Address == 0 || IRQ_Channel == 0 || IRQ_Channel >= NR_IRQS)
continue;
for (i = 0; i < BusLogic_ProbeInfoCount; i++)
{
BusLogic_ProbeInfo_T *ProbeInfo = &BusLogic_ProbeInfoList[i];
if (ProbeInfo->IO_Address == IO_Address &&
ProbeInfo->HostAdapterType == BusLogic_MultiMaster)
{
ProbeInfo->HostAdapterBusType = BusLogic_PCI_Bus;
ProbeInfo->PCI_Address = 0;
ProbeInfo->Bus = Bus;
ProbeInfo->Device = Device;
ProbeInfo->IRQ_Channel = IRQ_Channel;
break;
}
}
}
return PCIMultiMasterCount;
}
static int BusLogic_InitializeFlashPointProbeInfo(BusLogic_HostAdapter_T
*PrototypeHostAdapter)
{
int FlashPointIndex = BusLogic_ProbeInfoCount, FlashPointCount = 0;
unsigned char Bus, DeviceFunction;
unsigned int BaseAddress0, BaseAddress1;
unsigned char IRQ_Channel;
BusLogic_IO_Address_T IO_Address;
BusLogic_PCI_Address_T PCI_Address;
unsigned short Index = 0;
while (pcibios_find_device(PCI_VENDOR_ID_BUSLOGIC,
PCI_DEVICE_ID_BUSLOGIC_FLASHPOINT,
Index++, &Bus, &DeviceFunction) == 0)
if (pcibios_read_config_dword(Bus, DeviceFunction,
PCI_BASE_ADDRESS_0, &BaseAddress0) == 0 &&
pcibios_read_config_dword(Bus, DeviceFunction,
PCI_BASE_ADDRESS_1, &BaseAddress1) == 0 &&
pcibios_read_config_byte(Bus, DeviceFunction,
PCI_INTERRUPT_LINE, &IRQ_Channel) == 0)
{
unsigned char Device = DeviceFunction >> 3;
IO_Address = BaseAddress0 & PCI_BASE_ADDRESS_IO_MASK;
PCI_Address = BaseAddress1 & PCI_BASE_ADDRESS_MEM_MASK;
#ifndef CONFIG_SCSI_OMIT_FLASHPOINT
if ((BaseAddress0 & PCI_BASE_ADDRESS_SPACE)
!= PCI_BASE_ADDRESS_SPACE_IO)
{
BusLogic_Error("BusLogic: Base Address0 0x%X not I/O for "
"FlashPoint Host Adapter\n", NULL, BaseAddress0);
BusLogic_Error("at PCI Bus %d Device %d I/O Address 0x%X\n",
NULL, Bus, Device, IO_Address);
continue;
}
if ((BaseAddress1 & PCI_BASE_ADDRESS_SPACE)
!= PCI_BASE_ADDRESS_SPACE_MEMORY)
{
BusLogic_Error("BusLogic: Base Address1 0x%X not Memory for "
"FlashPoint Host Adapter\n", NULL, BaseAddress1);
BusLogic_Error("at PCI Bus %d Device %d PCI Address 0x%X\n",
NULL, Bus, Device, PCI_Address);
continue;
}
if (IRQ_Channel == 0 || IRQ_Channel >= NR_IRQS)
{
BusLogic_Error("BusLogic: IRQ Channel %d illegal for "
"FlashPoint Host Adapter\n", NULL, IRQ_Channel);
BusLogic_Error("at PCI Bus %d Device %d I/O Address 0x%X\n",
NULL, Bus, Device, IO_Address);
continue;
}
if (BusLogic_GlobalOptions.TraceProbe)
{
BusLogic_Notice("BusLogic: FlashPoint Host Adapter "
"detected at\n", NULL);
BusLogic_Notice("BusLogic: PCI Bus %d Device %d I/O Address "
"0x%X PCI Address 0x%X\n", NULL,
Bus, Device, IO_Address, PCI_Address);
}
if (BusLogic_ProbeInfoCount < BusLogic_MaxHostAdapters)
{
BusLogic_ProbeInfo_T *ProbeInfo =
&BusLogic_ProbeInfoList[BusLogic_ProbeInfoCount++];
ProbeInfo->HostAdapterType = BusLogic_FlashPoint;
ProbeInfo->HostAdapterBusType = BusLogic_PCI_Bus;
ProbeInfo->IO_Address = IO_Address;
ProbeInfo->PCI_Address = PCI_Address;
ProbeInfo->Bus = Bus;
ProbeInfo->Device = Device;
ProbeInfo->IRQ_Channel = IRQ_Channel;
FlashPointCount++;
}
else BusLogic_Warning("BusLogic: Too many Host Adapters "
"detected\n", NULL);
#else
BusLogic_Error("BusLogic: FlashPoint Host Adapter detected at "
"PCI Bus %d Device %d\n", NULL, Bus, Device);
BusLogic_Error("BusLogic: I/O Address 0x%X PCI Address 0x%X, "
"but FlashPoint\n", NULL, IO_Address, PCI_Address);
BusLogic_Error("BusLogic: support was omitted in this kernel "
"configuration.\n", NULL);
#endif
}
BusLogic_SortProbeInfo(&BusLogic_ProbeInfoList[FlashPointIndex],
FlashPointCount);
return FlashPointCount;
}
static void BusLogic_InitializeProbeInfoList(BusLogic_HostAdapter_T
*PrototypeHostAdapter)
{
if (!BusLogic_ProbeOptions.NoProbePCI && pcibios_present())
{
if (BusLogic_ProbeOptions.MultiMasterFirst)
{
BusLogic_InitializeMultiMasterProbeInfo(PrototypeHostAdapter);
BusLogic_InitializeFlashPointProbeInfo(PrototypeHostAdapter);
}
else if (BusLogic_ProbeOptions.FlashPointFirst)
{
BusLogic_InitializeFlashPointProbeInfo(PrototypeHostAdapter);
BusLogic_InitializeMultiMasterProbeInfo(PrototypeHostAdapter);
}
else
{
int FlashPointCount =
BusLogic_InitializeFlashPointProbeInfo(PrototypeHostAdapter);
int PCIMultiMasterCount =
BusLogic_InitializeMultiMasterProbeInfo(PrototypeHostAdapter);
if (FlashPointCount > 0 && PCIMultiMasterCount > 0)
{
BusLogic_ProbeInfo_T *ProbeInfo =
&BusLogic_ProbeInfoList[FlashPointCount];
BusLogic_HostAdapter_T *HostAdapter = PrototypeHostAdapter;
BusLogic_FetchHostAdapterLocalRAMRequest_T
FetchHostAdapterLocalRAMRequest;
BusLogic_BIOSDriveMapByte_T Drive0MapByte;
while (ProbeInfo->HostAdapterBusType != BusLogic_PCI_Bus)
ProbeInfo++;
HostAdapter->IO_Address = ProbeInfo->IO_Address;
FetchHostAdapterLocalRAMRequest.ByteOffset =
BusLogic_BIOS_BaseOffset + BusLogic_BIOS_DriveMapOffset + 0;
FetchHostAdapterLocalRAMRequest.ByteCount =
sizeof(Drive0MapByte);
BusLogic_Command(HostAdapter,
BusLogic_FetchHostAdapterLocalRAM,
&FetchHostAdapterLocalRAMRequest,
sizeof(FetchHostAdapterLocalRAMRequest),
&Drive0MapByte, sizeof(Drive0MapByte));
if (Drive0MapByte.DiskGeometry !=
BusLogic_BIOS_Disk_Not_Installed)
{
BusLogic_ProbeInfo_T
SavedProbeInfo[BusLogic_MaxHostAdapters];
int MultiMasterCount =
BusLogic_ProbeInfoCount - FlashPointCount;
memcpy(SavedProbeInfo,
BusLogic_ProbeInfoList,
BusLogic_ProbeInfoCount
* sizeof(BusLogic_ProbeInfo_T));
memcpy(&BusLogic_ProbeInfoList[0],
&SavedProbeInfo[FlashPointCount],
MultiMasterCount * sizeof(BusLogic_ProbeInfo_T));
memcpy(&BusLogic_ProbeInfoList[MultiMasterCount],
&SavedProbeInfo[0],
FlashPointCount * sizeof(BusLogic_ProbeInfo_T));
}
}
}
}
else BusLogic_InitializeProbeInfoListISA(PrototypeHostAdapter);
}
#endif
static boolean BusLogic_Failure(BusLogic_HostAdapter_T *HostAdapter,
char *ErrorMessage)
{
BusLogic_AnnounceDriver(HostAdapter);
if (HostAdapter->HostAdapterBusType == BusLogic_PCI_Bus)
{
BusLogic_Error("While configuring BusLogic PCI Host Adapter at\n",
HostAdapter);
BusLogic_Error("Bus %d Device %d I/O Address 0x%X PCI Address 0x%X:\n",
HostAdapter, HostAdapter->Bus, HostAdapter->Device,
HostAdapter->IO_Address, HostAdapter->PCI_Address);
}
else BusLogic_Error("While configuring BusLogic Host Adapter at "
"I/O Address 0x%X:\n", HostAdapter,
HostAdapter->IO_Address);
BusLogic_Error("%s FAILED - DETACHING\n", HostAdapter, ErrorMessage);
if (BusLogic_CommandFailureReason != NULL)
BusLogic_Error("ADDITIONAL FAILURE INFO - %s\n", HostAdapter,
BusLogic_CommandFailureReason);
return false;
}
static boolean BusLogic_ProbeHostAdapter(BusLogic_HostAdapter_T *HostAdapter)
{
BusLogic_StatusRegister_T StatusRegister;
BusLogic_InterruptRegister_T InterruptRegister;
BusLogic_GeometryRegister_T GeometryRegister;
if (BusLogic_FlashPointHostAdapterP(HostAdapter))
{
FlashPoint_Info_T *FlashPointInfo = &HostAdapter->FlashPointInfo;
FlashPointInfo->BaseAddress =
(BusLogic_Base_Address_T) HostAdapter->IO_Address;
FlashPointInfo->IRQ_Channel = HostAdapter->IRQ_Channel;
FlashPointInfo->Present = false;
if (!(FlashPoint_ProbeHostAdapter(FlashPointInfo) == 0 &&
FlashPointInfo->Present))
{
BusLogic_Error("BusLogic: FlashPoint Host Adapter detected at "
"PCI Bus %d Device %d\n", HostAdapter,
HostAdapter->Bus, HostAdapter->Device);
BusLogic_Error("BusLogic: I/O Address 0x%X PCI Address 0x%X, "
"but FlashPoint\n", HostAdapter,
HostAdapter->IO_Address, HostAdapter->PCI_Address);
BusLogic_Error("BusLogic: Probe Function failed to validate it.\n",
HostAdapter);
return false;
}
if (BusLogic_GlobalOptions.TraceProbe)
BusLogic_Notice("BusLogic_Probe(0x%X): FlashPoint Found\n",
HostAdapter, HostAdapter->IO_Address);
return true;
}
StatusRegister.All = BusLogic_ReadStatusRegister(HostAdapter);
InterruptRegister.All = BusLogic_ReadInterruptRegister(HostAdapter);
GeometryRegister.All = BusLogic_ReadGeometryRegister(HostAdapter);
if (BusLogic_GlobalOptions.TraceProbe)
BusLogic_Notice("BusLogic_Probe(0x%X): Status 0x%02X, Interrupt 0x%02X, "
"Geometry 0x%02X\n", HostAdapter,
HostAdapter->IO_Address, StatusRegister.All,
InterruptRegister.All, GeometryRegister.All);
if (StatusRegister.All == 0 ||
StatusRegister.Bits.DiagnosticActive ||
StatusRegister.Bits.CommandParameterRegisterBusy ||
StatusRegister.Bits.Reserved ||
StatusRegister.Bits.CommandInvalid ||
InterruptRegister.Bits.Reserved != 0)
return false;
if (GeometryRegister.All == 0xFF) return false;
return true;
}
static boolean BusLogic_HardwareResetHostAdapter(BusLogic_HostAdapter_T
*HostAdapter,
boolean HardReset)
{
BusLogic_StatusRegister_T StatusRegister;
int TimeoutCounter;
if (BusLogic_FlashPointHostAdapterP(HostAdapter))
{
FlashPoint_Info_T *FlashPointInfo = &HostAdapter->FlashPointInfo;
FlashPointInfo->HostSoftReset = !HardReset;
FlashPointInfo->ReportDataUnderrun = true;
HostAdapter->CardHandle =
FlashPoint_HardwareResetHostAdapter(FlashPointInfo);
if (HostAdapter->CardHandle == FlashPoint_BadCardHandle) return false;
return true;
}
if (HardReset)
BusLogic_HardReset(HostAdapter);
else BusLogic_SoftReset(HostAdapter);
TimeoutCounter = 5*10000;
while (--TimeoutCounter >= 0)
{
StatusRegister.All = BusLogic_ReadStatusRegister(HostAdapter);
if (StatusRegister.Bits.DiagnosticActive) break;
udelay(100);
}
if (BusLogic_GlobalOptions.TraceHardwareReset)
BusLogic_Notice("BusLogic_HardwareReset(0x%X): Diagnostic Active, "
"Status 0x%02X\n", HostAdapter,
HostAdapter->IO_Address, StatusRegister.All);
if (TimeoutCounter < 0) return false;
udelay(100);
TimeoutCounter = 10*10000;
while (--TimeoutCounter >= 0)
{
StatusRegister.All = BusLogic_ReadStatusRegister(HostAdapter);
if (!StatusRegister.Bits.DiagnosticActive) break;
udelay(100);
}
if (BusLogic_GlobalOptions.TraceHardwareReset)
BusLogic_Notice("BusLogic_HardwareReset(0x%X): Diagnostic Completed, "
"Status 0x%02X\n", HostAdapter,
HostAdapter->IO_Address, StatusRegister.All);
if (TimeoutCounter < 0) return false;
TimeoutCounter = 10000;
while (--TimeoutCounter >= 0)
{
StatusRegister.All = BusLogic_ReadStatusRegister(HostAdapter);
if (StatusRegister.Bits.DiagnosticFailure ||
StatusRegister.Bits.HostAdapterReady ||
StatusRegister.Bits.DataInRegisterReady)
break;
udelay(100);
}
if (BusLogic_GlobalOptions.TraceHardwareReset)
BusLogic_Notice("BusLogic_HardwareReset(0x%X): Host Adapter Ready, "
"Status 0x%02X\n", HostAdapter,
HostAdapter->IO_Address, StatusRegister.All);
if (TimeoutCounter < 0) return false;
if (StatusRegister.Bits.DiagnosticFailure ||
!StatusRegister.Bits.HostAdapterReady)
{
BusLogic_CommandFailureReason = NULL;
BusLogic_Failure(HostAdapter, "HARD RESET DIAGNOSTICS");
BusLogic_Error("HOST ADAPTER STATUS REGISTER = %02X\n",
HostAdapter, StatusRegister.All);
if (StatusRegister.Bits.DataInRegisterReady)
{
unsigned char ErrorCode = BusLogic_ReadDataInRegister(HostAdapter);
BusLogic_Error("HOST ADAPTER ERROR CODE = %d\n",
HostAdapter, ErrorCode);
}
return false;
}
return true;
}
static boolean BusLogic_CheckHostAdapter(BusLogic_HostAdapter_T *HostAdapter)
{
BusLogic_ExtendedSetupInformation_T ExtendedSetupInformation;
BusLogic_RequestedReplyLength_T RequestedReplyLength;
boolean Result = true;
if (BusLogic_FlashPointHostAdapterP(HostAdapter)) return true;
RequestedReplyLength = sizeof(ExtendedSetupInformation);
if (BusLogic_Command(HostAdapter,
BusLogic_InquireExtendedSetupInformation,
&RequestedReplyLength,
sizeof(RequestedReplyLength),
&ExtendedSetupInformation,
sizeof(ExtendedSetupInformation))
!= sizeof(ExtendedSetupInformation))
Result = false;
if (BusLogic_GlobalOptions.TraceProbe)
BusLogic_Notice("BusLogic_Check(0x%X): MultiMaster %s\n", HostAdapter,
HostAdapter->IO_Address, (Result ? "Found" : "Not Found"));
return Result;
}
static boolean BusLogic_ReadHostAdapterConfiguration(BusLogic_HostAdapter_T
*HostAdapter)
{
BusLogic_BoardID_T BoardID;
BusLogic_Configuration_T Configuration;
BusLogic_SetupInformation_T SetupInformation;
BusLogic_ExtendedSetupInformation_T ExtendedSetupInformation;
BusLogic_HostAdapterModelNumber_T HostAdapterModelNumber;
BusLogic_FirmwareVersion3rdDigit_T FirmwareVersion3rdDigit;
BusLogic_FirmwareVersionLetter_T FirmwareVersionLetter;
BusLogic_PCIHostAdapterInformation_T PCIHostAdapterInformation;
BusLogic_FetchHostAdapterLocalRAMRequest_T FetchHostAdapterLocalRAMRequest;
BusLogic_AutoSCSIData_T AutoSCSIData;
BusLogic_GeometryRegister_T GeometryRegister;
BusLogic_RequestedReplyLength_T RequestedReplyLength;
unsigned char *TargetPointer, Character;
int TargetID, i;
if (BusLogic_FlashPointHostAdapterP(HostAdapter))
{
FlashPoint_Info_T *FlashPointInfo = &HostAdapter->FlashPointInfo;
TargetPointer = HostAdapter->ModelName;
*TargetPointer++ = 'B';
*TargetPointer++ = 'T';
*TargetPointer++ = '-';
for (i = 0; i < sizeof(FlashPointInfo->ModelNumber); i++)
*TargetPointer++ = FlashPointInfo->ModelNumber[i];
*TargetPointer++ = '\0';
strcpy(HostAdapter->FirmwareVersion, FlashPoint_FirmwareVersion);
HostAdapter->SCSI_ID = FlashPointInfo->SCSI_ID;
HostAdapter->ExtendedTranslationEnabled =
FlashPointInfo->ExtendedTranslationEnabled;
HostAdapter->ParityCheckingEnabled =
FlashPointInfo->ParityCheckingEnabled;
HostAdapter->BusResetEnabled = !FlashPointInfo->HostSoftReset;
HostAdapter->LevelSensitiveInterrupt = true;
HostAdapter->HostWideSCSI = FlashPointInfo->HostWideSCSI;
HostAdapter->HostDifferentialSCSI = false;
HostAdapter->HostSupportsSCAM = true;
HostAdapter->HostUltraSCSI = true;
HostAdapter->ExtendedLUNSupport = true;
HostAdapter->TerminationInfoValid = true;
HostAdapter->LowByteTerminated = FlashPointInfo->LowByteTerminated;
HostAdapter->HighByteTerminated = FlashPointInfo->HighByteTerminated;
HostAdapter->SCAM_Enabled = FlashPointInfo->SCAM_Enabled;
HostAdapter->SCAM_Level2 = FlashPointInfo->SCAM_Level2;
HostAdapter->DriverScatterGatherLimit = BusLogic_ScatterGatherLimit;
HostAdapter->MaxTargetDevices = (HostAdapter->HostWideSCSI ? 16 : 8);
HostAdapter->MaxLogicalUnits = 32;
HostAdapter->InitialCCBs = 4 * BusLogic_CCB_AllocationGroupSize;
HostAdapter->IncrementalCCBs = BusLogic_CCB_AllocationGroupSize;
HostAdapter->DriverQueueDepth = 255;
HostAdapter->HostAdapterQueueDepth = HostAdapter->DriverQueueDepth;
HostAdapter->SynchronousPermitted = FlashPointInfo->SynchronousPermitted;
HostAdapter->FastPermitted = FlashPointInfo->FastPermitted;
HostAdapter->UltraPermitted = FlashPointInfo->UltraPermitted;
HostAdapter->WidePermitted = FlashPointInfo->WidePermitted;
HostAdapter->DisconnectPermitted = FlashPointInfo->DisconnectPermitted;
HostAdapter->TaggedQueuingPermitted = 0xFFFF;
goto Common;
}
if (BusLogic_Command(HostAdapter, BusLogic_InquireBoardID, NULL, 0,
&BoardID, sizeof(BoardID)) != sizeof(BoardID))
return BusLogic_Failure(HostAdapter, "INQUIRE BOARD ID");
if (BusLogic_Command(HostAdapter, BusLogic_InquireConfiguration, NULL, 0,
&Configuration, sizeof(Configuration))
!= sizeof(Configuration))
return BusLogic_Failure(HostAdapter, "INQUIRE CONFIGURATION");
RequestedReplyLength = sizeof(SetupInformation);
if (BusLogic_Command(HostAdapter, BusLogic_InquireSetupInformation,
&RequestedReplyLength, sizeof(RequestedReplyLength),
&SetupInformation, sizeof(SetupInformation))
!= sizeof(SetupInformation))
return BusLogic_Failure(HostAdapter, "INQUIRE SETUP INFORMATION");
RequestedReplyLength = sizeof(ExtendedSetupInformation);
if (BusLogic_Command(HostAdapter, BusLogic_InquireExtendedSetupInformation,
&RequestedReplyLength, sizeof(RequestedReplyLength),
&ExtendedSetupInformation,
sizeof(ExtendedSetupInformation))
!= sizeof(ExtendedSetupInformation))
return BusLogic_Failure(HostAdapter, "INQUIRE EXTENDED SETUP INFORMATION");
FirmwareVersion3rdDigit = '\0';
if (BoardID.FirmwareVersion1stDigit > '0')
if (BusLogic_Command(HostAdapter, BusLogic_InquireFirmwareVersion3rdDigit,
NULL, 0, &FirmwareVersion3rdDigit,
sizeof(FirmwareVersion3rdDigit))
!= sizeof(FirmwareVersion3rdDigit))
return BusLogic_Failure(HostAdapter, "INQUIRE FIRMWARE 3RD DIGIT");
if (ExtendedSetupInformation.BusType == 'A' &&
BoardID.FirmwareVersion1stDigit == '2')
strcpy(HostAdapterModelNumber, "542B");
else if (ExtendedSetupInformation.BusType == 'E' &&
BoardID.FirmwareVersion1stDigit == '2' &&
(BoardID.FirmwareVersion2ndDigit <= '1' ||
(BoardID.FirmwareVersion2ndDigit == '2' &&
FirmwareVersion3rdDigit == '0')))
strcpy(HostAdapterModelNumber, "742A");
else if (ExtendedSetupInformation.BusType == 'E' &&
BoardID.FirmwareVersion1stDigit == '0')
strcpy(HostAdapterModelNumber, "747A");
else
{
RequestedReplyLength = sizeof(HostAdapterModelNumber);
if (BusLogic_Command(HostAdapter, BusLogic_InquireHostAdapterModelNumber,
&RequestedReplyLength, sizeof(RequestedReplyLength),
&HostAdapterModelNumber,
sizeof(HostAdapterModelNumber))
!= sizeof(HostAdapterModelNumber))
return BusLogic_Failure(HostAdapter,
"INQUIRE HOST ADAPTER MODEL NUMBER");
}
TargetPointer = HostAdapter->ModelName;
*TargetPointer++ = 'B';
*TargetPointer++ = 'T';
*TargetPointer++ = '-';
for (i = 0; i < sizeof(HostAdapterModelNumber); i++)
{
Character = HostAdapterModelNumber[i];
if (Character == ' ' || Character == '\0') break;
*TargetPointer++ = Character;
}
*TargetPointer++ = '\0';
TargetPointer = HostAdapter->FirmwareVersion;
*TargetPointer++ = BoardID.FirmwareVersion1stDigit;
*TargetPointer++ = '.';
*TargetPointer++ = BoardID.FirmwareVersion2ndDigit;
if (FirmwareVersion3rdDigit != ' ' && FirmwareVersion3rdDigit != '\0')
*TargetPointer++ = FirmwareVersion3rdDigit;
*TargetPointer = '\0';
if (strcmp(HostAdapter->FirmwareVersion, "3.3") >= 0)
{
if (BusLogic_Command(HostAdapter, BusLogic_InquireFirmwareVersionLetter,
NULL, 0, &FirmwareVersionLetter,
sizeof(FirmwareVersionLetter))
!= sizeof(FirmwareVersionLetter))
return BusLogic_Failure(HostAdapter,
"INQUIRE FIRMWARE VERSION LETTER");
if (FirmwareVersionLetter != ' ' && FirmwareVersionLetter != '\0')
*TargetPointer++ = FirmwareVersionLetter;
*TargetPointer = '\0';
}
HostAdapter->SCSI_ID = Configuration.HostAdapterID;
HostAdapter->HostAdapterBusType =
BusLogic_HostAdapterBusTypes[HostAdapter->ModelName[3] - '4'];
if (HostAdapter->IRQ_Channel == 0)
{
if (Configuration.IRQ_Channel9)
HostAdapter->IRQ_Channel = 9;
else if (Configuration.IRQ_Channel10)
HostAdapter->IRQ_Channel = 10;
else if (Configuration.IRQ_Channel11)
HostAdapter->IRQ_Channel = 11;
else if (Configuration.IRQ_Channel12)
HostAdapter->IRQ_Channel = 12;
else if (Configuration.IRQ_Channel14)
HostAdapter->IRQ_Channel = 14;
else if (Configuration.IRQ_Channel15)
HostAdapter->IRQ_Channel = 15;
}
if (HostAdapter->HostAdapterBusType == BusLogic_ISA_Bus)
{
if (Configuration.DMA_Channel5)
HostAdapter->DMA_Channel = 5;
else if (Configuration.DMA_Channel6)
HostAdapter->DMA_Channel = 6;
else if (Configuration.DMA_Channel7)
HostAdapter->DMA_Channel = 7;
}
GeometryRegister.All = BusLogic_ReadGeometryRegister(HostAdapter);
HostAdapter->ExtendedTranslationEnabled =
GeometryRegister.Bits.ExtendedTranslationEnabled;
HostAdapter->HostAdapterScatterGatherLimit =
ExtendedSetupInformation.ScatterGatherLimit;
HostAdapter->DriverScatterGatherLimit =
HostAdapter->HostAdapterScatterGatherLimit;
if (HostAdapter->HostAdapterScatterGatherLimit > BusLogic_ScatterGatherLimit)
HostAdapter->DriverScatterGatherLimit = BusLogic_ScatterGatherLimit;
if (ExtendedSetupInformation.Misc.LevelSensitiveInterrupt)
HostAdapter->LevelSensitiveInterrupt = true;
HostAdapter->HostWideSCSI = ExtendedSetupInformation.HostWideSCSI;
HostAdapter->HostDifferentialSCSI =
ExtendedSetupInformation.HostDifferentialSCSI;
HostAdapter->HostSupportsSCAM = ExtendedSetupInformation.HostSupportsSCAM;
HostAdapter->HostUltraSCSI = ExtendedSetupInformation.HostUltraSCSI;
if (HostAdapter->FirmwareVersion[0] == '5' ||
(HostAdapter->FirmwareVersion[0] == '4' && HostAdapter->HostWideSCSI))
HostAdapter->ExtendedLUNSupport = true;
if (HostAdapter->FirmwareVersion[0] == '5')
{
if (BusLogic_Command(HostAdapter,
BusLogic_InquirePCIHostAdapterInformation,
NULL, 0, &PCIHostAdapterInformation,
sizeof(PCIHostAdapterInformation))
!= sizeof(PCIHostAdapterInformation))
return BusLogic_Failure(HostAdapter,
"INQUIRE PCI HOST ADAPTER INFORMATION");
if (PCIHostAdapterInformation.GenericInfoValid)
{
HostAdapter->TerminationInfoValid = true;
HostAdapter->LowByteTerminated =
PCIHostAdapterInformation.LowByteTerminated;
HostAdapter->HighByteTerminated =
PCIHostAdapterInformation.HighByteTerminated;
}
}
if (HostAdapter->FirmwareVersion[0] >= '4')
{
FetchHostAdapterLocalRAMRequest.ByteOffset =
BusLogic_AutoSCSI_BaseOffset;
FetchHostAdapterLocalRAMRequest.ByteCount = sizeof(AutoSCSIData);
if (BusLogic_Command(HostAdapter,
BusLogic_FetchHostAdapterLocalRAM,
&FetchHostAdapterLocalRAMRequest,
sizeof(FetchHostAdapterLocalRAMRequest),
&AutoSCSIData, sizeof(AutoSCSIData))
!= sizeof(AutoSCSIData))
return BusLogic_Failure(HostAdapter, "FETCH HOST ADAPTER LOCAL RAM");
HostAdapter->ParityCheckingEnabled = AutoSCSIData.ParityCheckingEnabled;
HostAdapter->BusResetEnabled = AutoSCSIData.BusResetEnabled;
if (HostAdapter->FirmwareVersion[0] == '4')
{
HostAdapter->TerminationInfoValid = true;
HostAdapter->LowByteTerminated = AutoSCSIData.LowByteTerminated;
HostAdapter->HighByteTerminated = AutoSCSIData.HighByteTerminated;
}
HostAdapter->WidePermitted = AutoSCSIData.WidePermitted;
HostAdapter->FastPermitted = AutoSCSIData.FastPermitted;
HostAdapter->SynchronousPermitted =
AutoSCSIData.SynchronousPermitted;
HostAdapter->DisconnectPermitted =
AutoSCSIData.DisconnectPermitted;
if (HostAdapter->HostUltraSCSI)
HostAdapter->UltraPermitted = AutoSCSIData.UltraPermitted;
if (HostAdapter->HostSupportsSCAM)
{
HostAdapter->SCAM_Enabled = AutoSCSIData.SCAM_Enabled;
HostAdapter->SCAM_Level2 = AutoSCSIData.SCAM_Level2;
}
}
if (HostAdapter->FirmwareVersion[0] < '4')
{
if (SetupInformation.SynchronousInitiationEnabled)
{
HostAdapter->SynchronousPermitted = 0xFF;
if (HostAdapter->HostAdapterBusType == BusLogic_EISA_Bus)
{
if (ExtendedSetupInformation.Misc.FastOnEISA)
HostAdapter->FastPermitted = 0xFF;
if (strcmp(HostAdapter->ModelName, "BT-757") == 0)
HostAdapter->WidePermitted = 0xFF;
}
}
HostAdapter->DisconnectPermitted = 0xFF;
HostAdapter->ParityCheckingEnabled =
SetupInformation.ParityCheckingEnabled;
HostAdapter->BusResetEnabled = true;
}
HostAdapter->MaxTargetDevices = (HostAdapter->HostWideSCSI ? 16 : 8);
HostAdapter->MaxLogicalUnits = (HostAdapter->ExtendedLUNSupport ? 32 : 8);
if (HostAdapter->FirmwareVersion[0] == '5')
HostAdapter->HostAdapterQueueDepth = 192;
else if (HostAdapter->FirmwareVersion[0] == '4')
HostAdapter->HostAdapterQueueDepth =
(HostAdapter->HostAdapterBusType != BusLogic_ISA_Bus ? 100 : 50);
else HostAdapter->HostAdapterQueueDepth = 30;
if (strcmp(HostAdapter->FirmwareVersion, "3.31") >= 0)
{
HostAdapter->StrictRoundRobinModeSupport = true;
HostAdapter->MailboxCount = BusLogic_MaxMailboxes;
}
else
{
HostAdapter->StrictRoundRobinModeSupport = false;
HostAdapter->MailboxCount = 32;
}
HostAdapter->DriverQueueDepth = HostAdapter->MailboxCount;
HostAdapter->InitialCCBs = 4 * BusLogic_CCB_AllocationGroupSize;
HostAdapter->IncrementalCCBs = BusLogic_CCB_AllocationGroupSize;
HostAdapter->TaggedQueuingPermitted = 0;
switch (HostAdapter->FirmwareVersion[0])
{
case '5':
HostAdapter->TaggedQueuingPermitted = 0xFFFF;
break;
case '4':
if (strcmp(HostAdapter->FirmwareVersion, "4.22") >= 0)
HostAdapter->TaggedQueuingPermitted = 0xFFFF;
break;
case '3':
if (strcmp(HostAdapter->FirmwareVersion, "3.35") >= 0)
HostAdapter->TaggedQueuingPermitted = 0xFFFF;
break;
}
HostAdapter->BIOS_Address = ExtendedSetupInformation.BIOS_Address << 12;
if (HostAdapter->HostAdapterBusType == BusLogic_ISA_Bus &&
(void *) high_memory > (void *) MAX_DMA_ADDRESS)
HostAdapter->BounceBuffersRequired = true;
if (HostAdapter->BIOS_Address > 0 &&
strcmp(HostAdapter->ModelName, "BT-445S") == 0 &&
strcmp(HostAdapter->FirmwareVersion, "3.37") < 0 &&
(void *) high_memory > (void *) MAX_DMA_ADDRESS)
HostAdapter->BounceBuffersRequired = true;
Common:
strcpy(HostAdapter->FullModelName, "BusLogic ");
strcat(HostAdapter->FullModelName, HostAdapter->ModelName);
for (TargetID = 0; TargetID < BusLogic_MaxTargetDevices; TargetID++)
{
unsigned char QueueDepth = 0;
if (HostAdapter->DriverOptions != NULL &&
HostAdapter->DriverOptions->QueueDepth[TargetID] > 0)
QueueDepth = HostAdapter->DriverOptions->QueueDepth[TargetID];
else if (HostAdapter->BounceBuffersRequired)
QueueDepth = BusLogic_TaggedQueueDepthBB;
HostAdapter->QueueDepth[TargetID] = QueueDepth;
}
if (HostAdapter->BounceBuffersRequired)
HostAdapter->UntaggedQueueDepth = BusLogic_UntaggedQueueDepthBB;
else HostAdapter->UntaggedQueueDepth = BusLogic_UntaggedQueueDepth;
if (HostAdapter->DriverOptions != NULL)
HostAdapter->CommonQueueDepth =
HostAdapter->DriverOptions->CommonQueueDepth;
if (HostAdapter->CommonQueueDepth > 0 &&
HostAdapter->CommonQueueDepth < HostAdapter->UntaggedQueueDepth)
HostAdapter->UntaggedQueueDepth = HostAdapter->CommonQueueDepth;
HostAdapter->TaggedQueuingPermitted &= HostAdapter->DisconnectPermitted;
if (HostAdapter->DriverOptions != NULL)
HostAdapter->TaggedQueuingPermitted =
(HostAdapter->DriverOptions->TaggedQueuingPermitted &
HostAdapter->DriverOptions->TaggedQueuingPermittedMask) |
(HostAdapter->TaggedQueuingPermitted &
~HostAdapter->DriverOptions->TaggedQueuingPermittedMask);
for (TargetID = 0; TargetID < BusLogic_MaxTargetDevices; TargetID++)
if (HostAdapter->DriverOptions != NULL)
HostAdapter->ErrorRecoveryStrategy[TargetID] =
HostAdapter->DriverOptions->ErrorRecoveryStrategy[TargetID];
else HostAdapter->ErrorRecoveryStrategy[TargetID] =
BusLogic_ErrorRecovery_Default;
if (HostAdapter->DriverOptions != NULL &&
HostAdapter->DriverOptions->BusSettleTime > 0)
HostAdapter->BusSettleTime = HostAdapter->DriverOptions->BusSettleTime;
else HostAdapter->BusSettleTime = BusLogic_DefaultBusSettleTime;
return true;
}
static boolean BusLogic_ReportHostAdapterConfiguration(BusLogic_HostAdapter_T
*HostAdapter)
{
unsigned short AllTargetsMask = (1 << HostAdapter->MaxTargetDevices) - 1;
unsigned short SynchronousPermitted, FastPermitted;
unsigned short UltraPermitted, WidePermitted;
unsigned short DisconnectPermitted, TaggedQueuingPermitted;
boolean CommonSynchronousNegotiation, CommonTaggedQueueDepth;
boolean CommonErrorRecovery;
char SynchronousString[BusLogic_MaxTargetDevices+1];
char WideString[BusLogic_MaxTargetDevices+1];
char DisconnectString[BusLogic_MaxTargetDevices+1];
char TaggedQueuingString[BusLogic_MaxTargetDevices+1];
char ErrorRecoveryString[BusLogic_MaxTargetDevices+1];
char *SynchronousMessage = SynchronousString;
char *WideMessage = WideString;
char *DisconnectMessage = DisconnectString;
char *TaggedQueuingMessage = TaggedQueuingString;
char *ErrorRecoveryMessage = ErrorRecoveryString;
int TargetID;
BusLogic_Info("Configuring BusLogic Model %s %s%s%s%s SCSI Host Adapter\n",
HostAdapter, HostAdapter->ModelName,
BusLogic_HostAdapterBusNames[HostAdapter->HostAdapterBusType],
(HostAdapter->HostWideSCSI ? " Wide" : ""),
(HostAdapter->HostDifferentialSCSI ? " Differential" : ""),
(HostAdapter->HostUltraSCSI ? " Ultra" : ""));
BusLogic_Info("  Firmware Version: %s, I/O Address: 0x%X, "
"IRQ Channel: %d/%s\n", HostAdapter,
HostAdapter->FirmwareVersion,
HostAdapter->IO_Address, HostAdapter->IRQ_Channel,
(HostAdapter->LevelSensitiveInterrupt ? "Level" : "Edge"));
if (HostAdapter->HostAdapterBusType != BusLogic_PCI_Bus)
{
BusLogic_Info("  DMA Channel: ", HostAdapter);
if (HostAdapter->DMA_Channel > 0)
BusLogic_Info("%d, ", HostAdapter, HostAdapter->DMA_Channel);
else BusLogic_Info("None, ", HostAdapter);
if (HostAdapter->BIOS_Address > 0)
BusLogic_Info("BIOS Address: 0x%X, ", HostAdapter,
HostAdapter->BIOS_Address);
else BusLogic_Info("BIOS Address: None, ", HostAdapter);
}
else
{
BusLogic_Info("  PCI Bus: %d, Device: %d, Address: ",
HostAdapter, HostAdapter->Bus, HostAdapter->Device);
if (HostAdapter->PCI_Address > 0)
BusLogic_Info("0x%X, ", HostAdapter, HostAdapter->PCI_Address);
else BusLogic_Info("Unassigned, ", HostAdapter);
}
BusLogic_Info("Host Adapter SCSI ID: %d\n", HostAdapter,
HostAdapter->SCSI_ID);
BusLogic_Info("  Parity Checking: %s, Extended Translation: %s\n",
HostAdapter,
(HostAdapter->ParityCheckingEnabled
? "Enabled" : "Disabled"),
(HostAdapter->ExtendedTranslationEnabled
? "Enabled" : "Disabled"));
AllTargetsMask &= ~(1 << HostAdapter->SCSI_ID);
SynchronousPermitted = HostAdapter->SynchronousPermitted & AllTargetsMask;
FastPermitted = HostAdapter->FastPermitted & AllTargetsMask;
UltraPermitted = HostAdapter->UltraPermitted & AllTargetsMask;
if ((BusLogic_MultiMasterHostAdapterP(HostAdapter) &&
(HostAdapter->FirmwareVersion[0] >= '4' ||
HostAdapter->HostAdapterBusType == BusLogic_EISA_Bus)) ||
BusLogic_FlashPointHostAdapterP(HostAdapter))
{
CommonSynchronousNegotiation = false;
if (SynchronousPermitted == 0)
{
SynchronousMessage = "Disabled";
CommonSynchronousNegotiation = true;
}
else if (SynchronousPermitted == AllTargetsMask)
{
if (FastPermitted == 0)
{
SynchronousMessage = "Slow";
CommonSynchronousNegotiation = true;
}
else if (FastPermitted == AllTargetsMask)
{
if (UltraPermitted == 0)
{
SynchronousMessage = "Fast";
CommonSynchronousNegotiation = true;
}
else if (UltraPermitted == AllTargetsMask)
{
SynchronousMessage = "Ultra";
CommonSynchronousNegotiation = true;
}
}
}
if (!CommonSynchronousNegotiation)
{
for (TargetID = 0;
TargetID < HostAdapter->MaxTargetDevices;
TargetID++)
SynchronousString[TargetID] =
((!(SynchronousPermitted & (1 << TargetID))) ? 'N' :
(!(FastPermitted & (1 << TargetID)) ? 'S' :
(!(UltraPermitted & (1 << TargetID)) ? 'F' : 'U')));
SynchronousString[HostAdapter->SCSI_ID] = '#';
SynchronousString[HostAdapter->MaxTargetDevices] = '\0';
}
}
else SynchronousMessage =
(SynchronousPermitted == 0 ? "Disabled" : "Enabled");
WidePermitted = HostAdapter->WidePermitted & AllTargetsMask;
if (WidePermitted == 0)
WideMessage = "Disabled";
else if (WidePermitted == AllTargetsMask)
WideMessage = "Enabled";
else
{
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
WideString[TargetID] =
((WidePermitted & (1 << TargetID)) ? 'Y' : 'N');
WideString[HostAdapter->SCSI_ID] = '#';
WideString[HostAdapter->MaxTargetDevices] = '\0';
}
DisconnectPermitted = HostAdapter->DisconnectPermitted & AllTargetsMask;
if (DisconnectPermitted == 0)
DisconnectMessage = "Disabled";
else if (DisconnectPermitted == AllTargetsMask)
DisconnectMessage = "Enabled";
else
{
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
DisconnectString[TargetID] =
((DisconnectPermitted & (1 << TargetID)) ? 'Y' : 'N');
DisconnectString[HostAdapter->SCSI_ID] = '#';
DisconnectString[HostAdapter->MaxTargetDevices] = '\0';
}
TaggedQueuingPermitted =
HostAdapter->TaggedQueuingPermitted & AllTargetsMask;
if (TaggedQueuingPermitted == 0)
TaggedQueuingMessage = "Disabled";
else if (TaggedQueuingPermitted == AllTargetsMask)
TaggedQueuingMessage = "Enabled";
else
{
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
TaggedQueuingString[TargetID] =
((TaggedQueuingPermitted & (1 << TargetID)) ? 'Y' : 'N');
TaggedQueuingString[HostAdapter->SCSI_ID] = '#';
TaggedQueuingString[HostAdapter->MaxTargetDevices] = '\0';
}
BusLogic_Info("  Synchronous Negotiation: %s, Wide Negotiation: %s\n",
HostAdapter, SynchronousMessage, WideMessage);
BusLogic_Info("  Disconnect/Reconnect: %s, Tagged Queuing: %s\n",
HostAdapter, DisconnectMessage, TaggedQueuingMessage);
if (BusLogic_MultiMasterHostAdapterP(HostAdapter))
{
BusLogic_Info("  Scatter/Gather Limit: %d of %d segments, "
"Mailboxes: %d\n", HostAdapter,
HostAdapter->DriverScatterGatherLimit,
HostAdapter->HostAdapterScatterGatherLimit,
HostAdapter->MailboxCount);
BusLogic_Info("  Driver Queue Depth: %d, "
"Host Adapter Queue Depth: %d\n",
HostAdapter, HostAdapter->DriverQueueDepth,
HostAdapter->HostAdapterQueueDepth);
}
else BusLogic_Info("  Driver Queue Depth: %d, "
"Scatter/Gather Limit: %d segments\n",
HostAdapter, HostAdapter->DriverQueueDepth,
HostAdapter->DriverScatterGatherLimit);
BusLogic_Info("  Tagged Queue Depth: ", HostAdapter);
CommonTaggedQueueDepth = true;
for (TargetID = 1; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
if (HostAdapter->QueueDepth[TargetID] != HostAdapter->QueueDepth[0])
{
CommonTaggedQueueDepth = false;
break;
}
if (CommonTaggedQueueDepth)
{
if (HostAdapter->QueueDepth[0] > 0)
BusLogic_Info("%d", HostAdapter, HostAdapter->QueueDepth[0]);
else BusLogic_Info("Automatic", HostAdapter);
}
else BusLogic_Info("Individual", HostAdapter);
BusLogic_Info(", Untagged Queue Depth: %d\n", HostAdapter,
HostAdapter->UntaggedQueueDepth);
CommonErrorRecovery = true;
for (TargetID = 1; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
if (HostAdapter->ErrorRecoveryStrategy[TargetID] !=
HostAdapter->ErrorRecoveryStrategy[0])
{
CommonErrorRecovery = false;
break;
}
if (CommonErrorRecovery)
ErrorRecoveryMessage =
BusLogic_ErrorRecoveryStrategyNames[
HostAdapter->ErrorRecoveryStrategy[0]];
else
{
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
ErrorRecoveryString[TargetID] =
BusLogic_ErrorRecoveryStrategyLetters[
HostAdapter->ErrorRecoveryStrategy[TargetID]];
ErrorRecoveryString[HostAdapter->SCSI_ID] = '#';
ErrorRecoveryString[HostAdapter->MaxTargetDevices] = '\0';
}
BusLogic_Info("  Error Recovery Strategy: %s, SCSI Bus Reset: %s\n",
HostAdapter, ErrorRecoveryMessage,
(HostAdapter->BusResetEnabled ? "Enabled" : "Disabled"));
if (HostAdapter->TerminationInfoValid)
{
if (HostAdapter->HostWideSCSI)
BusLogic_Info("  SCSI Bus Termination: %s", HostAdapter,
(HostAdapter->LowByteTerminated
? (HostAdapter->HighByteTerminated
? "Both Enabled" : "Low Enabled")
: (HostAdapter->HighByteTerminated
? "High Enabled" : "Both Disabled")));
else BusLogic_Info("  SCSI Bus Termination: %s", HostAdapter,
(HostAdapter->LowByteTerminated ?
"Enabled" : "Disabled"));
if (HostAdapter->HostSupportsSCAM)
BusLogic_Info(", SCAM: %s", HostAdapter,
(HostAdapter->SCAM_Enabled
? (HostAdapter->SCAM_Level2
? "Enabled, Level 2" : "Enabled, Level 1")
: "Disabled"));
BusLogic_Info("\n", HostAdapter);
}
return true;
}
static boolean BusLogic_AcquireResources(BusLogic_HostAdapter_T *HostAdapter)
{
if (HostAdapter->IRQ_Channel == 0)
{
BusLogic_Error("NO LEGAL INTERRUPT CHANNEL ASSIGNED - DETACHING\n",
HostAdapter);
return false;
}
if (request_irq(HostAdapter->IRQ_Channel, BusLogic_InterruptHandler,
SA_INTERRUPT | SA_SHIRQ,
HostAdapter->FullModelName, HostAdapter) < 0)
{
BusLogic_Error("UNABLE TO ACQUIRE IRQ CHANNEL %d - DETACHING\n",
HostAdapter, HostAdapter->IRQ_Channel);
return false;
}
HostAdapter->IRQ_ChannelAcquired = true;
if (HostAdapter->DMA_Channel > 0)
{
if (request_dma(HostAdapter->DMA_Channel,
HostAdapter->FullModelName) < 0)
{
BusLogic_Error("UNABLE TO ACQUIRE DMA CHANNEL %d - DETACHING\n",
HostAdapter, HostAdapter->DMA_Channel);
return false;
}
set_dma_mode(HostAdapter->DMA_Channel, DMA_MODE_CASCADE);
enable_dma(HostAdapter->DMA_Channel);
HostAdapter->DMA_ChannelAcquired = true;
}
return true;
}
static void BusLogic_ReleaseResources(BusLogic_HostAdapter_T *HostAdapter)
{
if (HostAdapter->IRQ_ChannelAcquired)
free_irq(HostAdapter->IRQ_Channel, HostAdapter);
if (HostAdapter->DMA_ChannelAcquired)
free_dma(HostAdapter->DMA_Channel);
}
static boolean BusLogic_InitializeHostAdapter(BusLogic_HostAdapter_T
*HostAdapter)
{
BusLogic_ExtendedMailboxRequest_T ExtendedMailboxRequest;
BusLogic_RoundRobinModeRequest_T RoundRobinModeRequest;
BusLogic_SetCCBFormatRequest_T SetCCBFormatRequest;
int TargetID;
HostAdapter->FirstCompletedCCB = NULL;
HostAdapter->LastCompletedCCB = NULL;
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
{
HostAdapter->BusDeviceResetPendingCCB[TargetID] = NULL;
HostAdapter->TargetFlags[TargetID].TaggedQueuingActive = false;
HostAdapter->TargetFlags[TargetID].CommandSuccessfulFlag = false;
HostAdapter->ActiveCommands[TargetID] = 0;
HostAdapter->CommandsSinceReset[TargetID] = 0;
}
if (BusLogic_FlashPointHostAdapterP(HostAdapter)) goto Done;
HostAdapter->FirstOutgoingMailbox =
(BusLogic_OutgoingMailbox_T *) HostAdapter->MailboxSpace;
HostAdapter->LastOutgoingMailbox =
HostAdapter->FirstOutgoingMailbox + HostAdapter->MailboxCount - 1;
HostAdapter->NextOutgoingMailbox = HostAdapter->FirstOutgoingMailbox;
HostAdapter->FirstIncomingMailbox =
(BusLogic_IncomingMailbox_T *) (HostAdapter->LastOutgoingMailbox + 1);
HostAdapter->LastIncomingMailbox =
HostAdapter->FirstIncomingMailbox + HostAdapter->MailboxCount - 1;
HostAdapter->NextIncomingMailbox = HostAdapter->FirstIncomingMailbox;
memset(HostAdapter->FirstOutgoingMailbox, 0,
HostAdapter->MailboxCount * sizeof(BusLogic_OutgoingMailbox_T));
memset(HostAdapter->FirstIncomingMailbox, 0,
HostAdapter->MailboxCount * sizeof(BusLogic_IncomingMailbox_T));
ExtendedMailboxRequest.MailboxCount = HostAdapter->MailboxCount;
ExtendedMailboxRequest.BaseMailboxAddress =
Virtual_to_Bus(HostAdapter->FirstOutgoingMailbox);
if (BusLogic_Command(HostAdapter, BusLogic_InitializeExtendedMailbox,
&ExtendedMailboxRequest,
sizeof(ExtendedMailboxRequest), NULL, 0) < 0)
return BusLogic_Failure(HostAdapter, "MAILBOX INITIALIZATION");
if (HostAdapter->StrictRoundRobinModeSupport)
{
RoundRobinModeRequest = BusLogic_StrictRoundRobinMode;
if (BusLogic_Command(HostAdapter, BusLogic_EnableStrictRoundRobinMode,
&RoundRobinModeRequest,
sizeof(RoundRobinModeRequest), NULL, 0) < 0)
return BusLogic_Failure(HostAdapter, "ENABLE STRICT ROUND ROBIN MODE");
}
if (HostAdapter->ExtendedLUNSupport)
{
SetCCBFormatRequest = BusLogic_ExtendedLUNFormatCCB;
if (BusLogic_Command(HostAdapter, BusLogic_SetCCBFormat,
&SetCCBFormatRequest, sizeof(SetCCBFormatRequest),
NULL, 0) < 0)
return BusLogic_Failure(HostAdapter, "SET CCB FORMAT");
}
Done:
if (!HostAdapter->HostAdapterInitialized)
{
BusLogic_Info("*** %s Initialized Successfully ***\n",
HostAdapter, HostAdapter->FullModelName);
BusLogic_Info("\n", HostAdapter);
}
else BusLogic_Warning("*** %s Initialized Successfully ***\n",
HostAdapter, HostAdapter->FullModelName);
HostAdapter->HostAdapterInitialized = true;
return true;
}
static boolean BusLogic_TargetDeviceInquiry(BusLogic_HostAdapter_T
*HostAdapter)
{
BusLogic_InstalledDevices_T InstalledDevices;
BusLogic_InstalledDevices8_T InstalledDevicesID0to7;
BusLogic_SetupInformation_T SetupInformation;
BusLogic_SynchronousPeriod_T SynchronousPeriod;
BusLogic_RequestedReplyLength_T RequestedReplyLength;
int TargetID;
BusLogic_Delay(HostAdapter->BusSettleTime);
if (BusLogic_FlashPointHostAdapterP(HostAdapter)) return true;
if (HostAdapter->DriverOptions != NULL &&
HostAdapter->DriverOptions->LocalOptions.InhibitTargetInquiry)
return true;
if (strcmp(HostAdapter->FirmwareVersion, "4.25") >= 0)
{
if (BusLogic_Command(HostAdapter, BusLogic_InquireTargetDevices, NULL, 0,
&InstalledDevices, sizeof(InstalledDevices))
!= sizeof(InstalledDevices))
return BusLogic_Failure(HostAdapter, "INQUIRE TARGET DEVICES");
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
HostAdapter->TargetFlags[TargetID].TargetExists =
(InstalledDevices & (1 << TargetID) ? true : false);
}
else
{
if (BusLogic_Command(HostAdapter, BusLogic_InquireInstalledDevicesID0to7,
NULL, 0, &InstalledDevicesID0to7,
sizeof(InstalledDevicesID0to7))
!= sizeof(InstalledDevicesID0to7))
return BusLogic_Failure(HostAdapter,
"INQUIRE INSTALLED DEVICES ID 0 TO 7");
for (TargetID = 0; TargetID < 8; TargetID++)
HostAdapter->TargetFlags[TargetID].TargetExists =
(InstalledDevicesID0to7[TargetID] != 0 ? true : false);
}
RequestedReplyLength = sizeof(SetupInformation);
if (BusLogic_Command(HostAdapter, BusLogic_InquireSetupInformation,
&RequestedReplyLength, sizeof(RequestedReplyLength),
&SetupInformation, sizeof(SetupInformation))
!= sizeof(SetupInformation))
return BusLogic_Failure(HostAdapter, "INQUIRE SETUP INFORMATION");
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
HostAdapter->SynchronousOffset[TargetID] =
(TargetID < 8
? SetupInformation.SynchronousValuesID0to7[TargetID].Offset
: SetupInformation.SynchronousValuesID8to15[TargetID-8].Offset);
if (strcmp(HostAdapter->FirmwareVersion, "5.06L") >= 0)
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
HostAdapter->TargetFlags[TargetID].WideTransfersActive =
(TargetID < 8
? (SetupInformation.WideTransfersActiveID0to7 & (1 << TargetID)
? true : false)
: (SetupInformation.WideTransfersActiveID8to15 & (1 << (TargetID-8))
? true : false));
if (HostAdapter->FirmwareVersion[0] >= '3')
{
RequestedReplyLength = sizeof(SynchronousPeriod);
if (BusLogic_Command(HostAdapter, BusLogic_InquireSynchronousPeriod,
&RequestedReplyLength, sizeof(RequestedReplyLength),
&SynchronousPeriod, sizeof(SynchronousPeriod))
!= sizeof(SynchronousPeriod))
return BusLogic_Failure(HostAdapter, "INQUIRE SYNCHRONOUS PERIOD");
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
HostAdapter->SynchronousPeriod[TargetID] = SynchronousPeriod[TargetID];
}
else
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
if (SetupInformation.SynchronousValuesID0to7[TargetID].Offset > 0)
HostAdapter->SynchronousPeriod[TargetID] =
20 + 5 * SetupInformation.SynchronousValuesID0to7[TargetID]
.TransferPeriod;
return true;
}
static void BusLogic_ReportTargetDeviceInfo(BusLogic_HostAdapter_T
*HostAdapter)
{
int TargetID;
if (BusLogic_MultiMasterHostAdapterP(HostAdapter) &&
HostAdapter->DriverOptions != NULL &&
HostAdapter->DriverOptions->LocalOptions.InhibitTargetInquiry)
return;
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
{
BusLogic_TargetFlags_T *TargetFlags = &HostAdapter->TargetFlags[TargetID];
if (TargetFlags->TargetExists && !TargetFlags->TargetInfoReported)
{
int SynchronousTransferRate = 0;
if (BusLogic_FlashPointHostAdapterP(HostAdapter))
{
boolean WideTransfersActive;
FlashPoint_InquireTargetInfo(
HostAdapter->CardHandle, TargetID,
&HostAdapter->SynchronousPeriod[TargetID],
&HostAdapter->SynchronousOffset[TargetID],
&WideTransfersActive);
TargetFlags->WideTransfersActive = WideTransfersActive;
}
else if (TargetFlags->WideTransfersSupported &&
(HostAdapter->WidePermitted & (1 << TargetID)) &&
strcmp(HostAdapter->FirmwareVersion, "5.06L") < 0)
TargetFlags->WideTransfersActive = true;
if (HostAdapter->SynchronousPeriod[TargetID] > 0)
SynchronousTransferRate =
100000 / HostAdapter->SynchronousPeriod[TargetID];
if (TargetFlags->WideTransfersActive)
SynchronousTransferRate <<= 1;
if (SynchronousTransferRate >= 9950)
{
SynchronousTransferRate = (SynchronousTransferRate + 50) / 100;
BusLogic_Info("Target %d: Queue Depth %d, %sSynchronous at "
"%d.%01d MB/sec, offset %d\n",
HostAdapter, TargetID,
HostAdapter->QueueDepth[TargetID],
(TargetFlags->WideTransfersActive ? "Wide " : ""),
SynchronousTransferRate / 10,
SynchronousTransferRate % 10,
HostAdapter->SynchronousOffset[TargetID]);
}
else if (SynchronousTransferRate > 0)
{
SynchronousTransferRate = (SynchronousTransferRate + 5) / 10;
BusLogic_Info("Target %d: Queue Depth %d, %sSynchronous at "
"%d.%02d MB/sec, offset %d\n",
HostAdapter, TargetID,
HostAdapter->QueueDepth[TargetID],
(TargetFlags->WideTransfersActive ? "Wide " : ""),
SynchronousTransferRate / 100,
SynchronousTransferRate % 100,
HostAdapter->SynchronousOffset[TargetID]);
}
else BusLogic_Info("Target %d: Queue Depth %d, Asynchronous\n",
HostAdapter, TargetID,
HostAdapter->QueueDepth[TargetID]);
TargetFlags->TargetInfoReported = true;
}
}
}
static void BusLogic_InitializeHostStructure(BusLogic_HostAdapter_T
*HostAdapter,
SCSI_Host_T *Host)
{
Host->max_id = HostAdapter->MaxTargetDevices;
Host->max_lun = HostAdapter->MaxLogicalUnits;
Host->max_channel = 0;
Host->unique_id = HostAdapter->IO_Address;
Host->this_id = HostAdapter->SCSI_ID;
Host->can_queue = HostAdapter->DriverQueueDepth;
Host->sg_tablesize = HostAdapter->DriverScatterGatherLimit;
Host->unchecked_isa_dma = HostAdapter->BounceBuffersRequired;
Host->cmd_per_lun = HostAdapter->UntaggedQueueDepth;
}
static void BusLogic_SelectQueueDepths(SCSI_Host_T *Host,
SCSI_Device_T *DeviceList)
{
BusLogic_HostAdapter_T *HostAdapter =
(BusLogic_HostAdapter_T *) Host->hostdata;
int TaggedDeviceCount = 0, AutomaticTaggedDeviceCount = 0;
int UntaggedDeviceCount = 0, AutomaticTaggedQueueDepth = 0;
int AllocatedQueueDepth = 0;
SCSI_Device_T *Device;
int TargetID;
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
if (HostAdapter->TargetFlags[TargetID].TargetExists)
{
int QueueDepth = HostAdapter->QueueDepth[TargetID];
if (HostAdapter->TargetFlags[TargetID].TaggedQueuingSupported &&
(HostAdapter->TaggedQueuingPermitted & (1 << TargetID)))
{
TaggedDeviceCount++;
if (QueueDepth == 0) AutomaticTaggedDeviceCount++;
}
else
{
UntaggedDeviceCount++;
if (QueueDepth == 0 ||
QueueDepth > HostAdapter->UntaggedQueueDepth)
{
QueueDepth = HostAdapter->UntaggedQueueDepth;
HostAdapter->QueueDepth[TargetID] = QueueDepth;
}
}
AllocatedQueueDepth += QueueDepth;
if (QueueDepth == 1)
HostAdapter->TaggedQueuingPermitted &= ~(1 << TargetID);
}
HostAdapter->TargetDeviceCount = TaggedDeviceCount + UntaggedDeviceCount;
if (AutomaticTaggedDeviceCount > 0)
{
AutomaticTaggedQueueDepth =
(HostAdapter->HostAdapterQueueDepth - AllocatedQueueDepth)
/ AutomaticTaggedDeviceCount;
if (AutomaticTaggedQueueDepth > BusLogic_MaxAutomaticTaggedQueueDepth)
AutomaticTaggedQueueDepth = BusLogic_MaxAutomaticTaggedQueueDepth;
if (AutomaticTaggedQueueDepth < BusLogic_MinAutomaticTaggedQueueDepth)
AutomaticTaggedQueueDepth = BusLogic_MinAutomaticTaggedQueueDepth;
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
if (HostAdapter->TargetFlags[TargetID].TargetExists &&
HostAdapter->QueueDepth[TargetID] == 0)
{
AllocatedQueueDepth += AutomaticTaggedQueueDepth;
HostAdapter->QueueDepth[TargetID] = AutomaticTaggedQueueDepth;
}
}
for (Device = DeviceList; Device != NULL; Device = Device->next)
if (Device->host == Host)
Device->queue_depth = HostAdapter->QueueDepth[Device->id];
AllocatedQueueDepth += HostAdapter->TargetDeviceCount;
if (AllocatedQueueDepth > HostAdapter->DriverQueueDepth)
AllocatedQueueDepth = HostAdapter->DriverQueueDepth;
BusLogic_CreateAdditionalCCBs(HostAdapter,
AllocatedQueueDepth
- HostAdapter->AllocatedCCBs,
false);
if (HostAdapter == BusLogic_LastRegisteredHostAdapter)
for (HostAdapter = BusLogic_FirstRegisteredHostAdapter;
HostAdapter != NULL;
HostAdapter = HostAdapter->Next)
BusLogic_ReportTargetDeviceInfo(HostAdapter);
}
int BusLogic_DetectHostAdapter(SCSI_Host_Template_T *HostTemplate)
{
int BusLogicHostAdapterCount = 0, DriverOptionsIndex = 0, ProbeIndex;
BusLogic_HostAdapter_T *PrototypeHostAdapter;
if (BusLogic_ProbeOptions.NoProbe) return 0;
BusLogic_ProbeInfoList = (BusLogic_ProbeInfo_T *)
kmalloc(BusLogic_MaxHostAdapters * sizeof(BusLogic_ProbeInfo_T),
GFP_ATOMIC);
if (BusLogic_ProbeInfoList == NULL)
{
BusLogic_Error("BusLogic: Unable to allocate Probe Info List\n", NULL);
return 0;
}
memset(BusLogic_ProbeInfoList, 0,
BusLogic_MaxHostAdapters * sizeof(BusLogic_ProbeInfo_T));
PrototypeHostAdapter = (BusLogic_HostAdapter_T *)
kmalloc(sizeof(BusLogic_HostAdapter_T), GFP_ATOMIC);
if (PrototypeHostAdapter == NULL)
{
kfree(BusLogic_ProbeInfoList);
BusLogic_Error("BusLogic: Unable to allocate Prototype "
"Host Adapter\n", NULL);
return 0;
}
memset(PrototypeHostAdapter, 0, sizeof(BusLogic_HostAdapter_T));
if (BusLogic_Options != NULL)
BusLogic_ParseDriverOptions(BusLogic_Options);
BusLogic_InitializeProbeInfoList(PrototypeHostAdapter);
for (ProbeIndex = 0; ProbeIndex < BusLogic_ProbeInfoCount; ProbeIndex++)
{
BusLogic_ProbeInfo_T *ProbeInfo = &BusLogic_ProbeInfoList[ProbeIndex];
BusLogic_HostAdapter_T *HostAdapter = PrototypeHostAdapter;
SCSI_Host_T *Host;
if (ProbeInfo->IO_Address == 0) continue;
memset(HostAdapter, 0, sizeof(BusLogic_HostAdapter_T));
HostAdapter->HostAdapterType = ProbeInfo->HostAdapterType;
HostAdapter->HostAdapterBusType = ProbeInfo->HostAdapterBusType;
HostAdapter->IO_Address = ProbeInfo->IO_Address;
HostAdapter->PCI_Address = ProbeInfo->PCI_Address;
HostAdapter->Bus = ProbeInfo->Bus;
HostAdapter->Device = ProbeInfo->Device;
HostAdapter->IRQ_Channel = ProbeInfo->IRQ_Channel;
HostAdapter->AddressCount =
BusLogic_HostAdapterAddressCount[HostAdapter->HostAdapterType];
if (!BusLogic_ProbeHostAdapter(HostAdapter)) continue;
if (!BusLogic_HardwareResetHostAdapter(HostAdapter, true)) continue;
if (!BusLogic_CheckHostAdapter(HostAdapter)) continue;
if (DriverOptionsIndex < BusLogic_DriverOptionsCount)
HostAdapter->DriverOptions =
&BusLogic_DriverOptions[DriverOptionsIndex++];
BusLogic_AnnounceDriver(HostAdapter);
request_region(HostAdapter->IO_Address, HostAdapter->AddressCount,
"BusLogic");
Host = scsi_register(HostTemplate, sizeof(BusLogic_HostAdapter_T));
HostAdapter = (BusLogic_HostAdapter_T *) Host->hostdata;
memcpy(HostAdapter, PrototypeHostAdapter, sizeof(BusLogic_HostAdapter_T));
HostAdapter->SCSI_Host = Host;
HostAdapter->HostNumber = Host->host_no;
Host->select_queue_depths = BusLogic_SelectQueueDepths;
BusLogic_RegisterHostAdapter(HostAdapter);
if (BusLogic_ReadHostAdapterConfiguration(HostAdapter) &&
BusLogic_ReportHostAdapterConfiguration(HostAdapter) &&
BusLogic_AcquireResources(HostAdapter) &&
BusLogic_CreateInitialCCBs(HostAdapter) &&
BusLogic_InitializeHostAdapter(HostAdapter) &&
BusLogic_TargetDeviceInquiry(HostAdapter))
{
release_region(HostAdapter->IO_Address,
HostAdapter->AddressCount);
request_region(HostAdapter->IO_Address,
HostAdapter->AddressCount,
HostAdapter->FullModelName);
BusLogic_InitializeHostStructure(HostAdapter, Host);
BusLogicHostAdapterCount++;
}
else
{
BusLogic_DestroyCCBs(HostAdapter);
BusLogic_ReleaseResources(HostAdapter);
BusLogic_UnregisterHostAdapter(HostAdapter);
scsi_unregister(Host);
}
}
kfree(PrototypeHostAdapter);
kfree(BusLogic_ProbeInfoList);
BusLogic_ProbeInfoList = NULL;
return BusLogicHostAdapterCount;
}
int BusLogic_ReleaseHostAdapter(SCSI_Host_T *Host)
{
BusLogic_HostAdapter_T *HostAdapter =
(BusLogic_HostAdapter_T *) Host->hostdata;
if (BusLogic_FlashPointHostAdapterP(HostAdapter))
FlashPoint_ReleaseHostAdapter(HostAdapter->CardHandle);
BusLogic_DestroyCCBs(HostAdapter);
BusLogic_ReleaseResources(HostAdapter);
release_region(HostAdapter->IO_Address, HostAdapter->AddressCount);
BusLogic_UnregisterHostAdapter(HostAdapter);
return 0;
}
static void BusLogic_QueueCompletedCCB(BusLogic_CCB_T *CCB)
{
BusLogic_HostAdapter_T *HostAdapter = CCB->HostAdapter;
CCB->Status = BusLogic_CCB_Completed;
CCB->Next = NULL;
if (HostAdapter->FirstCompletedCCB == NULL)
{
HostAdapter->FirstCompletedCCB = CCB;
HostAdapter->LastCompletedCCB = CCB;
}
else
{
HostAdapter->LastCompletedCCB->Next = CCB;
HostAdapter->LastCompletedCCB = CCB;
}
HostAdapter->ActiveCommands[CCB->TargetID]--;
}
static int BusLogic_ComputeResultCode(BusLogic_HostAdapter_T *HostAdapter,
BusLogic_HostAdapterStatus_T
HostAdapterStatus,
BusLogic_TargetDeviceStatus_T
TargetDeviceStatus)
{
int HostStatus;
switch (HostAdapterStatus)
{
case BusLogic_CommandCompletedNormally:
case BusLogic_LinkedCommandCompleted:
case BusLogic_LinkedCommandCompletedWithFlag:
HostStatus = DID_OK;
break;
case BusLogic_SCSISelectionTimeout:
HostStatus = DID_TIME_OUT;
break;
case BusLogic_InvalidOutgoingMailboxActionCode:
case BusLogic_InvalidCommandOperationCode:
case BusLogic_InvalidCommandParameter:
BusLogic_Warning("BusLogic Driver Protocol Error 0x%02X\n",
HostAdapter, HostAdapterStatus);
case BusLogic_DataUnderRun:
case BusLogic_DataOverRun:
case BusLogic_UnexpectedBusFree:
case BusLogic_LinkedCCBhasInvalidLUN:
case BusLogic_AutoRequestSenseFailed:
case BusLogic_TaggedQueuingMessageRejected:
case BusLogic_UnsupportedMessageReceived:
case BusLogic_HostAdapterHardwareFailed:
case BusLogic_TargetDeviceReconnectedImproperly:
case BusLogic_AbortQueueGenerated:
case BusLogic_HostAdapterSoftwareError:
case BusLogic_HostAdapterHardwareTimeoutError:
case BusLogic_SCSIParityErrorDetected:
HostStatus = DID_ERROR;
break;
case BusLogic_InvalidBusPhaseRequested:
case BusLogic_TargetFailedResponseToATN:
case BusLogic_HostAdapterAssertedRST:
case BusLogic_OtherDeviceAssertedRST:
case BusLogic_HostAdapterAssertedBusDeviceReset:
HostStatus = DID_RESET;
break;
default:
BusLogic_Warning("Unknown Host Adapter Status 0x%02X\n",
HostAdapter, HostAdapterStatus);
HostStatus = DID_ERROR;
break;
}
return (HostStatus << 16) | TargetDeviceStatus;
}
static void BusLogic_ScanIncomingMailboxes(BusLogic_HostAdapter_T *HostAdapter)
{
BusLogic_IncomingMailbox_T *NextIncomingMailbox =
HostAdapter->NextIncomingMailbox;
BusLogic_CompletionCode_T CompletionCode;
while ((CompletionCode = NextIncomingMailbox->CompletionCode) !=
BusLogic_IncomingMailboxFree)
{
BusLogic_CCB_T *CCB = (BusLogic_CCB_T *)
Bus_to_Virtual(NextIncomingMailbox->CCB);
if (CompletionCode != BusLogic_AbortedCommandNotFound)
{
if (CCB->Status == BusLogic_CCB_Active ||
CCB->Status == BusLogic_CCB_Reset)
{
CCB->CompletionCode = CompletionCode;
BusLogic_QueueCompletedCCB(CCB);
}
else
{
BusLogic_Warning("Illegal CCB #%ld status %d in "
"Incoming Mailbox\n", HostAdapter,
CCB->SerialNumber, CCB->Status);
}
}
NextIncomingMailbox->CompletionCode = BusLogic_IncomingMailboxFree;
if (++NextIncomingMailbox > HostAdapter->LastIncomingMailbox)
NextIncomingMailbox = HostAdapter->FirstIncomingMailbox;
}
HostAdapter->NextIncomingMailbox = NextIncomingMailbox;
}
static void BusLogic_ProcessCompletedCCBs(BusLogic_HostAdapter_T *HostAdapter)
{
if (HostAdapter->ProcessCompletedCCBsActive) return;
HostAdapter->ProcessCompletedCCBsActive = true;
while (HostAdapter->FirstCompletedCCB != NULL)
{
BusLogic_CCB_T *CCB = HostAdapter->FirstCompletedCCB;
SCSI_Command_T *Command = CCB->Command;
HostAdapter->FirstCompletedCCB = CCB->Next;
if (HostAdapter->FirstCompletedCCB == NULL)
HostAdapter->LastCompletedCCB = NULL;
if (CCB->Opcode == BusLogic_BusDeviceReset)
{
int TargetID = CCB->TargetID;
BusLogic_Warning("Bus Device Reset CCB #%ld to Target "
"%d Completed\n", HostAdapter,
CCB->SerialNumber, TargetID);
BusLogic_IncrementErrorCounter(
&HostAdapter->TargetStatistics[TargetID].BusDeviceResetsCompleted);
HostAdapter->TargetFlags[TargetID].TaggedQueuingActive = false;
HostAdapter->CommandsSinceReset[TargetID] = 0;
HostAdapter->LastResetCompleted[TargetID] = jiffies;
BusLogic_DeallocateCCB(CCB);
while (Command != NULL)
{
SCSI_Command_T *NextCommand = Command->reset_chain;
Command->reset_chain = NULL;
Command->result = DID_RESET << 16;
Command->scsi_done(Command);
Command = NextCommand;
}
for (CCB = HostAdapter->All_CCBs; CCB != NULL; CCB = CCB->NextAll)
if (CCB->Status == BusLogic_CCB_Reset && CCB->TargetID == TargetID)
{
Command = CCB->Command;
BusLogic_DeallocateCCB(CCB);
HostAdapter->ActiveCommands[TargetID]--;
Command->result = DID_RESET << 16;
Command->scsi_done(Command);
}
HostAdapter->BusDeviceResetPendingCCB[TargetID] = NULL;
}
else
{
switch (CCB->CompletionCode)
{
case BusLogic_IncomingMailboxFree:
case BusLogic_AbortedCommandNotFound:
case BusLogic_InvalidCCB:
BusLogic_Warning("CCB #%ld to Target %d Impossible State\n",
HostAdapter, CCB->SerialNumber, CCB->TargetID);
break;
case BusLogic_CommandCompletedWithoutError:
HostAdapter->TargetStatistics[CCB->TargetID]
.CommandsCompleted++;
HostAdapter->TargetFlags[CCB->TargetID]
.CommandSuccessfulFlag = true;
Command->result = DID_OK << 16;
break;
case BusLogic_CommandAbortedAtHostRequest:
BusLogic_Warning("CCB #%ld to Target %d Aborted\n",
HostAdapter, CCB->SerialNumber, CCB->TargetID);
BusLogic_IncrementErrorCounter(
&HostAdapter->TargetStatistics[CCB->TargetID]
.CommandAbortsCompleted);
Command->result = DID_ABORT << 16;
break;
case BusLogic_CommandCompletedWithError:
Command->result =
BusLogic_ComputeResultCode(HostAdapter,
CCB->HostAdapterStatus,
CCB->TargetDeviceStatus);
if (CCB->HostAdapterStatus != BusLogic_SCSISelectionTimeout)
{
HostAdapter->TargetStatistics[CCB->TargetID]
.CommandsCompleted++;
if (BusLogic_GlobalOptions.TraceErrors)
{
int i;
BusLogic_Notice("CCB #%ld Target %d: Result %X Host "
"Adapter Status %02X "
"Target Status %02X\n",
HostAdapter, CCB->SerialNumber,
CCB->TargetID, Command->result,
CCB->HostAdapterStatus,
CCB->TargetDeviceStatus);
BusLogic_Notice("CDB   ", HostAdapter);
for (i = 0; i < CCB->CDB_Length; i++)
BusLogic_Notice(" %02X", HostAdapter, CCB->CDB[i]);
BusLogic_Notice("\n", HostAdapter);
BusLogic_Notice("Sense ", HostAdapter);
for (i = 0; i < CCB->SenseDataLength; i++)
BusLogic_Notice(" %02X", HostAdapter,
Command->sense_buffer[i]);
BusLogic_Notice("\n", HostAdapter);
}
}
break;
}
if (CCB->CDB[0] == INQUIRY && CCB->CDB[1] == 0 &&
CCB->HostAdapterStatus == BusLogic_CommandCompletedNormally)
{
BusLogic_TargetFlags_T *TargetFlags =
&HostAdapter->TargetFlags[CCB->TargetID];
SCSI_Inquiry_T *InquiryResult =
(SCSI_Inquiry_T *) Command->request_buffer;
TargetFlags->TargetExists = true;
TargetFlags->TaggedQueuingSupported = InquiryResult->CmdQue;
TargetFlags->WideTransfersSupported = InquiryResult->WBus16;
}
BusLogic_DeallocateCCB(CCB);
Command->scsi_done(Command);
}
}
HostAdapter->ProcessCompletedCCBsActive = false;
}
static void BusLogic_InterruptHandler(int IRQ_Channel,
void *DeviceIdentifier,
Registers_T *InterruptRegisters)
{
BusLogic_HostAdapter_T *HostAdapter =
(BusLogic_HostAdapter_T *) DeviceIdentifier;
ProcessorFlags_T ProcessorFlags;
BusLogic_AcquireHostAdapterLockIH(HostAdapter, &ProcessorFlags);
if (BusLogic_MultiMasterHostAdapterP(HostAdapter))
{
BusLogic_InterruptRegister_T InterruptRegister;
InterruptRegister.All = BusLogic_ReadInterruptRegister(HostAdapter);
if (InterruptRegister.Bits.InterruptValid)
{
BusLogic_InterruptReset(HostAdapter);
if (InterruptRegister.Bits.ExternalBusReset)
HostAdapter->HostAdapterExternalReset = true;
else if (InterruptRegister.Bits.IncomingMailboxLoaded)
BusLogic_ScanIncomingMailboxes(HostAdapter);
else if (InterruptRegister.Bits.CommandComplete)
HostAdapter->HostAdapterCommandCompleted = true;
}
}
else
{
if (FlashPoint_InterruptPending(HostAdapter->CardHandle))
switch (FlashPoint_HandleInterrupt(HostAdapter->CardHandle))
{
case FlashPoint_NormalInterrupt:
break;
case FlashPoint_ExternalBusReset:
HostAdapter->HostAdapterExternalReset = true;
break;
case FlashPoint_InternalError:
BusLogic_Warning("Internal FlashPoint Error detected"
" - Resetting Host Adapter\n", HostAdapter);
HostAdapter->HostAdapterInternalError = true;
break;
}
}
if (HostAdapter->FirstCompletedCCB != NULL)
BusLogic_ProcessCompletedCCBs(HostAdapter);
if (HostAdapter->HostAdapterExternalReset ||
HostAdapter->HostAdapterInternalError)
{
BusLogic_ResetHostAdapter(HostAdapter, NULL, 0);
HostAdapter->HostAdapterExternalReset = false;
HostAdapter->HostAdapterInternalError = false;
scsi_mark_host_reset(HostAdapter->SCSI_Host);
}
BusLogic_ReleaseHostAdapterLockIH(HostAdapter, &ProcessorFlags);
}
static boolean BusLogic_WriteOutgoingMailbox(BusLogic_HostAdapter_T
*HostAdapter,
BusLogic_ActionCode_T ActionCode,
BusLogic_CCB_T *CCB)
{
BusLogic_OutgoingMailbox_T *NextOutgoingMailbox;
NextOutgoingMailbox = HostAdapter->NextOutgoingMailbox;
if (NextOutgoingMailbox->ActionCode == BusLogic_OutgoingMailboxFree)
{
CCB->Status = BusLogic_CCB_Active;
NextOutgoingMailbox->CCB = Virtual_to_Bus(CCB);
NextOutgoingMailbox->ActionCode = ActionCode;
BusLogic_StartMailboxCommand(HostAdapter);
if (++NextOutgoingMailbox > HostAdapter->LastOutgoingMailbox)
NextOutgoingMailbox = HostAdapter->FirstOutgoingMailbox;
HostAdapter->NextOutgoingMailbox = NextOutgoingMailbox;
if (ActionCode == BusLogic_MailboxStartCommand)
{
HostAdapter->ActiveCommands[CCB->TargetID]++;
if (CCB->Opcode != BusLogic_BusDeviceReset)
HostAdapter->TargetStatistics[CCB->TargetID].CommandsAttempted++;
}
return true;
}
return false;
}
int BusLogic_QueueCommand(SCSI_Command_T *Command,
void (*CompletionRoutine)(SCSI_Command_T *))
{
BusLogic_HostAdapter_T *HostAdapter =
(BusLogic_HostAdapter_T *) Command->host->hostdata;
BusLogic_TargetFlags_T *TargetFlags =
&HostAdapter->TargetFlags[Command->target];
BusLogic_TargetStatistics_T *TargetStatistics =
HostAdapter->TargetStatistics;
unsigned char *CDB = Command->cmnd;
int CDB_Length = Command->cmd_len;
int TargetID = Command->target;
int LogicalUnit = Command->lun;
void *BufferPointer = Command->request_buffer;
int BufferLength = Command->request_bufflen;
int SegmentCount = Command->use_sg;
ProcessorFlags_T ProcessorFlags;
BusLogic_CCB_T *CCB;
if (CDB[0] == REQUEST_SENSE && Command->sense_buffer[0] != 0)
{
Command->result = DID_OK << 16;
CompletionRoutine(Command);
return 0;
}
BusLogic_AcquireHostAdapterLock(HostAdapter, &ProcessorFlags);
CCB = BusLogic_AllocateCCB(HostAdapter);
if (CCB == NULL)
{
BusLogic_Delay(1);
CCB = BusLogic_AllocateCCB(HostAdapter);
if (CCB == NULL)
{
Command->result = DID_ERROR << 16;
CompletionRoutine(Command);
goto Done;
}
}
if (SegmentCount == 0)
{
CCB->Opcode = BusLogic_InitiatorCCB;
CCB->DataLength = BufferLength;
CCB->DataPointer = Virtual_to_Bus(BufferPointer);
}
else
{
SCSI_ScatterList_T *ScatterList = (SCSI_ScatterList_T *) BufferPointer;
int Segment;
CCB->Opcode = BusLogic_InitiatorCCB_ScatterGather;
CCB->DataLength = SegmentCount * sizeof(BusLogic_ScatterGatherSegment_T);
if (BusLogic_MultiMasterHostAdapterP(HostAdapter))
CCB->DataPointer = Virtual_to_Bus(CCB->ScatterGatherList);
else CCB->DataPointer = Virtual_to_32Bit_Virtual(CCB->ScatterGatherList);
for (Segment = 0; Segment < SegmentCount; Segment++)
{
CCB->ScatterGatherList[Segment].SegmentByteCount =
ScatterList[Segment].length;
CCB->ScatterGatherList[Segment].SegmentDataPointer =
Virtual_to_Bus(ScatterList[Segment].address);
}
}
switch (CDB[0])
{
case READ_6:
case READ_10:
CCB->DataDirection = BusLogic_DataInLengthChecked;
TargetStatistics[TargetID].ReadCommands++;
BusLogic_IncrementByteCounter(
&TargetStatistics[TargetID].TotalBytesRead, BufferLength);
BusLogic_IncrementSizeBucket(
TargetStatistics[TargetID].ReadCommandSizeBuckets, BufferLength);
break;
case WRITE_6:
case WRITE_10:
CCB->DataDirection = BusLogic_DataOutLengthChecked;
TargetStatistics[TargetID].WriteCommands++;
BusLogic_IncrementByteCounter(
&TargetStatistics[TargetID].TotalBytesWritten, BufferLength);
BusLogic_IncrementSizeBucket(
TargetStatistics[TargetID].WriteCommandSizeBuckets, BufferLength);
break;
default:
CCB->DataDirection = BusLogic_UncheckedDataTransfer;
break;
}
CCB->CDB_Length = CDB_Length;
CCB->SenseDataLength = sizeof(Command->sense_buffer);
CCB->HostAdapterStatus = 0;
CCB->TargetDeviceStatus = 0;
CCB->TargetID = TargetID;
CCB->LogicalUnit = LogicalUnit;
CCB->TagEnable = false;
CCB->LegacyTagEnable = false;
if (HostAdapter->CommandsSinceReset[TargetID]++ >=
BusLogic_MaxTaggedQueueDepth &&
!TargetFlags->TaggedQueuingActive &&
HostAdapter->ActiveCommands[TargetID] == 0 &&
TargetFlags->TaggedQueuingSupported &&
(HostAdapter->TaggedQueuingPermitted & (1 << TargetID)))
{
TargetFlags->TaggedQueuingActive = true;
BusLogic_Notice("Tagged Queuing now active for Target %d\n",
HostAdapter, TargetID);
}
if (TargetFlags->TaggedQueuingActive)
{
BusLogic_QueueTag_T QueueTag = BusLogic_SimpleQueueTag;
if (HostAdapter->ActiveCommands[TargetID] == 0)
HostAdapter->LastSequencePoint[TargetID] = jiffies;
else if (jiffies - HostAdapter->LastSequencePoint[TargetID] > 4*HZ)
{
HostAdapter->LastSequencePoint[TargetID] = jiffies;
QueueTag = BusLogic_OrderedQueueTag;
}
if (HostAdapter->ExtendedLUNSupport)
{
CCB->TagEnable = true;
CCB->QueueTag = QueueTag;
}
else
{
CCB->LegacyTagEnable = true;
CCB->LegacyQueueTag = QueueTag;
}
}
memcpy(CCB->CDB, CDB, CDB_Length);
CCB->SenseDataPointer = Virtual_to_Bus(&Command->sense_buffer);
CCB->Command = Command;
Command->scsi_done = CompletionRoutine;
if (BusLogic_MultiMasterHostAdapterP(HostAdapter))
{
if (!BusLogic_WriteOutgoingMailbox(
HostAdapter, BusLogic_MailboxStartCommand, CCB))
{
BusLogic_Warning("Unable to write Outgoing Mailbox - "
"Pausing for 1 second\n", HostAdapter);
BusLogic_Delay(1);
if (!BusLogic_WriteOutgoingMailbox(
HostAdapter, BusLogic_MailboxStartCommand, CCB))
{
BusLogic_Warning("Still unable to write Outgoing Mailbox - "
"Host Adapter Dead?\n", HostAdapter);
BusLogic_DeallocateCCB(CCB);
Command->result = DID_ERROR << 16;
Command->scsi_done(Command);
}
}
}
else
{
CCB->Status = BusLogic_CCB_Active;
HostAdapter->ActiveCommands[TargetID]++;
TargetStatistics[TargetID].CommandsAttempted++;
FlashPoint_StartCCB(HostAdapter->CardHandle, CCB);
if (CCB->Status == BusLogic_CCB_Completed)
BusLogic_ProcessCompletedCCBs(HostAdapter);
}
Done:
BusLogic_ReleaseHostAdapterLock(HostAdapter, &ProcessorFlags);
return 0;
}
int BusLogic_AbortCommand(SCSI_Command_T *Command)
{
BusLogic_HostAdapter_T *HostAdapter =
(BusLogic_HostAdapter_T *) Command->host->hostdata;
int TargetID = Command->target;
ProcessorFlags_T ProcessorFlags;
BusLogic_CCB_T *CCB;
int Result;
BusLogic_IncrementErrorCounter(
&HostAdapter->TargetStatistics[TargetID].CommandAbortsRequested);
BusLogic_AcquireHostAdapterLock(HostAdapter, &ProcessorFlags);
if (Command->serial_number != Command->serial_number_at_timeout)
{
BusLogic_Warning("Unable to Abort Command to Target %d - "
"Already Completed\n", HostAdapter, TargetID);
Result = SCSI_ABORT_NOT_RUNNING;
goto Done;
}
for (CCB = HostAdapter->All_CCBs; CCB != NULL; CCB = CCB->NextAll)
if (CCB->Command == Command) break;
if (CCB == NULL)
{
BusLogic_Warning("Unable to Abort Command to Target %d - "
"No CCB Found\n", HostAdapter, TargetID);
Result = SCSI_ABORT_NOT_RUNNING;
goto Done;
}
else if (CCB->Status == BusLogic_CCB_Completed)
{
BusLogic_Warning("Unable to Abort Command to Target %d - "
"CCB Completed\n", HostAdapter, TargetID);
Result = SCSI_ABORT_NOT_RUNNING;
goto Done;
}
else if (CCB->Status == BusLogic_CCB_Reset)
{
BusLogic_Warning("Unable to Abort Command to Target %d - "
"CCB Reset\n", HostAdapter, TargetID);
Result = SCSI_ABORT_PENDING;
goto Done;
}
if (BusLogic_MultiMasterHostAdapterP(HostAdapter))
{
if (HostAdapter->TargetFlags[TargetID].TaggedQueuingActive &&
HostAdapter->FirmwareVersion[0] < '5')
{
BusLogic_Warning("Unable to Abort CCB #%ld to Target %d - "
"Abort Tag Not Supported\n",
HostAdapter, CCB->SerialNumber, TargetID);
Result = SCSI_ABORT_SNOOZE;
}
else if (BusLogic_WriteOutgoingMailbox(
HostAdapter, BusLogic_MailboxAbortCommand, CCB))
{
BusLogic_Warning("Aborting CCB #%ld to Target %d\n",
HostAdapter, CCB->SerialNumber, TargetID);
BusLogic_IncrementErrorCounter(
&HostAdapter->TargetStatistics[TargetID].CommandAbortsAttempted);
Result = SCSI_ABORT_PENDING;
}
else
{
BusLogic_Warning("Unable to Abort CCB #%ld to Target %d - "
"No Outgoing Mailboxes\n",
HostAdapter, CCB->SerialNumber, TargetID);
Result = SCSI_ABORT_BUSY;
}
}
else
{
BusLogic_Warning("Aborting CCB #%ld to Target %d\n",
HostAdapter, CCB->SerialNumber, TargetID);
BusLogic_IncrementErrorCounter(
&HostAdapter->TargetStatistics[TargetID].CommandAbortsAttempted);
FlashPoint_AbortCCB(HostAdapter->CardHandle, CCB);
Result = SCSI_ABORT_PENDING;
if (CCB->Status == BusLogic_CCB_Completed)
{
BusLogic_ProcessCompletedCCBs(HostAdapter);
Result = SCSI_ABORT_SUCCESS;
}
}
Done:
BusLogic_ReleaseHostAdapterLock(HostAdapter, &ProcessorFlags);
return Result;
}
static int BusLogic_ResetHostAdapter(BusLogic_HostAdapter_T *HostAdapter,
SCSI_Command_T *Command,
unsigned int ResetFlags)
{
ProcessorFlags_T ProcessorFlags;
BusLogic_CCB_T *CCB;
int TargetID, Result;
boolean HardReset;
if (HostAdapter->HostAdapterExternalReset)
{
BusLogic_IncrementErrorCounter(&HostAdapter->ExternalHostAdapterResets);
HardReset = false;
}
else if (HostAdapter->HostAdapterInternalError)
{
BusLogic_IncrementErrorCounter(&HostAdapter->HostAdapterInternalErrors);
HardReset = true;
}
else
{
BusLogic_IncrementErrorCounter(
&HostAdapter->TargetStatistics[Command->target]
.HostAdapterResetsRequested);
HardReset = true;
}
BusLogic_AcquireHostAdapterLock(HostAdapter, &ProcessorFlags);
if (ResetFlags & SCSI_RESET_ASYNCHRONOUS)
{
TargetID = Command->target;
if (Command->serial_number != Command->serial_number_at_timeout)
{
BusLogic_Warning("Unable to Reset Command to Target %d - "
"Already Completed or Reset\n",
HostAdapter, TargetID);
Result = SCSI_RESET_NOT_RUNNING;
goto Done;
}
for (CCB = HostAdapter->All_CCBs; CCB != NULL; CCB = CCB->NextAll)
if (CCB->Command == Command) break;
if (CCB == NULL)
{
BusLogic_Warning("Unable to Reset Command to Target %d - "
"No CCB Found\n", HostAdapter, TargetID);
Result = SCSI_RESET_NOT_RUNNING;
goto Done;
}
else if (CCB->Status == BusLogic_CCB_Completed)
{
BusLogic_Warning("Unable to Reset Command to Target %d - "
"CCB Completed\n", HostAdapter, TargetID);
Result = SCSI_RESET_NOT_RUNNING;
goto Done;
}
else if (CCB->Status == BusLogic_CCB_Reset &&
HostAdapter->BusDeviceResetPendingCCB[TargetID] == NULL)
{
BusLogic_Warning("Unable to Reset Command to Target %d - "
"Reset Pending\n", HostAdapter, TargetID);
Result = SCSI_RESET_PENDING;
goto Done;
}
}
if (Command == NULL)
{
if (HostAdapter->HostAdapterInternalError)
BusLogic_Warning("Resetting %s due to Host Adapter Internal Error\n",
HostAdapter, HostAdapter->FullModelName);
else BusLogic_Warning("Resetting %s due to External SCSI Bus Reset\n",
HostAdapter, HostAdapter->FullModelName);
}
else
{
BusLogic_Warning("Resetting %s due to Target %d\n", HostAdapter,
HostAdapter->FullModelName, Command->target);
BusLogic_IncrementErrorCounter(
&HostAdapter->TargetStatistics[Command->target]
.HostAdapterResetsAttempted);
}
if (!(BusLogic_HardwareResetHostAdapter(HostAdapter, HardReset) &&
BusLogic_InitializeHostAdapter(HostAdapter)))
{
BusLogic_Error("Resetting %s Failed\n", HostAdapter,
HostAdapter->FullModelName);
Result = SCSI_RESET_ERROR;
goto Done;
}
if (Command != NULL)
BusLogic_IncrementErrorCounter(
&HostAdapter->TargetStatistics[Command->target]
.HostAdapterResetsCompleted);
for (CCB = HostAdapter->All_CCBs; CCB != NULL; CCB = CCB->NextAll)
if (CCB->Status == BusLogic_CCB_Active)
CCB->Status = BusLogic_CCB_Reset;
if (HardReset)
BusLogic_Delay(HostAdapter->BusSettleTime);
if (ResetFlags & SCSI_RESET_SYNCHRONOUS)
{
Command->result = DID_RESET << 16;
Command->scsi_done(Command);
}
for (CCB = HostAdapter->All_CCBs; CCB != NULL; CCB = CCB->NextAll)
if (CCB->Status == BusLogic_CCB_Reset)
{
Command = CCB->Command;
BusLogic_DeallocateCCB(CCB);
while (Command != NULL)
{
SCSI_Command_T *NextCommand = Command->reset_chain;
Command->reset_chain = NULL;
Command->result = DID_RESET << 16;
Command->scsi_done(Command);
Command = NextCommand;
}
}
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
{
HostAdapter->LastResetAttempted[TargetID] = jiffies;
HostAdapter->LastResetCompleted[TargetID] = jiffies;
}
Result = SCSI_RESET_SUCCESS | SCSI_RESET_HOST_RESET;
Done:
BusLogic_ReleaseHostAdapterLock(HostAdapter, &ProcessorFlags);
return Result;
}
static int BusLogic_SendBusDeviceReset(BusLogic_HostAdapter_T *HostAdapter,
SCSI_Command_T *Command,
unsigned int ResetFlags)
{
int TargetID = Command->target;
BusLogic_CCB_T *CCB, *XCCB;
ProcessorFlags_T ProcessorFlags;
int Result = -1;
BusLogic_IncrementErrorCounter(
&HostAdapter->TargetStatistics[TargetID].BusDeviceResetsRequested);
BusLogic_AcquireHostAdapterLock(HostAdapter, &ProcessorFlags);
if (ResetFlags & SCSI_RESET_ASYNCHRONOUS)
{
if (Command->serial_number != Command->serial_number_at_timeout)
{
BusLogic_Warning("Unable to Reset Command to Target %d - "
"Already Completed\n", HostAdapter, TargetID);
Result = SCSI_RESET_NOT_RUNNING;
goto Done;
}
for (CCB = HostAdapter->All_CCBs; CCB != NULL; CCB = CCB->NextAll)
if (CCB->Command == Command) break;
if (CCB == NULL)
{
BusLogic_Warning("Unable to Reset Command to Target %d - "
"No CCB Found\n", HostAdapter, TargetID);
Result = SCSI_RESET_NOT_RUNNING;
goto Done;
}
else if (CCB->Status == BusLogic_CCB_Completed)
{
BusLogic_Warning("Unable to Reset Command to Target %d - "
"CCB Completed\n", HostAdapter, TargetID);
Result = SCSI_RESET_NOT_RUNNING;
goto Done;
}
else if (CCB->Status == BusLogic_CCB_Reset)
{
BusLogic_Warning("Unable to Reset Command to Target %d - "
"Reset Pending\n", HostAdapter, TargetID);
Result = SCSI_RESET_PENDING;
goto Done;
}
else if (HostAdapter->BusDeviceResetPendingCCB[TargetID] != NULL)
{
BusLogic_Warning("Bus Device Reset already pending to Target %d\n",
HostAdapter, TargetID);
goto Done;
}
}
if (ResetFlags & SCSI_RESET_SYNCHRONOUS)
if ((CCB = HostAdapter->BusDeviceResetPendingCCB[TargetID]) != NULL)
{
Command->reset_chain = CCB->Command;
CCB->Command = Command;
BusLogic_Warning("Unable to Reset Command to Target %d - "
"Reset Pending\n", HostAdapter, TargetID);
Result = SCSI_RESET_PENDING;
goto Done;
}
if (BusLogic_MultiMasterHostAdapterP(HostAdapter))
{
if (HostAdapter->TargetFlags[TargetID].TaggedQueuingActive &&
HostAdapter->ActiveCommands[TargetID] > 0 &&
HostAdapter->FirmwareVersion[0] < '5')
goto Done;
}
CCB = BusLogic_AllocateCCB(HostAdapter);
if (CCB == NULL) goto Done;
BusLogic_Warning("Sending Bus Device Reset CCB #%ld to Target %d\n",
HostAdapter, CCB->SerialNumber, TargetID);
CCB->Opcode = BusLogic_BusDeviceReset;
CCB->TargetID = TargetID;
if (ResetFlags & SCSI_RESET_SYNCHRONOUS)
{
Command->reset_chain = NULL;
CCB->Command = Command;
}
if (BusLogic_MultiMasterHostAdapterP(HostAdapter))
{
if (!(BusLogic_WriteOutgoingMailbox(
HostAdapter, BusLogic_MailboxStartCommand, CCB)))
{
BusLogic_Warning("Unable to write Outgoing Mailbox for "
"Bus Device Reset\n", HostAdapter);
BusLogic_DeallocateCCB(CCB);
goto Done;
}
}
else
{
CCB->Status = BusLogic_CCB_Active;
HostAdapter->ActiveCommands[TargetID]++;
FlashPoint_StartCCB(HostAdapter->CardHandle, CCB);
}
BusLogic_IncrementErrorCounter(
&HostAdapter->TargetStatistics[TargetID].BusDeviceResetsAttempted);
HostAdapter->BusDeviceResetPendingCCB[TargetID] = CCB;
HostAdapter->LastResetAttempted[TargetID] = jiffies;
for (XCCB = HostAdapter->All_CCBs; XCCB != NULL; XCCB = XCCB->NextAll)
if (XCCB->Status == BusLogic_CCB_Active && XCCB->TargetID == TargetID)
XCCB->Status = BusLogic_CCB_Reset;
Result = SCSI_RESET_PENDING;
if (BusLogic_FlashPointHostAdapterP(HostAdapter))
if (CCB->Status == BusLogic_CCB_Completed)
{
BusLogic_ProcessCompletedCCBs(HostAdapter);
Result = SCSI_RESET_SUCCESS;
}
Done:
if (Result < 0)
Result = BusLogic_ResetHostAdapter(HostAdapter, Command, ResetFlags);
BusLogic_ReleaseHostAdapterLock(HostAdapter, &ProcessorFlags);
return Result;
}
int BusLogic_ResetCommand(SCSI_Command_T *Command, unsigned int ResetFlags)
{
BusLogic_HostAdapter_T *HostAdapter =
(BusLogic_HostAdapter_T *) Command->host->hostdata;
int TargetID = Command->target;
BusLogic_ErrorRecoveryStrategy_T
ErrorRecoveryStrategy = HostAdapter->ErrorRecoveryStrategy[TargetID];
if (HostAdapter->TargetFlags[TargetID].TaggedQueuingActive &&
jiffies - HostAdapter->LastResetCompleted[TargetID] < 10*60*HZ)
{
HostAdapter->TaggedQueuingPermitted &= ~(1 << TargetID);
HostAdapter->TargetFlags[TargetID].TaggedQueuingActive = false;
BusLogic_Warning("Tagged Queuing now disabled for Target %d\n",
HostAdapter, TargetID);
}
switch (ErrorRecoveryStrategy)
{
case BusLogic_ErrorRecovery_Default:
if (ResetFlags & SCSI_RESET_SUGGEST_HOST_RESET)
return BusLogic_ResetHostAdapter(HostAdapter, Command, ResetFlags);
else if (ResetFlags & SCSI_RESET_SUGGEST_BUS_RESET)
return BusLogic_ResetHostAdapter(HostAdapter, Command, ResetFlags);
case BusLogic_ErrorRecovery_BusDeviceReset:
if (HostAdapter->TargetFlags[TargetID].CommandSuccessfulFlag ||
jiffies - HostAdapter->LastResetAttempted[TargetID] < HZ/10)
{
HostAdapter->TargetFlags[TargetID].CommandSuccessfulFlag = false;
return BusLogic_SendBusDeviceReset(HostAdapter, Command, ResetFlags);
}
case BusLogic_ErrorRecovery_HardReset:
return BusLogic_ResetHostAdapter(HostAdapter, Command, ResetFlags);
case BusLogic_ErrorRecovery_None:
BusLogic_Warning("Error Recovery for Target %d Suppressed\n",
HostAdapter, TargetID);
break;
}
return SCSI_RESET_PUNT;
}
int BusLogic_BIOSDiskParameters(SCSI_Disk_T *Disk, KernelDevice_T Device,
int *Parameters)
{
BusLogic_HostAdapter_T *HostAdapter =
(BusLogic_HostAdapter_T *) Disk->device->host->hostdata;
BIOS_DiskParameters_T *DiskParameters = (BIOS_DiskParameters_T *) Parameters;
struct buffer_head *BufferHead;
if (HostAdapter->ExtendedTranslationEnabled &&
Disk->capacity >= 2*1024*1024 )
{
if (Disk->capacity >= 4*1024*1024 )
{
DiskParameters->Heads = 255;
DiskParameters->Sectors = 63;
}
else
{
DiskParameters->Heads = 128;
DiskParameters->Sectors = 32;
}
}
else
{
DiskParameters->Heads = 64;
DiskParameters->Sectors = 32;
}
DiskParameters->Cylinders =
Disk->capacity / (DiskParameters->Heads * DiskParameters->Sectors);
BufferHead = bread(MKDEV(MAJOR(Device), MINOR(Device) & ~0x0F), 0, 1024);
if (BufferHead == NULL) return 0;
if (*(unsigned short *) (BufferHead->b_data + 0x1FE) == 0xAA55)
{
PartitionTable_T *FirstPartitionEntry =
(PartitionTable_T *) (BufferHead->b_data + 0x1BE);
PartitionTable_T *PartitionEntry = FirstPartitionEntry;
int SavedCylinders = DiskParameters->Cylinders, PartitionNumber;
unsigned char PartitionEntryEndHead, PartitionEntryEndSector;
for (PartitionNumber = 0; PartitionNumber < 4; PartitionNumber++)
{
PartitionEntryEndHead = PartitionEntry->end_head;
PartitionEntryEndSector = PartitionEntry->end_sector & 0x3F;
if (PartitionEntryEndHead == 64-1)
{
DiskParameters->Heads = 64;
DiskParameters->Sectors = 32;
break;
}
else if (PartitionEntryEndHead == 128-1)
{
DiskParameters->Heads = 128;
DiskParameters->Sectors = 32;
break;
}
else if (PartitionEntryEndHead == 255-1)
{
DiskParameters->Heads = 255;
DiskParameters->Sectors = 63;
break;
}
PartitionEntry++;
}
if (PartitionNumber == 4)
{
PartitionEntryEndHead = FirstPartitionEntry->end_head;
PartitionEntryEndSector = FirstPartitionEntry->end_sector & 0x3F;
}
DiskParameters->Cylinders =
Disk->capacity / (DiskParameters->Heads * DiskParameters->Sectors);
if (PartitionNumber < 4 &&
PartitionEntryEndSector == DiskParameters->Sectors)
{
if (DiskParameters->Cylinders != SavedCylinders)
BusLogic_Warning("Adopting Geometry %d/%d from Partition Table\n",
HostAdapter,
DiskParameters->Heads, DiskParameters->Sectors);
}
else if (PartitionEntryEndHead > 0 || PartitionEntryEndSector > 0)
{
BusLogic_Warning("Warning: Partition Table appears to "
"have Geometry %d/%d which is\n", HostAdapter,
PartitionEntryEndHead + 1,
PartitionEntryEndSector);
BusLogic_Warning("not compatible with current BusLogic "
"Host Adapter Geometry %d/%d\n", HostAdapter,
DiskParameters->Heads, DiskParameters->Sectors);
}
}
brelse(BufferHead);
return 0;
}
int BusLogic_ProcDirectoryInfo(char *ProcBuffer, char **StartPointer,
off_t Offset, int BytesAvailable,
int HostNumber, int WriteFlag)
{
BusLogic_HostAdapter_T *HostAdapter;
BusLogic_TargetStatistics_T *TargetStatistics;
int TargetID, Length;
char *Buffer;
for (HostAdapter = BusLogic_FirstRegisteredHostAdapter;
HostAdapter != NULL;
HostAdapter = HostAdapter->Next)
if (HostAdapter->HostNumber == HostNumber) break;
if (HostAdapter == NULL)
{
BusLogic_Error("Cannot find Host Adapter for SCSI Host %d\n",
NULL, HostNumber);
return 0;
}
TargetStatistics = HostAdapter->TargetStatistics;
if (WriteFlag)
{
HostAdapter->ExternalHostAdapterResets = 0;
HostAdapter->HostAdapterInternalErrors = 0;
memset(TargetStatistics, 0,
BusLogic_MaxTargetDevices * sizeof(BusLogic_TargetStatistics_T));
return 0;
}
Buffer = HostAdapter->MessageBuffer;
Length = HostAdapter->MessageBufferLength;
Length += sprintf(&Buffer[Length], "\n\
Current Driver Queue Depth:	%d\n\
Currently Allocated CCBs:	%d\n",
HostAdapter->DriverQueueDepth,
HostAdapter->AllocatedCCBs);
Length += sprintf(&Buffer[Length], "\n\n\
DATA TRANSFER STATISTICS\n\
\n\
Target	Tagged Queuing	Queue Depth  Active  Attempted	Completed\n\
======	==============	===========  ======  =========	=========\n");
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
{
BusLogic_TargetFlags_T *TargetFlags = &HostAdapter->TargetFlags[TargetID];
if (!TargetFlags->TargetExists) continue;
Length +=
sprintf(&Buffer[Length], "  %2d	%s", TargetID,
(TargetFlags->TaggedQueuingSupported
? (TargetFlags->TaggedQueuingActive
? "    Active"
: (HostAdapter->TaggedQueuingPermitted & (1 << TargetID)
? "  Permitted" : "   Disabled"))
: "Not Supported"));
Length += sprintf(&Buffer[Length],
"	    %3d       %3u    %9u	%9u\n",
HostAdapter->QueueDepth[TargetID],
HostAdapter->ActiveCommands[TargetID],
TargetStatistics[TargetID].CommandsAttempted,
TargetStatistics[TargetID].CommandsCompleted);
}
Length += sprintf(&Buffer[Length], "\n\
Target  Read Commands  Write Commands   Total Bytes Read    Total Bytes Written\n\
======  =============  ==============  ===================  ===================\n");
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
{
BusLogic_TargetFlags_T *TargetFlags = &HostAdapter->TargetFlags[TargetID];
if (!TargetFlags->TargetExists) continue;
Length +=
sprintf(&Buffer[Length], "  %2d	  %9u	 %9u", TargetID,
TargetStatistics[TargetID].ReadCommands,
TargetStatistics[TargetID].WriteCommands);
if (TargetStatistics[TargetID].TotalBytesRead.Billions > 0)
Length +=
sprintf(&Buffer[Length], "     %9u%09u",
TargetStatistics[TargetID].TotalBytesRead.Billions,
TargetStatistics[TargetID].TotalBytesRead.Units);
else
Length +=
sprintf(&Buffer[Length], "		%9u",
TargetStatistics[TargetID].TotalBytesRead.Units);
if (TargetStatistics[TargetID].TotalBytesWritten.Billions > 0)
Length +=
sprintf(&Buffer[Length], "   %9u%09u\n",
TargetStatistics[TargetID].TotalBytesWritten.Billions,
TargetStatistics[TargetID].TotalBytesWritten.Units);
else
Length +=
sprintf(&Buffer[Length], "	     %9u\n",
TargetStatistics[TargetID].TotalBytesWritten.Units);
}
Length += sprintf(&Buffer[Length], "\n\
Target  Command    0-1KB      1-2KB      2-4KB      4-8KB     8-16KB\n\
======  =======  =========  =========  =========  =========  =========\n");
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
{
BusLogic_TargetFlags_T *TargetFlags = &HostAdapter->TargetFlags[TargetID];
if (!TargetFlags->TargetExists) continue;
Length +=
sprintf(&Buffer[Length],
"  %2d	 Read	 %9u  %9u  %9u  %9u  %9u\n", TargetID,
TargetStatistics[TargetID].ReadCommandSizeBuckets[0],
TargetStatistics[TargetID].ReadCommandSizeBuckets[1],
TargetStatistics[TargetID].ReadCommandSizeBuckets[2],
TargetStatistics[TargetID].ReadCommandSizeBuckets[3],
TargetStatistics[TargetID].ReadCommandSizeBuckets[4]);
Length +=
sprintf(&Buffer[Length],
"  %2d	 Write	 %9u  %9u  %9u  %9u  %9u\n", TargetID,
TargetStatistics[TargetID].WriteCommandSizeBuckets[0],
TargetStatistics[TargetID].WriteCommandSizeBuckets[1],
TargetStatistics[TargetID].WriteCommandSizeBuckets[2],
TargetStatistics[TargetID].WriteCommandSizeBuckets[3],
TargetStatistics[TargetID].WriteCommandSizeBuckets[4]);
}
Length += sprintf(&Buffer[Length], "\n\
Target  Command   16-32KB    32-64KB   64-128KB   128-256KB   256KB+\n\
======  =======  =========  =========  =========  =========  =========\n");
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
{
BusLogic_TargetFlags_T *TargetFlags = &HostAdapter->TargetFlags[TargetID];
if (!TargetFlags->TargetExists) continue;
Length +=
sprintf(&Buffer[Length],
"  %2d	 Read	 %9u  %9u  %9u  %9u  %9u\n", TargetID,
TargetStatistics[TargetID].ReadCommandSizeBuckets[5],
TargetStatistics[TargetID].ReadCommandSizeBuckets[6],
TargetStatistics[TargetID].ReadCommandSizeBuckets[7],
TargetStatistics[TargetID].ReadCommandSizeBuckets[8],
TargetStatistics[TargetID].ReadCommandSizeBuckets[9]);
Length +=
sprintf(&Buffer[Length],
"  %2d	 Write	 %9u  %9u  %9u  %9u  %9u\n", TargetID,
TargetStatistics[TargetID].WriteCommandSizeBuckets[5],
TargetStatistics[TargetID].WriteCommandSizeBuckets[6],
TargetStatistics[TargetID].WriteCommandSizeBuckets[7],
TargetStatistics[TargetID].WriteCommandSizeBuckets[8],
TargetStatistics[TargetID].WriteCommandSizeBuckets[9]);
}
Length += sprintf(&Buffer[Length], "\n\n\
ERROR RECOVERY STATISTICS\n\
\n\
Command Aborts      Bus Device Resets	  Host Adapter Resets\n\
Target	Requested Completed  Requested Completed  Requested Completed\n\
ID	\\\\\\\\ Attempted
======	 ===== ===== =====    ===== ===== =====	   ===== ===== =====\n");
for (TargetID = 0; TargetID < HostAdapter->MaxTargetDevices; TargetID++)
{
BusLogic_TargetFlags_T *TargetFlags = &HostAdapter->TargetFlags[TargetID];
if (!TargetFlags->TargetExists) continue;
Length +=
sprintf(&Buffer[Length], "\
%2d	 %5d %5d %5d    %5d %5d %5d	   %5d %5d %5d\n", TargetID,
TargetStatistics[TargetID].CommandAbortsRequested,
TargetStatistics[TargetID].CommandAbortsAttempted,
TargetStatistics[TargetID].CommandAbortsCompleted,
TargetStatistics[TargetID].BusDeviceResetsRequested,
TargetStatistics[TargetID].BusDeviceResetsAttempted,
TargetStatistics[TargetID].BusDeviceResetsCompleted,
TargetStatistics[TargetID].HostAdapterResetsRequested,
TargetStatistics[TargetID].HostAdapterResetsAttempted,
TargetStatistics[TargetID].HostAdapterResetsCompleted);
}
Length += sprintf(&Buffer[Length], "\nExternal Host Adapter Resets: %d\n",
HostAdapter->ExternalHostAdapterResets);
Length += sprintf(&Buffer[Length], "Host Adapter Internal Errors: %d\n",
HostAdapter->HostAdapterInternalErrors);
if (Length >= BusLogic_MessageBufferSize)
BusLogic_Error("Message Buffer length %d exceeds size %d\n",
HostAdapter, Length, BusLogic_MessageBufferSize);
if ((Length -= Offset) <= 0) return 0;
if (Length >= BytesAvailable) Length = BytesAvailable;
*StartPointer = &HostAdapter->MessageBuffer[Offset];
return Length;
}
static void BusLogic_Message(BusLogic_MessageLevel_T MessageLevel,
char *Format,
BusLogic_HostAdapter_T *HostAdapter,
...)
{
static char Buffer[BusLogic_LineBufferSize];
static boolean BeginningOfLine = true;
va_list Arguments;
int Length = 0;
va_start(Arguments, HostAdapter);
Length = vsprintf(Buffer, Format, Arguments);
va_end(Arguments);
if (MessageLevel == BusLogic_AnnounceLevel)
{
static int AnnouncementLines = 0;
strcpy(&HostAdapter->MessageBuffer[HostAdapter->MessageBufferLength],
Buffer);
HostAdapter->MessageBufferLength += Length;
if (++AnnouncementLines <= 2)
printk("%sscsi: %s", BusLogic_MessageLevelMap[MessageLevel], Buffer);
}
else if (MessageLevel == BusLogic_InfoLevel)
{
strcpy(&HostAdapter->MessageBuffer[HostAdapter->MessageBufferLength],
Buffer);
HostAdapter->MessageBufferLength += Length;
if (BeginningOfLine)
{
if (Buffer[0] != '\n' || Length > 1)
printk("%sscsi%d: %s", BusLogic_MessageLevelMap[MessageLevel],
HostAdapter->HostNumber, Buffer);
}
else printk("%s", Buffer);
}
else
{
if (BeginningOfLine)
{
if (HostAdapter != NULL && HostAdapter->HostAdapterInitialized)
printk("%sscsi%d: %s", BusLogic_MessageLevelMap[MessageLevel],
HostAdapter->HostNumber, Buffer);
else printk("%s%s", BusLogic_MessageLevelMap[MessageLevel], Buffer);
}
else printk("%s", Buffer);
}
BeginningOfLine = (Buffer[Length-1] == '\n');
}
static boolean BusLogic_ParseKeyword(char **StringPointer, char *Keyword)
{
char *Pointer = *StringPointer;
while (*Keyword != '\0')
{
char StringChar = *Pointer++;
char KeywordChar = *Keyword++;
if (StringChar >= 'A' && StringChar <= 'Z')
StringChar += 'a' - 'Z';
if (KeywordChar >= 'A' && KeywordChar <= 'Z')
KeywordChar += 'a' - 'Z';
if (StringChar != KeywordChar) return false;
}
*StringPointer = Pointer;
return true;
}
static void BusLogic_ParseDriverOptions(char *OptionsString)
{
while (true)
{
BusLogic_DriverOptions_T *DriverOptions =
&BusLogic_DriverOptions[BusLogic_DriverOptionsCount++];
int TargetID;
memset(DriverOptions, 0, sizeof(BusLogic_DriverOptions_T));
for (TargetID = 0; TargetID < BusLogic_MaxTargetDevices; TargetID++)
DriverOptions->ErrorRecoveryStrategy[TargetID] =
BusLogic_ErrorRecovery_Default;
while (*OptionsString != '\0' && *OptionsString != ';')
{
if (BusLogic_ParseKeyword(&OptionsString, "IO:"))
{
BusLogic_IO_Address_T IO_Address =
simple_strtoul(OptionsString, &OptionsString, 0);
BusLogic_ProbeOptions.LimitedProbeISA = true;
switch (IO_Address)
{
case 0x330:
BusLogic_ProbeOptions.Probe330 = true;
break;
case 0x334:
BusLogic_ProbeOptions.Probe334 = true;
break;
case 0x230:
BusLogic_ProbeOptions.Probe230 = true;
break;
case 0x234:
BusLogic_ProbeOptions.Probe234 = true;
break;
case 0x130:
BusLogic_ProbeOptions.Probe130 = true;
break;
case 0x134:
BusLogic_ProbeOptions.Probe134 = true;
break;
default:
BusLogic_Error("BusLogic: Invalid Driver Options "
"(illegal I/O Address 0x%X)\n",
NULL, IO_Address);
return;
}
}
else if (BusLogic_ParseKeyword(&OptionsString, "NoProbeISA"))
BusLogic_ProbeOptions.NoProbeISA = true;
else if (BusLogic_ParseKeyword(&OptionsString, "NoProbePCI"))
BusLogic_ProbeOptions.NoProbePCI = true;
else if (BusLogic_ParseKeyword(&OptionsString, "NoProbe"))
BusLogic_ProbeOptions.NoProbe = true;
else if (BusLogic_ParseKeyword(&OptionsString, "NoSortPCI"))
BusLogic_ProbeOptions.NoSortPCI = true;
else if (BusLogic_ParseKeyword(&OptionsString, "MultiMasterFirst"))
BusLogic_ProbeOptions.MultiMasterFirst = true;
else if (BusLogic_ParseKeyword(&OptionsString, "FlashPointFirst"))
BusLogic_ProbeOptions.FlashPointFirst = true;
else if (BusLogic_ParseKeyword(&OptionsString, "QueueDepth:[") ||
BusLogic_ParseKeyword(&OptionsString, "QD:["))
{
for (TargetID = 0;
TargetID < BusLogic_MaxTargetDevices;
TargetID++)
{
unsigned short QueueDepth =
simple_strtoul(OptionsString, &OptionsString, 0);
if (QueueDepth > BusLogic_MaxTaggedQueueDepth)
{
BusLogic_Error("BusLogic: Invalid Driver Options "
"(illegal Queue Depth %d)\n",
NULL, QueueDepth);
return;
}
DriverOptions->QueueDepth[TargetID] = QueueDepth;
if (*OptionsString == ',')
OptionsString++;
else if (*OptionsString == ']')
break;
else
{
BusLogic_Error("BusLogic: Invalid Driver Options "
"(',' or ']' expected at '%s')\n",
NULL, OptionsString);
return;
}
}
if (*OptionsString != ']')
{
BusLogic_Error("BusLogic: Invalid Driver Options "
"(']' expected at '%s')\n",
NULL, OptionsString);
return;
}
else OptionsString++;
}
else if (BusLogic_ParseKeyword(&OptionsString, "QueueDepth:") ||
BusLogic_ParseKeyword(&OptionsString, "QD:"))
{
unsigned short QueueDepth =
simple_strtoul(OptionsString, &OptionsString, 0);
if (QueueDepth == 0 || QueueDepth > BusLogic_MaxTaggedQueueDepth)
{
BusLogic_Error("BusLogic: Invalid Driver Options "
"(illegal Queue Depth %d)\n",
NULL, QueueDepth);
return;
}
DriverOptions->CommonQueueDepth = QueueDepth;
for (TargetID = 0;
TargetID < BusLogic_MaxTargetDevices;
TargetID++)
DriverOptions->QueueDepth[TargetID] = QueueDepth;
}
else if (BusLogic_ParseKeyword(&OptionsString, "TaggedQueuing:") ||
BusLogic_ParseKeyword(&OptionsString, "TQ:"))
{
if (BusLogic_ParseKeyword(&OptionsString, "Default"))
{
DriverOptions->TaggedQueuingPermitted = 0x0000;
DriverOptions->TaggedQueuingPermittedMask = 0x0000;
}
else if (BusLogic_ParseKeyword(&OptionsString, "Enable"))
{
DriverOptions->TaggedQueuingPermitted = 0xFFFF;
DriverOptions->TaggedQueuingPermittedMask = 0xFFFF;
}
else if (BusLogic_ParseKeyword(&OptionsString, "Disable"))
{
DriverOptions->TaggedQueuingPermitted = 0x0000;
DriverOptions->TaggedQueuingPermittedMask = 0xFFFF;
}
else
{
unsigned short TargetBit;
for (TargetID = 0, TargetBit = 1;
TargetID < BusLogic_MaxTargetDevices;
TargetID++, TargetBit <<= 1)
switch (*OptionsString++)
{
case 'Y':
DriverOptions->TaggedQueuingPermitted |= TargetBit;
DriverOptions->TaggedQueuingPermittedMask |= TargetBit;
break;
case 'N':
DriverOptions->TaggedQueuingPermitted &= ~TargetBit;
DriverOptions->TaggedQueuingPermittedMask |= TargetBit;
break;
case 'X':
break;
default:
OptionsString--;
TargetID = BusLogic_MaxTargetDevices;
break;
}
}
}
else if (BusLogic_ParseKeyword(&OptionsString, "ErrorRecovery:") ||
BusLogic_ParseKeyword(&OptionsString, "ER:"))
{
if (BusLogic_ParseKeyword(&OptionsString, "Default"))
for (TargetID = 0;
TargetID < BusLogic_MaxTargetDevices;
TargetID++)
DriverOptions->ErrorRecoveryStrategy[TargetID] =
BusLogic_ErrorRecovery_Default;
else if (BusLogic_ParseKeyword(&OptionsString, "HardReset"))
for (TargetID = 0;
TargetID < BusLogic_MaxTargetDevices;
TargetID++)
DriverOptions->ErrorRecoveryStrategy[TargetID] =
BusLogic_ErrorRecovery_HardReset;
else if (BusLogic_ParseKeyword(&OptionsString, "BusDeviceReset"))
for (TargetID = 0;
TargetID < BusLogic_MaxTargetDevices;
TargetID++)
DriverOptions->ErrorRecoveryStrategy[TargetID] =
BusLogic_ErrorRecovery_BusDeviceReset;
else if (BusLogic_ParseKeyword(&OptionsString, "None"))
for (TargetID = 0;
TargetID < BusLogic_MaxTargetDevices;
TargetID++)
DriverOptions->ErrorRecoveryStrategy[TargetID] =
BusLogic_ErrorRecovery_None;
else
for (TargetID = 0;
TargetID < BusLogic_MaxTargetDevices;
TargetID++)
switch (*OptionsString++)
{
case 'D':
DriverOptions->ErrorRecoveryStrategy[TargetID] =
BusLogic_ErrorRecovery_Default;
break;
case 'H':
DriverOptions->ErrorRecoveryStrategy[TargetID] =
BusLogic_ErrorRecovery_HardReset;
break;
case 'B':
DriverOptions->ErrorRecoveryStrategy[TargetID] =
BusLogic_ErrorRecovery_BusDeviceReset;
break;
case 'N':
DriverOptions->ErrorRecoveryStrategy[TargetID] =
BusLogic_ErrorRecovery_None;
break;
default:
OptionsString--;
TargetID = BusLogic_MaxTargetDevices;
break;
}
}
else if (BusLogic_ParseKeyword(&OptionsString, "BusSettleTime:") ||
BusLogic_ParseKeyword(&OptionsString, "BST:"))
{
unsigned short BusSettleTime =
simple_strtoul(OptionsString, &OptionsString, 0);
if (BusSettleTime > 5 * 60)
{
BusLogic_Error("BusLogic: Invalid Driver Options "
"(illegal Bus Settle Time %d)\n",
NULL, BusSettleTime);
return;
}
DriverOptions->BusSettleTime = BusSettleTime;
}
else if (BusLogic_ParseKeyword(&OptionsString,
"InhibitTargetInquiry"))
DriverOptions->LocalOptions.InhibitTargetInquiry = true;
else if (BusLogic_ParseKeyword(&OptionsString, "TraceProbe"))
BusLogic_GlobalOptions.TraceProbe = true;
else if (BusLogic_ParseKeyword(&OptionsString, "TraceHardwareReset"))
BusLogic_GlobalOptions.TraceHardwareReset = true;
else if (BusLogic_ParseKeyword(&OptionsString, "TraceConfiguration"))
BusLogic_GlobalOptions.TraceConfiguration = true;
else if (BusLogic_ParseKeyword(&OptionsString, "TraceErrors"))
BusLogic_GlobalOptions.TraceErrors = true;
else if (BusLogic_ParseKeyword(&OptionsString, "Debug"))
{
BusLogic_GlobalOptions.TraceProbe = true;
BusLogic_GlobalOptions.TraceHardwareReset = true;
BusLogic_GlobalOptions.TraceConfiguration = true;
BusLogic_GlobalOptions.TraceErrors = true;
}
if (*OptionsString == ',')
OptionsString++;
else if (*OptionsString != ';' && *OptionsString != '\0')
{
BusLogic_Error("BusLogic: Unexpected Driver Option '%s' "
"ignored\n", NULL, OptionsString);
*OptionsString = '\0';
}
}
if (!(BusLogic_DriverOptionsCount == 0 ||
BusLogic_ProbeInfoCount == 0 ||
BusLogic_DriverOptionsCount == BusLogic_ProbeInfoCount))
{
BusLogic_Error("BusLogic: Invalid Driver Options "
"(all or no I/O Addresses must be specified)\n", NULL);
return;
}
for (TargetID = 0; TargetID < BusLogic_MaxTargetDevices; TargetID++)
if (DriverOptions->QueueDepth[TargetID] == 1)
{
unsigned short TargetBit = 1 << TargetID;
DriverOptions->TaggedQueuingPermitted &= ~TargetBit;
DriverOptions->TaggedQueuingPermittedMask |= TargetBit;
}
if (*OptionsString == ';') OptionsString++;
if (*OptionsString == '\0') return;
}
}
void BusLogic_Setup(char *CommandLineString, int *CommandLineIntegers)
{
if (CommandLineIntegers[0] != 0)
{
BusLogic_Error("BusLogic: Obsolete Command Line Entry "
"Format Ignored\n", NULL);
return;
}
if (CommandLineString == NULL || *CommandLineString == '\0') return;
BusLogic_ParseDriverOptions(CommandLineString);
}
#ifdef MODULE
SCSI_Host_Template_T driver_template = BUSLOGIC;
#include "scsi_module.c"
#endif