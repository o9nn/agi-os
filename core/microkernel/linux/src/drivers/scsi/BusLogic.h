#include <linux/config.h>
typedef kdev_t KernelDevice_T;
typedef struct proc_dir_entry PROC_DirectoryEntry_T;
typedef unsigned long ProcessorFlags_T;
typedef struct pt_regs Registers_T;
typedef struct partition PartitionTable_T;
typedef Scsi_Host_Template SCSI_Host_Template_T;
typedef struct Scsi_Host SCSI_Host_T;
typedef struct scsi_device SCSI_Device_T;
typedef struct scsi_disk SCSI_Disk_T;
typedef struct scsi_cmnd SCSI_Command_T;
typedef struct scatterlist SCSI_ScatterList_T;
extern PROC_DirectoryEntry_T BusLogic_ProcDirectoryEntry;
extern const char *BusLogic_DriverInfo(SCSI_Host_T *);
extern int BusLogic_DetectHostAdapter(SCSI_Host_Template_T *);
extern int BusLogic_ReleaseHostAdapter(SCSI_Host_T *);
extern int BusLogic_QueueCommand(SCSI_Command_T *,
void (*CompletionRoutine)(SCSI_Command_T *));
extern int BusLogic_AbortCommand(SCSI_Command_T *);
extern int BusLogic_ResetCommand(SCSI_Command_T *, unsigned int);
extern int BusLogic_BIOSDiskParameters(SCSI_Disk_T *, KernelDevice_T, int *);
extern int BusLogic_ProcDirectoryInfo(char *, char **, off_t, int, int, int);
#define BUSLOGIC \
{ proc_dir: &BusLogic_ProcDirectoryEntry, \
proc_info: BusLogic_ProcDirectoryInfo, \
name: "BusLogic", \
detect: BusLogic_DetectHostAdapter, \
release: BusLogic_ReleaseHostAdapter, \
info: BusLogic_DriverInfo, \
queuecommand: BusLogic_QueueCommand, \
abort: BusLogic_AbortCommand, \
reset: BusLogic_ResetCommand, \
bios_param: BusLogic_BIOSDiskParameters, \
unchecked_isa_dma: 1, \
use_clustering: ENABLE_CLUSTERING }
#ifdef BusLogic_DriverVersion
#ifndef __i386__
#undef CONFIG_SCSI_OMIT_FLASHPOINT
#define CONFIG_SCSI_OMIT_FLASHPOINT
#endif
#ifndef CONFIG_PCI
#undef CONFIG_SCSI_OMIT_FLASHPOINT
#define CONFIG_SCSI_OMIT_FLASHPOINT
#define BusLogic_InitializeProbeInfoListISA \
BusLogic_InitializeProbeInfoList
#endif
#define BusLogic_MaxHostAdapters 16
#define BusLogic_MaxTargetDevices 16
#define BusLogic_ScatterGatherLimit 128
#define BusLogic_MaxTaggedQueueDepth 64
#define BusLogic_MaxAutomaticTaggedQueueDepth 28
#define BusLogic_MinAutomaticTaggedQueueDepth 7
#define BusLogic_TaggedQueueDepthBB 3
#define BusLogic_UntaggedQueueDepth 3
#define BusLogic_UntaggedQueueDepthBB 2
#define BusLogic_DefaultBusSettleTime 2
#define BusLogic_MaxMailboxes 211
#define BusLogic_CCB_AllocationGroupSize 7
#define BusLogic_LineBufferSize 100
#define BusLogic_MessageBufferSize 9700
typedef enum BusLogic_MessageLevel
{
BusLogic_AnnounceLevel = 0,
BusLogic_InfoLevel = 1,
BusLogic_NoticeLevel = 2,
BusLogic_WarningLevel = 3,
BusLogic_ErrorLevel = 4
}
BusLogic_MessageLevel_T;
static char
*BusLogic_MessageLevelMap[] =
{ KERN_NOTICE, KERN_NOTICE, KERN_NOTICE, KERN_WARNING, KERN_ERR };
#define BusLogic_Announce(Format, Arguments...) \
BusLogic_Message(BusLogic_AnnounceLevel, Format, ##Arguments)
#define BusLogic_Info(Format, Arguments...) \
BusLogic_Message(BusLogic_InfoLevel, Format, ##Arguments)
#define BusLogic_Notice(Format, Arguments...) \
BusLogic_Message(BusLogic_NoticeLevel, Format, ##Arguments)
#define BusLogic_Warning(Format, Arguments...) \
BusLogic_Message(BusLogic_WarningLevel, Format, ##Arguments)
#define BusLogic_Error(Format, Arguments...) \
BusLogic_Message(BusLogic_ErrorLevel, Format, ##Arguments)
typedef enum
{
BusLogic_MultiMaster = 1,
BusLogic_FlashPoint = 2
}
__attribute__ ((packed))
BusLogic_HostAdapterType_T;
#define BusLogic_MultiMasterAddressCount 4
#define BusLogic_FlashPointAddressCount 256
static int
BusLogic_HostAdapterAddressCount[3] =
{ 0, BusLogic_MultiMasterAddressCount, BusLogic_FlashPointAddressCount };
#ifndef CONFIG_SCSI_OMIT_FLASHPOINT
#define BusLogic_MultiMasterHostAdapterP(HostAdapter) \
(HostAdapter->HostAdapterType == BusLogic_MultiMaster)
#define BusLogic_FlashPointHostAdapterP(HostAdapter) \
(HostAdapter->HostAdapterType == BusLogic_FlashPoint)
#else
#define BusLogic_MultiMasterHostAdapterP(HostAdapter) \
(true)
#define BusLogic_FlashPointHostAdapterP(HostAdapter) \
(false)
#endif
typedef enum
{
BusLogic_Unknown_Bus = 0,
BusLogic_ISA_Bus = 1,
BusLogic_EISA_Bus = 2,
BusLogic_PCI_Bus = 3,
BusLogic_VESA_Bus = 4,
BusLogic_MCA_Bus = 5
}
__attribute__ ((packed))
BusLogic_HostAdapterBusType_T;
static char
*BusLogic_HostAdapterBusNames[] =
{ "Unknown", "ISA", "EISA", "PCI", "VESA", "MCA" };
static BusLogic_HostAdapterBusType_T
BusLogic_HostAdapterBusTypes[] =
{ BusLogic_VESA_Bus,
BusLogic_ISA_Bus,
BusLogic_MCA_Bus,
BusLogic_EISA_Bus,
BusLogic_Unknown_Bus,
BusLogic_PCI_Bus };
typedef enum BusLogic_BIOS_DiskGeometryTranslation
{
BusLogic_BIOS_Disk_Not_Installed = 0,
BusLogic_BIOS_Disk_Installed_64x32 = 1,
BusLogic_BIOS_Disk_Installed_128x32 = 2,
BusLogic_BIOS_Disk_Installed_255x63 = 3
}
__attribute__ ((packed))
BusLogic_BIOS_DiskGeometryTranslation_T;
#if defined(__bool_true_false_are_defined) || __STDC_VERSION__ > 201710L
typedef bool boolean;
#else
typedef enum { false, true } __attribute__ ((packed)) boolean;
#endif
typedef unsigned int BusLogic_IO_Address_T;
typedef unsigned int BusLogic_PCI_Address_T;
typedef unsigned int BusLogic_Base_Address_T;
typedef unsigned int BusLogic_BusAddress_T;
typedef unsigned int BusLogic_ByteCount_T;
typedef struct BusLogic_ByteCounter
{
unsigned int Units;
unsigned int Billions;
}
BusLogic_ByteCounter_T;
typedef struct BusLogic_ProbeInfo
{
BusLogic_HostAdapterType_T HostAdapterType;
BusLogic_HostAdapterBusType_T HostAdapterBusType;
BusLogic_IO_Address_T IO_Address;
BusLogic_PCI_Address_T PCI_Address;
unsigned char Bus;
unsigned char Device;
unsigned char IRQ_Channel;
}
BusLogic_ProbeInfo_T;
typedef struct BusLogic_ProbeOptions
{
boolean NoProbe:1;
boolean NoProbeISA:1;
boolean NoProbePCI:1;
boolean NoSortPCI:1;
boolean MultiMasterFirst:1;
boolean FlashPointFirst:1;
boolean LimitedProbeISA:1;
boolean Probe330:1;
boolean Probe334:1;
boolean Probe230:1;
boolean Probe234:1;
boolean Probe130:1;
boolean Probe134:1;
}
BusLogic_ProbeOptions_T;
typedef struct BusLogic_GlobalOptions
{
boolean TraceProbe:1;
boolean TraceHardwareReset:1;
boolean TraceConfiguration:1;
boolean TraceErrors:1;
}
BusLogic_GlobalOptions_T;
typedef struct BusLogic_LocalOptions
{
boolean InhibitTargetInquiry:1;
}
BusLogic_LocalOptions_T;
typedef enum
{
BusLogic_ErrorRecovery_Default = 0,
BusLogic_ErrorRecovery_BusDeviceReset = 1,
BusLogic_ErrorRecovery_HardReset = 2,
BusLogic_ErrorRecovery_None = 3
}
__attribute__ ((packed))
BusLogic_ErrorRecoveryStrategy_T;
static char
*BusLogic_ErrorRecoveryStrategyNames[] =
{ "Default", "Bus Device Reset", "Hard Reset", "None" },
BusLogic_ErrorRecoveryStrategyLetters[] =
{ 'D', 'B', 'H', 'N' };
#define BusLogic_ControlRegisterOffset 0
#define BusLogic_StatusRegisterOffset 0
#define BusLogic_CommandParameterRegisterOffset 1
#define BusLogic_DataInRegisterOffset 1
#define BusLogic_InterruptRegisterOffset 2
#define BusLogic_GeometryRegisterOffset 3
typedef union BusLogic_ControlRegister
{
unsigned char All;
struct {
unsigned char :4;
boolean SCSIBusReset:1;
boolean InterruptReset:1;
boolean SoftReset:1;
boolean HardReset:1;
} Bits;
}
BusLogic_ControlRegister_T;
typedef union BusLogic_StatusRegister
{
unsigned char All;
struct {
boolean CommandInvalid:1;
boolean Reserved:1;
boolean DataInRegisterReady:1;
boolean CommandParameterRegisterBusy:1;
boolean HostAdapterReady:1;
boolean InitializationRequired:1;
boolean DiagnosticFailure:1;
boolean DiagnosticActive:1;
} Bits;
}
BusLogic_StatusRegister_T;
typedef union BusLogic_InterruptRegister
{
unsigned char All;
struct {
boolean IncomingMailboxLoaded:1;
boolean OutgoingMailboxAvailable:1;
boolean CommandComplete:1;
boolean ExternalBusReset:1;
unsigned char Reserved:3;
boolean InterruptValid:1;
} Bits;
}
BusLogic_InterruptRegister_T;
typedef union BusLogic_GeometryRegister
{
unsigned char All;
struct {
BusLogic_BIOS_DiskGeometryTranslation_T Drive0Geometry:2;
BusLogic_BIOS_DiskGeometryTranslation_T Drive1Geometry:2;
unsigned char :3;
boolean ExtendedTranslationEnabled:1;
} Bits;
}
BusLogic_GeometryRegister_T;
typedef enum
{
BusLogic_TestCommandCompleteInterrupt = 0x00,
BusLogic_InitializeMailbox = 0x01,
BusLogic_ExecuteMailboxCommand = 0x02,
BusLogic_ExecuteBIOSCommand = 0x03,
BusLogic_InquireBoardID = 0x04,
BusLogic_EnableOutgoingMailboxAvailableInt = 0x05,
BusLogic_SetSCSISelectionTimeout = 0x06,
BusLogic_SetPreemptTimeOnBus = 0x07,
BusLogic_SetTimeOffBus = 0x08,
BusLogic_SetBusTransferRate = 0x09,
BusLogic_InquireInstalledDevicesID0to7 = 0x0A,
BusLogic_InquireConfiguration = 0x0B,
BusLogic_EnableTargetMode = 0x0C,
BusLogic_InquireSetupInformation = 0x0D,
BusLogic_WriteAdapterLocalRAM = 0x1A,
BusLogic_ReadAdapterLocalRAM = 0x1B,
BusLogic_WriteBusMasterChipFIFO = 0x1C,
BusLogic_ReadBusMasterChipFIFO = 0x1D,
BusLogic_EchoCommandData = 0x1F,
BusLogic_HostAdapterDiagnostic = 0x20,
BusLogic_SetAdapterOptions = 0x21,
BusLogic_InquireInstalledDevicesID8to15 = 0x23,
BusLogic_InquireTargetDevices = 0x24,
BusLogic_DisableHostAdapterInterrupt = 0x25,
BusLogic_InitializeExtendedMailbox = 0x81,
BusLogic_ExecuteSCSICommand = 0x83,
BusLogic_InquireFirmwareVersion3rdDigit = 0x84,
BusLogic_InquireFirmwareVersionLetter = 0x85,
BusLogic_InquirePCIHostAdapterInformation = 0x86,
BusLogic_InquireHostAdapterModelNumber = 0x8B,
BusLogic_InquireSynchronousPeriod = 0x8C,
BusLogic_InquireExtendedSetupInformation = 0x8D,
BusLogic_EnableStrictRoundRobinMode = 0x8F,
BusLogic_StoreHostAdapterLocalRAM = 0x90,
BusLogic_FetchHostAdapterLocalRAM = 0x91,
BusLogic_StoreLocalDataInEEPROM = 0x92,
BusLogic_UploadAutoSCSICode = 0x94,
BusLogic_ModifyIOAddress = 0x95,
BusLogic_SetCCBFormat = 0x96,
BusLogic_WriteInquiryBuffer = 0x9A,
BusLogic_ReadInquiryBuffer = 0x9B,
BusLogic_FlashROMUploadDownload = 0xA7,
BusLogic_ReadSCAMData = 0xA8,
BusLogic_WriteSCAMData = 0xA9
}
BusLogic_OperationCode_T;
typedef struct BusLogic_BoardID
{
unsigned char BoardType;
unsigned char CustomFeatures;
unsigned char FirmwareVersion1stDigit;
unsigned char FirmwareVersion2ndDigit;
}
BusLogic_BoardID_T;
typedef unsigned char BusLogic_InstalledDevices8_T[8];
typedef unsigned short BusLogic_InstalledDevices_T;
typedef struct BusLogic_Configuration
{
unsigned char :5;
boolean DMA_Channel5:1;
boolean DMA_Channel6:1;
boolean DMA_Channel7:1;
boolean IRQ_Channel9:1;
boolean IRQ_Channel10:1;
boolean IRQ_Channel11:1;
boolean IRQ_Channel12:1;
unsigned char :1;
boolean IRQ_Channel14:1;
boolean IRQ_Channel15:1;
unsigned char :1;
unsigned char HostAdapterID:4;
unsigned char :4;
}
BusLogic_Configuration_T;
typedef struct BusLogic_SynchronousValue
{
unsigned char Offset:4;
unsigned char TransferPeriod:3;
boolean Synchronous:1;
}
BusLogic_SynchronousValue_T;
typedef BusLogic_SynchronousValue_T
BusLogic_SynchronousValues8_T[8];
typedef BusLogic_SynchronousValue_T
BusLogic_SynchronousValues_T[BusLogic_MaxTargetDevices];
typedef struct BusLogic_SetupInformation
{
boolean SynchronousInitiationEnabled:1;
boolean ParityCheckingEnabled:1;
unsigned char :6;
unsigned char BusTransferRate;
unsigned char PreemptTimeOnBus;
unsigned char TimeOffBus;
unsigned char MailboxCount;
unsigned char MailboxAddress[3];
BusLogic_SynchronousValues8_T SynchronousValuesID0to7;
unsigned char DisconnectPermittedID0to7;
unsigned char Signature;
unsigned char CharacterD;
unsigned char HostBusType;
unsigned char WideTransfersPermittedID0to7;
unsigned char WideTransfersActiveID0to7;
BusLogic_SynchronousValues8_T SynchronousValuesID8to15;
unsigned char DisconnectPermittedID8to15;
unsigned char :8;
unsigned char WideTransfersPermittedID8to15;
unsigned char WideTransfersActiveID8to15;
}
BusLogic_SetupInformation_T;
typedef struct BusLogic_ExtendedMailboxRequest
{
unsigned char MailboxCount;
BusLogic_BusAddress_T BaseMailboxAddress;
}
__attribute__ ((packed))
BusLogic_ExtendedMailboxRequest_T;
typedef unsigned char BusLogic_FirmwareVersion3rdDigit_T;
typedef unsigned char BusLogic_FirmwareVersionLetter_T;
typedef enum BusLogic_ISACompatibleIOPort
{
BusLogic_IO_330 = 0,
BusLogic_IO_334 = 1,
BusLogic_IO_230 = 2,
BusLogic_IO_234 = 3,
BusLogic_IO_130 = 4,
BusLogic_IO_134 = 5,
BusLogic_IO_Disable = 6,
BusLogic_IO_Disable2 = 7
}
__attribute__ ((packed))
BusLogic_ISACompatibleIOPort_T;
typedef struct BusLogic_PCIHostAdapterInformation
{
BusLogic_ISACompatibleIOPort_T ISACompatibleIOPort;
unsigned char PCIAssignedIRQChannel;
boolean LowByteTerminated:1;
boolean HighByteTerminated:1;
unsigned char :2;
boolean JP1:1;
boolean JP2:1;
boolean JP3:1;
boolean GenericInfoValid:1;
unsigned char :8;
}
BusLogic_PCIHostAdapterInformation_T;
typedef unsigned char BusLogic_HostAdapterModelNumber_T[5];
typedef unsigned char BusLogic_SynchronousPeriod_T[BusLogic_MaxTargetDevices];
typedef struct BusLogic_ExtendedSetupInformation
{
unsigned char BusType;
unsigned char BIOS_Address;
unsigned short ScatterGatherLimit;
unsigned char MailboxCount;
BusLogic_BusAddress_T BaseMailboxAddress;
struct { unsigned char :2;
boolean FastOnEISA:1;
unsigned char :3;
boolean LevelSensitiveInterrupt:1;
unsigned char :1; } Misc;
unsigned char FirmwareRevision[3];
boolean HostWideSCSI:1;
boolean HostDifferentialSCSI:1;
boolean HostSupportsSCAM:1;
boolean HostUltraSCSI:1;
boolean HostSmartTermination:1;
unsigned char :3;
}
__attribute__ ((packed))
BusLogic_ExtendedSetupInformation_T;
typedef enum BusLogic_RoundRobinModeRequest
{
BusLogic_AggressiveRoundRobinMode = 0,
BusLogic_StrictRoundRobinMode = 1
}
__attribute__ ((packed))
BusLogic_RoundRobinModeRequest_T;
#define BusLogic_BIOS_BaseOffset 0
#define BusLogic_AutoSCSI_BaseOffset 64
typedef struct BusLogic_FetchHostAdapterLocalRAMRequest
{
unsigned char ByteOffset;
unsigned char ByteCount;
}
BusLogic_FetchHostAdapterLocalRAMRequest_T;
typedef struct BusLogic_AutoSCSIData
{
unsigned char InternalFactorySignature[2];
unsigned char InformationByteCount;
unsigned char HostAdapterType[6];
unsigned char :8;
boolean FloppyEnabled:1;
boolean FloppySecondary:1;
boolean LevelSensitiveInterrupt:1;
unsigned char :2;
unsigned char SystemRAMAreaForBIOS:3;
unsigned char DMA_Channel:7;
boolean DMA_AutoConfiguration:1;
unsigned char IRQ_Channel:7;
boolean IRQ_AutoConfiguration:1;
unsigned char DMA_TransferRate;
unsigned char SCSI_ID;
boolean LowByteTerminated:1;
boolean ParityCheckingEnabled:1;
boolean HighByteTerminated:1;
boolean NoisyCablingEnvironment:1;
boolean FastSynchronousNegotiation:1;
boolean BusResetEnabled:1;
boolean :1;
boolean ActiveNegationEnabled:1;
unsigned char BusOnDelay;
unsigned char BusOffDelay;
boolean HostAdapterBIOSEnabled:1;
boolean BIOSRedirectionOfINT19Enabled:1;
boolean ExtendedTranslationEnabled:1;
boolean MapRemovableAsFixedEnabled:1;
boolean :1;
boolean BIOSSupportsMoreThan2DrivesEnabled:1;
boolean BIOSInterruptModeEnabled:1;
boolean FlopticalSupportEnabled:1;
unsigned short DeviceEnabled;
unsigned short WidePermitted;
unsigned short FastPermitted;
unsigned short SynchronousPermitted;
unsigned short DisconnectPermitted;
unsigned short SendStartUnitCommand;
unsigned short IgnoreInBIOSScan;
unsigned char PCIInterruptPin:2;
unsigned char HostAdapterIOPortAddress:2;
boolean StrictRoundRobinModeEnabled:1;
boolean VESABusSpeedGreaterThan33MHz:1;
boolean VESABurstWriteEnabled:1;
boolean VESABurstReadEnabled:1;
unsigned short UltraPermitted;
unsigned int :32;
unsigned char :8;
unsigned char AutoSCSIMaximumLUN;
boolean :1;
boolean SCAM_Dominant:1;
boolean SCAM_Enabled:1;
boolean SCAM_Level2:1;
unsigned char :4;
boolean INT13ExtensionEnabled:1;
boolean :1;
boolean CDROMBootEnabled:1;
unsigned char :5;
unsigned char BootTargetID:4;
unsigned char BootChannel:4;
unsigned char ForceBusDeviceScanningOrder:1;
unsigned char :7;
unsigned short NonTaggedToAlternateLUNPermitted;
unsigned short RenegotiateSyncAfterCheckCondition;
unsigned char Reserved[10];
unsigned char ManufacturingDiagnostic[2];
unsigned short Checksum;
}
__attribute__ ((packed))
BusLogic_AutoSCSIData_T;
typedef struct BusLogic_AutoSCSIByte45
{
unsigned char ForceBusDeviceScanningOrder:1;
unsigned char :7;
}
BusLogic_AutoSCSIByte45_T;
#define BusLogic_BIOS_DriveMapOffset 17
typedef struct BusLogic_BIOSDriveMapByte
{
unsigned char TargetIDBit3:1;
unsigned char :2;
BusLogic_BIOS_DiskGeometryTranslation_T DiskGeometry:2;
unsigned char TargetID:3;
}
BusLogic_BIOSDriveMapByte_T;
typedef BusLogic_ISACompatibleIOPort_T BusLogic_ModifyIOAddressRequest_T;
typedef enum BusLogic_SetCCBFormatRequest
{
BusLogic_LegacyLUNFormatCCB = 0,
BusLogic_ExtendedLUNFormatCCB = 1
}
__attribute__ ((packed))
BusLogic_SetCCBFormatRequest_T;
typedef unsigned char BusLogic_RequestedReplyLength_T;
typedef enum
{
BusLogic_OutgoingMailboxFree = 0x00,
BusLogic_MailboxStartCommand = 0x01,
BusLogic_MailboxAbortCommand = 0x02
}
__attribute__ ((packed))
BusLogic_ActionCode_T;
typedef enum
{
BusLogic_IncomingMailboxFree = 0x00,
BusLogic_CommandCompletedWithoutError = 0x01,
BusLogic_CommandAbortedAtHostRequest = 0x02,
BusLogic_AbortedCommandNotFound = 0x03,
BusLogic_CommandCompletedWithError = 0x04,
BusLogic_InvalidCCB = 0x05
}
__attribute__ ((packed))
BusLogic_CompletionCode_T;
typedef enum
{
BusLogic_InitiatorCCB = 0x00,
BusLogic_TargetCCB = 0x01,
BusLogic_InitiatorCCB_ScatterGather = 0x02,
BusLogic_InitiatorCCB_ResidualDataLength = 0x03,
BusLogic_InitiatorCCB_ScatterGatherResidual = 0x04,
BusLogic_BusDeviceReset = 0x81
}
__attribute__ ((packed))
BusLogic_CCB_Opcode_T;
typedef enum
{
BusLogic_UncheckedDataTransfer = 0,
BusLogic_DataInLengthChecked = 1,
BusLogic_DataOutLengthChecked = 2,
BusLogic_NoDataTransfer = 3
}
BusLogic_DataDirection_T;
typedef enum
{
BusLogic_CommandCompletedNormally = 0x00,
BusLogic_LinkedCommandCompleted = 0x0A,
BusLogic_LinkedCommandCompletedWithFlag = 0x0B,
BusLogic_DataUnderRun = 0x0C,
BusLogic_SCSISelectionTimeout = 0x11,
BusLogic_DataOverRun = 0x12,
BusLogic_UnexpectedBusFree = 0x13,
BusLogic_InvalidBusPhaseRequested = 0x14,
BusLogic_InvalidOutgoingMailboxActionCode = 0x15,
BusLogic_InvalidCommandOperationCode = 0x16,
BusLogic_LinkedCCBhasInvalidLUN = 0x17,
BusLogic_InvalidCommandParameter = 0x1A,
BusLogic_AutoRequestSenseFailed = 0x1B,
BusLogic_TaggedQueuingMessageRejected = 0x1C,
BusLogic_UnsupportedMessageReceived = 0x1D,
BusLogic_HostAdapterHardwareFailed = 0x20,
BusLogic_TargetFailedResponseToATN = 0x21,
BusLogic_HostAdapterAssertedRST = 0x22,
BusLogic_OtherDeviceAssertedRST = 0x23,
BusLogic_TargetDeviceReconnectedImproperly = 0x24,
BusLogic_HostAdapterAssertedBusDeviceReset = 0x25,
BusLogic_AbortQueueGenerated = 0x26,
BusLogic_HostAdapterSoftwareError = 0x27,
BusLogic_HostAdapterHardwareTimeoutError = 0x30,
BusLogic_SCSIParityErrorDetected = 0x34
}
__attribute__ ((packed))
BusLogic_HostAdapterStatus_T;
typedef enum
{
BusLogic_OperationGood = 0x00,
BusLogic_CheckCondition = 0x02,
BusLogic_DeviceBusy = 0x08
}
__attribute__ ((packed))
BusLogic_TargetDeviceStatus_T;
typedef enum
{
BusLogic_SimpleQueueTag = 0,
BusLogic_HeadOfQueueTag = 1,
BusLogic_OrderedQueueTag = 2,
BusLogic_ReservedQT = 3
}
BusLogic_QueueTag_T;
#define BusLogic_CDB_MaxLength 12
typedef unsigned char SCSI_CDB_T[BusLogic_CDB_MaxLength];
typedef struct BusLogic_ScatterGatherSegment
{
BusLogic_ByteCount_T SegmentByteCount;
BusLogic_BusAddress_T SegmentDataPointer;
}
BusLogic_ScatterGatherSegment_T;
typedef enum
{
BusLogic_CCB_Free = 0,
BusLogic_CCB_Active = 1,
BusLogic_CCB_Completed = 2,
BusLogic_CCB_Reset = 3
}
__attribute__ ((packed))
BusLogic_CCB_Status_T;
typedef struct BusLogic_CCB
{
BusLogic_CCB_Opcode_T Opcode;
unsigned char :3;
BusLogic_DataDirection_T DataDirection:2;
boolean TagEnable:1;
BusLogic_QueueTag_T QueueTag:2;
unsigned char CDB_Length;
unsigned char SenseDataLength;
BusLogic_ByteCount_T DataLength;
BusLogic_BusAddress_T DataPointer;
unsigned char :8;
unsigned char :8;
BusLogic_HostAdapterStatus_T HostAdapterStatus;
BusLogic_TargetDeviceStatus_T TargetDeviceStatus;
unsigned char TargetID;
unsigned char LogicalUnit:5;
boolean LegacyTagEnable:1;
BusLogic_QueueTag_T LegacyQueueTag:2;
SCSI_CDB_T CDB;
unsigned char :8;
unsigned char :8;
unsigned int :32;
BusLogic_BusAddress_T SenseDataPointer;
void (*CallbackFunction)(struct BusLogic_CCB *);
BusLogic_Base_Address_T BaseAddress;
BusLogic_CompletionCode_T CompletionCode;
#ifndef CONFIG_SCSI_OMIT_FLASHPOINT
unsigned char :8;
unsigned short OS_Flags;
unsigned char Private[48];
#endif
boolean AllocationGroupHead;
BusLogic_CCB_Status_T Status;
unsigned long SerialNumber;
SCSI_Command_T *Command;
struct BusLogic_HostAdapter *HostAdapter;
struct BusLogic_CCB *Next;
struct BusLogic_CCB *NextAll;
BusLogic_ScatterGatherSegment_T
ScatterGatherList[BusLogic_ScatterGatherLimit];
}
BusLogic_CCB_T;
typedef struct BusLogic_OutgoingMailbox
{
BusLogic_BusAddress_T CCB;
unsigned int :24;
BusLogic_ActionCode_T ActionCode;
}
BusLogic_OutgoingMailbox_T;
typedef struct BusLogic_IncomingMailbox
{
BusLogic_BusAddress_T CCB;
BusLogic_HostAdapterStatus_T HostAdapterStatus;
BusLogic_TargetDeviceStatus_T TargetDeviceStatus;
unsigned char :8;
BusLogic_CompletionCode_T CompletionCode;
}
BusLogic_IncomingMailbox_T;
typedef struct BusLogic_DriverOptions
{
unsigned short TaggedQueuingPermitted;
unsigned short TaggedQueuingPermittedMask;
unsigned short BusSettleTime;
BusLogic_LocalOptions_T LocalOptions;
unsigned char CommonQueueDepth;
unsigned char QueueDepth[BusLogic_MaxTargetDevices];
BusLogic_ErrorRecoveryStrategy_T
ErrorRecoveryStrategy[BusLogic_MaxTargetDevices];
}
BusLogic_DriverOptions_T;
typedef struct BusLogic_TargetFlags
{
boolean TargetExists:1;
boolean TaggedQueuingSupported:1;
boolean WideTransfersSupported:1;
boolean TaggedQueuingActive:1;
boolean WideTransfersActive:1;
boolean CommandSuccessfulFlag:1;
boolean TargetInfoReported:1;
}
BusLogic_TargetFlags_T;
#define BusLogic_SizeBuckets 10
typedef unsigned int BusLogic_CommandSizeBuckets_T[BusLogic_SizeBuckets];
typedef struct BusLogic_TargetStatistics
{
unsigned int CommandsAttempted;
unsigned int CommandsCompleted;
unsigned int ReadCommands;
unsigned int WriteCommands;
BusLogic_ByteCounter_T TotalBytesRead;
BusLogic_ByteCounter_T TotalBytesWritten;
BusLogic_CommandSizeBuckets_T ReadCommandSizeBuckets;
BusLogic_CommandSizeBuckets_T WriteCommandSizeBuckets;
unsigned short CommandAbortsRequested;
unsigned short CommandAbortsAttempted;
unsigned short CommandAbortsCompleted;
unsigned short BusDeviceResetsRequested;
unsigned short BusDeviceResetsAttempted;
unsigned short BusDeviceResetsCompleted;
unsigned short HostAdapterResetsRequested;
unsigned short HostAdapterResetsAttempted;
unsigned short HostAdapterResetsCompleted;
}
BusLogic_TargetStatistics_T;
#define FlashPoint_BadCardHandle 0xFFFFFFFF
typedef unsigned int FlashPoint_CardHandle_T;
typedef struct FlashPoint_Info
{
BusLogic_Base_Address_T BaseAddress;
boolean Present;
unsigned char IRQ_Channel;
unsigned char SCSI_ID;
unsigned char SCSI_LUN;
unsigned short FirmwareRevision;
unsigned short SynchronousPermitted;
unsigned short FastPermitted;
unsigned short UltraPermitted;
unsigned short DisconnectPermitted;
unsigned short WidePermitted;
boolean ParityCheckingEnabled:1;
boolean HostWideSCSI:1;
boolean HostSoftReset:1;
boolean ExtendedTranslationEnabled:1;
boolean LowByteTerminated:1;
boolean HighByteTerminated:1;
boolean ReportDataUnderrun:1;
boolean SCAM_Enabled:1;
boolean SCAM_Level2:1;
unsigned char :7;
unsigned char Family;
unsigned char BusType;
unsigned char ModelNumber[3];
unsigned char RelativeCardNumber;
unsigned char Reserved[4];
unsigned int OS_Reserved;
unsigned char TranslationInfo[4];
unsigned int Reserved2[5];
unsigned int SecondaryRange;
}
FlashPoint_Info_T;
typedef struct BusLogic_HostAdapter
{
SCSI_Host_T *SCSI_Host;
BusLogic_HostAdapterType_T HostAdapterType;
BusLogic_HostAdapterBusType_T HostAdapterBusType;
BusLogic_IO_Address_T IO_Address;
BusLogic_PCI_Address_T PCI_Address;
unsigned short AddressCount;
unsigned char HostNumber;
unsigned char ModelName[9];
unsigned char FirmwareVersion[6];
unsigned char FullModelName[18];
unsigned char Bus;
unsigned char Device;
unsigned char IRQ_Channel;
unsigned char DMA_Channel;
unsigned char SCSI_ID;
boolean IRQ_ChannelAcquired:1;
boolean DMA_ChannelAcquired:1;
boolean ExtendedTranslationEnabled:1;
boolean ParityCheckingEnabled:1;
boolean BusResetEnabled:1;
boolean LevelSensitiveInterrupt:1;
boolean HostWideSCSI:1;
boolean HostDifferentialSCSI:1;
boolean HostSupportsSCAM:1;
boolean HostUltraSCSI:1;
boolean ExtendedLUNSupport:1;
boolean TerminationInfoValid:1;
boolean LowByteTerminated:1;
boolean HighByteTerminated:1;
boolean BounceBuffersRequired:1;
boolean StrictRoundRobinModeSupport:1;
boolean SCAM_Enabled:1;
boolean SCAM_Level2:1;
boolean HostAdapterInitialized:1;
boolean HostAdapterExternalReset:1;
boolean HostAdapterInternalError:1;
boolean ProcessCompletedCCBsActive;
volatile boolean HostAdapterCommandCompleted;
unsigned short HostAdapterScatterGatherLimit;
unsigned short DriverScatterGatherLimit;
unsigned short MaxTargetDevices;
unsigned short MaxLogicalUnits;
unsigned short MailboxCount;
unsigned short InitialCCBs;
unsigned short IncrementalCCBs;
unsigned short AllocatedCCBs;
unsigned short DriverQueueDepth;
unsigned short HostAdapterQueueDepth;
unsigned short UntaggedQueueDepth;
unsigned short CommonQueueDepth;
unsigned short BusSettleTime;
unsigned short SynchronousPermitted;
unsigned short FastPermitted;
unsigned short UltraPermitted;
unsigned short WidePermitted;
unsigned short DisconnectPermitted;
unsigned short TaggedQueuingPermitted;
unsigned short ExternalHostAdapterResets;
unsigned short HostAdapterInternalErrors;
unsigned short TargetDeviceCount;
unsigned short MessageBufferLength;
BusLogic_BusAddress_T BIOS_Address;
BusLogic_DriverOptions_T *DriverOptions;
FlashPoint_Info_T FlashPointInfo;
FlashPoint_CardHandle_T CardHandle;
struct BusLogic_HostAdapter *Next;
BusLogic_CCB_T *All_CCBs;
BusLogic_CCB_T *Free_CCBs;
BusLogic_CCB_T *FirstCompletedCCB;
BusLogic_CCB_T *LastCompletedCCB;
BusLogic_CCB_T *BusDeviceResetPendingCCB[BusLogic_MaxTargetDevices];
BusLogic_ErrorRecoveryStrategy_T
ErrorRecoveryStrategy[BusLogic_MaxTargetDevices];
BusLogic_TargetFlags_T TargetFlags[BusLogic_MaxTargetDevices];
unsigned char QueueDepth[BusLogic_MaxTargetDevices];
unsigned char SynchronousPeriod[BusLogic_MaxTargetDevices];
unsigned char SynchronousOffset[BusLogic_MaxTargetDevices];
unsigned char ActiveCommands[BusLogic_MaxTargetDevices];
unsigned int CommandsSinceReset[BusLogic_MaxTargetDevices];
unsigned long LastSequencePoint[BusLogic_MaxTargetDevices];
unsigned long LastResetAttempted[BusLogic_MaxTargetDevices];
unsigned long LastResetCompleted[BusLogic_MaxTargetDevices];
BusLogic_OutgoingMailbox_T *FirstOutgoingMailbox;
BusLogic_OutgoingMailbox_T *LastOutgoingMailbox;
BusLogic_OutgoingMailbox_T *NextOutgoingMailbox;
BusLogic_IncomingMailbox_T *FirstIncomingMailbox;
BusLogic_IncomingMailbox_T *LastIncomingMailbox;
BusLogic_IncomingMailbox_T *NextIncomingMailbox;
BusLogic_TargetStatistics_T TargetStatistics[BusLogic_MaxTargetDevices];
unsigned char MailboxSpace[BusLogic_MaxMailboxes
* (sizeof(BusLogic_OutgoingMailbox_T)
+ sizeof(BusLogic_IncomingMailbox_T))];
char MessageBuffer[BusLogic_MessageBufferSize];
}
BusLogic_HostAdapter_T;
typedef struct BIOS_DiskParameters
{
int Heads;
int Sectors;
int Cylinders;
}
BIOS_DiskParameters_T;
typedef struct SCSI_Inquiry
{
unsigned char PeripheralDeviceType:5;
unsigned char PeripheralQualifier:3;
unsigned char DeviceTypeModifier:7;
boolean RMB:1;
unsigned char ANSI_ApprovedVersion:3;
unsigned char ECMA_Version:3;
unsigned char ISO_Version:2;
unsigned char ResponseDataFormat:4;
unsigned char :2;
boolean TrmIOP:1;
boolean AENC:1;
unsigned char AdditionalLength;
unsigned char :8;
unsigned char :8;
boolean SftRe:1;
boolean CmdQue:1;
boolean :1;
boolean Linked:1;
boolean Sync:1;
boolean WBus16:1;
boolean WBus32:1;
boolean RelAdr:1;
unsigned char VendorIdentification[8];
unsigned char ProductIdentification[16];
unsigned char ProductRevisionLevel[4];
}
SCSI_Inquiry_T;
static inline
void BusLogic_AcquireHostAdapterLock(BusLogic_HostAdapter_T *HostAdapter,
ProcessorFlags_T *ProcessorFlags)
{
save_flags(*ProcessorFlags);
cli();
}
static inline
void BusLogic_ReleaseHostAdapterLock(BusLogic_HostAdapter_T *HostAdapter,
ProcessorFlags_T *ProcessorFlags)
{
restore_flags(*ProcessorFlags);
}
static inline
void BusLogic_AcquireHostAdapterLockIH(BusLogic_HostAdapter_T *HostAdapter,
ProcessorFlags_T *ProcessorFlags)
{
}
static inline
void BusLogic_ReleaseHostAdapterLockIH(BusLogic_HostAdapter_T *HostAdapter,
ProcessorFlags_T *ProcessorFlags)
{
}
static inline
void BusLogic_SCSIBusReset(BusLogic_HostAdapter_T *HostAdapter)
{
BusLogic_ControlRegister_T ControlRegister;
ControlRegister.All = 0;
ControlRegister.Bits.SCSIBusReset = true;
outb(ControlRegister.All,
HostAdapter->IO_Address + BusLogic_ControlRegisterOffset);
}
static inline
void BusLogic_InterruptReset(BusLogic_HostAdapter_T *HostAdapter)
{
BusLogic_ControlRegister_T ControlRegister;
ControlRegister.All = 0;
ControlRegister.Bits.InterruptReset = true;
outb(ControlRegister.All,
HostAdapter->IO_Address + BusLogic_ControlRegisterOffset);
}
static inline
void BusLogic_SoftReset(BusLogic_HostAdapter_T *HostAdapter)
{
BusLogic_ControlRegister_T ControlRegister;
ControlRegister.All = 0;
ControlRegister.Bits.SoftReset = true;
outb(ControlRegister.All,
HostAdapter->IO_Address + BusLogic_ControlRegisterOffset);
}
static inline
void BusLogic_HardReset(BusLogic_HostAdapter_T *HostAdapter)
{
BusLogic_ControlRegister_T ControlRegister;
ControlRegister.All = 0;
ControlRegister.Bits.HardReset = true;
outb(ControlRegister.All,
HostAdapter->IO_Address + BusLogic_ControlRegisterOffset);
}
static inline
unsigned char BusLogic_ReadStatusRegister(BusLogic_HostAdapter_T *HostAdapter)
{
return inb(HostAdapter->IO_Address + BusLogic_StatusRegisterOffset);
}
static inline
void BusLogic_WriteCommandParameterRegister(BusLogic_HostAdapter_T
*HostAdapter,
unsigned char Value)
{
outb(Value,
HostAdapter->IO_Address + BusLogic_CommandParameterRegisterOffset);
}
static inline
unsigned char BusLogic_ReadDataInRegister(BusLogic_HostAdapter_T *HostAdapter)
{
return inb(HostAdapter->IO_Address + BusLogic_DataInRegisterOffset);
}
static inline
unsigned char BusLogic_ReadInterruptRegister(BusLogic_HostAdapter_T
*HostAdapter)
{
return inb(HostAdapter->IO_Address + BusLogic_InterruptRegisterOffset);
}
static inline
unsigned char BusLogic_ReadGeometryRegister(BusLogic_HostAdapter_T
*HostAdapter)
{
return inb(HostAdapter->IO_Address + BusLogic_GeometryRegisterOffset);
}
static inline
void BusLogic_StartMailboxCommand(BusLogic_HostAdapter_T *HostAdapter)
{
BusLogic_WriteCommandParameterRegister(HostAdapter,
BusLogic_ExecuteMailboxCommand);
}
static inline void BusLogic_Delay(int Seconds)
{
int Milliseconds = 1000 * Seconds;
unsigned long ProcessorFlags;
save_flags(ProcessorFlags);
sti();
while (--Milliseconds >= 0) udelay(1000);
restore_flags(ProcessorFlags);
}
static inline BusLogic_BusAddress_T Virtual_to_Bus(void *VirtualAddress)
{
return (BusLogic_BusAddress_T) virt_to_bus(VirtualAddress);
}
static inline void *Bus_to_Virtual(BusLogic_BusAddress_T BusAddress)
{
return (void *) bus_to_virt(BusAddress);
}
static inline
BusLogic_BusAddress_T Virtual_to_32Bit_Virtual(void *VirtualAddress)
{
return (BusLogic_BusAddress_T) (unsigned long) VirtualAddress;
}
static inline void BusLogic_IncrementErrorCounter(unsigned short *ErrorCounter)
{
if (*ErrorCounter < 65535) (*ErrorCounter)++;
}
static inline void BusLogic_IncrementByteCounter(BusLogic_ByteCounter_T
*ByteCounter,
unsigned int Amount)
{
ByteCounter->Units += Amount;
if (ByteCounter->Units > 999999999)
{
ByteCounter->Units -= 1000000000;
ByteCounter->Billions++;
}
}
static inline void BusLogic_IncrementSizeBucket(BusLogic_CommandSizeBuckets_T
CommandSizeBuckets,
unsigned int Amount)
{
int Index = 0;
if (Amount < 8*1024)
{
if (Amount < 2*1024)
Index = (Amount < 1*1024 ? 0 : 1);
else Index = (Amount < 4*1024 ? 2 : 3);
}
else if (Amount < 128*1024)
{
if (Amount < 32*1024)
Index = (Amount < 16*1024 ? 4 : 5);
else Index = (Amount < 64*1024 ? 6 : 7);
}
else Index = (Amount < 256*1024 ? 8 : 9);
CommandSizeBuckets[Index]++;
}
#define FlashPoint_FirmwareVersion "5.02"
#define FlashPoint_NormalInterrupt 0x00
#define FlashPoint_InternalError 0xFE
#define FlashPoint_ExternalBusReset 0xFF
static void BusLogic_QueueCompletedCCB(BusLogic_CCB_T *);
static void BusLogic_InterruptHandler(int, void *, Registers_T *);
static int BusLogic_ResetHostAdapter(BusLogic_HostAdapter_T *,
SCSI_Command_T *, unsigned int);
static void BusLogic_Message(BusLogic_MessageLevel_T, char *,
BusLogic_HostAdapter_T *, ...);
static void BusLogic_ParseDriverOptions(char *);
#endif