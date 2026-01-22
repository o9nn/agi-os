enum {
BOOTCS = 0,
DRAM1 = 1,
DRAM2 = 2,
};
enum {
PanelI2C = 0x21<<1,
DisableVGA = ~0xFD,
DisableTFT = ~0xFB,
DisableSPIBus = ~0xF7,
DisablePanelVCC5 = ~0xEF,
DisablePanelVCC3 = ~0xDF,
DisableMonoPanel = ~0xBF,
DisableSPISelect = ~0x7F,
ContrastI2C = 0x2E<<1,
LEDRegI2C = 0x20<<1,
DisableGreenLED = ~0xFE,
DisableYellowLED = ~0xFD,
DisableRedLED = ~0xFB,
EnableLCD = IBIT(23),
};