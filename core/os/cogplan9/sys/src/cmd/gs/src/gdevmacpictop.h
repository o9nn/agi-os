#ifndef gdevmacpictop_INCLUDED
# define gdevmacpictop_INCLUDED
#include <QDOffscreen.h>
#define PICTWriteByte(ptr, data) *((unsigned char*) (ptr))++ = data;
#define PICTWriteInt(ptr, data) *((short*) (ptr))++ = data;
#define PICTWriteLong(ptr, data) *((long*) (ptr))++ = data;
#define PICTWriteFillByte(ptr) PICTWriteByte(ptr, 0);
#define PICTWriteOpcode(ptr, op) PICTWriteInt(ptr, op);
#define PICTWritePoint(ptr, h, v) \
{ \
PICTWriteInt(ptr, v); \
PICTWriteInt(ptr, h); \
}
#define PICTWriteRect(ptr, x, y, w, h) \
{ \
PICTWritePoint(ptr, x, y); \
PICTWritePoint(ptr, x+w, y+h); \
}
#define PICTWriteRegionRectangular(ptr, x, y, w, h) \
{ \
PICTWriteInt(ptr, 10); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICTWriteRegion(ptr, x, y, b, h, size, dataptr) \
{ \
PICTWriteInt(ptr, 10+size); \
PICTWriteRect(ptr, x, y, w, h); \
memcpy(ptr, dataptr, size); \
((char*)(ptr)) += size; \
}
#define PICTWritePattern(ptr, byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8) \
{ \
PICTWriteByte(ptr, byte1); \
PICTWriteByte(ptr, byte2); \
PICTWriteByte(ptr, byte3); \
PICTWriteByte(ptr, byte4); \
PICTWriteByte(ptr, byte5); \
PICTWriteByte(ptr, byte6); \
PICTWriteByte(ptr, byte7); \
PICTWriteByte(ptr, byte8); \
}
#define PICTWriteRGBColor(ptr, r, g, b) \
{ \
PICTWriteInt(ptr, r); \
PICTWriteInt(ptr, g); \
PICTWriteInt(ptr, b); \
}
#define PICTWriteColorSpec(ptr, value, r, g, b) \
{ \
PICTWriteInt(ptr, value); \
PICTWriteRGBColor(ptr, r, g, b); \
}
#define PICTWriteColorTable(ptr, seed, numEntries, cspecarr) \
{ \
int i; \
PICTWriteLong(ptr, seed); \
PICTWriteInt(ptr, 0); \
PICTWriteInt(ptr, numEntries-1); \
for (i=0; i<numEntries; i++) \
PICTWriteColorSpec(ptr, cspecarr[i].value, \
cspecarr[i].rgb.red, \
cspecarr[i].rgb.green, \
cspecarr[i].rgb.blue); \
}
#define PICTWritePixMap(ptr, x, y, w, h, rowBytes, \
packType, packSize, \
hRes, vRes, pixelSize) \
{ \
PICTWriteInt(ptr, 0x8000+rowBytes); \
PICTWriteRect(ptr, x, y, w, h); \
PICTWriteInt(ptr, 0); \
PICTWriteInt(ptr, packType); \
PICTWriteLong(ptr, (packType ? packSize : 0)); \
PICTWriteLong(ptr, hRes); \
PICTWriteLong(ptr, vRes); \
if (pixelSize < 16) { \
PICTWriteInt(ptr, 0); \
PICTWriteInt(ptr, pixelSize); \
PICTWriteInt(ptr, 1); \
PICTWriteInt(ptr, pixelSize); \
} else { \
PICTWriteInt(ptr, RGBDirect); \
PICTWriteInt(ptr, pixelSize); \
PICTWriteInt(ptr, 3); \
PICTWriteInt(ptr, (pixelSize==16 ? 5 : 8)); \
} \
PICTWriteLong(ptr, 0); \
PICTWriteLong(ptr, 0); \
PICTWriteLong(ptr, 0); \
}
#define PICTWriteDataPackBits(ptr, base, rowBytes, lines) \
{ \
short byteCount; \
if (raster < 8) { \
byteCount = rowBytes * lines; \
memcpy(ptr, base, byteCount); \
(char*)(ptr) += byteCount; \
} else { \
Ptr destBufBegin = (Ptr) malloc(raster + (raster+126)/127), destBuf, \
srcBuf = (Ptr) base; \
short i, len; \
\
byteCount = 0; \
for (i=0; i<lines; i++) { \
destBuf = destBufBegin; \
PackBits(&srcBuf, &destBuf, rowBytes); \
len = destBuf - destBufBegin; \
if (rowBytes > 250) { \
PICTWriteInt(ptr, len); \
byteCount += 2; \
} else { \
PICTWriteByte(ptr, len); \
byteCount++; \
} \
\
memcpy(ptr, destBufBegin, len); \
(char*)(ptr) += len; \
byteCount += len; \
} \
free(destBufBegin); \
} \
\
if (byteCount % 2) \
PICTWriteFillByte(ptr); \
}
#define PICTWriteText(ptr, textptr ) \
{ \
memcpy(ptr, textptr, textptr[0]+1); \
(char*)(ptr) += textptr[0]+1; \
}
#define PICT_NOP(ptr) \
{ \
PICTWriteOpcode(ptr, 0x0000); \
}
#define PICT_Clip_Rectangular(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0001); \
PICTWriteRegionRectangular(ptr, x, y, w, h); \
}
#define PICT_Clip(ptr, x, y, w, h, size, dataptr) \
{ \
PICTWriteOpcode(ptr, 0x0001); \
PICTWriteRegion(ptr, x, y, w, h, size, dataptr); \
}
#define PICT_BkPat(ptr, byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8) \
{ \
PICTWriteOpcode(ptr, 0x0002); \
PICTWritePattern(ptr, byte1, byte2, byte3, byte4, \
byte5, byte6, byte7, byte8); \
}
#define PICT_TxFont(ptr, font) \
{ \
PICTWriteOpcode(ptr, 0x0003); \
PICTWriteInt(ptr, font); \
}
#define PICT_TxFace(ptr, style) \
{ \
PICTWriteOpcode(ptr, 0x0004); \
PICTWriteByte(ptr, style); \
PICTWriteFillByte(ptr); \
}
#define PICT_TxMode(ptr, mode) \
{ \
PICTWriteOpcode(ptr, 0x0005); \
PICTWriteInt(ptr, mode); \
}
#define PICT_PnSize(ptr, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0006); \
PICTWritePoint(w, h); \
}
#define PICT_PnMode(ptr, mode) \
{ \
PICTWriteOpcode(ptr, 0x0007); \
PICTWriteInt(ptr, mode); \
}
#define PICT_PnPat(ptr, byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8) \
{ \
PICTWriteOpcode(ptr, 0x0009); \
PICTWritePattern(ptr, byte1, byte2, byte3, byte4, \
byte5, byte6, byte7, byte8); \
}
#define PICT_FillPat(ptr, byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8) \
{ \
PICTWriteOpcode(ptr, 0x000A); \
PICTWritePattern(ptr, byte1, byte2, byte3, byte4, \
byte5, byte6, byte7, byte8); \
}
#define PICT_OvSize(ptr, w, h) \
{ \
PICTWriteOpcode(ptr, 0x000B); \
PICTWritePoint(w, h); \
}
#define PICT_Origin(ptr, dh, dv) \
{ \
PICTWriteOpcode(ptr, 0x000C); \
PICTWriteInt(ptr, dh); \
PICTWriteInt(ptr, dv); \
}
#define PICT_TxSize(ptr, size) \
{ \
PICTWriteOpcode(ptr, 0x000D); \
PICTWriteInt(ptr, size); \
}
#define PICT_FgColor(ptr, color) \
{ \
PICTWriteOpcode(ptr, 0x000E); \
PICTWriteLong(ptr, color); \
}
#define PICT_BkColor(ptr, color) \
{ \
PICTWriteOpcode(ptr, 0x000F); \
PICTWriteLong(ptr, color); \
}
#define PICT_TxRatio(ptr, num, denom) \
{ \
PICTWriteOpcode(ptr, 0x0010); \
PICTWritePoint(ptr, num); \
PICTWritePoint(ptr, denom); \
}
#define PICT_VersionOp(ptr, version) \
{ \
PICTWriteOpcode(ptr, 0x0011); \
PICTWriteByte(ptr, version); \
PICTWriteFillByte(ptr); \
}
#define PICT_RGBFgCol(ptr, r, g, b) \
{ \
PICTWriteOpcode(ptr, 0x001A); \
PICTWriteRGBColor(ptr, r, g, b); \
}
#define PICT_RGBBkCol(ptr, r, g, b) \
{ \
PICTWriteOpcode(ptr, 0x001B); \
PICTWriteRGBColor(ptr, r, g, b); \
}
#define PICT_HiliteMode(ptr) \
{ \
PICTWriteOpcode(ptr, 0x001C); \
}
#define PICT_HiliteColor(ptr, r, g, b) \
{ \
PICTWriteOpcode(ptr, 0x001D); \
PICTWriteRGBColor(ptr, r, g, b); \
}
#define PICT_DefHilite(ptr) \
{ \
PICTWriteOpcode(ptr, 0x001E); \
}
#define PICT_OpColor(ptr, r, g, b) \
{ \
PICTWriteOpcode(ptr, 0x001F); \
PICTWriteRGBColor(ptr, r, g, b); \
}
#define PICT_Line(ptr, x0, y0, x1, y1) \
{ \
PICTWriteOpcode(ptr, 0x0020); \
PICTWritePoint(ptr, x0, y0); \
PICTWritePoint(ptr, x1, y1); \
}
#define PICT_LineFrom(ptr, x, y) \
{ \
PICTWriteOpcode(ptr, 0x0021); \
PICTWritePoint(ptr, x, y); \
}
#define PICT_ShortLine(ptr, x, y, dh, dv) \
{ \
PICTWriteOpcode(ptr, 0x0022); \
PICTWritePoint(ptr, x, y); \
PICTWriteByte(ptr, dh); \
PICTWriteByte(ptr, dv); \
}
#define PICT_ShortLineFrom(ptr, dh, dv) \
{ \
PICTWriteOpcode(ptr, 0x0023); \
PICTWriteByte(ptr, dh); \
PICTWriteByte(ptr, dv); \
}
#define PICT_LongText(ptr, x, y, textptr ) \
{ \
PICTWriteOpcode(ptr, 0x0028); \
PICTWritePoint(ptr, x, y); \
PICTWriteText(ptr, textptr); \
if ((textptr[0]+1) % 2) PICTWriteFillByte(ptr); \
}
#define PICT_DHText(ptr, dh, textptr ) \
{ \
PICTWriteOpcode(ptr, 0x0029); \
PICTWriteByte(ptr, dh); \
PICTWriteText(ptr, textptr); \
if (textptr[0] % 2) PICTWriteFillByte(ptr); \
}
#define PICT_DVText(ptr, dv, textptr ) \
{ \
PICTWriteOpcode(ptr, 0x002A); \
PICTWriteByte(ptr, dv); \
PICTWriteText(ptr, textptr); \
if (textptr[0] % 2) PICTWriteFillByte(ptr); \
}
#define PICT_DHDVText(ptr, dh, dv, textptr ) \
{ \
PICTWriteOpcode(ptr, 0x002B); \
PICTWriteByte(ptr, dh); \
PICTWriteByte(ptr, dv); \
PICTWriteText(ptr, textptr); \
if ((textptr[0]+1) % 2) PICTWriteFillByte(ptr); \
}
#define PICT_fontName(ptr, id, nameptr ) \
{ \
PICTWriteOpcode(ptr, 0x002C); \
PICTWriteInt(ptr, nameptr[0]+1+2); \
PICTWriteInt(ptr, id); \
PICTWriteText(ptr, nameptr); \
if ((nameptr[0]+1) % 2) PICTWriteFillByte(ptr); \
}
#define PICT_frameRect(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0030); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_paintRect(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0031); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_eraseRect(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0032); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_invertRect(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0033); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_fillRect(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0034); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_frameSameRect(ptr) \
{ \
PICTWriteOpcode(ptr, 0x0038); \
}
#define PICT_paintSameRect(ptr) \
{ \
PICTWriteOpcode(ptr, 0x0039); \
}
#define PICT_eraseSameRect(ptr) \
{ \
PICTWriteOpcode(ptr, 0x003A); \
}
#define PICT_invertSameRect(ptr) \
{ \
PICTWriteOpcode(ptr, 0x003B); \
}
#define PICT_fillSameRect(ptr) \
{ \
PICTWriteOpcode(ptr, 0x003C); \
}
#define PICT_frameRRect(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0040); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_paintRRect(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0041); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_eraseRRect(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0042); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_invertRRect(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0043); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_fillRRect(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0044); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_frameSameRRect(ptr) \
{ \
PICTWriteOpcode(ptr, 0x0048); \
}
#define PICT_paintSameRRect(ptr) \
{ \
PICTWriteOpcode(ptr, 0x0049); \
}
#define PICT_eraseSameRRect(ptr) \
{ \
PICTWriteOpcode(ptr, 0x004A); \
}
#define PICT_invertSameRRect(ptr) \
{ \
PICTWriteOpcode(ptr, 0x004B); \
}
#define PICT_fillSameRRect(ptr) \
{ \
PICTWriteOpcode(ptr, 0x004C); \
}
#define PICT_frameOval(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0050); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_paintOval(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0051); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_eraseOval(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0052); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_invertOval(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0053); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_fillOval(ptr, x, y, w, h) \
{ \
PICTWriteOpcode(ptr, 0x0054); \
PICTWriteRect(ptr, x, y, w, h); \
}
#define PICT_frameSameOval(ptr) \
{ \
PICTWriteOpcode(ptr, 0x0058); \
}
#define PICT_paintSameOval(ptr) \
{ \
PICTWriteOpcode(ptr, 0x0059); \
}
#define PICT_eraseSameOval(ptr) \
{ \
PICTWriteOpcode(ptr, 0x005A); \
}
#define PICT_invertSameOval(ptr) \
{ \
PICTWriteOpcode(ptr, 0x005B); \
}
#define PICT_fillSameOval(ptr) \
{ \
PICTWriteOpcode(ptr, 0x005C); \
}
#define PICT_frameArc(ptr, x, y, w, h, startAngle, arcAngle) \
{ \
PICTWriteOpcode(ptr, 0x0060); \
PICTWriteRect(ptr, x, y, w, h); \
PICTWriteInt(ptr, startAngle); \
PICTWriteInt(ptr, arcAngle); \
}
#define PICT_paintArc(ptr, x, y, w, h, startAngle, arcAngle) \
{ \
PICTWriteOpcode(ptr, 0x0061); \
PICTWriteRect(ptr, x, y, w, h); \
PICTWriteInt(ptr, startAngle); \
PICTWriteInt(ptr, arcAngle); \
}
#define PICT_eraseArc(ptr, x, y, w, h, startAngle, arcAngle) \
{ \
PICTWriteOpcode(ptr, 0x0062); \
PICTWriteRect(ptr, x, y, w, h); \
PICTWriteInt(ptr, startAngle); \
PICTWriteInt(ptr, arcAngle); \
}
#define PICT_invertArc(ptr, x, y, w, h, startAngle, arcAngle) \
{ \
PICTWriteOpcode(ptr, 0x0063); \
PICTWriteRect(ptr, x, y, w, h); \
PICTWriteInt(ptr, startAngle); \
PICTWriteInt(ptr, arcAngle); \
}
#define PICT_fillArc(ptr, x, y, w, h, startAngle, arcAngle) \
{ \
PICTWriteOpcode(ptr, 0x0064); \
PICTWriteRect(ptr, x, y, w, h); \
PICTWriteInt(ptr, startAngle); \
PICTWriteInt(ptr, arcAngle); \
}
#define PICT_BitsRect_BitMap(ptr, x0, y0, w0, h0, x1, y1, w1, h1, rowBytes, mode, dataPtr) \
{ \
PICTWriteOpcode(ptr, 0x0090); \
PICTWriteInt(ptr, rowBytes); \
PICTWriteRect(ptr, x1, y1, w1, h1); \
PICTWriteRect(ptr, x0, y0, w0, h0); \
PICTWriteRect(ptr, x1, y1, w1, h1); \
PICTWriteInt(ptr, mode); \
memcpy(ptr, dataPtr, h0*rowBytes); \
}
#define PICT_PackBitsRect_BitMap(ptr, x0, y0, w0, h0, x1, y1, w1, h1, rowBytes, mode, \
dataPtr, size) \
{ \
PICTWriteOpcode(ptr, 0x0098); \
PICTWriteInt(ptr, rowBytes); \
PICTWriteRect(ptr, x1, y1, w1, h1); \
PICTWriteRect(ptr, x0, y0, w0, h0); \
PICTWriteRect(ptr, x1, y1, w1, h1); \
PICTWriteInt(ptr, mode); \
memcpy(ptr, dataPtr, size); \
}
#define PICT_OpEndPic(ptr) \
{ \
PICTWriteOpcode(ptr, 0x00FF); \
}
#define PICT_OpEndPicGoOn(ptr) \
{ \
*(ptr) = 0x00FF; \
}
#define GSSetStdCol(ptr) \
{ \
PICT_RGBFgCol(ptr, 0x0000, 0x0000, 0x0000); \
PICT_RGBBkCol(ptr, 0xFFFF, 0xFFFF, 0xFFFF); \
}
#define GSSetFgCol(dev, ptr, col) \
{ \
gx_color_value rgb[3]; \
(*dev_proc(dev, map_color_rgb))(dev, col, rgb); \
PICT_RGBFgCol(ptr, rgb[0], rgb[1], rgb[2]); \
}
#define GSSetBkCol(dev, ptr, col) \
{ \
gx_color_value rgb[3]; \
(*dev_proc(dev, map_color_rgb))(dev, col, rgb); \
PICT_RGBBkCol(ptr, rgb[0], rgb[1], rgb[2]); \
}
#endif