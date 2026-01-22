#ifndef iscannum_INCLUDED
#  define iscannum_INCLUDED
int scan_number(const byte * sp, const byte * end, int sign, ref * pref,
const byte ** psp, const bool PDFScanRules);
#endif