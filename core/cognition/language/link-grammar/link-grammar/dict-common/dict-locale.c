#include <string.h>
#include "dict-api.h"
#include "dict-common.h"
#include "dict-defines.h"
#include "dict-locale.h"
#include "string-id.h"
#include "string-set.h"
#ifdef __MINGW32__
int callGetLocaleInfoEx(LPCWSTR, LCTYPE, LPWSTR, int);
#endif
#if _WINVER == 0x501
int callGetLocaleInfoEx(LPCWSTR lpLocaleName, LCTYPE LCType, LPWSTR lpLCData, int cchData)
{
int rc = -1;
int (WINAPI * pfnGetLocaleInfoEx)(LPCWSTR, LCTYPE, LPWSTR, int);
*(FARPROC*)&pfnGetLocaleInfoEx = GetProcAddress(GetModuleHandleA("Kernel32"), "GetLocaleInfoEx");
if (pfnGetLocaleInfoEx)
{
rc = pfnGetLocaleInfoEx(lpLocaleName, LCType, lpLCData, cchData);
}
else
{
HMODULE module = LoadLibraryA("Mlang");
HRESULT (WINAPI * pfnRfc1766ToLcidW)(LCID*, LPCWSTR);
*(FARPROC*)&pfnRfc1766ToLcidW = GetProcAddress(module, "Rfc1766ToLcidW");
if (pfnRfc1766ToLcidW)
{
LCID lcid;
if (SUCCEEDED(pfnRfc1766ToLcidW(&lcid, lpLocaleName)))
{
rc = GetLocaleInfoW(lcid, LCType, lpLCData, cchData);
}
}
FreeLibrary(module);
}
return rc;
}
#else
#define callGetLocaleInfoEx GetLocaleInfoEx
#endif
const char *linkgrammar_get_dict_define(Dictionary dict, const char *name)
{
unsigned int id = string_id_lookup(name, dict->dfine.set);
if (id == 0) return NULL;
return dict->dfine.value[id - 1];
}
static const char * format_locale(Dictionary dict,
const char *ll, const char *cc)
{
unsigned char *locale_ll = (unsigned char *)strdupa(ll);
unsigned char *locale_cc = (unsigned char *)strdupa(cc);
for (unsigned char *p = locale_ll; '\0' != *p; p++) *p = tolower(*p);
for (unsigned char *p = locale_cc; '\0' != *p; p++) *p = toupper(*p);
#ifdef _WIN32
const int locale_size = strlen(ll) + 1 + strlen(cc) + 1;
char *locale = alloca(locale_size);
snprintf(locale, locale_size, "%s-%s", locale_ll, locale_cc);
wchar_t wlocale[LOCALE_NAME_MAX_LENGTH];
wchar_t wtmpbuf[LOCALE_NAME_MAX_LENGTH];
char tmpbuf[LOCALE_NAME_MAX_LENGTH];
char locale_buf[LOCALE_NAME_MAX_LENGTH];
size_t r;
r = mbstowcs(wlocale, locale, LOCALE_NAME_MAX_LENGTH);
if ((size_t)-1 == r)
{
prt_error("Error: Error converting %s to wide character.\n", locale);
return NULL;
}
wlocale[LOCALE_NAME_MAX_LENGTH-1] = L'\0';
if (0 >= callGetLocaleInfoEx(wlocale, LOCALE_SENGLISHLANGUAGENAME,
wtmpbuf, LOCALE_NAME_MAX_LENGTH))
{
prt_error("Error: GetLocaleInfoEx LOCALE_SENGLISHLANGUAGENAME Locale=%s: \n"
"Error %d", locale, (int)GetLastError());
return NULL;
}
r = wcstombs(tmpbuf, wtmpbuf, LOCALE_NAME_MAX_LENGTH);
if ((size_t)-1 == r)
{
prt_error("Error: Error converting locale language from wide character.\n");
return NULL;
}
tmpbuf[LOCALE_NAME_MAX_LENGTH-1] = '\0';
if (0 == strncmp(tmpbuf, "Unknown", 7))
{
prt_error("Error: Unknown territory code in locale \"%s\"\n", locale);
return NULL;
}
strcpy(locale_buf, tmpbuf);
strcat(locale_buf, "_");
if (0 >= callGetLocaleInfoEx(wlocale, LOCALE_SENGLISHCOUNTRYNAME,
wtmpbuf, LOCALE_NAME_MAX_LENGTH))
{
prt_error("Error: GetLocaleInfoEx LOCALE_SENGLISHCOUNTRYNAME Locale=%s: \n"
"Error %d", locale, (int)GetLastError());
return NULL;
}
r = wcstombs(tmpbuf, wtmpbuf, LOCALE_NAME_MAX_LENGTH);
if ((size_t)-1 == r)
{
prt_error("Error: Error converting locale territory from wide character.\n");
return NULL;
}
tmpbuf[LOCALE_NAME_MAX_LENGTH-1] = '\0';
if (0 == strncmp(tmpbuf, "Unknown", 7))
{
prt_error("Error: Unknown territory code in locale \"%s\"\n", locale);
return NULL;
}
locale = strcat(locale_buf, tmpbuf);
#else
const int locale_size = strlen(ll) + 1 + strlen(cc) + sizeof(".UTF-8");
char *locale = alloca(locale_size);
snprintf(locale, locale_size, "%s_%s.UTF-8", locale_ll, locale_cc);
#endif
return string_set_add(locale, dict->string_set);
}
const char * linkgrammar_get_dict_locale(Dictionary dict)
{
if (dict->locale) return dict->locale;
Dict_node *dn = NULL;
const char *locale =
linkgrammar_get_dict_define(dict, LG_DICTIONARY_LOCALE);
if (NULL == locale)
{
dn = dict->lookup_list(dict, "<"LG_DICTIONARY_LOCALE">");
if (NULL == dn)
{
lgdebug(D_USER_FILES, "Debug: Dictionary '%s': Locale is not defined.\n",
dict->name);
goto locale_error;
}
else
{
locale = dn->exp->condesc->more->string;
}
}
if (0 == strcmp(locale, "C"))
{
locale = string_set_add("C", dict->string_set);
}
else
{
char locale_ll[4], locale_cc[3], c;
if (NULL == dn)
{
int locale_numelement = sscanf(locale, "%3[a-z]_%2[A-Z].UTF-8%c",
locale_ll, locale_cc, &c);
if (2 != locale_numelement)
{
prt_error("Error: "LG_DICTIONARY_LOCALE": \"%s\" "
"should be in the form ll_CC.UTF-8\n"
"\t(ll: language code; CC: territory code) "
"or \"C\" for transliterated dictionaries.\n",
locale);
goto locale_error;
}
}
else
{
int locale_numelement = sscanf(locale, "%3[A-Z]4%2[a-z]%c",
locale_ll, locale_cc, &c);
if (2 != locale_numelement)
{
prt_error("Error: <"LG_DICTIONARY_LOCALE">: \"%s\" "
"should be in the form LL4cc+\n"
"\t(LL: language code; cc: territory code) "
"or \"C\" for transliterated dictionaries.\n",
locale);
goto locale_error;
}
}
locale = format_locale(dict, locale_ll, locale_cc);
if (!try_locale(locale))
{
prt_error("Debug: Dictionary \"%s\": Locale \"%s\" unknown\n",
dict->name, locale);
goto locale_error;
}
}
if (NULL != dn) dict->free_lookup(dict, dn);
lgdebug(D_USER_FILES, "Debug: Dictionary locale: \"%s\"\n", locale);
dict->locale = locale;
return locale;
locale_error:
{
dict->free_lookup(dict, dn);
locale = get_default_locale();
if (NULL == locale) return NULL;
const char *sslocale = string_set_add(locale, dict->string_set);
free((void *)locale);
prt_error("Info: Dictionary '%s': No locale definition - "
"\"%s\" will be used.\n", dict->name, sslocale);
if (!try_locale(sslocale))
{
lgdebug(D_USER_FILES, "Debug: Unknown locale \"%s\"...\n", sslocale);
return NULL;
}
return sslocale;
}
}
const char * linkgrammar_get_version(void)
{
const char *s = "link-grammar-" LINK_VERSION_STRING;
return s;
}
const char * linkgrammar_get_dict_version(Dictionary dict)
{
if (dict->version) return dict->version;
const char *version =
linkgrammar_get_dict_define(dict, LG_DICTIONARY_VERSION_NUMBER);
if (NULL != version)
{
dict->version = version;
return dict->version;
}
char * ver;
char * p;
Dict_node *dn;
Exp *e;
dn = dict->lookup_list(dict, "<"LG_DICTIONARY_VERSION_NUMBER">");
if (NULL == dn) return "[unknown]";
e = dn->exp;
ver = strdup(&e->condesc->more->string[1]);
p = strchr(ver, 'v');
while (p)
{
*p = '.';
p = strchr(p+1, 'v');
}
dict->free_lookup(dict, dn);
dict->version = string_set_add(ver, dict->string_set);
free(ver);
return dict->version;
}
float linkgrammar_get_dict_max_disjunct_cost(Dictionary dict)
{
return dict->default_max_disjunct_cost;
}
void dictionary_setup_locale(Dictionary dict)
{
dict->locale = linkgrammar_get_dict_locale(dict);
set_utf8_program_locale();
if (NULL == dict->locale)
{
dict->locale = setlocale(LC_CTYPE, NULL);
prt_error("Warning: Couldn't set dictionary locale! "
"Using current program locale \"%s\"\n", dict->locale);
}
dict->locale = string_set_add(dict->locale, dict->string_set);
#ifdef HAVE_LOCALE_T
dict->lctype = newlocale_LC_CTYPE(dict->locale);
assert((locale_t) 0 != dict->lctype, "Dictionary locale is not set.");
#else
dict->lctype = 0;
#endif
dict->locale = string_set_add(dict->locale, dict->string_set);
}
static bool dictionary_setup_max_disjunct_cost(Dictionary dict)
{
const char *valstr = linkgrammar_get_dict_define(dict, LG_DISJUNCT_COST);
if (NULL == valstr)
{
dict->default_max_disjunct_cost = DEFAULT_MAX_DISJUNCT_COST;
return true;
}
float value;
if (!strtofC(valstr, &value))
{
prt_error("Error: %s: Invalid cost \"%s\"\n",
LG_DISJUNCT_COST, valstr);
return false;
}
dict->default_max_disjunct_cost = value;
return true;
}
bool dictionary_setup_defines(Dictionary dict)
{
dict->left_wall_defined  = dict_has_word(dict, LEFT_WALL_WORD);
dict->right_wall_defined = dict_has_word(dict, RIGHT_WALL_WORD);
dict->unknown_word_defined = dict_has_word(dict, UNKNOWN_WORD);
dict->use_unknown_word = true;
if (!dict->unknown_word_defined &&
dict_has_word(dict, "UNKNOWN-WORD"))
{
prt_error("Warning: Old name \"UNKNOWN-WORD\" is defined in the "
"dictionary. Please use \"<UNKNOWN-WORD>\" instead.\n");
}
dict->shuffle_linkages = false;
dict->zzz_connector = linkgrammar_get_dict_define(dict, EMPTY_CONNECTOR);
if (NULL != dict->zzz_connector)
dict->zzz_connector = string_set_add(dict->zzz_connector, dict->string_set);
dictionary_setup_locale(dict);
dict->disable_downcasing = false;
const char * ddn =
linkgrammar_get_dict_define(dict, LG_DISABLE_DOWNCASING);
if (NULL != ddn && 0 != strcmp(ddn, "false") && 0 != strcmp(ddn, "0"))
dict->disable_downcasing = true;
dict->default_max_disjuncts = 0;
const char *mdstr = linkgrammar_get_dict_define(dict, LG_MAX_DISJUNCTS);
if (mdstr)
dict->default_max_disjuncts = atoi(mdstr);
if (!dictionary_setup_max_disjunct_cost(dict)) return false;
return true;
}