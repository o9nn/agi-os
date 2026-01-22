#ifndef SPIRVREMAPPER_H
#define SPIRVREMAPPER_H
#include <string>
#include <vector>
#include <cstdlib>
#include <exception>
#ifdef GLSLANG_IS_SHARED_LIBRARY
#ifdef _WIN32
#ifdef GLSLANG_EXPORTING
#define GLSLANG_EXPORT __declspec(dllexport)
#else
#define GLSLANG_EXPORT __declspec(dllimport)
#endif
#elif __GNUC__ >= 4
#define GLSLANG_EXPORT __attribute__((visibility("default")))
#endif
#endif
#ifndef GLSLANG_EXPORT
#define GLSLANG_EXPORT
#endif
namespace spv {
class spirvbin_base_t
{
public:
enum Options {
NONE          = 0,
STRIP         = (1<<0),
MAP_TYPES     = (1<<1),
MAP_NAMES     = (1<<2),
MAP_FUNCS     = (1<<3),
DCE_FUNCS     = (1<<4),
DCE_VARS      = (1<<5),
DCE_TYPES     = (1<<6),
OPT_LOADSTORE = (1<<7),
OPT_FWD_LS    = (1<<8),
MAP_ALL       = (MAP_TYPES | MAP_NAMES | MAP_FUNCS),
DCE_ALL       = (DCE_FUNCS | DCE_VARS | DCE_TYPES),
OPT_ALL       = (OPT_LOADSTORE),
ALL_BUT_STRIP = (MAP_ALL | DCE_ALL | OPT_ALL),
DO_EVERYTHING = (STRIP | ALL_BUT_STRIP)
};
};
}
#include <functional>
#include <cstdint>
#include <unordered_map>
#include <unordered_set>
#include <map>
#include <set>
#include <cassert>
#include "spirv.hpp"
namespace spv {
static inline constexpr Id NoResult = 0;
class GLSLANG_EXPORT spirvbin_t : public spirvbin_base_t
{
public:
spirvbin_t(int verbose = 0) : entryPoint(spv::NoResult), largestNewId(0), verbose(verbose), errorLatch(false)
{ }
virtual ~spirvbin_t() { }
void remap(std::vector<std::uint32_t>& spv, const std::vector<std::string>& whiteListStrings,
std::uint32_t opts = DO_EVERYTHING);
void remap(std::vector<std::uint32_t>& spv, std::uint32_t opts = DO_EVERYTHING);
typedef std::function<void(const std::string&)> errorfn_t;
typedef std::function<void(const std::string&)> logfn_t;
static void registerErrorHandler(errorfn_t handler) { errorHandler = handler; }
static void registerLogHandler(logfn_t handler)     { logHandler   = handler; }
protected:
virtual void msg(int minVerbosity, int indent, const std::string& txt) const;
private:
typedef std::unordered_map<spv::Id, spv::Id> idmap_t;
typedef std::unordered_set<spv::Id>          idset_t;
typedef std::unordered_map<spv::Id, int>     blockmap_t;
void remap(std::uint32_t opts = DO_EVERYTHING);
typedef std::unordered_map<std::string, spv::Id> namemap_t;
typedef std::uint32_t spirword_t;
typedef std::pair<unsigned, unsigned> range_t;
typedef std::function<void(spv::Id&)>                idfn_t;
typedef std::function<bool(spv::Op, unsigned start)> instfn_t;
static const spv::Id unmapped;
static const spv::Id unused;
static const int     header_size;
class id_iterator_t;
typedef std::vector<spirword_t>        typeentry_t;
typedef std::map<spv::Id, typeentry_t> globaltypes_t;
typedef std::set<int>                    posmap_t;
typedef std::unordered_map<spv::Id, int> posmap_rev_t;
typedef std::unordered_map<spv::Id, unsigned> typesize_map_t;
void error(const std::string& txt) const { errorLatch = true; errorHandler(txt); }
bool     isConstOp(spv::Op opCode)      const;
bool     isTypeOp(spv::Op opCode)       const;
bool     isStripOp(spv::Op opCode)      const;
bool     isFlowCtrl(spv::Op opCode)     const;
range_t  literalRange(spv::Op opCode)   const;
range_t  typeRange(spv::Op opCode)      const;
range_t  constRange(spv::Op opCode)     const;
unsigned typeSizeInWords(spv::Id id)    const;
unsigned idTypeSizeInWords(spv::Id id)  const;
bool isStripOp(spv::Op opCode, unsigned start) const;
spv::Id&        asId(unsigned word)                { return spv[word]; }
const spv::Id&  asId(unsigned word)          const { return spv[word]; }
spv::Op         asOpCode(unsigned word)      const { return opOpCode(spv[word]); }
std::uint32_t   asOpCodeHash(unsigned word);
spv::Decoration asDecoration(unsigned word)  const { return spv::Decoration(spv[word]); }
unsigned        asWordCount(unsigned word)   const { return opWordCount(spv[word]); }
spv::Id         asTypeConstId(unsigned word) const { return asId(word + (isTypeOp(asOpCode(word)) ? 1 : 2)); }
unsigned        idPos(spv::Id id)            const;
static unsigned opWordCount(spirword_t data) { return data >> spv::WordCountShift; }
static spv::Op  opOpCode(spirword_t data)    { return spv::Op(data & spv::OpCodeMask); }
spirword_t  magic()    const       { return spv[0]; }
spirword_t  bound()    const       { return spv[3]; }
spirword_t  bound(spirword_t b)    { return spv[3] = b; }
spirword_t  genmagic() const       { return spv[2]; }
spirword_t  genmagic(spirword_t m) { return spv[2] = m; }
spirword_t  schemaNum() const      { return spv[4]; }
spv::Id     localId(spv::Id id) const { return idMapL[id]; }
inline spv::Id   localId(spv::Id id, spv::Id newId);
void             countIds(spv::Id id);
inline spv::Id   nextUnusedId(spv::Id id);
void buildLocalMaps();
std::string literalString(unsigned word) const;
int literalStringWords(const std::string& str) const { return (int(str.size())+4)/4; }
bool isNewIdMapped(spv::Id newId)   const { return isMapped(newId);            }
bool isOldIdUnmapped(spv::Id oldId) const { return localId(oldId) == unmapped; }
bool isOldIdUnused(spv::Id oldId)   const { return localId(oldId) == unused;   }
bool isOldIdMapped(spv::Id oldId)   const { return !isOldIdUnused(oldId) && !isOldIdUnmapped(oldId); }
bool isFunction(spv::Id oldId)      const { return fnPos.find(oldId) != fnPos.end(); }
std::uint32_t hashType(unsigned typeStart) const;
spirvbin_t& process(instfn_t, idfn_t, unsigned begin = 0, unsigned end = 0);
int         processInstruction(unsigned word, instfn_t, idfn_t);
void        validate() const;
void        mapTypeConst();
void        mapFnBodies();
void        optLoadStore();
void        dceFuncs();
void        dceVars();
void        dceTypes();
void        mapNames();
void        foldIds();
void        forwardLoadStores();
void        offsetIds();
void        applyMap();
void        mapRemainder();
void        stripDebug();
void        stripDeadRefs();
void        strip();
std::vector<spirword_t> spv;
std::vector<std::string> stripWhiteList;
namemap_t               nameMap;
typedef std::uint64_t bits_t;
std::vector<bits_t> mapped;
static const int mBits = sizeof(bits_t) * 4;
bool isMapped(spv::Id id) const  { return id < maxMappedId() && ((mapped[id/mBits] & (1LL<<(id%mBits))) != 0); }
void setMapped(spv::Id id) { resizeMapped(id); mapped[id/mBits] |= (1LL<<(id%mBits)); }
void resizeMapped(spv::Id id) { if (id >= maxMappedId()) mapped.resize(id/mBits+1, 0); }
size_t maxMappedId() const { return mapped.size() * mBits; }
void stripInst(unsigned start) { stripRange.push_back(range_t(start, start + asWordCount(start))); }
std::unordered_map<spv::Id, range_t> fnPos;
std::unordered_map<spv::Id, int> fnCalls;
posmap_t       typeConstPos;
posmap_rev_t   idPosR;
typesize_map_t idTypeSizeMap;
std::vector<spv::Id>  idMapL;
spv::Id entryPoint;
spv::Id largestNewId;
std::vector<range_t> stripRange;
std::uint32_t options;
int           verbose;
mutable bool errorLatch;
static errorfn_t errorHandler;
static logfn_t   logHandler;
};
}
#endif