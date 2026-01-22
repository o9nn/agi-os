#pragma src "/sys/src/9/port"
#pragma lib "libcognitive.a"
typedef struct CogSegment CogSegment;
typedef struct CogMemory CogMemory;
enum {
CogMemNormal = 0,
CogMemHypergraph = 1,
CogMemPattern = 2,
CogMemAttention = 4,
CogMemShared = 8,
CogMemPersist = 16,
};
struct CogSegment {
ulong base;
ulong size;
int attrs;
void *atomspace;
Lock;
};
struct CogMemory {
CogSegment **segs;
int nsegs;
int maxsegs;
ulong totalsize;
ulong usedsize;
Lock;
};
CogMemory* cogmeminit(void);
void cogmemfree(CogMemory *cm);
CogSegment* cogmemalloc(CogMemory *cm, ulong size, int attrs);
void cogmemsegfree(CogMemory *cm, CogSegment *seg);
void* cogmemmap(CogSegment *seg, ulong offset, ulong len);
void cogmemunmap(CogSegment *seg, void *addr, ulong len);
typedef struct CogMemStats CogMemStats;
struct CogMemStats {
ulong totalsegs;
ulong hypergraphsegs;
ulong patternsegs;
ulong attentionsegs;
ulong totalmem;
ulong usedmem;
ulong freemem;
};
void cogmemstats(CogMemory *cm, CogMemStats *stats);
int cogmempagefault(void *addr, int write);
void coghypergraphopt(CogSegment *seg);
typedef struct CogPatternCache CogPatternCache;
struct CogPatternCache {
void *patterns;
int npatterns;
ulong hits;
ulong misses;
};
CogPatternCache* cogpatcacheinit(CogSegment *seg);
void cogpatcachefree(CogPatternCache *cache);
void* cogpatcachelookup(CogPatternCache *cache, void *key);
void cogpatcacheinsert(CogPatternCache *cache, void *key, void *val);
enum {
AttentionThresholdHigh = 100,
AttentionThresholdMid = 50,
AttentionThresholdLow = 10,
};
typedef struct AttentionAlloc AttentionAlloc;
struct AttentionAlloc {
short threshold;
ulong allocated;
ulong freed;
};
AttentionAlloc* attallocinit(CogSegment *seg);
void attallocfree(AttentionAlloc *aa);
void* attalloc(AttentionAlloc *aa, ulong size, short sti);
void attfree(AttentionAlloc *aa, void *addr);
typedef struct CogGC CogGC;
struct CogGC {
ulong collected;
ulong scanned;
ulong retained;
};
void coggcrun(CogMemory *cm, CogGC *stats);
void coggcmark(void *obj);
void coggcsweep(CogMemory *cm);