from __future__ import annotations
import array
import unicodedata
import requests
MAX_CODEPOINTS = 1114112
UNICODE_DATA_URL = 'https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt'
def unicode_data_iter():
    res = requests.get(UNICODE_DATA_URL)
    res.raise_for_status()
    data = res.content.decode()
    prev = []
    for line in data.splitlines():
        line = line.split(';')
        cpt = int(line[0], base=16)
        assert cpt < MAX_CODEPOINTS
        cpt_lower = int(line[-2] or '0', base=16)
        assert cpt_lower < MAX_CODEPOINTS
        cpt_upper = int(line[-3] or '0', base=16)
        assert cpt_upper < MAX_CODEPOINTS
        categ = line[2].strip()
        assert len(categ) == 2
        bidir = line[4].strip()
        assert len(categ) == 2
        name = line[1]
        if name.endswith(', First>'):
            prev = (cpt, cpt_lower, cpt_upper, categ, bidir)
            continue
        if name.endswith(', Last>'):
            assert prev[1:] == (0, 0, categ, bidir)
            for c in range(prev[0], cpt):
                yield (c, cpt_lower, cpt_upper, categ, bidir)
        yield (cpt, cpt_lower, cpt_upper, categ, bidir)
CODEPOINT_FLAG_UNDEFINED = 1
CODEPOINT_FLAG_NUMBER = 2
CODEPOINT_FLAG_LETTER = 4
CODEPOINT_FLAG_SEPARATOR = 8
CODEPOINT_FLAG_MARK = 16
CODEPOINT_FLAG_PUNCTUATION = 32
CODEPOINT_FLAG_SYMBOL = 64
CODEPOINT_FLAG_CONTROL = 128
UNICODE_CATEGORY_TO_FLAG = {'Cn': CODEPOINT_FLAG_UNDEFINED, 'Cc': CODEPOINT_FLAG_CONTROL, 'Cf': CODEPOINT_FLAG_CONTROL, 'Co': CODEPOINT_FLAG_CONTROL, 'Cs': CODEPOINT_FLAG_CONTROL, 'Ll': CODEPOINT_FLAG_LETTER, 'Lm': CODEPOINT_FLAG_LETTER, 'Lo': CODEPOINT_FLAG_LETTER, 'Lt': CODEPOINT_FLAG_LETTER, 'Lu': CODEPOINT_FLAG_LETTER, 'L&': CODEPOINT_FLAG_LETTER, 'Mc': CODEPOINT_FLAG_MARK, 'Me': CODEPOINT_FLAG_MARK, 'Mn': CODEPOINT_FLAG_MARK, 'Nd': CODEPOINT_FLAG_NUMBER, 'Nl': CODEPOINT_FLAG_NUMBER, 'No': CODEPOINT_FLAG_NUMBER, 'Pc': CODEPOINT_FLAG_PUNCTUATION, 'Pd': CODEPOINT_FLAG_PUNCTUATION, 'Pe': CODEPOINT_FLAG_PUNCTUATION, 'Pf': CODEPOINT_FLAG_PUNCTUATION, 'Pi': CODEPOINT_FLAG_PUNCTUATION, 'Po': CODEPOINT_FLAG_PUNCTUATION, 'Ps': CODEPOINT_FLAG_PUNCTUATION, 'Sc': CODEPOINT_FLAG_SYMBOL, 'Sk': CODEPOINT_FLAG_SYMBOL, 'Sm': CODEPOINT_FLAG_SYMBOL, 'So': CODEPOINT_FLAG_SYMBOL, 'Zl': CODEPOINT_FLAG_SEPARATOR, 'Zp': CODEPOINT_FLAG_SEPARATOR, 'Zs': CODEPOINT_FLAG_SEPARATOR}
codepoint_flags = array.array('H', [CODEPOINT_FLAG_UNDEFINED]) * MAX_CODEPOINTS
table_whitespace = []
table_lowercase = []
table_uppercase = []
table_nfd = []
for cpt, cpt_lower, cpt_upper, categ, bidir in unicode_data_iter():
    char = chr(cpt)
    codepoint_flags[cpt] = UNICODE_CATEGORY_TO_FLAG[categ]
    if cpt_lower:
        table_lowercase.append((cpt, cpt_lower))
    if cpt_upper:
        table_uppercase.append((cpt, cpt_upper))
    norm = ord(unicodedata.normalize('NFD', char)[0])
    if cpt != norm:
        table_nfd.append((cpt, norm))
table_whitespace.extend(range(9, 13 + 1))
table_whitespace.extend(range(8192, 8202 + 1))
table_whitespace.extend([32, 133, 160, 5760, 8232, 8233, 8239, 8287, 12288])
table_whitespace.sort()
table_lowercase.sort()
table_uppercase.sort()
table_nfd.sort()
ranges_flags: list[tuple[int, int]] = [(0, codepoint_flags[0])]
for codepoint, flags in enumerate(codepoint_flags):
    if flags != ranges_flags[-1][1]:
        ranges_flags.append((codepoint, flags))
ranges_flags.append((MAX_CODEPOINTS, 0))
ranges_nfd: list[tuple[int, int, int]] = [(0, 0, 0)]
for codepoint, norm in table_nfd:
    start = ranges_nfd[-1][0]
    if ranges_nfd[-1] != (start, codepoint - 1, norm):
        ranges_nfd.append(None)
        start = codepoint
    ranges_nfd[-1] = (start, codepoint, norm)
def out(line=''):
    print(line, end='\n')
out('// generated with scripts/gen-unicode-data.py\n\n#include "unicode-data.h"\n\n#include <cstdint>\n#include <vector>\n#include <unordered_map>\n#include <unordered_set>\n')
out('const std::vector<std::pair<uint32_t, uint16_t>> unicode_ranges_flags = {  // start, flags // last=next_start-1')
for codepoint, flags in ranges_flags:
    out('{0x%06X, 0x%04X},' % (codepoint, flags))
out('};\n')
out('const std::unordered_set<uint32_t> unicode_set_whitespace = {')
for codepoint in table_whitespace:
    out('0x%06X,' % codepoint)
out('};\n')
out('const std::unordered_map<uint32_t, uint32_t> unicode_map_lowercase = {')
for tuple_lw in table_lowercase:
    out('{0x%06X, 0x%06X},' % tuple_lw)
out('};\n')
out('const std::unordered_map<uint32_t, uint32_t> unicode_map_uppercase = {')
for tuple_up in table_uppercase:
    out('{0x%06X, 0x%06X},' % tuple_up)
out('};\n')
out('const std::vector<range_nfd> unicode_ranges_nfd = {  // start, last, nfd')
for triple in ranges_nfd:
    out('{0x%06X, 0x%06X, 0x%06X},' % triple)
out('};\n')