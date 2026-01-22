#define ffz(word)	(ffs (~(unsigned int) (word)) - 1)
static int nibblemap[] = {4, 3, 3, 2, 3, 2, 2, 1, 3, 2, 2, 1, 2, 1, 1, 0};
static inline
unsigned long count_free (unsigned char *map, unsigned int numchars)
{
unsigned int i;
unsigned long sum = 0;
if (!map)
return (0);
for (i = 0; i < numchars; i++)
sum += nibblemap[map[i] & 0xf] +
nibblemap[(map[i] >> 4) & 0xf];
return (sum);
}
static inline uint32_t
find_next_zero_bit(void *addr, unsigned long size, unsigned long offset)
{
uint32_t *p = ((uint32_t *) addr) + (offset >> 5);
unsigned long result = offset & ~31UL;
uint32_t tmp;
if (offset >= size)
return size;
size -= result;
offset &= 31UL;
if (offset)
{
tmp = *(p++);
if (offset)
tmp |= ~0UL >> (32-offset);
if (size < 32)
goto found_first;
if (~tmp)
goto found_middle;
size -= 32;
result += 32;
}
while (size & ~31UL)
{
if (~(tmp = *(p++)))
goto found_middle;
result += 32;
size -= 32;
}
if (!size)
return result;
tmp = *p;
found_first:
tmp |= ~0UL << size;
if (!~tmp)
return result + size;
found_middle:
return result + ffz(tmp);
}
static inline int
find_first_zero_bit(void *buf, unsigned len)
{
return find_next_zero_bit(buf, len, 0);
}